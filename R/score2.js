const scoreAll = (rs) => {
    const meta = rs[0]; // The first object is the meta object
    const researchOutputs = rs.slice(1);

    const scores = researchOutputs.map(r => score(r, meta));
    const validScores = scores.filter(s => Object.keys(s).length && s.max > 0);

    const relative = validScores.length > 0
        ? validScores.reduce((sum, s) => sum + s.relative, 0) / validScores.length
        : 0;

    // Aggregate category scores
    const overallCategoryScores = validScores.reduce((acc, s) => {
        s.categories.forEach(cat => {
            let existing = acc.find(c => c.title === cat.title);
            if (existing) {
                existing.max += cat.max;
                existing.score += cat.score;
            } else {
                acc.push({ ...cat });
            }
        });
        return acc;
    }, []);

    return {
        scores: [{}, ...scores],
        overall: {
            relative,
            percentage: (relative * 100).toFixed(1),
            categories: overallCategoryScores
        }
    };
};

const score = (r, meta) => {
    const type = r.type;
    const formDef = meta?.forms?.[type];

    const categories = meta?.forms?.config?.score_categories || [];

    if (!formDef) return {};

    let maxScore = 0;
    let reachedScore = 0;
    let itemScores = {};

    const preprocessCondition = condition => {
        let processedCondition = condition
            .replaceAll(/([\$\w]+)\s*=\|=\s*\[(.*?)\]/g, (match, variable, list) => {
                return "(" + list.split(",")
                    .map(value => `${variable} === ${value.trim()}`)
                    .join(" || ") + ")";
            })
            .replaceAll(/([\$\w]+)\s*=&=\s*\[(.*?)\]/g, (match, variable, list) => {
                return "(" + list.split(",")
                    .map(value => `${variable} === ${value.trim()}`)
                    .join(" && ") + ")";
            })

        return processedCondition;
    }

    const insertValuesIntoCondition = (condition, context) => {
        return condition
            .replaceAll(/meta\$([a-zA-Z0-9_]+)/g, `meta['$1']`)
            .replaceAll(/\$(\w*)/g, `context['$1']`);
    }

    const evaluateCondition = condition => condition ? evaluateConditionInContext(condition, r) : true;

    const evaluateConditionInContext = (condition, context) => {
        if (!condition) {
            return true;
        }

        const exists = id => !!id && id !== "";

        const c = insertValuesIntoCondition(preprocessCondition(condition), context)

        return eval(c);
    }

    // Iterate through elements defined for this type
    formDef.elements.forEach(el => {
        // Skip if there's no scoring logic or if 'not_applicable' is true
        if (el.score?.not_applicable && evaluateCondition(el.score.not_applicable)) return;

        if (el.score?.score && !evaluateCondition(el.score.condition)) return;

        let itemMax = 0;
        let itemReached = 0;

        if (el.type === 'radio') {
            // Max is the highest possible value in options
            itemMax = Math.max(...el.options.map(o => o.value || 0));
            // Check which option matches the current answer in 'r'
            const selectedOption = el.options.find(o => r[el.id] === o.id);
            if (selectedOption && evaluateCondition(el.score?.condition)) {
                itemReached = selectedOption.value || 0;
            }
        }
        else if (el.type === 'checkbox') {
            // Max is the sum of all values
            itemMax = el.options.reduce((sum, o) => sum + (o.value || 0), 0);
            // Sum values for all checked IDs (r[id_optionId] === true)
            el.options.forEach(o => {
                if (r[`${el.id}_${o.id}`] === true && evaluateCondition(el.score?.condition)) {
                    itemReached += (o.value || 0);
                }
            });
        }

        maxScore += itemMax;
        reachedScore += itemReached;
        itemScores[el.id] = { max: itemMax, score: itemReached };
    });

    // Category mapping
    const categoryScores = categories.map(cat => {
        const indicators = Object.keys(itemScores).filter(id => new RegExp(cat.cue).test(id));
        return {
            title: cat.title,
            max: indicators.reduce((s, id) => s + itemScores[id].max, 0),
            score: indicators.reduce((s, id) => s + itemScores[id].score, 0)
        };
    });

    return {
        max: maxScore,
        score: reachedScore,
        relative: maxScore > 0 ? reachedScore / maxScore : 0,
        percentage: maxScore > 0 ? ((reachedScore / maxScore) * 100).toFixed(1) : "0.0",
        items: itemScores,
        categories: categoryScores
    };
};