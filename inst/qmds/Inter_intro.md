<!-- Conditionally render the section on interdisciplinarity -->
```{r hide_inter_section}
#| results: asis

if (length(applicant$internationalization) > 1) {
readLines(system.file("qmds/Inter_intro.md", package="RESQUER"))
}
```


# Internationality and Interdisciplinarity

::: {.callout-note title="Methods: Computation of Internationality and Interdisciplinarity" collapse="true"}
The analysis is only based on the submitted publications (not the entire publication list) of the applicant. Publication and co-author data is retrieved from the OpenAlex data base. Note that preprints are not indexed by OpenAlex and therefore do not contribute to this analysis.

- *Internationality*: All co-authors are retrieved from OpenAlex with their current affiliation. The index is measured by Pielou's Evenness Index (Pielou 1966) of the country codes of all co-authors. It considers the 10 most frequent country codes.
- *Interdisciplinarity* is measured by the Evenness Index of the fields (as classified by OpenAlex) of the publications. It considers the 6 most frequent fields.

The evenness indexes are normalized to a scale from 0 (no diversity, everything is one category) to 1 (maximum diversity, all categories are equally represented). It is computed as a normalized Shannon entropy.
:::
