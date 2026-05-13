## Some internal documentation (needs to be properly documented)

- `$indicators` contains all data (also publications without any data).
  - `$impact_pubs` is a subset of `indicators`: contains all publications that are eligible for the impact table (i.e., papers with sufficient indicator information and papers where a manual processing was requested)
    - `$OAlex_papers` is the same set as `$impact_pubs`, but contains the full OpenAlex information
  - `$rigor_pubs` is a subset of `$indicators`: contains all publications that are eligible for the rigor score and other descriptives (i.e., papers with sufficient indicator information)

- There can be `impact_pubs` which are not `rigor_pubs`: E.g., opinion papers (which have impact metrics), but where no rigor score could be computed.
- There can be `rigor_pubs` which are not `impact_pubs`: E.g., submitted papers which have no doi (e.g., a paper on OSF; technically a preprint without a doi), but where a rigor score could be computed.