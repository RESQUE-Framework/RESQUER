#----------------------------------------------------------------
# Create table of publications: this is currently only used in the dashboard
# and superseded by the impact table?
#----------------------------------------------------------------

ref_list <- left_join(applicant$OAlex_papers, applicant$indicators %>% select(doi, CRediT_involvement, CRediT_involvement_roles, title_links_html, P_TopPaper_Select), by="doi") %>%
  arrange(-P_TopPaper_Select, -as.numeric(CRediT_involvement))

names_vec <- c()
for (i in 1:nrow(ref_list)) {
  names_vec <- c(names_vec, format_names(ref_list[i, ], alphabetical = TRUE))
}

ref_table <- data.frame(
  Title=paste0(ifelse(ref_list$P_TopPaper_Select, "\u2B50", ""), ref_list$title_links_html),
  Authors = names_vec,
  ref_list$CRediT_involvement,
  ref_list$CRediT_involvement_roles
)

colnames(ref_table) <- c("Title", "Authors (alphabetical)", "Candidates' CRediT involvement", "Candidates' CRediT main roles")

applicant$ref_table <- ref_table
rm(ref_table, ref_list)



