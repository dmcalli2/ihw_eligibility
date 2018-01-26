library(readr)
library(dplyr)
library(purrr)
library("dplyr")
library( extrafont )
library( tagcloud )
library( Rcpp)

load("~/ihw/data_for_network.rdata")

eligibility_trialData_final <- read_csv("Downloads/eligibility_trialData_final.csv")

names(eligibility_trialData_final)[3] <- "InclusionExclusion"
names(eligibility_trialData_final)[2] <- "nct_id"

eligibility_trialData_final %>% 
  filter(Caption == "Condition" 
         & ((InclusionExclusion == "Exclusion" & Negated == "True" ) 
            | (InclusionExclusion == "Inclusion" & Negated == "False" ) ) 
         ) -> eligibilityConditions


## NCT02414958 delimiters were not working properly when gathering the data. Need to use another delimiter. ";" not good.  I think it may apply only to caption == "Other" but need to investigate.
eligibilityConditions %>% filter(grepl("^[[:upper:]]+$", substring(Caption,1,1)) ) -> eligibilityConditions


eligibilityConditions %>% 
  group_by(CUI) %>% 
  mutate(AllTerms = paste0(Term, collapse = ";")) %>% 
  select(CUI,AllTerms) %>% 
  distinct() %>% 
  mutate( singleTerm = strsplit(AllTerms,";")[[1]][1]) %>%
  select( singleTerm) -> unifiedCUITerms


eligibilityConditions %>% 
  left_join(unifiedCUITerms) %>%
  select(singleTerm) %>% 
  group_by(singleTerm) %>%
  count(singleTerm) -> termCount

tagcloud( termCount$singleTerm, termCount$n )
dev.copy2pdf( file= paste("ihw/plots/pdfs/","general",".pdf"), out.type= "cairo" )
dev.copy(png,"ihw/plots/pics/general.png")
dev.off()

eligibilityConditions %>% left_join(unifiedCUITerms) -> joinedSet

conditions_lkp %>% select(mesh_broad_label,nct_id) %>% inner_join(joinedSet,nct_id  = nct_id) -> broadConditionsJoined

broadConditionsJoined %>% 
  select(mesh_broad_label,singleTerm) %>% 
  group_by(mesh_broad_label) %>%
  count(singleTerm) -> broadCategoriesTermCounts


broadCategoriesTermCounts %>% select(mesh_broad_label) %>% distinct() -> uniqueBroadCategories

for (row in 1:nrow(uniqueBroadCategories)) {
  uniqueBroadCategories[row, "mesh_broad_label"] -> category
  broadCategoriesTermCounts %>% filter(mesh_broad_label == category) -> res
  
  tagcloud( res$singleTerm, res$n, algorithm= "fill", scale= 0.7)
  dev.copy2pdf( file= paste("ihw/plots/pdfs/",trimws(category),".pdf"), out.type= "cairo" )
  dev.copy(png,paste("ihw/plots/pics/",trimws(category),".png"))  
  dev.off()
}

save(joinedSet, broadConditionsJoined, broadCategoriesTermCounts, uniqueBroadCategories, file="ihw/filteredEligibilityTrials.RData")

# uniqueBroadCategories %>% map( function (bcat){
#   term <- bcat$mesh_broad_label
#    print(term)
#   # broadCategoriesTermCounts %>% 
#   #    filter(mesh_broad_label == term)
# })


#dev.copy2pdf( file= "sample1.pdf", out.type= "cairo" )

# for (row in 1:nrow(eligibilityConditions)) {
#   cui <- eligibilityConditions[row, "CUI"]
#   
#   
#   
# }
