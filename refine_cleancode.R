library(dplyr,tidyr)
##STEP 0: load CSV
refine <- tbl_df(read.csv("C:/Users/mattb/Downloads/refine_original.csv", stringsAsFactors = FALSE))

##STEP 1: Clean company names (using gsub())
refine$company <- gsub(".*l.*ps","phillips",refine$company,ignore.case = TRUE)
refine$company <- gsub("van houten","van houten",refine$company,ignore.case = TRUE)
refine$company <- gsub("un.*ver","unilever",refine$company,ignore.case = TRUE)
refine$company <- gsub("ak.*z.*","akzo",refine$company,ignore.case = TRUE)
refine$company

##STEP 2: separate product code and product number (using separate())
refine <- refine %>% separate(Product.code...number,c("product_code","product_number"),sep = "-")

##STEP 3: add product categories (using left_join())
pkey <- tbl_df(matrix(data = c("p","v","x","q","smartphone","tv","laptop","tablet"),nrow = 4, ncol = 2))
colnames(pkey) <- c("product_code","product")
refine <- left_join(refine,pkey, by = "product_code")

##STEP 4: add full address for geocoding (using unite())
refine <- refine %>% unite(full_address,c(address,city,country),sep = ", ")

##STEP 5: create dummy  variables for company and product category (using mutate())
refine <- refine %>% mutate(company_phillips = as.numeric(company == "phillips"))
refine <- refine %>% mutate(company_akzo = as.numeric(company == "akzo"))
refine <- refine %>% mutate(company_vanhouten = as.numeric(company == "van houten"))
refine <- refine %>% mutate(company_unilever = as.numeric(company == "unilever"))
refine <- refine %>% mutate(product_smartphone = as.numeric(product_code == "p"))
refine <- refine %>% mutate(product_tv = as.numeric(product_code == "v"))
refine <- refine %>% mutate(product_laptop = as.numeric(product_code == "x"))
refine <- refine %>% mutate(product_tablet = as.numeric(product_code == "q"))

##STEP 6: write CSV and publish to github
write.csv(refine,"C:/Users/mattb/Downloads/refine_clean.csv",row.names = FALSE)
