# DATA WRANGLING ASSIGNMENT 1

# 0. Bring in file and libraries
my_data <- read.csv("refine_original.csv",header=TRUE, 
                    quote="\"",
                    stringsAsFactors = TRUE,
                    strip.white = TRUE)
library ("tidyr")
library ("dplyr")

# store refine_original.csv as my_data
my_data<-refine_original.csv

# convert data to local data frame
tbl_df(my_data)

# 1. Clean up brand names
#    First convert company data to all lower case
my_data$company <- tolower(my_data$company)

#   Next substitute to consistent names
my_data$company <- sub("^p.*","philips",my_data$company)
my_data$company <- sub("^f.*","philips",my_data$company)
my_data$company <- sub("^a.*","akzo",my_data$company)
my_data$company <- sub("^v.*","van houten",my_data$company)
my_data$company <- sub("^u.*","unilever",my_data$company)


# 2. Separate product code and number into "product_code" and "product_number"
my_data <-separate(data=my_data, col=Product.code...number, 
                   into=c("product_code","product_number"),sep="-")

# 3. Add product categories
#     first create lookup function
lookup_prod_cat <- function(product_code){
                if(product_code == "p"){
                  return ("Smartphone")
              } else if (product_code == "v") {
                  return ("TV")
              } else if (product_code == "x") {
                  return ("Laptop")
              } else if (product_code == "q") {
                  return ("Tablet")
              }
}

#     next use mutate to add Product_Category column
my_data <- my_data %>% 
  mutate (Product_Category = sapply (product_code,lookup_prod_cat))

# 4. Add full_address for geo-coding
my_data <- my_data %>% 
  mutate (full_address = paste(address, city, country, sep= ", "))

# 5. Create dummy variables for company and product
my_data <- my_data %>% 
  rowwise() %>% 
  mutate(company_philips = ifelse(company == 'philips',1,0),
        company_akzo = ifelse(company == 'akzo',1,0),
        company_van_houten = ifelse(company == 'van houten',1,0),
        company_unilever = ifelse(company == 'unilever',1,0))

my_data <- my_data %>% 
  rowwise() %>% 
  mutate(product_smartphone = ifelse(product_code == 'p',1,0),
        product_tv = ifelse(product_code == 'v',1,0),
        product_laptop= ifelse(product_code == 'x',1,0),
        product_tablet = ifelse(product_code == 'q',1,0))

glimpse(my_data)

# Write cleaned table into a csv file
write.csv(my_data,"refine_clean.csv", row.names = FALSE)