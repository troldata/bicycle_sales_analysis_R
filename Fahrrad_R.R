### Step 1. Receive initial data from database bikes PostgreSQL

#install.packages("DBI")
#install.packages('devtools')
#remotes::install_github("r-dbi/RPostgres")

library(DBI)
library(tidyverse)
library(scales)

db <- 'bikes'  # the name of the db

host_db <- 'localhost'   

db_port <- '5432'  

db_user <- "Set the own username"  

db_password <- "Set the own password"

con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  

dbListTables(con) 

query <- "Select o.order_id, o.order_date, o.store_id, oi.quantity, oi.list_price, 
  oi.discount, p.product_name, p.model_year, b.brand_name, c.category_name
from orders o join order_items oi on o.order_id=oi.order_id
	join products p on oi.product_id=p.product_id
	join brands b on p.brand_id=b.brand_id
	join categories c on p.category_id=c.category_id"

df <- dbGetQuery(con, query)

summary(df)

df$order_date <- as.Date(df$order_date)

### Step 2. Analyse of brand and categories

# Bildung Piechart (categories_name)

category_name<-unique(df$category_name)
print(category_name)

grouped_category <- df %>%
  group_by(category_name) %>%
  summarise(total = sum(quantity)) %>%
  mutate(percentage = total/sum(total) * 100) %>%
  arrange(desc(percentage))  %>%
  mutate(category_name = reorder(category_name, -percentage))
print(grouped_category)

ggplot(grouped_category, aes(x = "", y = percentage, fill = category_name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Bicycle sales by category (in %)", fill = "category_name") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) 


# Bildung Piechart (brand_name)

brand_name<-unique(df$brand_name)
print(brand_name)

grouped_brand <- df %>%
  group_by(brand_name) %>%
  summarise(total = sum(quantity)) %>%
  mutate(percentage = total/sum(total) * 100) %>%
  arrange(desc(percentage))  %>%
  mutate(brand_name = reorder(brand_name, -percentage))
print(grouped_brand)

ggplot(grouped_brand, aes(x = "", y = percentage, fill = brand_name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Bicycle sales by brand (in %)", fill = "brand_name") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5)) 

# Boxplot (bike prices of the different brands)
columns <- c("product_name", "brand_name", "list_price")

price_brand <- df %>%
  distinct(product_name, .keep_all = TRUE)%>%
  select(columns)
print(price_brand)

ggplot(price_brand, aes(x = brand_name, y = list_price, fill = brand_name)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Boxplot for each brand", x = "Brand_name", y = "Price") +
  theme_minimal()

# Find the most expensive bike
index_most_expensive <- which.max(df$list_price)
most_expensive_bike <- df[index_most_expensive, ]
print(most_expensive_bike)

# Choose the models of popular brands and categories
best_brand <- c("Electra", "Trek", "Surly", "Sun Bicycles")
best_category <- c("Сruisers Bicycles", "Mountain Bikes", "Children Bicycles", "Comfort Bicycles")
columns <- c("product_name", "brand_name", "list_price")

best_product_name <- df %>% 
  filter(category_name %in% best_category) %>%
  filter(brand_name %in%  best_brand) %>%
  distinct(product_name, brand_name, category_name, .keep_all = TRUE) %>%
  select(columns)
print(best_product_name)  # 126 models from 314 in stocks

### Step 3. Sale analyse

# Add new columns with data format YYYY-MM and with sales
df_time <- df %>%
  mutate(year_month = format(order_date, "%Y-%m")) %>%
  mutate(sales = df$list_price*(1-df$discount)*df$quantity)
print(df_time)

#Gruppen data by month_year
yearly_monthly_sales <- df_time %>%
  group_by(year_month) %>%
  summarise(total_sales = sum(sales))

new_row <- data.frame(
  year_month = "2018-05",
  total_sales = 0
)

yearly_monthly_sales <- bind_rows(yearly_monthly_sales, new_row) %>%
 arrange(year_month)%>%
 distinct() # Delete dublicates
print(n=36, yearly_monthly_sales)

ggplot(yearly_monthly_sales, aes(x = yearly_monthly_sales$year_month, y = yearly_monthly_sales$total_sales, fill = "blue")) +
  geom_col() +
  labs(title = "Sales distribution over time", x = "Month", y = "Sales (in $)") +
  scale_fill_manual(values = "blue") +  
  theme_minimal() +
  scale_y_continuous(labels = comma) +  # Change format for axis y
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels

dbDisconnect(con)

