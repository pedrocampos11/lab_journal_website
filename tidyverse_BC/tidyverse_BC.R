# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)


# 2.0 Importing Files ----
bikes_tbl <- read_excel(path = "tidyverse_BC/bikes.xlsx")
orderlines_tbl <- read_excel("tidyverse_BC/orderlines.xlsx")
bikeshop_tbl <- read_excel("tidyverse_BC/bikeshops.xlsx")



# 3.0 Examining Data ----
orderlines_tbl
glimpse(orderlines_tbl)


# 4.0 Joining Data ----
left_join(orderlines_tbl, bikes_tbl, by=c("product.id" = "bike.id"))

bike_orderlines_joined_tbl<-orderlines_tbl%>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id"))%>%
  left_join(bikeshop_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl%>%glimpse()


# 5.0 Wrangling Data ----
bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col = category,
           into = c("category.1", "category.2", "category.3"),
           sep = " - ")%>%

  mutate(total.price = price*quantity)%>%
  
  select(-...1, -gender)%>%
  
  select(-ends_with(".id"))%>%
  
  bind_cols(bike_orderlines_joined_tbl%>%select(order.id))%>%
  
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything())%>%
  
  rename(bikeshop = name)%>%
  set_names(names(.)%>%str_replace_all("\\.", "_"))


# 6.0 Business Insights ----
# 6.1 Sales by Year ----
# Step 1 - Manipulate
library(lubridate)

sales_by_year_tbl<-bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price)%>%
  mutate(year = year(order_date))%>%
  group_by(year)%>%
  summarize(sales=sum(total_price))%>%
  mutate(sales_text=scales::dollar(sales, big.mark = ".",
                                   decimal.mark = ",",
                                   prefix = "",
                                   suffix = " €"))

sales_by_year_tbl





# Step 2 - Visualize
sales_by_year_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = year, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )



# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate
sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  select(order_date, total_price, category_1) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_cat_1_tbl  

# Step 2 - Visualize
sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ category_1) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )

  



# 7.0 Writing Files ----

install.packages("writexl")
library("writexl")
bike_orderlines_wrangled_tbl %>%
  write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")