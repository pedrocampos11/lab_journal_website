# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)


# 2.0 Importing Files ----
bikes_tbl <- read_excel(path = "tidyverse_challenge/bikes.xlsx")
orderlines_tbl <- read_excel("tidyverse_challenge/orderlines.xlsx")
bikeshop_tbl <- read_excel("tidyverse_challenge/bikeshops.xlsx")

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

bike_orderlines_wrangled_tblcity <- bike_orderlines_joined_tbl %>%
  separate(col = category,
           into = c("category.1", "category.2", "category.3"),
           sep = " - ")%>%
  separate(col = location,
           into = c("City", "State"),
           sep = ",")%>%

  
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
sales_by_year_tbl<-bike_orderlines_wrangled_tbl%>%
  #select columns
  select(order_date, total_price)%>%
  #add year column
  mutate(year=year(order_date))%>%
  #grouping by year and summarizing sales
  group_by(year)%>%
  summarize(sales=sum(total_price))%>%
  
  #adding a column that turns the numbers unto a currency format
  mutate(sales_text=scales::dollar(sales, big.mark = ".",
                                   decimal.mark = ",",
                                   prefix = "",
                                   suffix = "€"))
sales_by_year_tbl

# Step 2 - Visualize
sales_by_year_tbl%>%
  #setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x=year, y=sales))+
  #geometries
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
sales_by_year_state_tbl<-bike_orderlines_wrangled_tblcity%>%
  #Select columns and add a year
  select(order_date, total_price, State)%>%
  mutate(year = order_date)%>%
  
  #Group by and summarize year and main category
  group_by(year, State)%>%
  summarise(sales=sum(total_price))%>%
  ungroup()%>%
  
  #format $ text
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))
sales_by_year_state_tbl

# Step 2 - Visualize
sales_by_year_state_tbl%>%
  #set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = State))+
  #geometries
  geom_col()+
  facet_wrap(~ State) + 
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) + 
  labs(
    title="Revenue by year and State",
    subtitle = "12 States are presented", 
    fill = "Main category"
  )



# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----
