library(RSQLite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(forcats)
library(scales)

### Read database into tibbles ###

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "DATABASE/ecom.db")

# Get a list of tables in the database
tables <- dbListTables(con)

# Read tables into tibbles and name them
tibbles_list <- list()

for (table_name in tables) {
  tibbles_list[[table_name]] <- dbReadTable(con, table_name) %>% as_tibble()
}

# Close the database connection
dbDisconnect(con)

# Print out the tibbles
tibbles_list

### Analysis ###

# Sales by location in the last quarter:
# Latest year available:
max_year <- tibbles_list$orders %>% 
  summarise(max = max(year(dmy(order_date)))) %>% 
  pull()

# Top 10 cities by sales in current year
top_cities <- tibbles_list$order_details %>% 
  left_join(tibbles_list$product,
            by = "product_id") %>% 
  mutate(prod_sale = quantity * product_price) %>% 
  summarise(order_sale = sum(prod_sale),
            .by = order_id) %>% 
  right_join(tibbles_list$customer %>% 
              right_join(tibbles_list$orders %>% 
                          filter(year(dmy(order_date)) == max_year,
                                 status_of_order == "Processed"),
                        by = "customer_id"),
            by = "order_id") %>%
  summarise(city_sale = sum(order_sale, na.rm = T),
            .by = customer_city) %>%
  slice_max(n = 10, order_by = city_sale) %>% 
  ggplot( aes(x = city_sale, y = fct_reorder(customer_city, city_sale))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K")) +
  labs(title = paste0("Top 10 Cities by Sales (£), ", max_year),
       x = "Sales",
       y = "City") +
  theme(axis.text.y = element_text(size = 8))

# Sales by categories in current year
sales_by_categories <- tibbles_list$orders %>% 
  filter(year(dmy(order_date)) == max_year,
         status_of_order == "Processed") %>% 
  inner_join(tibbles_list$order_details,
             by = "order_id") %>% 
  left_join(tibbles_list$product,
            by = "product_id") %>% 
  mutate(prod_sale = quantity * product_price) %>% 
  summarise(cat_sale = sum(prod_sale),
            .by = category_id) %>% 
  left_join(tibbles_list$category,
            by = "category_id") %>% 
  ggplot(aes(x = cat_sale, y = fct_reorder(category_name, cat_sale))) +
  geom_bar(stat = "identity", fill = "tomato") +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K")) +
  labs(title = paste0("Sales (£) by Category, ", max_year),
       x = "Sales",
       y = "Category") +
  theme(axis.text.y = element_text(size = 8))

# Avg Monthly fulfillment type in current year
avg_fulfillment <- tibbles_list$orders %>% 
  filter(year(dmy(order_date)) == max_year,
         status_of_order == "Processed") %>% 
  left_join(tibbles_list$delivery,
            by = "order_id") %>% 
  mutate(ful_time = as.numeric(ymd(delivered_date) - dmy(order_date)),
         quarter = paste0("Q", quarter(dmy(order_date)))) %>% 
  summarise(mean_ful_time = mean(ful_time, na.rm = T),
            .by = quarter) %>% 
  ggplot(aes(x = quarter, y = mean_ful_time)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = paste0("Average time until order fulfillment, ", max_year),
       x = "Quarter",
       y = "Days") +
  theme(axis.text.y = element_text(size = 8))

# Avg order value in current year
avg_order_value <- tibbles_list$orders %>% 
  filter(year(dmy(order_date)) == max_year,
         status_of_order == "Processed") %>% 
  inner_join(tibbles_list$order_details,
             by = "order_id") %>% 
  left_join(tibbles_list$product,
            by = "product_id") %>% 
  mutate(prod_sale = quantity * product_price,
         quarter = paste0("Q", quarter(dmy(order_date)))) %>% 
  summarise(order_sale = sum(prod_sale, na.rm = T),
            number_of_sales = n(),
            .by = quarter) %>% 
  mutate(avg_sale = order_sale / number_of_sales) %>% 
  ggplot(aes(x = quarter, y = avg_sale)) +
  geom_bar(stat = "identity", fill = "gray10") +
  labs(title = paste0("Average order value (£), ", max_year),
       x = "Quarter",
       y = "Pounds") +
  theme(axis.text.y = element_text(size = 8))

# Top 10 highest-rated products
top_rated_products <- tibbles_list$product %>% 
  summarise(avg_rating = mean(rating, na.rm = TRUE),
            .by = category_id)  %>% 
  left_join(tibbles_list$category,
            by = "category_id") %>% 
  ggplot(aes(x = avg_rating, y = category_name)) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = paste0("Average Rating by Category, ", max_year),
       x = "Average Rating",
       y = "Category") +
  theme(axis.text.y = element_text(size = 8))

####

# Join order_details with orders to get order date
orders_joined <- inner_join(tibbles_list[["order_details"]], tibbles_list[["orders"]],
                            by = "order_id")

# Join the result with products to get product price and calculate revenue
sales_data <- inner_join(orders_joined, tibbles_list[["product"]],
                         by = "product_id") %>%
  mutate(revenue = quantity * product_price) %>%
  select(order_id, order_date, revenue)

# Convert order_date from "dd/mm/yyyy" format to Date type
sales_data$order_date <- lubridate::dmy(sales_data$order_date)

# Aggregate sales data by daily, monthly, and yearly revenue
daily_sales <- sales_data %>%
  group_by(order_date) %>%
  summarise(total_revenue = sum(revenue))

monthly_sales <- sales_data %>%
  mutate(month = floor_date(order_date, "month")) %>%
  group_by(month) %>%
  summarise(total_revenue = sum(revenue))

yearly_sales <- sales_data %>%
  mutate(year = floor_date(order_date, "year")) %>%
  group_by(year) %>%
  summarise(total_revenue = sum(revenue))

# Filter for daily sales in the year 2023
daily_sales_2023 <- daily_sales %>%
  filter(order_date >= as.Date(paste0(max_year,"-01-01")) & order_date <= as.Date(paste0(max_year,"-12-31")))

# Plotting the filtered daily sales
daily_sales_2023 <- ggplot(daily_sales_2023, aes(x = order_date, y = as.numeric(total_revenue))) +
  geom_bar(stat = "identity", fill = "lightblue") + # Bar plot
  geom_line(aes(group = 1), color = "red", size = 1) + # Sales line
  geom_point(color = "darkred", size = 2) + # Dots
  labs(title = "Daily Sales Over Time in 2023",
       x = "Date",
       y = "Total Revenue")

# 'year' column in your 'monthly_sales' dataframe
monthly_sales$year <- format(as.Date(monthly_sales$month), "%Y")

# Monthly Sales Plot with Bar Plot and Line, with bars colored by year
monthly_sales <- ggplot(monthly_sales, aes(x = month, y = total_revenue, fill = factor(year))) +
  geom_bar(stat = "identity") + # Bar plot with color mapping
  geom_line(aes(group = 1, color = "Total Revenue"), size = 1) + # Sales line
  geom_point(aes(color = "Total Revenue"), size = 2) + # Dots
  scale_fill_manual(values = c("orange", "pink", "blue", "darkgreen")) + 
  scale_color_manual(values = c("Total Revenue" = "red")) + # Set line and dot colors
  labs(title = "Monthly Sales Over Time",
       x = "Month",
       y = "Total Revenue",
       fill = "Year",
       color = "Legend Title") +
  theme(legend.position = "bottom")

# Yearly Sales Plot with Bar Plot and Line, with bars colored by year
yearly_sales <- ggplot(yearly_sales, aes(x = year, y = total_revenue, fill = factor(year))) +
  geom_bar(stat = "identity") + # Bar plot with color mapping
  geom_line(aes(group = 1), color = "red", size = 1) + # Sales line
  geom_point(color = "darkred", size = 2) + # Dots
  scale_fill_brewer(palette = "Set3") + # Color palette for different years
  labs(title = "Yearly Sales Over Time",
       x = "Year",
       y = "Total Revenue") +
  theme(legend.position = "none") # Hide the legend


# Get the first purchase date for each customer
first_purchase <- tibbles_list[["orders"]] %>%
  group_by(customer_id) %>%
  summarise(first_purchase_date = min(order_date)) %>%
  ungroup()

first_purchase <- first_purchase %>%
  mutate(first_purchase_date = as.Date(first_purchase_date, format = "%d/%m/%Y"))

# Join with customer table to get the city
customer_growth <- inner_join(first_purchase, tibbles_list[["customer"]], by = "customer_id")

customer_growth <- customer_growth %>%
  group_by(first_purchase_date, customer_city, date_of_birth) %>%
  summarise(new_customers = n(), .groups = 'drop') %>%
  ungroup() %>%
  arrange(first_purchase_date, customer_city, date_of_birth)

# Age Distribution of Customers at First Purchase
customer_growth <- customer_growth %>%
  mutate(age_at_first_purchase = year(first_purchase_date) - year(dmy(date_of_birth)))

customer_age <- ggplot(customer_growth, aes(x = age_at_first_purchase)) +
  geom_histogram(binwidth = 1, fill = "lightblue") +
  labs(title = "Age Distribution of Customers",
       x = "Age",
       y = "Count of Customers")

# Adding a 'quarter' and 'year' column to the dataframe
customer_growth_quarterly <- customer_growth %>%
  mutate(quarter = paste0("Q", quarter(first_purchase_date)),
         year = year(first_purchase_date)) %>%
  arrange(year, quarter) %>%
  group_by(year, quarter) %>%
  summarise(cust_quarter = sum(new_customers, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(cumulative_customers = cumsum(cust_quarter))%>%
  mutate(year_quarter = paste(year, quarter, sep = "-")) # Combines year and quarter for plotting

# Plotting cumulative customers with bar graphs grouped by quarter and year
cumulative_customers <- ggplot(customer_growth_quarterly, aes(x = year_quarter, y = cumulative_customers, group = year)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
  labs(title = "Cumulative Customer Growth by Quarter",
       x = "Year-Quarter",
       y = "Cumulative Number of Customers") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x axis labels to avoid overlap

# List of ggplot objects
ggplot_objects <- list(
  "top_cities" = top_cities,
  "sales_by_categories" = sales_by_categories,
  "avg_fulfillment" = avg_fulfillment,
  "avg_order_value" = avg_order_value,
  "top_rated_products" = top_rated_products,
  "monthly_sales" = monthly_sales,
  "yearly_sales" = yearly_sales,
  "customer_age" = customer_age,
  "cumulative_customers" = cumulative_customers
)

# Export each ggplot object as PNG with specified DPI
for (plot_name in names(ggplot_objects)) {
  filename <- paste("Plots/", plot_name, "_", max_year, ".png", sep = "")
  ggsave(filename = filename, plot = ggplot_objects[[plot_name]], height = 5, width = 8)
}
