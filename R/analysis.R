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

# List of ggplot objects
ggplot_objects <- list(
  top_cities = top_cities,
  sales_by_categories = sales_by_categories,
  avg_fulfillment = avg_fulfillment,
  avg_order_value = avg_order_value,
  top_rated_products = top_rated_products
)

# Export each ggplot object as PNG with specified DPI
for (plot_name in names(ggplot_objects)) {
  filename <- paste("Analysis/Plots/", plot_name, "_", max_year, ".png", sep = "")
  ggsave(filename = filename, plot = ggplot_objects[[plot_name]], dpi = 300)
}
