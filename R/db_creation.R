library(readr)
library(purrr)
library(RSQLite)

# Define paths
database_path <- "DATABASE/ecom.db"
csv_files_path <- "MOCKDATA"

# If database file does not exist, create file and tables
if (!file.exists(database_path)) {
  
  ecom_db <- dbConnect(RSQLite::SQLite(), database_path)
  
  create_statements <- c(
    "CREATE TABLE customer (
      customer_id VARCHAR(100) PRIMARY KEY NOT NULL,
      first_name VARCHAR(60) NOT NULL,
      last_name VARCHAR(60) NOT NULL,
      date_of_birth DATE NOT NULL,
      customer_country VARCHAR(40) NOT NULL,
      customer_email VARCHAR(50) NOT NULL,
      customer_phone VARCHAR(30) NOT NULL,
      customer_unit VARCHAR(10),
      customer_floor INT,
      customer_block VARCHAR(20),
      customer_street VARCHAR(100) NOT NULL,
      customer_city VARCHAR(60),
      customer_zip_code VARCHAR(20) NOT NULL,
      UNIQUE(customer_phone),
      UNIQUE(customer_email)
    );",
    "CREATE TABLE supplier (
      supplier_id VARCHAR(100) PRIMARY KEY NOT NULL,
      supplier_name VARCHAR(255) NOT NULL,
      supplier_country VARCHAR(40) NOT NULL,
      supplier_email VARCHAR(50) NOT NULL,
      supplier_phone VARCHAR(30) NOT NULL,
      supplier_unit VARCHAR(10),
      supplier_floor INT,
      supplier_block VARCHAR(20),
      supplier_street VARCHAR(100) NOT NULL,
      supplier_city VARCHAR(60),
      supplier_zip_code VARCHAR(20) NOT NULL,
      UNIQUE(supplier_phone),
      UNIQUE(supplier_email)
    );",
    "CREATE TABLE category (
      category_id VARCHAR(100) PRIMARY KEY NOT NULL,
      category_name VARCHAR(20) NOT NULL,
      category_description TEXT,
      parent_category_id VARCHAR(100),
      FOREIGN KEY (parent_category_id) REFERENCES category(category_id)
    );",
    "CREATE TABLE product (
      product_id VARCHAR(100) PRIMARY KEY NOT NULL,
      supplier_id VARCHAR(100) NOT NULL,
      category_id VARCHAR(100) NOT NULL,
      product_name VARCHAR(60) NOT NULL,
      product_description VARCHAR(255),
      product_price DECIMAL(10, 2) NOT NULL,
      product_stock INT NOT NULL,
      rating DECIMAL(3, 2),
      FOREIGN KEY (supplier_id) REFERENCES supplier(supplier_id),
      FOREIGN KEY (category_id) REFERENCES category(category_id)
    );",
    "CREATE TABLE advertisement (
      ad_id VARCHAR(100) PRIMARY KEY NOT NULL,
      product_id VARCHAR(100) NOT NULL,
      ad_url VARCHAR(255),
      ad_start_date DATE,
      ad_end_date DATE,
      discount DECIMAL(6, 4),
      FOREIGN KEY (product_id) REFERENCES product(product_id),
      UNIQUE (product_id)
    );",
    "CREATE TABLE orders (
      order_id VARCHAR(100) PRIMARY KEY NOT NULL,
      customer_id VARCHAR(100) NOT NULL,
      order_date DATE NOT NULL,
      status_of_order TEXT CHECK (status_of_order IN ('Processed', 'Cancelled')) NOT NULL,
      FOREIGN KEY (customer_id) REFERENCES customer(customer_id)
    );",
    "CREATE TABLE order_details (
      order_id VARCHAR(100) NOT NULL,
      product_id VARCHAR(100) NOT NULL,
      quantity INT NOT NULL,
      FOREIGN KEY (order_id) REFERENCES orders(order_id),
      FOREIGN KEY (product_id) REFERENCES product(product_id),
      PRIMARY KEY (order_id, product_id)
    );",
    "CREATE TABLE delivery (
      delivery_id VARCHAR(100) PRIMARY KEY NOT NULL,
      order_id VARCHAR(100) NOT NULL,
      delivery_type TEXT CHECK (delivery_type IN ('Standard', 'Premium')) NOT NULL,
      dispatched_date DATE,
      delivered_date DATE,
      delivery_status TEXT CHECK (delivery_status IN ('Pending', 'Shipped', 'Delivered')) NOT NULL,
      delivery_cost DECIMAL(6,2) NOT NULL,
      FOREIGN KEY (order_id) REFERENCES orders(order_id)
    );",
    "CREATE TABLE transactions (
      transaction_id VARCHAR(100) PRIMARY KEY NOT NULL,
      order_id VARCHAR(100) NOT NULL,
      transaction_date DATE NOT NULL,
      payment_type TEXT CHECK (payment_type IN ('Debit Card', 'Credit Card')) NOT NULL,
      card_number VARCHAR(16) NOT NULL,
      expiry_date DATE,
      cvv CHAR(3),
      payment_status TEXT CHECK (payment_status IN ('Pending', 'Processing', 'Processed')) NOT NULL,
      FOREIGN KEY (order_id) REFERENCES orders(order_id)
    );"
  )
  
  for (statement in create_statements) {
    dbExecute(ecom_db, statement)
  }
}

# Create vector with table names
table_names <- c("customer", "supplier", "category", "product", "advertisement", "orders", "order_details", "delivery", "transactions")

# Read mock data
mock <- purrr::map(table_names, ~ read_csv(file.path(csv_files_path, paste0(.x, ".csv")))) %>% 
  purrr::set_names(table_names)

# Foreign key check

# Keep a record of number of rows
test1 <- purrr::map_int(mock, ~ nrow(.))

# For tables with FK, filter rows that have a FK not present as PK in the Primary Table
mock$product <- mock$product %>%
  dplyr::filter(supplier_id %in% mock$supplier$supplier_id,
         category_id %in% mock$category$category_id)

mock$advertisement <- mock$advertisement %>%
  dplyr::filter(product_id %in% mock$product$product_id)

mock$orders <- mock$orders %>%
  dplyr::filter(customer_id %in% mock$customer$customer_id)

mock$order_details <- mock$order_details %>%
  dplyr::filter(order_id %in% mock$orders$order_id,
         product_id %in% mock$product$product_id)

mock$delivery <- mock$deliver %>%
  dplyr::filter(order_id %in% mock$orders$order_id)

mock$transactions <- mock$transactions %>%
  dplyr::filter(order_id %in% mock$orders$order_id)
  
test2 <- map_int(mock, ~ nrow(.))

# Check for differences (0 means no rows dropped because of FK problems)
print(test1 - test2)

# Connect to database
ecom_db <- dbConnect(RSQLite::SQLite(), database_path)

# Write data to database tables
for (table_name in table_names) {
  for (i in 1:nrow(mock[[table_name]])) {
    # Attempt to write each row
    tryCatch({
      dbWriteTable(ecom_db, name = table_name, value = mock[[table_name]][i, ], append = TRUE, overwrite = FALSE, row.names = FALSE)
    }, error = function(e) {
      # Log the error
      cat("Error writing row ", i, ": ", conditionMessage(e), "\n", sep = "")
    })
  }
}

# Disconnect from the database
dbDisconnect(ecom_db)
