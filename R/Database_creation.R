library(readr)
library(RSQLite)
library(dplyr)
library(DBI)
library(DBI)
library(RSQLite)

ecom_db <- dbConnect(RSQLite::SQLite(), "/cloud/project/DATABASE/ecom.db")

tables_to_drop <- c("advertisement", "order_details", "orders", "delivery", "transactions", "product", "customer", "supplier", "category", "address", "payment", "product_supplier")
for (table in tables_to_drop) {
  dbExecute(ecom_db, paste0("DROP TABLE IF EXISTS ", table, ";"))
}

create_statements <- c(
  "CREATE TABLE customer (
    customer_id VARCHAR(100) PRIMARY KEY NOT NULL,
    first_name VARCHAR(60) NOT NULL,
    last_name VARCHAR(60) NOT NULL,
    date_of_birth DATE NOT NULL,
    customer_phone VARCHAR(30) NOT NULL,
    customer_email VARCHAR(50) NOT NULL,
    cutomer_unit VARCHAR(10),
    cutomer_floor INT,
    cutomer_block VARCHAR(20),
    cutomer_street VARCHAR(100) NOT NULL,
    cutomer_city VARCHAR(60),
    cutomer_country VARCHAR(40) NOT NULL,
    cutomer_zip_code VARCHAR(20) NOT NULL
  );",
  "CREATE TABLE supplier (
    supplier_id VARCHAR(100) PRIMARY KEY NOT NULL,
    supplier_name VARCHAR(255) NOT NULL,
    supplier_phone VARCHAR(30) NOT NULL,
    supplier_email VARCHAR(50) NOT NULL,
    supplier_unit VARCHAR(10),
    supplier_floor INT,
    supplier_block VARCHAR(20),
    supplier_street VARCHAR(100) NOT NULL,
    supplier_city VARCHAR(60),
    supplier_country VARCHAR(40) NOT NULL,
    supplier_zip_code VARCHAR(20) NOT NULL
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
    is_discounted BOOLEAN,
    FOREIGN KEY (supplier_id) REFERENCES supplier(supplier_id),
    FOREIGN KEY (category_id) REFERENCES category(category_id)
  );",
  "CREATE TABLE advertisement (
    ad_id VARCHAR(100) PRIMARY KEY NOT NULL,
    product_id VARCHAR(100) NOT NULL,
    url VARCHAR(255),
    ad_start_date DATE,
    ad_end_date DATE,
    discount DECIMAL(5, 4),
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
    quantity_of_product INT NOT NULL,
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
    transactions_id VARCHAR(100) PRIMARY KEY NOT NULL,
    order_id VARCHAR(100) NOT NULL,
    transaction_date DATE NOT NULL,
    payment_type TEXT CHECK (payment_type IN ('Debit/Credit Card')) NOT NULL,
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



dbDisconnect(ecom_db)

