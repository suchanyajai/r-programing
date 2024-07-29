## load libraries
library(tidyverse)
library(glue)

yes <- "YES"

## Setup variable
dim_sum <- data.frame(
  menus_dim_sum = c(
    "Golden Cream Bun",
    "Shumai",
    "Jade Bun",
    "Minced Pork Bun",
    "BBQ Pork Bun",
    "Minced Pork Bun with Salted Egg (Large)",
    "Minced Pork Mantou with Crab Stick, Sausage and Salted Egg",
    "Pumpkin Mantou (Large)",
    "Purple Sweet Potato Mantou (Large)"
  ),
  store_price_dim_sum = c(
    23,
    39,
    23,
    23,
    23,
    33,
    29,
    25,
    25
  )
)

fried_food <- data.frame(
  menus_fried = c(
    "BBQ Chicken Wing (2 pieces)",
    "Chicken Strip",
    "Chicken Nugget",
    "Chicken Pop",
    "Spicy Chicken Pop",
    "Karaage Chicken",
    "Golden Fries"
  ),
  store_price_fried = c(
    39,
    55,
    55,
    55,
    55,
    55,
    35
  )
)

## Create function Ordering
goods_season <- function() {
  print("Welcome to GoodsSeason restaurant")
  print(dim_sum)

  is_dim_sum_order_correct <- FALSE
  dim_sum_order <- "UNKNOWN"
  while (!is_dim_sum_order_correct) {
    dim_sum_order <- readline("Your Dim-Sum Orders: ")
    if (!(dim_sum_order %in% dim_sum$menus_dim_sum)) {
      print("Sorry, We don't have your ordered menu. Please order again.")
      next
    }
    is_dim_sum_order_correct <- TRUE
  }

  print(fried_food)
  is_fried_food_order_correct <- FALSE
  fried_food_order <- "UNKNOWN"
  while (!is_fried_food_order_correct) {
    fried_food_order <- readline("Your Fried Food Orders: ")
    if (!(fried_food_order %in% fried_food$menus_fried)) {
      print("Sorry, We don't have your ordered menu. Please order again.")
      next
    }
    is_fried_food_order_correct <- TRUE
  }

  ## Get price dim-sum
  get_dim_sum_price <- function(dim_sum_order, dim_sum) {
    item_dim      <- which(dim_sum$menus_dim_sum == dim_sum_order)
    dim_sum_price <- dim_sum$store_price_dim_sum[item_dim]
    return(dim_sum_price)
  }

  ## Get price Fried Foods
  get_fried_foods_price <- function(fried_food_order, fried_food) {
    item_fried    <- which(fried_food$menus_fried == fried_food_order)
    fried_price   <- fried_food$store_price_fried[item_fried]
    return(fried_price)
  }

  ## Summary Order Details
  print(toupper(glue("Summary order ", dim_sum_order, " and ", fried_food_order))) # nolint
  print(glue(dim_sum_order, " : ", get_dim_sum_price(dim_sum_order, dim_sum), " THB")) # nolint
  print(glue(fried_food_order, " : ",get_fried_foods_price(fried_food_order, fried_food), " THB")) # nolint
  print(glue("Total Price : ", get_dim_sum_price(dim_sum_order,dim_sum) + get_fried_foods_price(fried_food_order,fried_food), " THB")) # nolint

  ##Confirm Orders
  confirm <- readline("Summit Order Now!(YES): ")
  if (confirm == yes) {
    print("Received Orders and Thank You for Comming ka :)")
  } else {
    re_order <- readline("Would you like to Re-order? (YES): ")
    if (re_order == yes) {
      goods_season()
    } else {
      print("Thank You for Comming ka :)")
    }
  }
}