# Executive Summary Slide - Exact Layout Match
# This replicates the layout from your image with chart and table on the same slide

library(officer)
library(flextable)
library(ggplot2)
library(dplyr)
library(tidyr)

# ============================================================================
# PREPARE DATA
# ============================================================================

set.seed(123)
sales_data <- data.frame(
  Month = month.name[1:12],
  Product_A = round(rnorm(12, mean = 50000, sd = 10000)),
  Product_B = round(rnorm(12, mean = 45000, sd = 8000)),
  Product_C = round(rnorm(12, mean = 35000, sd = 7000))
)

sales_data$Total <- sales_data$Product_A + sales_data$Product_B + sales_data$Product_C

# Calculate total annual sales
total_annual_sales <- sum(sales_data$Total)

# ============================================================================
# CREATE VISUALIZATIONS
# ============================================================================

# Line chart for trends
sales_chart <- sales_data %>%
  select(Month, Product_A, Product_B, Product_C) %>%
  pivot_longer(cols = -Month, names_to = "Product", values_to = "Sales") %>%
  mutate(Month = factor(Month, levels = month.name[1:12])) %>%
  ggplot(aes(x = Month, y = Sales, color = Product, group = Product)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c"),
                     labels = c("Product A", "Product B", "Product C")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Monthly Sales Trends", x = "Month", y = "Sales ($)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Bar chart for product comparison
product_totals <- data.frame(
  Product = c("Product A", "Product B", "Product C"),
  Total_Sales = c(sum(sales_data$Product_A), 
                  sum(sales_data$Product_B), 
                  sum(sales_data$Product_C))
)

bar_chart <- ggplot(product_totals, aes(x = Product, y = Total_Sales, fill = Product)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::dollar(Total_Sales, scale = 0.001, suffix = "K")), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  scale_y_continuous(labels = scales::comma, limits = c(0, max(product_totals$Total_Sales) * 1.15)) +
  labs(title = "Total Sales by Product", x = "", y = "Sales ($)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(size = 10)
  )

# ============================================================================
# CREATE TABLE
# ============================================================================

# Summary table with key metrics
summary_table_data <- data.frame(
  Metric = c("Total Sales", "Avg Monthly", "Best Month", "Growth Rate"),
  Product_A = c(
    paste0("$", format(sum(sales_data$Product_A), big.mark = ",")),
    paste0("$", format(round(mean(sales_data$Product_A)), big.mark = ",")),
    sales_data$Month[which.max(sales_data$Product_A)],
    "+12.5%"
  ),
  Product_B = c(
    paste0("$", format(sum(sales_data$Product_B), big.mark = ",")),
    paste0("$", format(round(mean(sales_data$Product_B)), big.mark = ",")),
    sales_data$Month[which.max(sales_data$Product_B)],
    "+8.3%"
  ),
  Product_C = c(
    paste0("$", format(sum(sales_data$Product_C), big.mark = ",")),
    paste0("$", format(round(mean(sales_data$Product_C)), big.mark = ",")),
    sales_data$Month[which.max(sales_data$Product_C)],
    "+5.7%"
  )
)

formatted_table <- flextable(summary_table_data) %>%
  set_header_labels(
    Metric = "Metric",
    Product_A = "Product A",
    Product_B = "Product B",
    Product_C = "Product C"
  ) %>%
  theme_booktabs() %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#4472C4") %>%
  bold(part = "header") %>%
  bold(j = 1) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

# ============================================================================
# BUILD PRESENTATION
# ============================================================================

ppt <- read_pptx()

# ============================================================================
# SLIDE 1: EXECUTIVE SUMMARY (Matching your image)
# ============================================================================

ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")

# Title
title_fp <- fpar(ftext("Executive Summary", 
                       prop = fp_text(font.size = 44, bold = TRUE, font.family = "Calibri")))
ppt <- ph_with(ppt, value = title_fp,
               location = ph_location(left = 0.5, top = 0.5, width = 9, height = 1))

# Key Findings section
key_findings <- block_list(
  fpar(ftext("Key Findings:", prop = fp_text(bold = TRUE, font.size = 18))),
  fpar(ftext(paste0("• Total annual sales across all products: $", 
                    format(total_annual_sales, big.mark = ",")), 
             prop = fp_text(font.size = 14))),
  fpar(ftext("• Product A led with the highest average monthly sales", 
             prop = fp_text(font.size = 14))),
  fpar(ftext("• All products showed positive growth trajectory", 
             prop = fp_text(font.size = 14))),
  fpar(ftext("• Peak sales occurred in mid-to-late year period", 
             prop = fp_text(font.size = 14)))
)

ppt <- ph_with(ppt, value = key_findings,
               location = ph_location(left = 0.5, top = 1.7, width = 9, height = 2))

# Recommendations section
recommendations <- block_list(
  fpar(ftext("Recommendations:", prop = fp_text(bold = TRUE, font.size = 18))),
  fpar(ftext("• Increase inventory for Product A to meet demand", 
             prop = fp_text(font.size = 14))),
  fpar(ftext("• Investigate factors driving Product C underperformance", 
             prop = fp_text(font.size = 14))),
  fpar(ftext("• Capitalize on seasonal trends identified in Q3-Q4", 
             prop = fp_text(font.size = 14)))
)

ppt <- ph_with(ppt, value = recommendations,
               location = ph_location(left = 0.5, top = 4, width = 9, height = 1.8))

# ============================================================================
# SLIDE 2: EXECUTIVE SUMMARY WITH CHART AND TABLE
# ============================================================================

ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")

# Title
ppt <- ph_with(ppt, value = title_fp,
               location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.8))

# Key Findings (more compact)
compact_findings <- block_list(
  fpar(ftext("Key Findings:", prop = fp_text(bold = TRUE, font.size = 14))),
  fpar(ftext(paste0("• Total annual sales: $", format(total_annual_sales, big.mark = ",")), 
             prop = fp_text(font.size = 11))),
  fpar(ftext("• Product A led | All products positive growth | Peak in Q3-Q4", 
             prop = fp_text(font.size = 11)))
)

ppt <- ph_with(ppt, value = compact_findings,
               location = ph_location(left = 0.5, top = 1.2, width = 4.5, height = 1.2))

# Recommendations (compact)
compact_recs <- block_list(
  fpar(ftext("Recommendations:", prop = fp_text(bold = TRUE, font.size = 14))),
  fpar(ftext("• Increase Product A inventory", prop = fp_text(font.size = 11))),
  fpar(ftext("• Investigate Product C performance", prop = fp_text(font.size = 11))),
  fpar(ftext("• Leverage Q3-Q4 seasonal trends", prop = fp_text(font.size = 11)))
)

ppt <- ph_with(ppt, value = compact_recs,
               location = ph_location(left = 0.5, top = 2.6, width = 4.5, height = 1.3))

# Add chart
ppt <- ph_with(ppt, value = sales_chart,
               location = ph_location(left = 0.5, top = 4.1, width = 4.5, height = 3))

# Add table
ppt <- ph_with(ppt, value = formatted_table,
               location = ph_location(left = 5.3, top = 1.2, width = 4.5, height = 2.5))

# Add bar chart
ppt <- ph_with(ppt, value = bar_chart,
               location = ph_location(left = 5.3, top = 4.1, width = 4.5, height = 3))

# ============================================================================
# SLIDE 3: FULL-WIDTH LAYOUT
# ============================================================================

ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")

# Title
ppt <- ph_with(ppt, value = title_fp,
               location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.7))

# Text summary across top
full_summary <- block_list(
  fpar(ftext("Key Findings: ", prop = fp_text(bold = TRUE, font.size = 12, color = "#4472C4")),
       ftext(paste0("Total sales: $", format(total_annual_sales, big.mark = ",")), 
             prop = fp_text(font.size = 11)),
       ftext(" | Product A leads with highest monthly average | Positive growth across all products | Peak sales in Q3-Q4", 
             prop = fp_text(font.size = 11))),
  fpar(ftext("Recommendations: ", prop = fp_text(bold = TRUE, font.size = 12, color = "#4472C4")),
       ftext("Increase Product A inventory | Investigate Product C underperformance | Capitalize on seasonal trends", 
             prop = fp_text(font.size = 11)))
)

ppt <- ph_with(ppt, value = full_summary,
               location = ph_location(left = 0.5, top = 1.1, width = 9, height = 1))

# Chart left
ppt <- ph_with(ppt, value = sales_chart,
               location = ph_location(left = 0.5, top = 2.3, width = 4.5, height = 4.5))

# Table right
ppt <- ph_with(ppt, value = formatted_table,
               location = ph_location(left = 5.3, top = 2.3, width = 4.5, height = 4.5))

# ============================================================================
# SAVE PRESENTATION
# ============================================================================

output_file <- "Executive_Summary_Combined.pptx"
print(ppt, target = output_file)

cat("\n=======================================================\n")
cat("✓ Executive Summary Presentation Created!\n")
cat("=======================================================\n")
cat("File:", output_file, "\n")
cat("\nSlides:\n")
cat("1. Executive Summary (Text only - matching your image)\n")
cat("2. Executive Summary with Chart + Table (4 elements)\n")
cat("3. Full-width layout (Text + Chart + Table)\n")
cat("\nTotal annual sales:", scales::dollar(total_annual_sales), "\n")
cat("=======================================================\n")
