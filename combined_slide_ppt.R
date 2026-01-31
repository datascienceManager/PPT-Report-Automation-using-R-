# PowerPoint with Chart, Table, and Summary on Same Slide
# This script demonstrates how to combine multiple elements on one slide

library(officer)
library(flextable)
library(ggplot2)
library(dplyr)
library(tidyr)

# ============================================================================
# PREPARE SAMPLE DATA
# ============================================================================

set.seed(123)
sales_data <- data.frame(
  Month = month.name[1:12],
  Product_A = round(rnorm(12, mean = 50000, sd = 10000)),
  Product_B = round(rnorm(12, mean = 45000, sd = 8000)),
  Product_C = round(rnorm(12, mean = 35000, sd = 7000))
)

sales_data$Total <- sales_data$Product_A + sales_data$Product_B + sales_data$Product_C

# ============================================================================
# CREATE CHART
# ============================================================================

# Create a compact line chart
create_compact_chart <- function(data) {
  data_long <- data %>%
    select(Month, Product_A, Product_B, Product_C) %>%
    pivot_longer(cols = -Month, names_to = "Product", values_to = "Sales")
  
  data_long$Month <- factor(data_long$Month, levels = month.name[1:12])
  
  ggplot(data_long, aes(x = Month, y = Sales, color = Product, group = Product)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Monthly Sales Trends",
      x = "",
      y = "Sales ($)",
      color = "Product"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# ============================================================================
# CREATE TABLE
# ============================================================================

create_summary_table <- function(data) {
  # Get last 6 months for compact display
  table_data <- data %>%
    tail(6) %>%
    select(Month, Product_A, Product_B, Product_C, Total)
  
  ft <- flextable(table_data) %>%
    set_header_labels(
      Month = "Month",
      Product_A = "Product A",
      Product_B = "Product B",
      Product_C = "Product C",
      Total = "Total"
    ) %>%
    colformat_double(j = c("Product_A", "Product_B", "Product_C", "Total"), 
                     big.mark = ",", digits = 0, prefix = "$") %>%
    theme_booktabs() %>%
    color(part = "header", color = "white") %>%
    bg(part = "header", bg = "#4472C4") %>%
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 9, part = "all") %>%
    autofit()
  
  return(ft)
}

# ============================================================================
# CREATE SUMMARY TEXT
# ============================================================================

create_summary_text <- function(data) {
  total_sales <- sum(data$Total)
  
  summary_text <- block_list(
    fpar(ftext("Key Findings:", prop = fp_text(bold = TRUE, font.size = 12, color = "#4472C4"))),
    fpar(ftext(paste0("• Total annual sales: $", format(total_sales, big.mark = ",")), 
               prop = fp_text(font.size = 10))),
    fpar(ftext("• Product A led with highest monthly sales", 
               prop = fp_text(font.size = 10))),
    fpar(ftext("• All products showed positive growth", 
               prop = fp_text(font.size = 10))),
    fpar(ftext("• Peak sales in mid-to-late year", 
               prop = fp_text(font.size = 10))),
    fpar(ftext("", prop = fp_text(font.size = 8))),
    fpar(ftext("Recommendations:", prop = fp_text(bold = TRUE, font.size = 12, color = "#4472C4"))),
    fpar(ftext("• Increase inventory for Product A", 
               prop = fp_text(font.size = 10))),
    fpar(ftext("• Investigate Product C underperformance", 
               prop = fp_text(font.size = 10))),
    fpar(ftext("• Capitalize on Q3-Q4 seasonal trends", 
               prop = fp_text(font.size = 10)))
  )
  
  return(summary_text)
}

# ============================================================================
# CREATE PRESENTATION WITH COMBINED SLIDE
# ============================================================================

# Initialize PowerPoint
ppt <- read_pptx()

# Add title slide
ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
ppt <- ph_with(ppt, value = "Sales Analysis Report", 
               location = ph_location_type(type = "ctrTitle"))
ppt <- ph_with(ppt, value = "Comprehensive Overview", 
               location = ph_location_type(type = "subTitle"))

# ============================================================================
# COMBINED SLIDE: Chart + Table + Summary
# ============================================================================

ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")

# Add slide title
title_text <- fpar(
  ftext("Executive Summary", prop = fp_text(font.size = 28, bold = TRUE, color = "#4472C4"))
)
ppt <- ph_with(ppt, value = title_text,
               location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.6))

# Add summary text (left side, top)
summary_text <- create_summary_text(sales_data)
ppt <- ph_with(ppt, value = summary_text,
               location = ph_location(left = 0.5, top = 1.1, width = 4.2, height = 2.5))

# Add chart (left side, bottom)
chart <- create_compact_chart(sales_data)
ppt <- ph_with(ppt, value = chart,
               location = ph_location(left = 0.5, top = 3.8, width = 4.2, height = 3.2))

# Add table (right side, full height)
table <- create_summary_table(sales_data)
ppt <- ph_with(ppt, value = table,
               location = ph_location(left = 5.2, top = 1.1, width = 4.5, height = 6))

# ============================================================================
# ALTERNATIVE LAYOUT: Horizontal arrangement
# ============================================================================

ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")

# Title
ppt <- ph_with(ppt, value = fpar(ftext("Alternative Layout - Horizontal", 
                                        prop = fp_text(font.size = 28, bold = TRUE, color = "#4472C4"))),
               location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.6))

# Summary text at top
ppt <- ph_with(ppt, value = summary_text,
               location = ph_location(left = 0.5, top = 1.1, width = 9, height = 1.8))

# Chart on left
ppt <- ph_with(ppt, value = chart,
               location = ph_location(left = 0.5, top = 3.2, width = 4.5, height = 3.5))

# Table on right
ppt <- ph_with(ppt, value = table,
               location = ph_location(left = 5.2, top = 3.2, width = 4.5, height = 3.5))

# ============================================================================
# COMPACT VERSION: Everything tightly packed
# ============================================================================

ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")

# Title
ppt <- ph_with(ppt, value = fpar(ftext("Compact Layout", 
                                        prop = fp_text(font.size = 24, bold = TRUE, color = "#4472C4"))),
               location = ph_location(left = 0.5, top = 0.2, width = 9, height = 0.5))

# Summary (top, full width, smaller)
compact_summary <- block_list(
  fpar(ftext("Key Findings: ", prop = fp_text(bold = TRUE, font.size = 10, color = "#4472C4")),
       ftext(paste0("Total sales: $", format(sum(sales_data$Total), big.mark = ",")), 
             prop = fp_text(font.size = 9)),
       ftext(" | Product A leads | Positive growth across all products | Peak in Q3-Q4", 
             prop = fp_text(font.size = 9))),
  fpar(ftext("Recommendations: ", prop = fp_text(bold = TRUE, font.size = 10, color = "#4472C4")),
       ftext("Increase Product A inventory | Investigate Product C | Leverage seasonal trends", 
             prop = fp_text(font.size = 9)))
)

ppt <- ph_with(ppt, value = compact_summary,
               location = ph_location(left = 0.5, top = 0.85, width = 9, height = 0.8))

# Chart (left, larger)
ppt <- ph_with(ppt, value = chart,
               location = ph_location(left = 0.5, top = 1.8, width = 5.5, height = 4.8))

# Table (right, smaller)
ppt <- ph_with(ppt, value = table,
               location = ph_location(left = 6.2, top = 1.8, width = 3.5, height = 4.8))

# ============================================================================
# SAVE PRESENTATION
# ============================================================================

output_file <- "Combined_Elements_Report.pptx"
print(ppt, target = output_file)

cat("\n=======================================================\n")
cat("PowerPoint with Combined Elements Created Successfully!\n")
cat("=======================================================\n")
cat("File:", output_file, "\n")
cat("\nSlides created:\n")
cat("1. Title Slide\n")
cat("2. Executive Summary (Chart + Table + Text - Vertical)\n")
cat("3. Alternative Layout (Chart + Table + Text - Horizontal)\n")
cat("4. Compact Layout (All elements tightly arranged)\n")
cat("\n=======================================================\n")
