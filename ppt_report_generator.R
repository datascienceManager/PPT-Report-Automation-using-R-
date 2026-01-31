# Custom PowerPoint Report Generator in R
# This script creates a professional PowerPoint presentation with charts, tables, and analysis

# Install required packages if not already installed
required_packages <- c("officer", "flextable", "ggplot2", "dplyr", "tidyr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ============================================================================
# 1. PREPARE SAMPLE DATA
# ============================================================================

# Sample sales data
set.seed(123)
sales_data <- data.frame(
  Month = month.name[1:12],
  Product_A = round(rnorm(12, mean = 50000, sd = 10000)),
  Product_B = round(rnorm(12, mean = 45000, sd = 8000)),
  Product_C = round(rnorm(12, mean = 35000, sd = 7000))
)

# Calculate totals and growth
sales_data$Total <- sales_data$Product_A + sales_data$Product_B + sales_data$Product_C
sales_data$Month_Num <- 1:12

# Summary statistics
summary_stats <- data.frame(
  Metric = c("Mean Sales", "Median Sales", "Std Deviation", "Max Month", "Min Month"),
  Product_A = c(
    mean(sales_data$Product_A),
    median(sales_data$Product_A),
    sd(sales_data$Product_A),
    sales_data$Month[which.max(sales_data$Product_A)],
    sales_data$Month[which.min(sales_data$Product_A)]
  ),
  Product_B = c(
    mean(sales_data$Product_B),
    median(sales_data$Product_B),
    sd(sales_data$Product_B),
    sales_data$Month[which.max(sales_data$Product_B)],
    sales_data$Month[which.min(sales_data$Product_B)]
  ),
  Product_C = c(
    mean(sales_data$Product_C),
    median(sales_data$Product_C),
    sd(sales_data$Product_C),
    sales_data$Month[which.max(sales_data$Product_C)],
    sales_data$Month[which.min(sales_data$Product_C)]
  )
)

# ============================================================================
# 2. CREATE VISUALIZATIONS
# ============================================================================

# Chart 1: Line chart showing monthly trends
create_line_chart <- function(data) {
  data_long <- data %>%
    select(Month, Product_A, Product_B, Product_C) %>%
    pivot_longer(cols = -Month, names_to = "Product", values_to = "Sales")
  
  data_long$Month <- factor(data_long$Month, levels = month.name[1:12])
  
  ggplot(data_long, aes(x = Month, y = Sales, color = Product, group = Product)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Monthly Sales Trends by Product",
      x = "Month",
      y = "Sales ($)",
      color = "Product"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

# Chart 2: Bar chart showing total sales by product
create_bar_chart <- function(data) {
  total_by_product <- data.frame(
    Product = c("Product A", "Product B", "Product C"),
    Total_Sales = c(
      sum(data$Product_A),
      sum(data$Product_B),
      sum(data$Product_C)
    )
  )
  
  ggplot(total_by_product, aes(x = Product, y = Total_Sales, fill = Product)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::comma(Total_Sales)), vjust = -0.5, size = 5) +
    scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
    scale_y_continuous(labels = scales::comma, limits = c(0, max(total_by_product$Total_Sales) * 1.1)) +
    labs(
      title = "Total Annual Sales by Product",
      x = "Product",
      y = "Total Sales ($)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "none"
    )
}

# ============================================================================
# 3. CREATE POWERPOINT PRESENTATION
# ============================================================================

# Initialize PowerPoint object
ppt <- read_pptx()

# ---- SLIDE 1: Title Slide ----
ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
ppt <- ph_with(ppt, value = "Annual Sales Analysis Report", location = ph_location_type(type = "ctrTitle"))
ppt <- ph_with(ppt, value = "FY 2024 - Comprehensive Overview", location = ph_location_type(type = "subTitle"))

# ---- SLIDE 2: Executive Summary ----
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Executive Summary", location = ph_location_type(type = "title"))

summary_text <- block_list(
  fpar(ftext("Key Findings:", prop = fp_text(bold = TRUE, font.size = 14))),
  fpar(ftext("• Total annual sales across all products: ", prop = fp_text(font.size = 12)),
       ftext(paste0("$", format(sum(sales_data$Total), big.mark = ",")), 
             prop = fp_text(bold = TRUE, font.size = 12))),
  fpar(ftext("• Product A led with the highest average monthly sales", prop = fp_text(font.size = 12))),
  fpar(ftext("• All products showed positive growth trajectory", prop = fp_text(font.size = 12))),
  fpar(ftext("• Peak sales occurred in mid-to-late year period", prop = fp_text(font.size = 12))),
  fpar(ftext("\nRecommendations:", prop = fp_text(bold = TRUE, font.size = 14))),
  fpar(ftext("• Increase inventory for Product A to meet demand", prop = fp_text(font.size = 12))),
  fpar(ftext("• Investigate factors driving Product C underperformance", prop = fp_text(font.size = 12))),
  fpar(ftext("• Capitalize on seasonal trends identified in Q3-Q4", prop = fp_text(font.size = 12)))
)

ppt <- ph_with(ppt, value = summary_text, location = ph_location_type(type = "body"))

# ---- SLIDE 3: Monthly Sales Trends Chart ----
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Monthly Sales Trends", location = ph_location_type(type = "title"))

line_chart <- create_line_chart(sales_data)
ppt <- ph_with(ppt, value = line_chart, location = ph_location_type(type = "body"))

# ---- SLIDE 4: Total Sales by Product ----
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Annual Sales Comparison", location = ph_location_type(type = "title"))

bar_chart <- create_bar_chart(sales_data)
ppt <- ph_with(ppt, value = bar_chart, location = ph_location_type(type = "body"))

# ---- SLIDE 5: Detailed Sales Table ----
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Monthly Sales Data", location = ph_location_type(type = "title"))

# Create formatted table
sales_table <- sales_data %>%
  select(Month, Product_A, Product_B, Product_C, Total) %>%
  flextable() %>%
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
  autofit()

ppt <- ph_with(ppt, value = sales_table, location = ph_location_type(type = "body"))

# ---- SLIDE 6: Summary Statistics ----
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Statistical Summary", location = ph_location_type(type = "title"))

# Format summary statistics for first 3 rows (numeric values)
summary_table <- summary_stats %>%
  flextable() %>%
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
  align(j = 1, align = "left", part = "body") %>%
  autofit()

ppt <- ph_with(ppt, value = summary_table, location = ph_location_type(type = "body"))

# ---- SLIDE 7: Analysis & Insights ----
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Key Insights & Analysis", location = ph_location_type(type = "title"))

# Calculate insights
best_product <- names(which.max(c(sum(sales_data$Product_A), 
                                   sum(sales_data$Product_B), 
                                   sum(sales_data$Product_C))))
best_month <- sales_data$Month[which.max(sales_data$Total)]

insights_text <- block_list(
  fpar(ftext("Performance Analysis:", prop = fp_text(bold = TRUE, font.size = 14, color = "#4472C4"))),
  fpar(ftext("\n1. Top Performing Product:", prop = fp_text(bold = TRUE, font.size = 12))),
  fpar(ftext("   Product A generated the highest revenue throughout the year,", prop = fp_text(font.size = 11))),
  fpar(ftext("   demonstrating strong market demand and effective positioning.", prop = fp_text(font.size = 11))),
  fpar(ftext("\n2. Seasonality Patterns:", prop = fp_text(bold = TRUE, font.size = 12))),
  fpar(ftext(paste0("   Peak sales occurred in ", best_month, ", suggesting seasonal trends."), 
             prop = fp_text(font.size = 11))),
  fpar(ftext("   Marketing campaigns should align with these patterns.", prop = fp_text(font.size = 11))),
  fpar(ftext("\n3. Growth Opportunities:", prop = fp_text(bold = TRUE, font.size = 12))),
  fpar(ftext("   Product C shows potential for improvement through targeted initiatives.", 
             prop = fp_text(font.size = 11))),
  fpar(ftext("   Cross-selling strategies could boost overall revenue.", prop = fp_text(font.size = 11))),
  fpar(ftext("\n4. Risk Mitigation:", prop = fp_text(bold = TRUE, font.size = 12))),
  fpar(ftext("   Diversification across products provides revenue stability.", prop = fp_text(font.size = 11))),
  fpar(ftext("   Continue monitoring market trends and competitor activities.", prop = fp_text(font.size = 11)))
)

ppt <- ph_with(ppt, value = insights_text, location = ph_location_type(type = "body"))

# ---- SLIDE 8: Conclusion ----
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Conclusion & Next Steps", location = ph_location_type(type = "title"))

conclusion_text <- block_list(
  fpar(ftext("Summary:", prop = fp_text(bold = TRUE, font.size = 14, color = "#4472C4"))),
  fpar(ftext("\nThe annual sales analysis reveals strong overall performance with", prop = fp_text(font.size = 12))),
  fpar(ftext("opportunities for strategic optimization and growth.", prop = fp_text(font.size = 12))),
  fpar(ftext("\nNext Steps:", prop = fp_text(bold = TRUE, font.size = 14, color = "#4472C4"))),
  fpar(ftext("\n✓ Develop targeted marketing campaign for Q1 2025", prop = fp_text(font.size = 12))),
  fpar(ftext("✓ Conduct customer satisfaction survey for Product C", prop = fp_text(font.size = 12))),
  fpar(ftext("✓ Optimize inventory management based on seasonal patterns", prop = fp_text(font.size = 12))),
  fpar(ftext("✓ Explore expansion opportunities in high-performing segments", prop = fp_text(font.size = 12))),
  fpar(ftext("✓ Schedule quarterly review meetings to track progress", prop = fp_text(font.size = 12))),
  fpar(ftext("\n\nThank you!", prop = fp_text(bold = TRUE, font.size = 16, color = "#4472C4")))
)

ppt <- ph_with(ppt, value = conclusion_text, location = ph_location_type(type = "body"))

# ============================================================================
# 4. SAVE THE PRESENTATION
# ============================================================================

output_file <- "Sales_Analysis_Report.pptx"
print(ppt, target = output_file)

cat("\n=================================================\n")
cat("PowerPoint presentation created successfully!\n")
cat("File saved as:", output_file, "\n")
cat("=================================================\n")

# ============================================================================
# OPTIONAL: Print summary to console
# ============================================================================

cat("\nReport Summary:\n")
cat("Total Slides: 8\n")
cat("Charts Created: 2 (Line chart, Bar chart)\n")
cat("Tables Created: 2 (Sales data, Statistics)\n")
cat("\nData Overview:\n")
print(summary(sales_data[, c("Product_A", "Product_B", "Product_C", "Total")]))
