# R PowerPoint Report Generator - User Guide

## Overview
This collection of R scripts demonstrates how to create professional PowerPoint presentations programmatically with charts, tables, and comprehensive analysis reports.

## Required Packages

```r
install.packages(c("officer", "flextable", "ggplot2", "dplyr", "tidyr"))
```

- **officer**: PowerPoint creation and manipulation
- **flextable**: Professional table formatting
- **ggplot2**: Data visualization
- **dplyr**: Data manipulation
- **tidyr**: Data reshaping

## Files Included

1. **ppt_report_generator.R**: Basic comprehensive report with 8 slides
2. **advanced_ppt_generator.R**: Advanced customization with custom functions

## Quick Start

### Option 1: Basic Report
```r
source("ppt_report_generator.R")
```

This creates a file called `Sales_Analysis_Report.pptx` with:
- Title slide
- Executive summary
- Line charts showing trends
- Bar charts for comparisons
- Detailed data tables
- Statistical summaries
- Analysis and insights
- Conclusion slide

### Option 2: Advanced Report
```r
source("advanced_ppt_generator.R")
```

This creates `Advanced_Business_Report.pptx` with:
- Custom branded colors
- Regional analysis
- Profit margin trends
- Distribution analysis
- Formatted financial tables

## Customization Guide

### 1. Using Your Own Data

Replace the sample data generation with your own data:

```r
# Instead of:
sales_data <- data.frame(...)

# Use:
sales_data <- read.csv("your_data.csv")
```

### 2. Customizing Colors

```r
brand_colors <- list(
  primary = "#YOUR_COLOR",
  secondary = "#YOUR_COLOR",
  accent = "#YOUR_COLOR"
)
```

### 3. Adding Custom Slides

```r
# Add a new content slide
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Your Title", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = your_content, location = ph_location_type(type = "body"))
```

### 4. Creating Custom Charts

```r
my_chart <- ggplot(data, aes(x = x_var, y = y_var)) +
  geom_bar(stat = "identity", fill = "#4472C4") +
  labs(title = "Your Title", x = "X Label", y = "Y Label") +
  theme_minimal()

ppt <- ph_with(ppt, value = my_chart, location = ph_location_type(type = "body"))
```

### 5. Formatting Tables

```r
my_table <- data %>%
  flextable() %>%
  theme_booktabs() %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = "#4472C4") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  autofit()

ppt <- ph_with(ppt, value = my_table, location = ph_location_type(type = "body"))
```

## Common Use Cases

### 1. Monthly Sales Report
```r
# Prepare monthly data
monthly_data <- your_sales_data %>%
  group_by(month) %>%
  summarise(total_sales = sum(sales))

# Create trend chart
trend_chart <- ggplot(monthly_data, aes(x = month, y = total_sales)) +
  geom_line(color = "#4472C4", size = 1.2) +
  geom_point(size = 3)

# Add to presentation
ppt <- add_slide(ppt)
ppt <- ph_with(ppt, value = "Monthly Sales Trend", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = trend_chart, location = ph_location_type(type = "body"))
```

### 2. KPI Dashboard
```r
kpi_data <- data.frame(
  Metric = c("Revenue", "Customers", "Growth Rate", "Satisfaction"),
  Value = c("$1.2M", "5,432", "15.3%", "94%"),
  Status = c("↑", "↑", "↑", "→")
)

kpi_table <- flextable(kpi_data) %>%
  theme_booktabs() %>%
  autofit()

ppt <- ph_with(ppt, value = kpi_table, location = ph_location_type(type = "body"))
```

### 3. Comparative Analysis
```r
comparison_chart <- ggplot(data, aes(x = category, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#4472C4", "#ED7D31")) +
  theme_minimal()
```

## Advanced Features

### 1. Custom Slide Layouts
```r
# Create slide with specific positioning
ppt <- add_slide(ppt, layout = "Blank")
ppt <- ph_with(ppt, value = content,
               location = ph_location(left = 1, top = 2, width = 8, height = 4))
```

### 2. Adding Images
```r
ppt <- ph_with(ppt, external_img("path/to/image.png"),
               location = ph_location(left = 2, top = 2, width = 5, height = 3))
```

### 3. Text Formatting
```r
formatted_text <- block_list(
  fpar(ftext("Bold Text", prop = fp_text(bold = TRUE))),
  fpar(ftext("Italic Text", prop = fp_text(italic = TRUE))),
  fpar(ftext("Colored Text", prop = fp_text(color = "#4472C4")))
)
```

### 4. Multiple Charts on One Slide
```r
ppt <- add_slide(ppt, layout = "Blank")

# Chart 1
ppt <- ph_with(ppt, value = chart1,
               location = ph_location(left = 0.5, top = 1.5, width = 4.5, height = 3))

# Chart 2
ppt <- ph_with(ppt, value = chart2,
               location = ph_location(left = 5, top = 1.5, width = 4.5, height = 3))
```

## Chart Types Examples

### Bar Chart
```r
ggplot(data, aes(x = category, y = value)) +
  geom_bar(stat = "identity", fill = "#4472C4")
```

### Line Chart
```r
ggplot(data, aes(x = time, y = value, group = 1)) +
  geom_line(size = 1.2) +
  geom_point(size = 3)
```

### Scatter Plot
```r
ggplot(data, aes(x = var1, y = var2)) +
  geom_point(size = 3, alpha = 0.6, color = "#4472C4")
```

### Pie Chart (using Bar Chart)
```r
ggplot(data, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0)
```

### Heatmap
```r
ggplot(data, aes(x = x_var, y = y_var, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#4472C4")
```

## Tips and Best Practices

1. **Keep it Simple**: Don't overcrowd slides with too much information
2. **Consistent Branding**: Use consistent colors and fonts throughout
3. **Data Quality**: Clean your data before visualization
4. **Chart Selection**: Choose the right chart type for your data
5. **Professional Tables**: Use flextable for well-formatted tables
6. **Annotations**: Add notes and context to your charts
7. **File Organization**: Keep data, scripts, and outputs organized
8. **Version Control**: Save different versions of your reports

## Troubleshooting

### Issue: Package Installation Fails
```r
# Try installing with dependencies
install.packages("officer", dependencies = TRUE)
```

### Issue: Chart Doesn't Display
```r
# Make sure chart object is created before adding to slide
print(my_chart)  # Test if chart renders
```

### Issue: Table Formatting Issues
```r
# Use autofit() to adjust column widths
table <- table %>% autofit()
```

### Issue: File Won't Save
```r
# Check file path and permissions
print(ppt, target = "C:/Users/YourName/Documents/report.pptx")
```

## Example Workflow

```r
# 1. Load libraries
library(officer)
library(flextable)
library(ggplot2)
library(dplyr)

# 2. Load data
data <- read.csv("your_data.csv")

# 3. Create presentation
ppt <- read_pptx()

# 4. Add title slide
ppt <- add_slide(ppt, layout = "Title Slide")
ppt <- ph_with(ppt, value = "My Report", location = ph_location_type(type = "ctrTitle"))

# 5. Add content slides
ppt <- add_slide(ppt, layout = "Title and Content")
# ... add charts, tables, etc.

# 6. Save
print(ppt, target = "my_report.pptx")
```

## Resources

- officer documentation: https://davidgohel.github.io/officer/
- flextable documentation: https://davidgohel.github.io/flextable/
- ggplot2 documentation: https://ggplot2.tidyverse.org/

