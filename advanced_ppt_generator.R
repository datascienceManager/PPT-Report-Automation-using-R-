# Advanced PowerPoint Report Generator with Custom Templates
# This version includes more customization and can work with CSV data

library(officer)
library(flextable)
library(ggplot2)
library(dplyr)
library(tidyr)

# ============================================================================
# CONFIGURATION SECTION
# ============================================================================

# Define custom colors for branding
brand_colors <- list(
  primary = "#4472C4",
  secondary = "#ED7D31",
  accent = "#70AD47",
  neutral = "#5B9BD5",
  text = "#404040"
)

# ============================================================================
# FUNCTION: Create custom template
# ============================================================================

create_custom_template <- function() {
  # Start with blank presentation
  ppt <- read_pptx()
  return(ppt)
}

# ============================================================================
# FUNCTION: Add custom formatted title slide
# ============================================================================

add_title_slide_custom <- function(ppt, title, subtitle, author = NULL, date = NULL) {
  ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
  
  # Add title with custom formatting
  title_text <- fpar(
    ftext(title, prop = fp_text(font.size = 40, bold = TRUE, color = brand_colors$primary))
  )
  
  ppt <- ph_with(ppt, value = title_text, 
                 location = ph_location(left = 0.5, top = 2.5, width = 9, height = 1.5))
  
  # Add subtitle
  if (!is.null(subtitle)) {
    subtitle_text <- fpar(
      ftext(subtitle, prop = fp_text(font.size = 24, color = brand_colors$text))
    )
    ppt <- ph_with(ppt, value = subtitle_text,
                   location = ph_location(left = 0.5, top = 4, width = 9, height = 1))
  }
  
  # Add author and date
  if (!is.null(author) || !is.null(date)) {
    footer_parts <- c()
    if (!is.null(author)) footer_parts <- c(footer_parts, author)
    if (!is.null(date)) footer_parts <- c(footer_parts, date)
    
    footer_text <- fpar(
      ftext(paste(footer_parts, collapse = " | "), 
            prop = fp_text(font.size = 14, color = brand_colors$neutral))
    )
    ppt <- ph_with(ppt, value = footer_text,
                   location = ph_location(left = 0.5, top = 6.5, width = 9, height = 0.5))
  }
  
  return(ppt)
}

# ============================================================================
# FUNCTION: Add slide with chart
# ============================================================================

add_chart_slide <- function(ppt, title, chart, notes = NULL) {
  ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
  
  # Add title
  title_text <- fpar(
    ftext(title, prop = fp_text(font.size = 28, bold = TRUE, color = brand_colors$primary))
  )
  ppt <- ph_with(ppt, value = title_text,
                 location = ph_location(left = 0.5, top = 0.5, width = 9, height = 0.8))
  
  # Add chart
  ppt <- ph_with(ppt, value = chart,
                 location = ph_location(left = 0.5, top = 1.5, width = 9, height = 5))
  
  # Add notes if provided
  if (!is.null(notes)) {
    notes_text <- fpar(
      ftext(notes, prop = fp_text(font.size = 10, italic = TRUE, color = brand_colors$neutral))
    )
    ppt <- ph_with(ppt, value = notes_text,
                   location = ph_location(left = 0.5, top = 6.7, width = 9, height = 0.5))
  }
  
  return(ppt)
}

# ============================================================================
# FUNCTION: Create correlation heatmap
# ============================================================================

create_correlation_heatmap <- function(data) {
  # Select numeric columns
  numeric_data <- data %>% select(where(is.numeric))
  
  # Calculate correlation
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Prepare data for ggplot
  cor_data <- as.data.frame(as.table(cor_matrix))
  names(cor_data) <- c("Var1", "Var2", "Correlation")
  
  # Create heatmap
  ggplot(cor_data, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3.5) +
    scale_fill_gradient2(low = "#ED7D31", mid = "white", high = "#4472C4",
                         midpoint = 0, limit = c(-1, 1)) +
    labs(title = "Correlation Matrix", x = "", y = "") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
}

# ============================================================================
# FUNCTION: Create box plot for distributions
# ============================================================================

create_boxplot <- function(data) {
  data_long <- data %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Value")
  
  ggplot(data_long, aes(x = Variable, y = Value, fill = Variable)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = c(brand_colors$primary, brand_colors$secondary, 
                                  brand_colors$accent, brand_colors$neutral)) +
    labs(
      title = "Distribution Analysis",
      x = "Product",
      y = "Sales Value"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

# ============================================================================
# FUNCTION: Create professional table with conditional formatting
# ============================================================================

create_formatted_table <- function(data, highlight_col = NULL, highlight_max = TRUE) {
  ft <- flextable(data) %>%
    theme_booktabs() %>%
    color(part = "header", color = "white") %>%
    bg(part = "header", bg = brand_colors$primary) %>%
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    autofit()
  
  # Add conditional formatting if specified
  if (!is.null(highlight_col) && highlight_col %in% names(data)) {
    if (highlight_max) {
      max_val <- max(data[[highlight_col]], na.rm = TRUE)
      ft <- bg(ft, i = ~ data[[highlight_col]] == max_val, 
               j = highlight_col, bg = "#C6E0B4")
    }
  }
  
  return(ft)
}

# ============================================================================
# MAIN EXECUTION: Create comprehensive report
# ============================================================================

# Load or create sample data
set.seed(456)
analysis_data <- data.frame(
  Quarter = rep(paste0("Q", 1:4), each = 3),
  Region = rep(c("North", "South", "West"), 4),
  Revenue = round(rnorm(12, mean = 100000, sd = 20000)),
  Expenses = round(rnorm(12, mean = 70000, sd = 15000)),
  Units_Sold = round(rnorm(12, mean = 500, sd = 100))
)

analysis_data$Profit <- analysis_data$Revenue - analysis_data$Expenses
analysis_data$Profit_Margin <- round((analysis_data$Profit / analysis_data$Revenue) * 100, 1)

# Initialize presentation
ppt <- create_custom_template()

# Slide 1: Custom Title
ppt <- add_title_slide_custom(
  ppt,
  title = "Quarterly Business Performance Report",
  subtitle = "Regional Analysis & Financial Overview",
  author = "Analytics Department",
  date = format(Sys.Date(), "%B %Y")
)

# Slide 2: Revenue by Quarter
revenue_chart <- ggplot(analysis_data, aes(x = Quarter, y = Revenue, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(brand_colors$primary, brand_colors$secondary, brand_colors$accent)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Revenue by Quarter and Region", x = "Quarter", y = "Revenue") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom")

ppt <- add_chart_slide(ppt, "Quarterly Revenue Analysis", revenue_chart,
                       notes = "All regions showed consistent growth across quarters")

# Slide 3: Profit Margins
margin_chart <- ggplot(analysis_data, aes(x = Quarter, y = Profit_Margin, color = Region, group = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(brand_colors$primary, brand_colors$secondary, brand_colors$accent)) +
  labs(title = "Profit Margin Trends", x = "Quarter", y = "Profit Margin (%)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom")

ppt <- add_chart_slide(ppt, "Profitability Analysis", margin_chart)

# Slide 4: Box plot distributions
box_plot <- create_boxplot(analysis_data %>% select(Revenue, Expenses, Profit))
ppt <- add_chart_slide(ppt, "Distribution Analysis", box_plot)

# Slide 5: Detailed Data Table
ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
title_text <- fpar(ftext("Detailed Financial Data", 
                         prop = fp_text(font.size = 28, bold = TRUE, color = brand_colors$primary)))
ppt <- ph_with(ppt, value = title_text,
               location = ph_location(left = 0.5, top = 0.5, width = 9, height = 0.8))

detailed_table <- analysis_data %>%
  mutate(Revenue = scales::dollar(Revenue),
         Expenses = scales::dollar(Expenses),
         Profit = scales::dollar(Profit),
         Profit_Margin = paste0(Profit_Margin, "%")) %>%
  flextable() %>%
  theme_booktabs() %>%
  color(part = "header", color = "white") %>%
  bg(part = "header", bg = brand_colors$primary) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  autofit()

ppt <- ph_with(ppt, value = detailed_table,
               location = ph_location(left = 0.5, top = 1.5, width = 9, height = 5))

# Save presentation
output_file <- "Advanced_Business_Report.pptx"
print(ppt, target = output_file)

cat("\n=================================================\n")
cat("Advanced PowerPoint Report Created Successfully!\n")
cat("File:", output_file, "\n")
cat("Total Slides: 5\n")
cat("=================================================\n")
