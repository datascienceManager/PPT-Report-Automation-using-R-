# FULLY DYNAMIC SALES ANALYSIS SYSTEM FOR POWERPOINT
# All text, insights, and recommendations are automatically generated based on data patterns

library(officer)
library(flextable)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ============================================================================
# 1. INTELLIGENT DATA ANALYZER ENGINE
# ============================================================================

analyze_data_patterns <- function(data) {
  
  analysis <- list()
  
  # ---- BASIC METRICS ----
  analysis$total_sales <- sum(data$Total)
  analysis$avg_monthly <- mean(data$Total)
  analysis$median_monthly <- median(data$Total)
  
  # Product totals
  analysis$product_totals <- c(
    Product_A = sum(data$Product_A),
    Product_B = sum(data$Product_B),
    Product_C = sum(data$Product_C)
  )
  
  # Best and worst performing product
  analysis$best_product <- names(which.max(analysis$product_totals))
  analysis$worst_product <- names(which.min(analysis$product_totals))
  
  # Market share
  analysis$market_share <- (analysis$product_totals / analysis$total_sales) * 100
  
  # ---- TREND ANALYSIS ----
  # Calculate trends for each product
  trend_A <- lm(Product_A ~ Month_Num, data = data)
  trend_B <- lm(Product_B ~ Month_Num, data = data)
  trend_C <- lm(Product_C ~ Month_Num, data = data)
  trend_Total <- lm(Total ~ Month_Num, data = data)
  
  analysis$trends <- list(
    Product_A = list(slope = coef(trend_A)[2], r2 = summary(trend_A)$r.squared),
    Product_B = list(slope = coef(trend_B)[2], r2 = summary(trend_B)$r.squared),
    Product_C = list(slope = coef(trend_C)[2], r2 = summary(trend_C)$r.squared),
    Total = list(slope = coef(trend_Total)[2], r2 = summary(trend_Total)$r.squared)
  )
  
  # Determine trend direction and strength
  analysis$trend_direction <- ifelse(analysis$trends$Total$slope > 0, "increasing", "decreasing")
  analysis$trend_strength <- ifelse(analysis$trends$Total$r2 > 0.7, "strong", 
                                    ifelse(analysis$trends$Total$r2 > 0.4, "moderate", "weak"))
  
  # ---- GROWTH ANALYSIS ----
  analysis$mom_growth <- data$MoM_Growth[!is.na(data$MoM_Growth)]
  analysis$avg_growth <- mean(analysis$mom_growth)
  analysis$growth_volatility <- sd(analysis$mom_growth)
  
  # Positive vs negative growth months
  analysis$positive_months <- sum(analysis$mom_growth > 0)
  analysis$negative_months <- sum(analysis$mom_growth < 0)
  analysis$growth_consistency <- analysis$positive_months / length(analysis$mom_growth) * 100
  
  # ---- SEASONALITY ANALYSIS ----
  data$Quarter <- paste0("Q", ceiling(data$Month_Num / 3))
  quarterly <- data %>%
    group_by(Quarter) %>%
    summarise(Total = sum(Total), Avg = mean(Total))
  
  analysis$best_quarter <- quarterly$Quarter[which.max(quarterly$Total)]
  analysis$worst_quarter <- quarterly$Quarter[which.min(quarterly$Total)]
  analysis$quarterly_data <- quarterly
  
  # Half year comparison
  h1_sales <- sum(data$Total[1:6])
  h2_sales <- sum(data$Total[7:12])
  analysis$h1_sales <- h1_sales
  analysis$h2_sales <- h2_sales
  analysis$h2_vs_h1 <- ((h2_sales - h1_sales) / h1_sales) * 100
  analysis$better_half <- ifelse(h2_sales > h1_sales, "second", "first")
  
  # ---- PERFORMANCE PEAKS ----
  analysis$best_month <- data$Month[which.max(data$Total)]
  analysis$worst_month <- data$Month[which.min(data$Total)]
  analysis$best_month_value <- max(data$Total)
  analysis$worst_month_value <- min(data$Total)
  analysis$range <- analysis$best_month_value - analysis$worst_month_value
  
  # ---- VOLATILITY ANALYSIS ----
  analysis$cv <- (sd(data$Total) / mean(data$Total)) * 100
  analysis$volatility_level <- ifelse(analysis$cv < 10, "low", 
                                      ifelse(analysis$cv < 20, "moderate", "high"))
  
  # ---- CORRELATION ANALYSIS ----
  cor_matrix <- cor(data %>% select(Product_A, Product_B, Product_C))
  analysis$correlations <- cor_matrix
  analysis$avg_correlation <- mean(cor_matrix[upper.tri(cor_matrix)])
  analysis$correlation_strength <- ifelse(analysis$avg_correlation > 0.7, "strong", 
                                         ifelse(analysis$avg_correlation > 0.4, "moderate", "weak"))
  
  # ---- PRODUCT PERFORMANCE GAPS ----
  analysis$product_gap <- max(analysis$product_totals) - min(analysis$product_totals)
  analysis$gap_percentage <- (analysis$product_gap / max(analysis$product_totals)) * 100
  
  # ---- CONSISTENCY METRICS ----
  # Check if any product is consistently growing
  for(prod in c("Product_A", "Product_B", "Product_C")) {
    trend_model <- lm(as.formula(paste(prod, "~ Month_Num")), data = data)
    analysis[[paste0(prod, "_consistent")]] <- summary(trend_model)$r.squared > 0.6
  }
  
  return(analysis)
}

# ============================================================================
# 2. DYNAMIC INSIGHT GENERATOR
# ============================================================================

generate_dynamic_insights <- function(analysis) {
  
  insights <- list()
  
  # ---- EXECUTIVE SUMMARY ----
  insights$executive_summary <- sprintf(
    "The company achieved total annual sales of %s across all product lines. %s emerged as the clear market leader, capturing %s of total revenue. Overall sales showed a %s %s trend throughout the year, with %s consistency in growth patterns.",
    dollar(analysis$total_sales),
    gsub("_", " ", analysis$best_product),
    percent(analysis$market_share[analysis$best_product]/100, accuracy = 0.1),
    analysis$trend_strength,
    analysis$trend_direction,
    ifelse(analysis$growth_consistency > 70, "high", 
           ifelse(analysis$growth_consistency > 50, "moderate", "low"))
  )
  
  # ---- KEY FINDINGS ----
  findings <- c()
  
  # Finding 1: Total sales
  findings <- c(findings, sprintf(
    "Total annual sales reached %s, representing an average of %s per month",
    dollar(analysis$total_sales), dollar(analysis$avg_monthly)
  ))
  
  # Finding 2: Best product
  findings <- c(findings, sprintf(
    "%s led all products with %s in sales (%s market share)",
    gsub("_", " ", analysis$best_product),
    dollar(analysis$product_totals[analysis$best_product]),
    percent(analysis$market_share[analysis$best_product]/100, accuracy = 0.1)
  ))
  
  # Finding 3: Growth trend
  if(analysis$trend_direction == "increasing") {
    findings <- c(findings, sprintf(
      "Sales demonstrated a %s upward trend with an average monthly increase of %s",
      analysis$trend_strength,
      dollar(abs(analysis$trends$Total$slope))
    ))
  } else {
    findings <- c(findings, sprintf(
      "Sales showed a %s downward trend, declining an average of %s per month",
      analysis$trend_strength,
      dollar(abs(analysis$trends$Total$slope))
    ))
  }
  
  # Finding 4: Seasonality
  findings <- c(findings, sprintf(
    "%s was the strongest performing quarter, while %s showed the weakest results",
    analysis$best_quarter, analysis$worst_quarter
  ))
  
  # Finding 5: Peak performance
  findings <- c(findings, sprintf(
    "Peak sales occurred in %s (%s), representing the highest monthly performance",
    analysis$best_month, dollar(analysis$best_month_value)
  ))
  
  # Finding 6: Growth consistency
  if(analysis$growth_consistency >= 70) {
    findings <- c(findings, sprintf(
      "Growth was highly consistent, with %d out of 11 months showing positive growth",
      analysis$positive_months
    ))
  } else if(analysis$growth_consistency >= 50) {
    findings <- c(findings, sprintf(
      "Growth showed moderate volatility, with %d months of positive growth and %d months of decline",
      analysis$positive_months, analysis$negative_months
    ))
  } else {
    findings <- c(findings, sprintf(
      "Sales exhibited significant volatility, with only %d months showing positive growth",
      analysis$positive_months
    ))
  }
  
  # Finding 7: Half-year comparison
  if(abs(analysis$h2_vs_h1) > 10) {
    findings <- c(findings, sprintf(
      "The %s half significantly outperformed the %s by %s",
      analysis$better_half,
      ifelse(analysis$better_half == "second", "first", "second"),
      percent(abs(analysis$h2_vs_h1)/100, accuracy = 0.1)
    ))
  } else {
    findings <- c(findings, "Both halves of the year showed relatively balanced performance")
  }
  
  # Finding 8: Product correlation
  if(analysis$correlation_strength == "strong") {
    findings <- c(findings, 
      "Product sales patterns show strong correlation, suggesting unified market dynamics"
    )
  } else if(analysis$correlation_strength == "moderate") {
    findings <- c(findings, 
      "Products demonstrate moderate independence in sales patterns"
    )
  } else {
    findings <- c(findings, 
      "Product sales patterns are largely independent, indicating diverse market segments"
    )
  }
  
  insights$key_findings <- findings
  
  # ---- STRATEGIC RECOMMENDATIONS ----
  recommendations <- c()
  
  # Recommendation 1: Based on best product
  if(analysis$market_share[analysis$best_product] > 40) {
    recommendations <- c(recommendations, sprintf(
      "Increase inventory and marketing investment for %s to capitalize on its dominant market position",
      gsub("_", " ", analysis$best_product)
    ))
  } else {
    recommendations <- c(recommendations, sprintf(
      "Develop targeted campaigns to strengthen %s's competitive position",
      gsub("_", " ", analysis$best_product)
    ))
  }
  
  # Recommendation 2: Based on worst product
  if(analysis$gap_percentage > 30) {
    recommendations <- c(recommendations, sprintf(
      "Investigate root causes of %s underperformance - consider product repositioning or enhanced support",
      gsub("_", " ", analysis$worst_product)
    ))
  } else {
    recommendations <- c(recommendations, sprintf(
      "Implement improvement initiatives for %s to close the performance gap with top products",
      gsub("_", " ", analysis$worst_product)
    ))
  }
  
  # Recommendation 3: Based on trend
  if(analysis$trend_direction == "increasing" && analysis$trend_strength == "strong") {
    recommendations <- c(recommendations, 
      "Maintain current strategies and scale operations to meet growing demand"
    )
  } else if(analysis$trend_direction == "increasing" && analysis$trend_strength == "weak") {
    recommendations <- c(recommendations, 
      "Strengthen growth momentum through enhanced marketing and product differentiation"
    )
  } else {
    recommendations <- c(recommendations, 
      "Implement immediate corrective measures to reverse declining sales trend"
    )
  }
  
  # Recommendation 4: Based on seasonality
  recommendations <- c(recommendations, sprintf(
    "Optimize inventory and staffing for %s peak season based on identified patterns",
    analysis$best_quarter
  ))
  
  # Recommendation 5: Based on volatility
  if(analysis$volatility_level == "high") {
    recommendations <- c(recommendations, 
      "Develop strategies to stabilize sales and reduce month-to-month volatility"
    )
  } else {
    recommendations <- c(recommendations, 
      "Leverage predictable sales patterns for improved forecasting and planning"
    )
  }
  
  # Recommendation 6: Based on growth consistency
  if(analysis$growth_consistency < 60) {
    recommendations <- c(recommendations, 
      "Focus on improving consistency through better demand management and customer retention"
    )
  }
  
  # Recommendation 7: Based on half-year performance
  if(abs(analysis$h2_vs_h1) > 15) {
    recommendations <- c(recommendations, sprintf(
      "Analyze factors driving %s half performance to replicate success year-round",
      analysis$better_half
    ))
  }
  
  # Recommendation 8: Based on correlation
  if(analysis$correlation_strength == "strong") {
    recommendations <- c(recommendations, 
      "Leverage cross-selling opportunities given strong correlation between products"
    )
  }
  
  insights$recommendations <- recommendations
  
  # ---- RISK FACTORS ----
  risks <- c()
  
  if(analysis$market_share[analysis$best_product] > 50) {
    risks <- c(risks, sprintf(
      "High dependency on %s creates concentration risk",
      gsub("_", " ", analysis$best_product)
    ))
  }
  
  if(analysis$volatility_level == "high") {
    risks <- c(risks, "High sales volatility makes forecasting challenging")
  }
  
  if(analysis$trend_direction == "decreasing") {
    risks <- c(risks, "Declining sales trend requires immediate attention")
  }
  
  if(analysis$gap_percentage > 40) {
    risks <- c(risks, "Significant performance gap between products indicates imbalanced portfolio")
  }
  
  insights$risks <- risks
  
  # ---- OPPORTUNITIES ----
  opportunities <- c()
  
  if(analysis$trend_direction == "increasing") {
    opportunities <- c(opportunities, "Strong growth momentum provides platform for expansion")
  }
  
  if(analysis$correlation_strength == "strong") {
    opportunities <- c(opportunities, "High product correlation enables effective bundling strategies")
  }
  
  if(abs(analysis$h2_vs_h1) > 10) {
    opportunities <- c(opportunities, sprintf(
      "Analyze %s half success factors to boost overall annual performance",
      analysis$better_half
    ))
  }
  
  opportunities <- c(opportunities, sprintf(
    "Replicate %s strategies across slower months",
    analysis$best_month
  ))
  
  insights$opportunities <- opportunities
  
  return(insights)
}

# ============================================================================
# 3. DYNAMIC TEXT BLOCK GENERATOR
# ============================================================================

create_dynamic_text_block <- function(texts, title = NULL, title_color = "#4472C4", 
                                     title_size = 14, body_size = 11) {
  
  blocks <- list()
  
  if(!is.null(title)) {
    blocks[[length(blocks) + 1]] <- fpar(
      ftext(title, prop = fp_text(bold = TRUE, font.size = title_size, color = title_color))
    )
    blocks[[length(blocks) + 1]] <- fpar(ftext("", prop = fp_text(font.size = 4)))
  }
  
  for(text in texts) {
    blocks[[length(blocks) + 1]] <- fpar(
      ftext(paste0("• ", text), prop = fp_text(font.size = body_size))
    )
  }
  
  do.call(block_list, blocks)
}

create_summary_block <- function(text, title_size = 12, body_size = 11) {
  block_list(
    fpar(ftext(text, prop = fp_text(font.size = body_size)))
  )
}

# ============================================================================
# 4. DATA PREPARATION
# ============================================================================

# Load your data or create sample
set.seed(123)
sales_data <- data.frame(
  Month = month.name[1:12],
  Month_Num = 1:12,
  Product_A = round(40000 + 15000 * sin((1:12) * pi/6) + rnorm(12, 0, 3000)),
  Product_B = round(35000 + 12000 * sin((1:12) * pi/6 - 0.5) + rnorm(12, 0, 2500)),
  Product_C = round(30000 + 10000 * sin((1:12) * pi/6 - 1) + rnorm(12, 0, 2000))
)

sales_data$Total <- sales_data$Product_A + sales_data$Product_B + sales_data$Product_C
sales_data$MoM_Growth <- c(NA, round((diff(sales_data$Total) / sales_data$Total[-12]) * 100, 1))

# ============================================================================
# 5. PERFORM DYNAMIC ANALYSIS
# ============================================================================

cat("\n========================================\n")
cat("Analyzing Data Patterns...\n")
cat("========================================\n")

analysis <- analyze_data_patterns(sales_data)
insights <- generate_dynamic_insights(analysis)

cat("✓ Pattern analysis complete\n")
cat("✓ Insights generated dynamically\n")
cat("✓ Recommendations created based on data\n\n")

# ============================================================================
# 6. CREATE VISUALIZATIONS (with dynamic titles)
# ============================================================================

# Chart 1: Trend with dynamic title
create_trend_chart <- function(data, analysis) {
  trend_desc <- sprintf("%s %s trend", 
                       tools::toTitleCase(analysis$trend_strength),
                       analysis$trend_direction)
  
  data_long <- data %>%
    select(Month, Month_Num, Product_A, Product_B, Product_C) %>%
    pivot_longer(cols = c(Product_A, Product_B, Product_C), 
                 names_to = "Product", values_to = "Sales")
  
  ggplot(data_long, aes(x = Month_Num, y = Sales, color = Product, group = Product)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2, linetype = "dashed") +
    scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c"),
                       labels = c("Product A", "Product B", "Product C")) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Sales Trend Analysis",
      subtitle = trend_desc,
      x = "Month",
      y = "Sales ($)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

# Chart 2: Performance with dynamic annotations
create_performance_chart <- function(data, analysis) {
  product_data <- data.frame(
    Product = c("Product A", "Product B", "Product C"),
    Sales = analysis$product_totals,
    Share = analysis$market_share
  )
  
  ggplot(product_data, aes(x = Product, y = Sales, fill = Product)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(dollar(Sales/1000, suffix = "K"), "\n", 
                                  round(Share, 1), "%")),
              vjust = -0.5, size = 4, fontface = "bold") +
    scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
    scale_y_continuous(labels = comma, limits = c(0, max(product_data$Sales) * 1.2)) +
    labs(
      title = "Product Performance & Market Share",
      subtitle = sprintf("%s leads with %s share", 
                        gsub("_", " ", analysis$best_product),
                        round(analysis$market_share[analysis$best_product], 1)),
      x = "",
      y = "Total Sales ($)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      legend.position = "none"
    )
}

# Chart 3: Growth with dynamic coloring
create_growth_chart <- function(data, analysis) {
  growth_data <- data %>% filter(!is.na(MoM_Growth))
  
  ggplot(growth_data, aes(x = factor(Month, levels = month.name[2:12]), y = MoM_Growth)) +
    geom_col(aes(fill = MoM_Growth > 0)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_text(aes(label = paste0(MoM_Growth, "%")), 
              vjust = ifelse(growth_data$MoM_Growth > 0, -0.5, 1.5), size = 3.5) +
    scale_fill_manual(values = c("#d62728", "#2ca02c"), guide = "none") +
    labs(
      title = "Month-over-Month Growth Pattern",
      subtitle = sprintf("Average growth: %s%% | %d positive months, %d negative months",
                        round(analysis$avg_growth, 1),
                        analysis$positive_months,
                        analysis$negative_months),
      x = "Month",
      y = "Growth Rate (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Chart 4: Quarterly with dynamic insights
create_quarterly_chart <- function(data, analysis) {
  quarterly <- analysis$quarterly_data
  
  ggplot(quarterly, aes(x = Quarter, y = Total, fill = Quarter)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = dollar(Total/1000, suffix = "K")), 
              vjust = -0.5, size = 4, fontface = "bold") +
    scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")) +
    scale_y_continuous(labels = comma, limits = c(0, max(quarterly$Total) * 1.15)) +
    labs(
      title = "Quarterly Sales Performance",
      subtitle = sprintf("%s was strongest, %s was weakest", 
                        analysis$best_quarter, analysis$worst_quarter),
      x = "",
      y = "Sales ($)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      legend.position = "none"
    )
}

# ============================================================================
# 7. BUILD DYNAMIC POWERPOINT
# ============================================================================

cat("Building PowerPoint Presentation...\n")

ppt <- read_pptx()

# SLIDE 1: Dynamic Title
ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
ppt <- ph_with(ppt, value = "Sales Performance Analysis", 
               location = ph_location_type(type = "ctrTitle"))
ppt <- ph_with(ppt, value = sprintf("Comprehensive Data-Driven Insights | %s Trend Identified", 
                                    tools::toTitleCase(analysis$trend_direction)),
               location = ph_location_type(type = "subTitle"))
cat("✓ Slide 1: Dynamic title created\n")

# SLIDE 2: Executive Summary (Dynamic)
ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
ppt <- ph_with(ppt, 
               value = fpar(ftext("Executive Summary", 
                                 prop = fp_text(font.size = 32, bold = TRUE, color = "#4472C4"))),
               location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.7))

# Dynamic summary text
summary_text <- create_summary_block(insights$executive_summary, body_size = 12)
ppt <- ph_with(ppt, value = summary_text,
               location = ph_location(left = 0.5, top = 1.1, width = 9, height = 1.2))

# Dynamic findings (first 5)
findings_text <- create_dynamic_text_block(
  insights$key_findings[1:min(5, length(insights$key_findings))],
  title = "Key Findings:",
  title_size = 14,
  body_size = 11
)
ppt <- ph_with(ppt, value = findings_text,
               location = ph_location(left = 0.5, top = 2.5, width = 9, height = 2.5))

# Dynamic recommendations (first 4)
rec_text <- create_dynamic_text_block(
  insights$recommendations[1:min(4, length(insights$recommendations))],
  title = "Strategic Recommendations:",
  title_color = "#2ca02c",
  title_size = 14,
  body_size = 11
)
ppt <- ph_with(ppt, value = rec_text,
               location = ph_location(left = 0.5, top = 5.2, width = 9, height = 2))
cat("✓ Slide 2: Dynamic executive summary\n")

# SLIDE 3: Detailed Findings with Charts
ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
ppt <- ph_with(ppt, 
               value = fpar(ftext("Detailed Analysis & Findings", 
                                 prop = fp_text(font.size = 28, bold = TRUE, color = "#4472C4"))),
               location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.7))

# All findings
all_findings <- create_dynamic_text_block(
  insights$key_findings,
  title = NULL,
  body_size = 10
)
ppt <- ph_with(ppt, value = all_findings,
               location = ph_location(left = 0.5, top = 1.1, width = 4.5, height = 6))

# Performance chart
ppt <- ph_with(ppt, value = create_performance_chart(sales_data, analysis),
               location = ph_location(left = 5.2, top = 1.1, width = 4.5, height = 3))

# Quarterly chart
ppt <- ph_with(ppt, value = create_quarterly_chart(sales_data, analysis),
               location = ph_location(left = 5.2, top = 4.3, width = 4.5, height = 2.8))
cat("✓ Slide 3: Detailed findings with charts\n")

# SLIDE 4: Trend Analysis
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = sprintf("Sales Trend Analysis - %s %s Pattern",
                                    tools::toTitleCase(analysis$trend_strength),
                                    tools::toTitleCase(analysis$trend_direction)),
               location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = create_trend_chart(sales_data, analysis),
               location = ph_location_type(type = "body"))
cat("✓ Slide 4: Dynamic trend analysis\n")

# SLIDE 5: Growth Analysis
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = sprintf("Growth Pattern Analysis - %s%% Average Growth",
                                    round(analysis$avg_growth, 1)),
               location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = create_growth_chart(sales_data, analysis),
               location = ph_location_type(type = "body"))
cat("✓ Slide 5: Dynamic growth analysis\n")

# SLIDE 6: Recommendations & Action Items
ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
ppt <- ph_with(ppt, 
               value = fpar(ftext("Strategic Recommendations & Action Plan", 
                                 prop = fp_text(font.size = 28, bold = TRUE, color = "#2ca02c"))),
               location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.7))

# Recommendations
rec_block <- create_dynamic_text_block(
  insights$recommendations,
  title = "Priority Actions:",
  title_color = "#2ca02c",
  title_size = 16,
  body_size = 11
)
ppt <- ph_with(ppt, value = rec_block,
               location = ph_location(left = 0.5, top = 1.2, width = 4.5, height = 6))

# Opportunities and Risks
if(length(insights$opportunities) > 0) {
  opp_block <- create_dynamic_text_block(
    insights$opportunities,
    title = "Opportunities:",
    title_color = "#2ca02c",
    title_size = 14,
    body_size = 10
  )
  ppt <- ph_with(ppt, value = opp_block,
                 location = ph_location(left = 5.2, top = 1.2, width = 4.5, height = 3))
}

if(length(insights$risks) > 0) {
  risk_block <- create_dynamic_text_block(
    insights$risks,
    title = "Risk Factors:",
    title_color = "#d62728",
    title_size = 14,
    body_size = 10
  )
  ppt <- ph_with(ppt, value = risk_block,
                 location = ph_location(left = 5.2, top = 4.5, width = 4.5, height = 2.7))
}
cat("✓ Slide 6: Dynamic recommendations and risks\n")

# SLIDE 7: Key Metrics Summary
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Key Performance Metrics",
               location = ph_location_type(type = "title"))

metrics_data <- data.frame(
  Metric = c(
    "Total Annual Sales",
    "Average Monthly Sales",
    "Best Performing Product",
    "Market Leader Share",
    "Sales Trend Direction",
    "Trend Strength (R²)",
    "Average Growth Rate",
    "Growth Consistency",
    "Best Quarter",
    "Worst Quarter",
    "Peak Month",
    "Sales Volatility (CV)",
    "Product Correlation",
    sprintf("%s vs %s Half", 
            tools::toTitleCase(analysis$better_half),
            tools::toTitleCase(ifelse(analysis$better_half == "first", "second", "first")))
  ),
  Value = c(
    dollar(analysis$total_sales),
    dollar(analysis$avg_monthly),
    gsub("_", " ", analysis$best_product),
    percent(analysis$market_share[analysis$best_product]/100, accuracy = 0.1),
    tools::toTitleCase(analysis$trend_direction),
    round(analysis$trends$Total$r2, 3),
    paste0(round(analysis$avg_growth, 1), "%"),
    paste0(round(analysis$growth_consistency, 1), "%"),
    analysis$best_quarter,
    analysis$worst_quarter,
    analysis$best_month,
    paste0(round(analysis$cv, 1), "%"),
    tools::toTitleCase(analysis$correlation_strength),
    ifelse(analysis$h2_vs_h1 > 0, 
           paste0("+", round(analysis$h2_vs_h1, 1), "%"),
           paste0(round(analysis$h2_vs_h1, 1), "%"))
  )
)

metrics_table <- flextable(metrics_data) %>%
  theme_booktabs() %>%
  bg(part = "header", bg = "#4472C4") %>%
  color(part = "header", color = "white") %>%
  bold(part = "header") %>%
  bold(j = 1) %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left") %>%
  fontsize(size = 10, part = "all") %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 2) %>%
  autofit()

ppt <- ph_with(ppt, value = metrics_table,
               location = ph_location_type(type = "body"))
cat("✓ Slide 7: Dynamic metrics summary\n")

# ============================================================================
# 8. SAVE AND REPORT
# ============================================================================

output_file <- "Dynamic_Sales_Analysis.pptx"
print(ppt, target = output_file)

cat("\n========================================\n")
cat("✓ DYNAMIC ANALYSIS COMPLETE!\n")
cat("========================================\n")
cat("File:", output_file, "\n")
cat("\nAll content generated dynamically based on data:\n")
cat("  • Executive summary\n")
cat("  • Key findings (", length(insights$key_findings), ")\n", sep = "")
cat("  • Recommendations (", length(insights$recommendations), ")\n", sep = "")
cat("  • Opportunities (", length(insights$opportunities), ")\n", sep = "")
cat("  • Risks (", length(insights$risks), ")\n", sep = "")
cat("\nKey Insights Detected:\n")
cat("  • Trend:", analysis$trend_direction, "(", analysis$trend_strength, ")\n")
cat("  • Best product:", gsub("_", " ", analysis$best_product), "\n")
cat("  • Best quarter:", analysis$best_quarter, "\n")
cat("  • Growth consistency:", round(analysis$growth_consistency, 1), "%\n")
cat("  • Volatility:", analysis$volatility_level, "\n")
cat("========================================\n\n")

# Print sample insights
cat("Sample Generated Insights:\n")
cat("\nExecutive Summary:\n", insights$executive_summary, "\n")
cat("\nFirst 3 Findings:\n")
for(i in 1:min(3, length(insights$key_findings))) {
  cat(i, ". ", insights$key_findings[i], "\n", sep = "")
}
cat("\nFirst 3 Recommendations:\n")
for(i in 1:min(3, length(insights$recommendations))) {
  cat(i, ". ", insights$recommendations[i], "\n", sep = "")
}
