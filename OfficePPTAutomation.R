
#!/usr/bin/env Rscript

# ============================================================================
# AUTOMATED DETAIL ANALYSIS REPORT GENERATOR
# ============================================================================
# This script generates a comprehensive PowerPoint presentation with:
# - Summary slides with key findings
# - Charts (bar, line, pie, etc.)
# - Tables with detailed data
# - Bullet point analysis and recommendations
# ============================================================================

# Load required libraries
library(officer)      # For PowerPoint generation
library(ggplot2)      # For creating charts
library(dplyr)        # For data manipulation
library(tidyr)        # For data tidying
library(flextable)    # For creating formatted tables
library(rvg)          # For vector graphics in PowerPoint

# Set random seed for reproducibility
set.seed(123)

# ============================================================================
# SECTION 1: DATA GENERATION (Replace with your actual data)
# ============================================================================

# Generate sample sales data
sales_data <- data.frame(
  Month = month.name[1:12],
  Sales = c(45000, 52000, 48000, 61000, 58000, 65000, 
            72000, 68000, 75000, 82000, 78000, 91000),
  Costs = c(28000, 31000, 29000, 36000, 35000, 38000,
            42000, 40000, 44000, 48000, 46000, 52000),
  Units = c(450, 520, 480, 610, 580, 650, 720, 680, 750, 820, 780, 910)
)

# Calculate profit and margin
sales_data <- sales_data %>%
  mutate(
    Profit = Sales - Costs,
    Margin = round((Profit / Sales) * 100, 1)
  )

# Generate sample customer data by segment
customer_data <- data.frame(
  Segment = c("Enterprise", "SMB", "Startup", "Government"),
  Customers = c(125, 340, 280, 45),
  Revenue = c(425000, 280000, 175000, 95000),
  Retention = c(92, 85, 78, 88)
)

# Generate sample product performance data
product_data <- data.frame(
  Product = c("Product A", "Product B", "Product C", "Product D", "Product E"),
  Q1_Sales = c(125000, 98000, 145000, 67000, 112000),
  Q2_Sales = c(142000, 105000, 138000, 72000, 128000),
  Q3_Sales = c(158000, 112000, 125000, 78000, 145000),
  Q4_Sales = c(175000, 118000, 115000, 85000, 162000)
)

product_data <- product_data %>%
  mutate(
    Total_Sales = Q1_Sales + Q2_Sales + Q3_Sales + Q4_Sales,
    Growth = round(((Q4_Sales - Q1_Sales) / Q1_Sales) * 100, 1)
  )

# ============================================================================
# SECTION 2: ANALYSIS & CALCULATIONS
# ============================================================================

# Calculate key metrics
total_revenue <- sum(sales_data$Sales)
total_profit <- sum(sales_data$Profit)
avg_margin <- mean(sales_data$Margin)
total_units <- sum(sales_data$Units)

# YoY growth (simulated)
revenue_growth <- 18.5
profit_growth <- 22.3

# Top performing month
top_month <- sales_data %>% 
  arrange(desc(Sales)) %>% 
  slice(1) %>% 
  pull(Month)

# Customer segment analysis
top_segment <- customer_data %>% 
  arrange(desc(Revenue)) %>% 
  slice(1) %>% 
  pull(Segment)

# Product analysis
top_product <- product_data %>% 
  arrange(desc(Total_Sales)) %>% 
  slice(1) %>% 
  pull(Product)

# ============================================================================
# SECTION 3: CHART CREATION
# ============================================================================

# Chart 1: Monthly Sales Trend
chart1 <- ggplot(sales_data, aes(x = factor(Month, levels = month.name[1:12]))) +
  geom_line(aes(y = Sales, group = 1), color = "#0D9488", size = 1.5) +
  geom_point(aes(y = Sales), color = "#0D9488", size = 3) +
  geom_bar(aes(y = Costs), stat = "identity", fill = "#94A3B8", alpha = 0.5, width = 0.6) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Monthly Sales vs Costs Trend",
    x = NULL,
    y = "Amount ($)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )+
  theme_classic()

# Chart 2: Customer Segment Revenue Distribution
chart2 <- ggplot(customer_data, aes(x = reorder(Segment, Revenue), y = Revenue)) +
  geom_bar(stat = "identity", fill = "#14B8A6", width = 0.7,alpha=0.4) +
  geom_text(aes(label = scales::dollar(Revenue, scale = 0.001, suffix = "K")), 
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Revenue by Customer Segment",
    x = NULL,
    y = "Revenue ($)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )+
  theme_classic()

# Chart 3: Product Performance Comparison
product_long <- product_data %>%
  select(Product, Q1_Sales, Q2_Sales, Q3_Sales, Q4_Sales) %>%
  pivot_longer(cols = -Product, names_to = "Quarter", values_to = "Sales") %>%
  mutate(Quarter = gsub("_Sales", "", Quarter))

chart3 <- ggplot(product_long, aes(x = Quarter, y = Sales, fill = Product)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4,alpha=.6) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_manual(values = c("#0D9488", "#14B8A6", "#5EEAD4", "#94A3B8", "#CBD5E1")) +
  labs(
    title = "Quarterly Sales by Product",
    x = NULL,
    y = "Sales ($)",
    fill = "Product"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )+
  theme_classic()

# Chart 4: Profit Margin Trend
chart4 <- ggplot(sales_data, aes(x = factor(Month, levels = month.name[1:12]), y = Margin, group = 1)) +
  geom_line(color = "#0891B2", size = 1.1,alpha=.5) +
  geom_point(color = "#0891B2", size = 2) +
  geom_hline(yintercept = mean(sales_data$Margin), 
             linetype = "dashed", color = "#DC2626", size = 1) +
  annotate("text", x = 10, y = mean(sales_data$Margin) + 0.5, 
           label = paste("Avg:", round(mean(sales_data$Margin), 1), "%"), 
           color = "#DC2626", fontface = "bold") +
  labs(
    title = "Profit Margin Trend (%)",
    x = NULL,
    y = "Margin (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )+
  theme_classic()

# 
# sales_data$MonthNum <- match(sales_data$Month, month.name)
# 
# chart5 <- ggplot(sales_data, aes(x = MonthNum, y = Margin)) +
#   geom_line(color = "#0891B2", linewidth = 1.5) +
#   geom_point(color = "#0891B2", size = 3) +
#   geom_hline(
#     yintercept = mean(sales_data$Margin),
#     linetype = "dashed",
#     color = "#DC2626",
#     linewidth = 1
#   ) +
#   annotate(
#     "text",
#     x = 10,
#     y = mean(sales_data$Margin) + 0.5,
#     label = paste("Avg:", round(mean(sales_data$Margin), 1), "%"),
#     color = "#DC2626",
#     fontface = "bold"
#   ) +
#   scale_x_continuous(
#     breaks = 1:12,
#     labels = month.name[1:12]
#   ) +
#   labs(
#     title = "Profit Margin Trend (%)",
#     x = NULL,
#     y = "Margin (%)"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     plot.title = element_text(face = "bold", size = 14),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     panel.grid.minor = element_blank()
#   )
# 
# print(chart5)


# ============================================================================
# SECTION 4: TABLE CREATION
# ============================================================================

# Table 1: Monthly Performance Summary
table1_data <- sales_data %>%
  select(Month, Sales, Costs, Profit, Margin) %>%
  mutate(
    Sales = scales::dollar(Sales),
    Costs = scales::dollar(Costs),
    Profit = scales::dollar(Profit),
    Margin = paste0(Margin, "%")
  )

table1 <- flextable(table1_data) %>%
  set_header_labels(
    Month = "Month",
    Sales = "Sales",
    Costs = "Costs",
    Profit = "Profit",
    Margin = "Margin %"
  ) %>%
  bg(bg = "#0D9488", part = "header") %>%
  color(color = "white", part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

# Table 2: Customer Segment Analysis
table2_data <- customer_data %>%
  mutate(
    Revenue = scales::dollar(Revenue),
    Avg_Revenue_Per_Customer = scales::dollar(round(customer_data$Revenue / customer_data$Customers)),
    Retention = paste0(Retention, "%")
  ) %>%
  select(Segment, Customers, Revenue, Avg_Revenue_Per_Customer, Retention)

table2 <- flextable(table2_data) %>%
  set_header_labels(
    Segment = "Segment",
    Customers = "Customers",
    Revenue = "Total Revenue",
    Avg_Revenue_Per_Customer = "Avg Revenue/Customer",
    Retention = "Retention Rate"
  ) %>%
  bg(bg = "#14B8A6", part = "header") %>%
  color(color = "white", part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

# Table 3: Product Performance Summary
table3_data <- product_data %>%
  select(Product, Q1_Sales, Q2_Sales, Q3_Sales, Q4_Sales, Total_Sales, Growth) %>%
  mutate(
    Q1_Sales = scales::dollar(Q1_Sales),
    Q2_Sales = scales::dollar(Q2_Sales),
    Q3_Sales = scales::dollar(Q3_Sales),
    Q4_Sales = scales::dollar(Q4_Sales),
    Total_Sales = scales::dollar(Total_Sales),
    Growth = paste0(ifelse(Growth > 0, "+", ""), Growth, "%")
  )

table3 <- flextable(table3_data) %>%
  set_header_labels(
    Product = "Product",
    Q1_Sales = "Q1",
    Q2_Sales = "Q2",
    Q3_Sales = "Q3",
    Q4_Sales = "Q4",
    Total_Sales = "Total",
    Growth = "Growth %"
  ) %>%
  bg(bg = "#5EEAD4", part = "header") %>%
  color(color = "#0F172A", part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  fontsize(size = 9, part = "all") %>%
  autofit()

# Table 4: Key Metrics Summary
table4_data <- data.frame(
  Metric = c("Total Revenue", "Total Profit", "Average Margin", "Total Units Sold",
             "Revenue Growth (YoY)", "Profit Growth (YoY)"),
  Value = c(
    scales::dollar(total_revenue),
    scales::dollar(total_profit),
    paste0(round(avg_margin, 1), "%"),
    format(total_units, big.mark = ","),
    paste0("+", revenue_growth, "%"),
    paste0("+", profit_growth, "%")
  ),
  Status = c("Strong", "Excellent", "Healthy", "Growing", "Above Target", "Above Target")
)

table4 <- flextable(table4_data) %>%
  bg(bg = "#0891B2", part = "header") %>%
  color(color = "white", part = "header") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "body") %>%
  fontsize(size = 10, part = "all") %>%
  autofit()

# ============================================================================
# SECTION 5: POWERPOINT GENERATION
# ============================================================================

# Create new PowerPoint presentation
pres <- read_pptx()

# Define color palette
color_primary <- "#0D9488"
color_secondary <- "#14B8A6"
color_accent <- "#5EEAD4"
color_text <- "#0F172A"

# ============================================================================
# SLIDE 1: Title Slide
# ============================================================================

pres <- add_slide(pres, layout = "Title Slide", master = "Office Theme")
pres <- ph_with(pres, value = "Comprehensive Business Analysis Report", 
                location = ph_location_type(type = "ctrTitle"))
pres <- ph_with(pres, value = "Detailed Performance Review & Strategic Recommendations", 
                location = ph_location_type(type = "subTitle"))

# ============================================================================
# SLIDE 2: Executive Summary - Sales Performance
# ============================================================================

pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")

# Title
pres <- ph_with(pres, value = "Sales Performance Overview", 
                location = ph_location_type(type = "title"))

# Summary text
summary_text <- paste0(
  "Key Findings:\n\n",
  "• Annual revenue reached ", scales::dollar(total_revenue), 
  " with a YoY growth of ", revenue_growth, "%\n",
  "• Total profit of ", scales::dollar(total_profit), 
  " representing ", profit_growth, "% growth\n",
  "• Average profit margin maintained at ", round(avg_margin, 1), "%\n",
  "• Peak performance in ", top_month, " with highest sales\n",
  "• Consistent upward trend throughout the year"
)

pres <- ph_with(pres,text_block, value = summary_text,
                location = ph_location(left = 0.5, top = 1.5, width = 4.5, height = 3.5))

# Add chart
pres <- ph_with(pres, value = dml(ggobj = chart1),
                location = ph_location(left = 5.2, top = 1.5, width = 4.5, height = 3.5))

# Add table
pres <- ph_with(pres, value = table1,
                location = ph_location(left = 0.5, top = 5.2, width = 9, height = 1.5))

# ============================================================================
# SLIDE 3: Customer Segment Analysis
# ============================================================================

pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")

# Title
pres <- ph_with(pres,text_block, value = "Customer Segment Analysis", 
                location = ph_location_type(type = "title"))

# Summary and analysis
segment_analysis <- paste0(
  "Segment Performance Insights:\n\n",
  "", top_segment, " segment leads with ", 
  scales::dollar(max(customer_data$Revenue)), " in revenue\n",
  "Total customer base: ", format(sum(customer_data$Customers), big.mark = ","), 
  " across all segments\n",
  "Enterprise segment shows highest retention at ", 
  max(customer_data$Retention), "%\n",
  "SMB segment has largest customer count with strong growth potential\n",
  "Government segment presents opportunities for expansion"
)

pres <- ph_with(pres,text_block, value = segment_analysis,
                location = ph_location(left = 0.5, top = 1.5, width = 4.5, height = 3))

# Add chart
pres <- ph_with(pres, value = dml(ggobj = chart2),
                location = ph_location(left = 5.2, top = 1.5, width = 4.5, height = 3.5))

# Add table
pres <- ph_with(pres, value = table2,
                location = ph_location(left = 0.5, top = 5.2, width = 9, height = 1.5))

# ============================================================================
# SLIDE 4: Product Performance Analysis
# ============================================================================

pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")

# Title
pres <- ph_with(pres, value = "Product Performance & Growth", 
                location = ph_location_type(type = "title"))

# Analysis text
product_analysis <- paste0(
  "Product Portfolio Analysis:\n\n",
  "• ", top_product, " leads the portfolio with highest total sales\n",
  "• Product A shows strongest growth at ", 
  max(product_data$Growth), "%\n",
  "• All products show positive year-over-year growth\n",
  "• Q4 performance exceeded expectations across all product lines\n",
  "• Product mix diversification reducing risk and maximizing revenue"
)

pres <- ph_with(pres,text_block, value = product_analysis,
                location = ph_location(left = 0.5, top = 1.5, width = 4.5, height = 3))

# Add chart
pres <- ph_with(pres, value = dml(ggobj = chart3),
                location = ph_location(left = 5.2, top = 1.5, width = 4.5, height = 3.5))

# Add table
pres <- ph_with(pres, value = table3,
                location = ph_location(left = 0.5, top = 5.2, width = 9, height = 1.5))

# ============================================================================
# SLIDE 5: Profitability & Margin Analysis
# ============================================================================

pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")

# Title
pres <- ph_with(pres, value = "Profitability & Margin Trends", 
                location = ph_location_type(type = "title"))

# Analysis text
margin_analysis <- paste0(
  "Margin Performance Insights:\n\n",
  "• Average profit margin stable at ", round(avg_margin, 1), "%\n",
  "• Consistent margin maintenance despite cost increases\n",
  "• Q4 showed improved cost efficiency\n",
  "• Operational improvements contributing to margin stability\n",
  "• Strong pricing power in key market segments"
)

pres <- ph_with(pres,text_block, value = margin_analysis,
                location = ph_location(left = 5.2, top = 5.2, width = 4.5, height = 3))

# Add chart
pres <- ph_with(pres, value = dml(ggobj = chart4),
                location = ph_location(left = 5.2, top = 1.5, width = 4.5, height = 3.5))

# Add table
pres <- ph_with(pres, value = table4,
                location = ph_location(left = 0.5, top = 1.5, width = 9, height = 1.5))

# ============================================================================
# SLIDE 6: Strategic Recommendations
# ============================================================================

pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")

# Title
pres <- ph_with(pres, value = "Strategic Recommendations & Next Steps", 
                location = ph_location_type(type = "title"),font.size=19)

# # Recommendations
# recommendations <- paste0(
#   "Strategic Priorities:\n\n",
#   "1. REVENUE ACCELERATION\n",
#   "   • Expand Enterprise segment presence to capitalize on high retention rates\n",
#   "   • Invest in SMB customer acquisition given large addressable market\n",
#   "   • Develop targeted campaigns for Government segment expansion\n\n",
#   "2. PRODUCT OPTIMIZATION\n",
#   "   • Scale Product A given exceptional growth trajectory\n",
#   "   • Bundle complementary products to increase average deal size\n",
#   "   • Sunset or reposition underperforming products\n\n",
#   "3. OPERATIONAL EFFICIENCY\n",
#   "   • Maintain focus on cost optimization to preserve margins\n",
#   "   • Invest in automation to improve operational leverage\n",
#   "   • Implement dynamic pricing strategies for margin enhancement\n\n",
#   "4. CUSTOMER SUCCESS\n",
#   "   • Launch retention program targeting segments below 90% retention\n",
#   "   • Implement customer health scoring to prevent churn\n",
#   "   • Expand customer success team to support growth"
# )

 

recommendations <- fpar(
  ftext("Strategic Priorities\n\n", fp_text(bold = TRUE, font.size = 14)),
  
  ftext("REVENUE ACCELERATION\n", fp_text(bold = TRUE)),
  ftext("Expand Enterprise segment presence to capitalize on high retention rates.\n", fp_text()),
  ftext("Invest in SMB customer acquisition given large addressable market.\n", fp_text()),
  ftext("Develop targeted campaigns for Government segment expansion.\n\n", fp_text()),
  
  ftext("PRODUCT OPTIMIZATION\n", fp_text(bold = TRUE)),
  ftext("Scale Product A given exceptional growth trajectory.\n", fp_text()),
  ftext("Bundle complementary products to increase average deal size.\n", fp_text()),
  ftext("Sunset or reposition underperforming products.", fp_text())
)


pres <- ph_with(pres,
                value = recommendations,
                location = ph_location(left = 0.5, top = 1.5, width = 9, height = 5))

# ============================================================================
# SLIDE 7: Action Plan & Timeline
# ============================================================================

pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")

# Title
pres <- ph_with(pres, value = "Implementation Action Plan", 
                location = ph_location_type(type = "title"))

# Action plan
action_plan <- paste0(
  "Immediate Actions (Next 30 Days):\n",
  "• Conduct deep-dive analysis of Enterprise segment opportunities\n",
  "• Launch Product A marketing campaign\n",
  "• Initiate cost optimization review across operations\n",
  "• Implement customer health scoring pilot program\n\n",
  "Short-term Priorities (30-90 Days):\n",
  "• Roll out SMB acquisition campaign\n",
  "• Complete product portfolio optimization\n",
  "• Deploy automation initiatives in key operational areas\n",
  "• Expand customer success team by 20%\n\n",
  "Long-term Initiatives (90+ Days):\n",
  "• Develop Government segment go-to-market strategy\n",
  "• Launch bundled product offerings\n",
  "• Achieve 5% improvement in operational efficiency\n",
  "• Target 95%+ retention rate across all segments"
)

pres <- ph_with(pres,text_block, value = action_plan,
                location = ph_location(left = 0.5, top = 1.5, width = 9, height = 5))

# ============================================================================
# SLIDE 8: Summary & Conclusion
# ============================================================================

pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")

# Title
pres <- ph_with(pres, value = "Executive Summary & Conclusions", 
                location = ph_location_type(type = "title"))

# Summary
conclusion <- paste0(
  "Key Takeaways:\n\n",
  "PERFORMANCE HIGHLIGHTS\n",
  "• Strong revenue growth of ", revenue_growth, "% demonstrates market traction\n",
  "• Profit growth of ", profit_growth, "% exceeds revenue growth, showing operational leverage\n",
  "• Healthy profit margins at ", round(avg_margin, 1), "% indicate competitive positioning\n",
  "• Diversified customer base across segments reduces concentration risk\n\n",
  "GROWTH OPPORTUNITIES\n",
  "• Enterprise segment expansion represents largest near-term opportunity\n",
  "• Product A momentum should be accelerated with additional investment\n",
  "• SMB segment volume potential justifies focused acquisition efforts\n",
  "• Government segment remains underpenetrated with high potential\n\n",
  "STRATEGIC IMPERATIVES\n",
  "• Maintain revenue momentum while preserving margin discipline\n",
  "• Balance growth investments with operational efficiency initiatives\n",
  "• Prioritize customer retention alongside new customer acquisition\n",
  "• Leverage product portfolio strengths for cross-sell opportunities"
)

pres <- ph_with(pres,text_block, value = conclusion,
                location = ph_location(left = 0.5, top = 1.5, width = 9, height = 5))

# ============================================================================
# SAVE PRESENTATION
# ============================================================================

output_file <- "Business_Analysis_Report_06Feb2026_V2.pptx"
print(pres, target = output_file)

cat("\n============================================================\n")
cat("REPORT GENERATION COMPLETE!\n")
cat("============================================================\n")
cat("Output file:", output_file, "\n")
cat("Total slides created: 8\n")
cat("- 1 Title slide\n")
cat("- 4 Analysis slides (with charts and tables)\n")
cat("- 1 Recommendations slide\n")
cat("- 1 Action plan slide\n")
cat("- 1 Summary slide\n")
cat("============================================================\n\n")













# 
# # ============ Not Great ========
# 
# 
# library(officer)
# library(ggplot2)
# library(dplyr)
# library(rvg)
# 
# 
# 
# df <- data.frame(
#   user_id = sample(1:5000, 20000, replace = TRUE),
#   month = sample(seq(as.Date("2025-01-01"), by = "month", length.out = 6), 20000, replace = TRUE),
#   content_type = sample(c("Sports", "Entertainment"), 20000, replace = TRUE, prob = c(0.65, 0.35)),
#   usage_minutes = rpois(20000, lambda = 35),
#   subscription = sample(c("Free", "Paid"), 20000, replace = TRUE, prob = c(0.6, 0.4))
# )
# 
# 
# 
# compute_kpis <- function(df) {
#   df %>%
#     summarise(
#       users = n_distinct(user_id),
#       avg_usage = round(mean(usage_minutes), 1),
#       heavy_users = round(mean(usage_minutes > 60) * 100, 1),
#       sports_share = round(mean(content_type == "Sports") * 100, 1),
#       paid_share = round(mean(subscription == "Paid") * 100, 1)
#     )
# }
# 
# 
# compute_trends <- function(df) {
#   df %>%
#     group_by(month, content_type) %>%
#     summarise(avg_usage = mean(usage_minutes), .groups = "drop")
# }
# 
# 
# segment_users <- function(df) {
#   df %>%
#     group_by(user_id) %>%
#     summarise(avg_usage = mean(usage_minutes)) %>%
#     mutate(
#       segment = case_when(
#         avg_usage >= 60 ~ "Power Users",
#         avg_usage >= 25 ~ "Regular Users",
#         TRUE ~ "Low Engagement"
#       )
#     )
# }
# 
# 
# 
# generate_insights <- function(kpis, segments) {
#   
#   insights <- c()
#   
#   if (kpis$sports_share > 60) {
#     insights <- c(
#       insights,
#       "Sports content is the primary engagement driver, accounting for over 60% of total usage."
#     )
#   }
#   
#   if (kpis$avg_usage < 40) {
#     insights <- c(
#       insights,
#       "Overall engagement remains moderate, indicating untapped consumption potential."
#     )
#   }
#   
#   low_eng_pct <- mean(segments$segment == "Low Engagement") * 100
#   
#   if (low_eng_pct > 35) {
#     insights <- c(
#       insights,
#       "More than one-third of users fall into the low-engagement segment, increasing churn risk."
#     )
#   }
#   
#   insights
# }
# 
# 
# generate_recommendations <- function(kpis, segments) {
#   
#   recs <- c()
#   
#   recs <- c(
#     recs,
#     "Launch sports-led subscription bundles to improve paid conversion and ARPU."
#   )
#   
#   if (kpis$avg_usage < 40) {
#     recs <- c(
#       recs,
#       "Introduce personalized content discovery to increase daily active usage."
#     )
#   }
#   
#   if (mean(segments$segment == "Low Engagement") > 0.3) {
#     recs <- c(
#       recs,
#       "Deploy targeted retention offers for low-engagement users using usage-based triggers."
#     )
#   }
#   
#   recs <- c(
#     recs,
#     "Leverage marquee sports events to cross-promote entertainment content."
#   )
#   
#   recs
# }
# 
# 
# library(ggplot2)
# 
# trend_plot <- function(trends) {
#   ggplot(trends, aes(month, avg_usage, color = content_type)) +
#     geom_line(size = 1.2) +
#     geom_point(size = 2) +
#     labs(
#       title = "Average Usage Trend by Content Type",
#       x = NULL,
#       y = "Avg Minutes"
#     ) +
#     theme_minimal(base_size = 12)
# }
# 
# 
# 
# 
# 
# library(officer)
# library(rvg)
# library(dplyr)
# 
# ppt <- read_pptx()
# 
# 
# kpis <- compute_kpis(df)
# segments <- segment_users(df)
# insights <- generate_insights(kpis, segments)
# recs <- generate_recommendations(kpis, segments)
# 
# ppt <- add_slide(ppt, layout = "Blank")
# 
# # Title
# ppt <- ph_with(
#   ppt,
#   "Executive Summary – Content Consumption",
#   location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.6)
# )
# 
# # KPI box
# ppt <- ph_with(
#   ppt,
#   paste0(
#     "Users: ", kpis$users, "\n",
#     "Avg Usage: ", kpis$avg_usage, " min\n",
#     "Sports Share: ", kpis$sports_share, "%\n",
#     "Paid Users: ", kpis$paid_share, "%"
#   ),
#   location = ph_location(left = 0.4, top = 1.1, width = 3.4, height = 2)
# )
# 
# # Insights
# ppt <- ph_with(
#   ppt,
#   paste("Key Insights\n•", paste(insights, collapse = "\n• ")),
#   location = ph_location(left = 4, top = 1.1, width = 5.5, height = 2)
# )
# 
# # Recommendations
# ppt <- ph_with(
#   ppt,
#   paste("Business Recommendations\n•", paste(recs, collapse = "\n• ")),
#   location = ph_location(left = 0.4, top = 3.3, width = 9, height = 2.2)
# )
# 
# 
# # Trend Slide
# 
# ppt <- add_slide(ppt, layout = "Blank")
# 
# ppt <- ph_with(
#   ppt,
#   "Usage Trend Analysis",
#   location = ph_location(left = 0.5, top = 0.3, width = 9, height = 0.6)
# )
# 
# ppt <- ph_with(
#   ppt,
#   dml(ggobj = trend_plot(compute_trends(df))),
#   location = ph_location(left = 0.5, top = 1.1, width = 9, height = 4.5)
# )
# 
# 
# print(ppt, target = "Content_Insights_Executive.pptx")
# 
