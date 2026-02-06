# Automatic Detailed Analysis Report with PowerPoint Generation

## Overview

This system automatically generates comprehensive business analysis reports with professional PowerPoint presentations. Each slide contains:
- **Summary section** - Overview and context
- **One professional chart** - Data visualization  
- **One data table** - Detailed metrics
- **Bullet points** - In-depth analysis and insights

## Quick Start

```bash
# Run the complete automated workflow
python3 /home/claude/analysis_report.py && node /home/claude/generate_presentation.js
```

## What This System Does

### Phase 1: Data Analysis (Python)
1. Generates or loads business data (48 records across regions, products, quarters)
2. Performs comprehensive statistical analysis
3. Creates 4 professional charts (300 DPI)
4. Generates insights and recommendations
5. Exports structured data to JSON

### Phase 2: Presentation Generation (Node.js)
1. Reads analysis data
2. Creates 4 beautifully designed PowerPoint slides
3. Embeds charts and tables
4. Applies professional teal theme
5. Exports final .pptx file

## The 4 Slides

### Slide 1: Executive Summary
- Regional sales comparison chart
- KPI metrics table (Sales, Profit, Margin, Units)
- 4 key performance insights

### Slide 2: Regional Performance Deep Dive  
- Quarterly sales trend chart
- Regional breakdown table
- 4 regional analysis points

### Slide 3: Product Portfolio Performance
- Product sales vs profit chart
- Product metrics table
- 4 product insights

### Slide 4: Strategic Recommendations
- Profit margin by region chart
- Quarterly growth table
- 5 strategic action items

## Files Included

- **analysis_report.py** - Python analysis engine
- **generate_presentation.js** - PowerPoint generator
- **analysis_report.R** - R alternative (requires R)
- **README.md** - This documentation

## Customizing for Your Data

Replace the sample data generation:

```python
# In analysis_report.py, replace generate_sample_data() with:
def load_custom_data():
    data = pd.read_csv('your_data.csv')
    # Required columns: Quarter, Region, Product, Sales, Profit, Units_Sold
    return data
```

## Requirements

**Python packages:**
```bash
pip install pandas matplotlib numpy --break-system-packages
```

**Node.js packages:**
```bash
npm install -g pptxgenjs
```

## Output

**Location:** `/mnt/user-data/outputs/Business_Analysis_Report.pptx`

The PowerPoint includes:
- 4 slides with consistent professional design
- 4 embedded high-resolution charts
- 4 formatted data tables  
- 17+ analysis bullet points
- 5 strategic recommendations

## Color Theme (Teal Professional)

- Primary: #0D9488 (Teal)
- Secondary: #14B8A6 (Light Teal)
- Accent: #5EEAD4 (Mint)
- Background: #F0FDFA (Very Light Teal)

Perfect for business, finance, consulting, and professional reports.
