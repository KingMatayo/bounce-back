# How to Create a Data-Driven Quarto Website Project

This document describes how to create a project like **Bounce Back** - a multi-chapter Quarto website with R-based data visualizations, interactive charts, and professional styling.

---

## Project Overview

**Bounce Back** is an NBA analytics website that:
- Uses **Quarto** for document rendering and website generation
- Uses **R** for data analysis and visualization
- Uses **Highcharter** for interactive JavaScript charts
- Uses **ggplot2** and **gganimate** for static charts and animations
- Deploys to **GitHub Pages**

---

## Step 1: Project Structure

Create this directory structure:

```
project-name/
├── _quarto.yml          # Website configuration
├── index.qmd            # Landing page
├── _common.R            # Shared R functions
├── styles.css           # Custom CSS styling
├── chapters/            # Content chapters
│   ├── chapter1.qmd
│   ├── chapter2.qmd
│   └── ...
├── appendices/          # Supplementary content
│   ├── appendix-a.qmd
│   └── ...
├── data/                # Centralized data files
│   ├── main-dataset.csv
│   └── ...
└── assets/              # Media files
    ├── images/
    ├── gifs/
    └── fonts/
```

### Key Principles:
1. **Centralize data** - Put all CSV files in one `data/` folder
2. **Centralize functions** - Put reusable R code in `_common.R`
3. **Centralize assets** - Keep images/fonts in `assets/`
4. **Use relative paths** - All chapters use `../data/`, `../assets/`, etc.

---

## Step 2: Create `_quarto.yml`

This is the heart of your Quarto website. Example:

```yaml
project:
  type: website
  output-dir: _site

website:
  title: "Your Project Title"
  description: "Project description"
  favicon: assets/images/logo.jpg

  navbar:
    title: "Project Name"
    logo: assets/images/logo.jpg
    background: dark
    left:
      - text: "Home"
        href: index.qmd
      - text: "Section 1"
        menu:
          - text: "Chapter 1"
            href: chapters/chapter1.qmd
          - text: "Chapter 2"
            href: chapters/chapter2.qmd
      - text: "Appendices"
        menu:
          - text: "Appendix A"
            href: appendices/appendix-a.qmd
    right:
      - icon: github
        href: https://github.com/username/repo

  sidebar:
    - title: "Chapters"
      style: "floating"
      search: true
      contents:
        - section: "Section Name"
          contents:
            - chapters/chapter1.qmd
            - chapters/chapter2.qmd

  page-footer:
    left: "Created by Your Name"
    right: "Built with Quarto"

format:
  html:
    theme:
      light: flatly
      dark: darkly
    css: styles.css
    toc: true
    toc-location: left
    code-fold: true
    code-summary: "Show code"

execute:
  freeze: auto
  echo: false
  warning: false
  message: false
```

---

## Step 3: Create `_common.R`

This file contains shared functions and is sourced by all chapters:

```r
# =============================================================================
# Project Name - Shared Functions
# =============================================================================

# Load required libraries
library(tidyverse)
library(highcharter)
library(ggplot2)
library(gganimate)
library(scales)

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

load_main_data <- function() {
  read.csv("data/main-dataset.csv")
}

# =============================================================================
# VISUALIZATION FUNCTIONS
# =============================================================================

# Highcharter line chart
create_hc_line <- function(data, x_col, y_col, title = "") {

  highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = title) %>%
    hc_xAxis(categories = data[[x_col]]) %>%
    hc_add_series(name = y_col, data = data[[y_col]]) %>%
    hc_add_theme(hc_theme_flatdark())
}

# ggplot theme for consistency
theme_project <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 11),
      legend.position = "bottom"
    )
}

# =============================================================================
# COLOR PALETTES
# =============================================================================

project_colors <- list(
  primary = "#1a1a2e",
  secondary = "#16213e",
  accent = "#e94560"
)
```

---

## Step 4: Create Chapter Templates

Each chapter follows this structure:

```markdown
---
title: "Chapter Title"
subtitle: "Chapter subtitle"
author:
  - name: "Your Name"
    email: your@email.com
date: 01/01/2025
image: "../assets/images/chapter-image.jpg"
categories: [Category1, Category2]
---

```{r setup, warning=FALSE, message=FALSE}
# Load shared functions
source("../_common.R")

# Load data
data <- read.csv("../data/main-dataset.csv")
```

> *"Opening quote"* – Author

## Section 1

Narrative text here...

```{r visualization-1, warning=FALSE, message=FALSE}
#| fig-cap: "Caption for the visualization"

# Create visualization
highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Chart Title") %>%
  # ... rest of chart code
```

## Section 2

More content...

::: {.callout-tip}
## Key Takeaways
1. Point one
2. Point two
:::

---

## References

- Reference 1
- Reference 2
```

---

## Step 5: Create Landing Page (`index.qmd`)

```markdown
---
title: "Project Title"
subtitle: "Project tagline"
page-layout: full
toc: false
---

::: {.hero-banner}
![](assets/images/logo.jpg){width="150px"}

### Welcome message

[Start Reading](chapters/chapter1.qmd){.btn .btn-primary}
:::

## Chapters

::: {.grid}
::: {.g-col-12 .g-col-md-4}
::: {.card .chapter-card}
[![](assets/images/ch1.jpg)](chapters/chapter1.qmd)
::: {.card-body}
#### [Chapter 1](chapters/chapter1.qmd)
Description here.
:::
:::
:::
:::
```

---

## Step 6: Create Custom Styling (`styles.css`)

```css
/* CSS Variables */
:root {
  --primary-color: #1a1a2e;
  --accent-color: #e94560;
  --shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  --border-radius: 12px;
}

/* Hero Banner */
.hero-banner {
  text-align: center;
  padding: 3rem 2rem;
  background: linear-gradient(135deg, var(--primary-color) 0%, #16213e 100%);
  color: white;
  border-radius: var(--border-radius);
}

/* Chapter Cards */
.card.chapter-card {
  border: none;
  border-radius: var(--border-radius);
  box-shadow: var(--shadow);
  transition: all 0.3s ease;
}

.card.chapter-card:hover {
  transform: translateY(-8px);
}

/* Responsive */
@media (max-width: 768px) {
  .hero-banner { padding: 2rem 1rem; }
}
```

---

## Step 7: Data Strategy

### For datasets with player-level data:
```r
# Filter for specific player
player_data <- all_data %>%
  filter(namePlayer == "Player Name")

# Filter by team
team_data <- all_data %>%
  filter(grepl("Team Name", nameTeam))

# Filter by season range
era_data <- all_data %>%
  filter(yearSeason >= 2010 & yearSeason <= 2020)
```

### For historical data (pre-dataset era):
Create manual tibbles with career statistics:
```r
player_career <- tibble(
  season = c("2010-11", "2011-12", "2012-13"),
  ppg = c(25.3, 27.1, 26.8),
  rpg = c(7.2, 7.9, 8.0)
)
```

---

## Step 8: Visualization Patterns

### Highcharter Interactive Charts

```r
# Line chart
highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(text = "Title") %>%
  hc_xAxis(categories = data$x) %>%
  hc_yAxis(title = list(text = "Y Label")) %>%
  hc_add_series(name = "Series", data = data$y, color = "#e94560") %>%
  hc_add_theme(hc_theme_flatdark())

# Column chart
highchart() %>%
  hc_chart(type = "column") %>%
  hc_add_series(data = values, colorByPoint = TRUE)

# Pie/Donut chart
highchart() %>%
  hc_chart(type = "pie") %>%
  hc_add_series(data = list(
    list(name = "A", y = 30),
    list(name = "B", y = 70)
  ))
```

### ggplot2 with gganimate

```r
# Animated line chart
p <- ggplot(data, aes(x = year, y = value, color = group)) +
  geom_line() +
  geom_point() +
  transition_reveal(year) +
  theme_minimal()

# Save animation
anim_save("assets/gifs/animation.gif", p)
```

---

## Step 9: Build and Deploy

### Local Preview
```bash
quarto preview
```

### Build Site
```bash
quarto render
```

### Deploy to GitHub Pages
```bash
quarto publish gh-pages
```

---

## Common Issues and Solutions

### 1. Column not found error
```
Error: object 'columnName' not found
```
**Solution:** Check actual column names with `names(data)` or `head -1 data.csv`

### 2. File path errors
```
Error: cannot open file '../data/file.csv'
```
**Solution:** Ensure paths are relative to the `.qmd` file location

### 3. Package not installed
```
Error: there is no package called 'packageName'
```
**Solution:** Add to `_common.R`: `library(packageName)` and install with `install.packages("packageName")`

### 4. Render timeout
**Solution:** Add to `_quarto.yml`:
```yaml
execute:
  freeze: auto
```

---

## Required R Packages

```r
install.packages(c(
  "tidyverse",      # Data manipulation
  "highcharter",    # Interactive charts
  "ggplot2",        # Static charts
  "gganimate",      # Animations
  "scales",         # Number formatting
  "glue",           # String interpolation
  "htmltools",      # HTML widgets
  "viridis",        # Color palettes
  "janitor"         # Data cleaning
))
```

---

## Project Checklist

- [ ] Create directory structure
- [ ] Create `_quarto.yml` with navigation
- [ ] Create `_common.R` with shared functions
- [ ] Create `styles.css` for theming
- [ ] Create `index.qmd` landing page
- [ ] Create all chapter `.qmd` files
- [ ] Centralize data in `data/` folder
- [ ] Organize assets in `assets/` folder
- [ ] Test with `quarto preview`
- [ ] Build with `quarto render`
- [ ] Deploy to GitHub Pages

---

## Tips for Success

1. **Start with one complete chapter** as a template, then replicate
2. **Use `freeze: auto`** to avoid re-rendering unchanged chapters
3. **Test visualizations individually** before adding to chapters
4. **Keep data paths consistent** - always use relative paths from chapter location
5. **Use Highcharter themes** for consistent interactive chart styling
6. **Add figure captions** with `#| fig-cap:` for accessibility
7. **Use callout blocks** (`::: {.callout-tip}`) for key takeaways

---

*This documentation was created for the Bounce Back NBA Analytics project.*
