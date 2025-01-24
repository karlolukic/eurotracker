# EuroTrackeR: An Interactive Dashboard for ECB Data

### Author: Karlo Lukic

**Date**: January 24, 2025

------------------------------------------------------------------------

## **Project Description**

EuroTrackeR is an interactive dashboard built with R and Shiny that replicates key features of the official European Central Bank (ECB) Data Portal. The project leverages the `ecb` package by Eric Persson for seamless retrieval of ECB data, enabling users to explore and visualize Euro Area economic indicators in an accessible and engaging way.

This repository includes the full source code, daily data update workflows, and all necessary files to reproduce and deploy the dashboard.

------------------------------------------------------------------------

## **Repository Structure**

```         
├── app.R                  # Shiny app script
├── update_data.R          # Script for updating data via the ECB API
├── .github/workflows/     # Contains the GitHub Actions YAML for automation
│   └── update_data.yml    # Workflow for daily data updates
├── data/                  # Directory containing the latest ECB data files (.rds)
├── README.md              # Project README file
```

------------------------------------------------------------------------

## **How the Project Works**

1.  **Daily Data Updates**\
    The `update_data.R` script uses the ECB API to retrieve the latest available economic data, including indicators such as inflation, GDP growth, and unemployment rates.\
    Updates are automated using a GitHub Actions workflow (`update_data.yml`), which fetches new data daily at 6 AM CET and pushes updates to this repository.

2.  **Interactive Dashboard**\
    The `app.R` script powers the interactive Shiny dashboard, allowing users to explore Euro Area economic data visually. It includes:

    -   Time series charts
    -   Customizable filters
    -   Tabbed navigation for different economic categories

------------------------------------------------------------------------

## **Key Features**

-   **Data Automation**: Daily updates ensure the dashboard always reflects the latest economic trends.
-   **Open Source**: The project demonstrates the use of R, Shiny, and the `ecb` package for real-world applications.
-   **Interactive Visualizations**: Explore time series data with customizable filters and sleek, responsive UI components.

------------------------------------------------------------------------

## **Attribution**

This project was inspired by the official ECB Data Portal. The data is sourced from publicly available ECB resources via the `ecb` package.

Special thanks to [Eric Persson](https://cran.r-project.org/web/packages/ecb/index.html) for creating the `ecb` package, which facilitates access to ECB statistical data in R.

**Disclaimer**: This project is not endorsed by the European Central Bank (ECB). All interpretations and visualizations are the responsibility of the author.

------------------------------------------------------------------------

## **Getting Started**

1.  **Run Locally**\
    Clone this repository and install the required packages. Run the `app.R` script in RStudio to launch the dashboard.

2.  **Live Updates**\
    Ensure the daily update workflow is active to keep the data current.

------------------------------------------------------------------------

## **Contact**

For questions, suggestions, or feature requests, feel free to reach out via [GitHub Issues](https://github.com/karlolukic/eurotracker/issues).
