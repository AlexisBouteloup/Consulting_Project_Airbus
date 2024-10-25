# Airline Ticket Price Elasticity Analysis

## Overview

This project examines the elasticity of airline ticket prices relative to fuel costs using two comprehensive panel datasets. By analyzing these datasets, we aim to provide insights into the extent to which fuel price changes influence ticket prices across different airline types, regions, and over time. Additionally, we investigate how variables like business model, flight distance, and market conditions affect this elasticity. 

## Project Goals

- **Estimate Elasticity of Ticket Prices to Fuel Costs**: Quantify the responsiveness of airline ticket prices to fluctuations in fuel prices across different timeframes, regions, and airline categories.
- **Model Influence of Airline Type and Route**: Understand how factors such as low-cost vs. full-service models and flight distance interact with fuel price sensitivity.
- **Assess Short vs. Long-term Elasticities**: Use econometric models to distinguish between short- and long-term effects of fuel price changes on ticket prices.

## Data Collection

The project utilizes two distinct datasets:
1. **Global Airline Dataset**: Covers the 2010-2019 period and includes data from major airlines across various regions. The dataset allowed us to test elasticity variability across continents and business models.
2. **U.S. Airline Dataset**: A more extensive dataset with quarterly data for the top 14 airlines in the United States from 1995 to 2023. The high-frequency nature of this data enables analysis of short- and long-term elasticities.

## Techniques Used

- **Fixed Effects Ordinary Least Squares (OLS)**: For baseline elasticity estimations.
- **Panel Within Model**: For robustness checks on the impact of fuel costs on ticket prices.
- **Autoregressive Distributed Lag Model (ARDL)**: To estimate short- and long-term elasticity differences.
- **Interaction Analysis**: Testing interaction effects between fuel price and variables like airline type (low-cost vs. non-low-cost) and route type (long-haul vs. short-haul).

## Files in this Repository

- **airline_price_elasticity_global.xlsx**: Global dataset covering ticket and fuel price data for major airlines (2010-2019).
- **airline_price_elasticity_us.xlsx**: U.S. dataset with quarterly data for top U.S. airlines (1995-2023).
- **analysis_script.R**: R script with data cleaning, analysis, and model specifications.
- **project_report.pdf**: Detailed report containing statistical results, interpretations, and visualizations.

## Packages Used

- `plm`
- `car`
- `lmtest`
- `dplyr`
- `ggplot2`
- `panelvar`

## Results

- **Global Analysis**:
  - **Overall Elasticity**: Ticket prices show an elasticity of 0.24% to 0.42% with respect to fuel costs.
  - **Regional and Airline Type**: Continental and business model dummy variables were not significant, but interaction terms showed that low-cost carriers exhibit lower elasticity to fuel price changes than full-service airlines.
  - **Flight Distance Interaction**: Long-haul flights show higher elasticity, indicating a stronger ticket price sensitivity to fuel costs.

- **U.S. Analysis**:
  - **Overall Elasticity**: A general elasticity of 0.047% was observed.
  - **Low-cost Interaction**: Contrarily to the global dataset, low-cost carriers in the U.S. showed higher elasticity, indicating potential market-specific dynamics.
  - **Fuel Price Dynamics**: Elasticity was lower during periods of rising fuel prices, with a stronger price response in periods of fuel price decline or stagnation.
  - **Short vs. Long-term Effects**: The ARDL model estimated short-term elasticity at 0.10% and long-term elasticity at around 0.06%.

## Authors

Jade Arpaliangeas, Alexis Bouteloup, Tadandjoa Kolani, Shen Shixuan
