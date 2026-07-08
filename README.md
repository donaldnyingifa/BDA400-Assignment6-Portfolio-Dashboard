# BDA400 - Assignment 6

**Name:** Donald Nyingifa
**Assignment Title:** Technical Analysis using R, Visualization Phase (15%)
**Course:** BDA400 / Data Science Tools and Techniques

## Project Description

This project is the third stage of the three-part portfolio analysis project. It's an R Shiny dashboard that pulls historical stock data from Yahoo Finance through `quantmod`, then lets you explore it as a line, candlestick, or area chart across daily, weekly, or monthly time frames. You can layer Moving Averages, RSI, and MACD on top of the price chart, and toggle each one on or off independently. A Moving Average crossover rule then flags Buy and Sell points directly on the chart (green up-triangles for buys, red down-triangles for sells), with a table underneath listing every signal and the price it fired at.

## Files

- `donaldnyingifa_BDA400_A06.R` – full Shiny app (UI + server + trading logic)

## Repository Link

\_\_

## How to Run

```r
install.packages(c("shiny", "ggplot2", "quantmod", "tidyquant", "dplyr", "patchwork", "scales"))
shiny::runApp("donaldnyingifa_BDA400_A06.R")
```
