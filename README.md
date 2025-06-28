# AgriPinas
This dashboard provides an interactive platform to explore and analyze the volume of production for staple crops, livestock, and fisheries in the Philippines.

Key Features
1. Overview & Trends Tab:
Summary Statistics: Value boxes display total production, average production, and the number of items selected based on current filters.
Production Trend: An interactive line chart visualizes the production volume over time for the selected item(s), geolocation, period type, and year range. A LOESS smoothing line is added for quarterly data if sufficient data points are available.
Production Comparison: A bar chart that dynamically changes to compare:
Items within a selected specific region for the latest year in the selected range.
Regions for a selected specific item (when 'PHILIPPINES' is chosen as geolocation) for the latest year in the selected range.
2. Regional Breakdown Tab:
Displays a bar chart showing the production volume of a selected specific item across different regions for a user-selected year and period type.
3. Spatio-Temporal Analysis Tab:
Animated Map: Displays a choropleth map of the Philippines, colored by production volume for the selected item and period type. A time slider allows animation across years (for Annual data) or year-quarters (for Quarterly data). Hovering over regions shows detailed information. Satellite and street map base layers are available.
Regional Bar Chart (Time-Linked): Complements the map by showing the production volume of the selected item across different regions for the time point selected on the slider.
4. Data Explorer Tab:
Provides a searchable and sortable data table containing the raw data filtered according to the user's selections in the sidebar.
5. Time Series Analysis & Forecasting Tab:
Dedicated controls allow selection of a specific item, geolocation (excluding national aggregate), and period type (Quarterly or Annual) for in-depth time series analysis.
Decomposition Plot: Visualizes the time series decomposed into its observed, trend, seasonal, and remainder components. It uses STL (Seasonal and Trend decomposition using Loess) for periodic data or classical decomposition as a fallback.
Forecast Plot: Displays the historical data along with forecasted values and 80%/95% prediction intervals. The forecast model (ARIMA or ETS) is chosen automatically based on data characteristics.
Forecast Data Table: Presents the numerical point forecasts and prediction intervals for the specified forecast horizon.
Model Information: Shows details of the fitted time series model (e.g., ARIMA or ETS parameters).


Statistical Methods

Descriptive Statistics: Calculation of sums, averages, and counts displayed in value boxes and used for visualizations.

Data Aggregation: Dynamic grouping and summarization of data based on user filters for trend and comparison plots.

Geospatial Visualization: Choropleth maps using Leaflet to display regional production data, with animation for spatio-temporal analysis.

Time Series Decomposition: Utilizes STL (Seasonal and Trend decomposition using Loess) or classical decomposition methods to identify underlying patterns in the data.

Inferential Statistics (Forecasting): Employs automated ARIMA (Autoregressive Integrated Moving Average) or ETS (Error, Trend, Seasonality / Exponential Smoothing) models from the `forecast` package to predict future production volumes. Prediction intervals provide a measure of uncertainty for these forecasts.

