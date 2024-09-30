# Forest Loss and Orangutan Translocation Analysis
### Overview
This repository contains a series of scripts developed for Sherman & Voigt et al (in review) to analyze forest cover changes in relation to orangutan translocation events. The aim is to measure forest loss before and after orangutan captures, assess whether forest loss influences the probability of orangutan captures, calculate distances between capture and release sites, and visualize forest loss over time. 
Data to run scripts is deposited under: xxxx

---

## Scripts

### 1. **Forest Loss Over Time with Capture Highlight (Calculate_forest_loss_over_time_figure.R)**
   - **Purpose:** Plot forest loss over time and highlight the years of orangutan captures.
   - **Key Features:**
     - Combines processed data from multiple regions (Sumatra and Kalimantan).
     - Calculates forest percentage loss relative to a baseline from the year 2000.
     - Creates plots to visualize forest cover changes over time, marking capture years in red.
     - Summarizes forest cover changes per year with confidence intervals.
   - **Input:**
     - CSV files with processed forest data from different regions (e.g., `Sumatra_processed_forest_data.csv`, `Kalimantan_processed_forest_data.csv`).
   - **Output:**
     - Plot showing intact forest cover over time (as a percentage relative to the year 2000).
     - Visualization highlighting years of orangutan captures.
     - Summary data of median forest cover changes per year.

---

### 2. **Forest Loss and Capture Probability (Calculate_forest_loss_before_capture.R)**
   - **Purpose:** Examine forest loss prior to orangutan capture and assess whether forest loss influences the probability of capture.
   - **Key Features:**
     - Calculates forest loss for up to 5 years prior to capture.
     - Performs correlation analysis between forest loss and capture year status.
     - Fits logistic regression models to predict capture probability based on forest loss.
     - Includes likelihood ratio tests to compare model performance.
     - Outputs summary statistics and boxplots visualizing the relationship between capture and forest loss.
   - **Input:**
     - CSV file with processed forest data (`forest_data.csv`).
   - **Output:**
     - Correlation matrix and visualizations for forest loss and capture probability.
     - Summary statistics of forest loss before capture.

---

### 3. **Forest Decline After Capture (Calculate_forest_loss_after_capture.R)**
   - **Purpose:** Calculate forest cover decline in the years following the capture of orangutans.
   - **Key Features:**
     - Calculates forest loss percentages 5 years before and after the capture year.
     - Filters the dataset to retain only observations with complete pre- and post-rescue data.
     - Exports summary statistics and visualizations of forest cover loss over time.
   - **Input:**
     - CSV file containing processed forest data (e.g., `Sumatra_processed_forest_data.csv`).
   - **Output:**
     - CSV file summarizing forest loss after rescue (`forest_loss_after_summary.csv`).
     - Plot visualizing forest loss after orangutan capture.

---

### 4. **Distance Calculation Between Capture and Release Sites (Calculate_distance_capture_release_centroid)**
   - **Purpose:** Calculate the distance between orangutan capture and release sites using centroid data from villages and protected areas.
   - **Key Features:**
     - Converts capture and release centroid coordinates into spatial points.
     - Calculates distances between capture and release sites.
     - Aggregates summary statistics of distances by province.
   - **Input:**
     - CSV file containing centroid coordinates for capture and release sites (e.g., `capture_release_centroids.csv`).
   - **Output:**
     - CSV file with calculated distances for each capture-release pair (`distance_table.csv`).
     - CSV file summarizing distances by province (`distance_summary_provinces.csv`).
     - Additional province-wise counts of capture-release events.

---

## Usage Instructions
1. Clone the repository and ensure all dependencies are installed.
2. Prepare the input data files as specified for each script.
3. Run the scripts in sequence or individually based on the analysis required.
4. Review the output CSV files and visualizations generated in the process.
