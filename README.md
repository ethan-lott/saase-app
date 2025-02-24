## SAASE Orientation Survey: Exploratory Data Application
### Prerequisites
Ensure that you have the following installed on your system:
- R (latest stable version)
- RStudio (optional but recommended)
- Required R packages: `shiny`, `ggplot2`, `tidyverse`, `readxl`, `patchwork`

You can install the necessary packages using:
```r
install.packages(c("shiny", "ggplot2", "tidyverse", "readxl", "patchwork"))
```

## Project Structure
The project consists of the following files:
```
/project_root
│── data/
│   ├── or_survey_sim_final_2023-11-29.xlsx
│   ├── pde_sim_final_2023-11-29.xlsx
│── global.R
│── app.R
```
- `global.R`: Loads data, performs preprocessing, and defines global variables.
- `app.R`: Contains the Shiny UI and server logic.
- `data/`: Stores the required dataset files.

### Running the Application
1. Open RStudio or a terminal with R.
2. Set the working directory to the project folder using:
   ```r
   setwd("/path/to/project_root")
   ```
3. Run the Shiny application with:
   ```r
   shiny::runApp("app.R")
   ```
4. The application should launch in your default web browser.

## Deployment
To deploy the app on **ShinyApps.io**:
1. Install `rsconnect` if not already installed:
   ```r
   install.packages("rsconnect")
   ```
2. Authenticate your account with:
   ```r
   rsconnect::setAccountInfo(name='your_name', token='your_token', secret='your_secret')
   ```
3. Deploy the app:
   ```r
   rsconnect::deployApp()
   ```
4. The deployed app will be available at your ShinyApps.io URL.

For **local deployment** using Shiny Server:
1. Install Shiny Server following [these instructions](https://rstudio.com/products/shiny/download-server/).
2. Copy the project files to `/srv/shiny-server/your_app`.
3. Restart Shiny Server:
   ```sh
   sudo systemctl restart shiny-server
   ```
4. Access the app via `http://your-server-ip:3838/your_app`.

## Troubleshooting
- Ensure all package dependencies are installed.
- Check file paths if encountering errors loading data.
- Review logs using `shiny::runApp("app.R", display.mode = "normal")`.
