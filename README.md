# Commute Explorer

This R Shiny application was submitted to the [RStudio Shiny Contest 2021](https://blog.rstudio.com/2021/03/11/time-to-shiny/). Check out the running app online: [https://nz-stefan.shinyapps.io/commute-explorer-2/](https://nz-stefan.shinyapps.io/commute-explorer-2/).

![recording.gif](https://github.com/nz-stefan/commute-explorer-2/blob/master/recording.gif)

## Data

The data behind this application is available in the [Datafinder](https://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/?_ga=2.143880757.1699143700.1594461462-854170190.1593163189) hosted by Stats NZ. Additionally, I used the [Statistal Area 2 2018 data set](https://datafinder.stats.govt.nz/layer/92212-statistical-area-2-2018-generalised/) which contains shape files of the statistical areas referenced in the commuter data set. I simplified the polygons in those shape files to reduce the amount of data to display in the browser for this application. I also processed the [Geographic Areas File 2020](https://datafinder.stats.govt.nz/table/104285-geographic-areas-file-2020/) to group statistical areas into the main NZ regions.


## Deployment

The app is deployed through RStudio's webservice [shinyapps.io](https://nz-stefan.shinyapps.io/commute-explorer-2/).


## Setup development environment

The development environment of this project is encapsulated in a Docker container.

1. Install Docker. Follow the instructions on [https://docs.docker.com/install/](https://docs.docker.com/install/)
2. Make docker run without sudo
    ```
    sudo groupadd docker
    sudo usermod -aG docker $USER
    ```
    Log out and log back in so that your group membership is re-evaluated
3. Clone the GIT repository
    ```
    git clone https://github.com/nz-stefan/commute-explorer-2.git
    ```
4. Setup development Docker container
    ```
    cd commute-explorer-2
    bin/setup-environment.sh
    ```
    You should see lots of container build messages
5. Spin up the container
    ```
    bin/start_rstudio.sh
    ```
6. Open [http://localhost:8791](http://localhost:8791) in your browser to start a new RStudio session
7. Install R packages required for this app. Type the following instructions into the R session window of RStudio
    ```
    renv::restore()
    ```
    The installation will take a few minutes. The package library will be installed into the `renv/library` directory of the project path.
8. Open the file `app/global.R` and hit the "Run app" button in the toolbar of the script editor (or type `shiny::runApp("app")` in the R session window). The Shiny app should open in a new window. You may need to instruct your browser to not block popup windows for this URL.

