# MAT167-Final

This project was created by Hao Li and Anmei Liu for MAT 167 during fall quarter 2020 at UC Davis. 

The live version of our application can be found [here](https://anmeiliu.shinyapps.io/InteractiveLeastSquares/).

The original dataset was sourced from [Kaggle](https://www.kaggle.com/arslanali4343/real-estate-dataset).

## Files in this repo
Here is a brief description of each file; the only file that really performs computation is LS_util.R.

### /
Files we used that aren't bundled with the final app.

**data.csv** - the original dataset

**clean_data.R** - file which preprocesses data.csv and writes it into the app directory

### InteractiveLeastSquares/
This folder contains the files for the Shiny app.

**ui.R** - implements the app UI only

**server.R** - implements the matrix computation and rendering logic

**house.csv** - our preprocessed dataset

**LS_util.R** - implements least squares matrix operations which are used by server.R

**matrix_prettify.R** - implements helper functions for matrix display/UI