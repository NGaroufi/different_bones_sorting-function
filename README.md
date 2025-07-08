# different_bones_sorting-function
An R function for sorting different long bones (femur, tibia, humerus) utilizing cross-sectional geometric properties of 3D long bone models and Support Vector Machine models. 

The present function allows the user to utilize a set of diaphyseal cross-sectional geometric properties of long bones (femur, tibia, and humerus), which have been previously extracted from 3D bone models using the [*csg-toolkit* GNU Octave package](https://github.com/pr0m1th3as/long-bone-diaphyseal-CSG-Toolkit/tree/v1.0.1). The only requirement for the `db_sorting` function is the CSV files computed from the *csg-toolkit* containing all the measurements for every individual of the desired sample. In order to use the function, the user must first download this repository from GitHub. Then, the downloaded folder must be unzipped and the CSV data file must be copied inside the different_bones_sorting folder. All necessary libraries are installed and loaded directly by the function.

The names for the samples must follow the format: Sample_ID-Bone_Type_Side (eg. ID_0001-Femur_L). The Sample_ID must be 6 characters long.

The function can be used either from the `app.R` file available upon download or from the command line of an R programming environment.

## `app.R` file: 
Upon opening the file with in an R programming environment, the user only needs to choose `Run App` to run the Shiny application. A GUI window will open, allowing the user to choose the type of bones, distance, the type of the dataset (left or right bones, a pooled sample of equal size, or a full dataset with single elements as well as pairs), and a threshold value needed for their study, as well as whether the software is being used for a validation study (i.e. when the user knows the ground truth) or as an application on an unknown dataset. Upon clicking on the `Choose your data files` button, two windows will pop up (one at a time) and the user can then choose the relevant datasets. The results of the most probable pairs for each bone will appear on the screen, while more detailed results will be saved as `.csv` files on the working directory.

## Console:
First, the user needs to load the function in the R workspace. This is achieved with the command:
```
source("different_bones_sorting.R")
```

A message showing the successful installation of the necessary packages will show on the R console upon loading the function. Additionally, once the function is properly loaded, it can be called from the R console as:
```
db_sorting(bone_1, bone_2, side, distance, threshold_value, ground_truth)
```
The three inputs the `db_sorting` requires are: 
1. bone_1 / bone_2: "femur", "tibia", or "humerus" - the two chosen bones are those utilized for the algorithm (e.g. femur and tibia). The order must be as such: femur-tibia, femur-humerus, tibia-humerus.
2. side: "left", "right" for separate analysis of the two anatomical sides, "pooled" for a pooled dataset of equal size and only paired elements, "full" for a dataset with both anatomical sides and both paired and single elements.
3. distance: "euclidean", "maximum", "manhattan", "canberra", or "minkowski" (the default p for minkowski is 1.5)
4. threshold_value: "2", "225" (=2.25), "25" (=2.5), "275" (=2.75), "3"
5. ground_truth: "TRUE" in case of a validation study, "FALSE" in case of an unknown datasets

This will open a window of the working directory, where the user can choose the CSV files containing the measurements. The function will then display a message reporting the number of samples from the data file. The results of the analysis are saved in .csv files:
1. regarding the 5 top predictions for each sample when utilizing the predictions from the two selected bones
2. regarding the sample statistics (sample size, definite matches, number of excluded pairs, True Negative Rate, number of false negatives) for known datasets
3. regarding the plausible pairs

A testing dataset for each long bone is provided as a use case. Currently, the software supports femur and tibia combination of bones, femur and humerus bones, as well as tibia and humerus bones. Datasets for the "left" and "right" use case are provided for the femur and tibia bones.
