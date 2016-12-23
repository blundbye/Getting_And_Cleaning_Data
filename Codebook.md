Codebook
========
This codebook describes the variables in the tab-delimited text file TidyData.txt. 
The dataset is produced running the script file `run_analysis.R`

Variables and descriptions
------------------------------

Variable name      | Description
-------------------|------------
SubjectID          | ID of the subject who performed the activity (range from 1 to 30).
Activity           | Activity performed when the corresponding measurements were taken (LAYING, SITTING, STANDING, WALKING, WALKING_DOWNSTAIRS, WALKING_UPSTAIRS)
Domain             | Time or frequency of domain signal (Time or Frequency)
Instrument         | Measuring instrument (Accelerometer or Gyroscope)
Acceleration       | Acceleration signal (Body or Gravity (if any))
JerkFlag           | Jerk signal (yes or no)
MagnitudeFlag      | Magnitude of the signals calculated using the Euclidean norm (yes or no)
MeasureType        | Type of measure (Mean or Standard Deviation)
AxialDirection     | Direction of 3-axial signals (X, Y, or Z (if any))
AverageValue       | Average of each measure for each activity and each subject
NumberObservations | Count of observations used to compute the average value

Structure
-----------------

```{r}
str(TidyData)
```

```
 Classes ‘data.table’ and 'data.frame':	11880 obs. of  11 variables:
 $ SubjectID         : int  1 1 1 1 1 1 1 1 1 1 ...
 $ Activity          : chr  "LAYING" "LAYING" "LAYING" "LAYING" ...
 $ Domain            : Factor w/ 2 levels "Time","Frequency": 1 1 1 1 1 1 1 1 1 1 ...
 $ Acceleration      : Factor w/ 3 levels NA,"Body","Gravity": 1 1 1 1 1 1 1 1 1 1 ...
 $ Instrument        : Factor w/ 2 levels "Accelerometer",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ JerkFlag          : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 2 2 ...
 $ MagnitudeFlag     : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 2 2 1 1 ...
 $ MeasureType       : Factor w/ 2 levels "Mean","Standard Deviation": 1 1 1 2 2 2 1 2 1 1 ...
 $ AxialDirection    : Factor w/ 4 levels NA,"X","Y","Z": 2 3 4 2 3 4 1 1 2 3 ...
 $ AverageValue      : num  -0.0166 -0.0645 0.1487 -0.8735 -0.9511 ...
 $ NumberObservations: int  50 50 50 50 50 50 50 50 50 50 ...
 - attr(*, "sorted")= chr  "SubjectID" "Activity" "Domain" "Acceleration" ...
 - attr(*, ".internal.selfref")=<externalptr> 
```
 
Summary of variables
--------------------

```{r}
summary(dtTidy)
```

```
   SubjectID      Activity               Domain      Acceleration          Instrument   JerkFlag   MagnitudeFlag             MeasureType  
 Min.   : 1.0   Length:11880       Time     :7200   NA     :4680   Accelerometer:7200   No :7200   No :8640      Mean              :5940  
 1st Qu.: 8.0   Class :character   Frequency:4680   Body   :5760   Gyroscope    :4680   Yes:4680   Yes:3240      Standard Deviation:5940  
 Median :15.5   Mode  :character                    Gravity:1440                                                                          
 Mean   :15.5                                                                                                                             
 3rd Qu.:23.0                                                                                                                             
 Max.   :30.0                                                                                                                             
 AxialDirection  AverageValue      NumberObservations
 NA:3240        Min.   :-0.99767   Min.   :36.00     
 X :2880        1st Qu.:-0.96205   1st Qu.:49.00     
 Y :2880        Median :-0.46989   Median :54.50     
 Z :2880        Mean   :-0.48436   Mean   :57.22     
                3rd Qu.:-0.07836   3rd Qu.:63.25     
                Max.   : 0.97451   Max.   :95.00     
```

The first 15 rows of the dataset
------------------------------

```{r}
head(TidyData, n = 15)
```

```
    SubjectID Activity Domain Acceleration Instrument JerkFlag MagnitudeFlag        MeasureType AxialDirection AverageValue NumberObservations
 1:         1   LAYING   Time           NA  Gyroscope       No            No               Mean              X  -0.01655309                 50
 2:         1   LAYING   Time           NA  Gyroscope       No            No               Mean              Y  -0.06448612                 50
 3:         1   LAYING   Time           NA  Gyroscope       No            No               Mean              Z   0.14868944                 50
 4:         1   LAYING   Time           NA  Gyroscope       No            No Standard Deviation              X  -0.87354387                 50
 5:         1   LAYING   Time           NA  Gyroscope       No            No Standard Deviation              Y  -0.95109044                 50
 6:         1   LAYING   Time           NA  Gyroscope       No            No Standard Deviation              Z  -0.90828466                 50
 7:         1   LAYING   Time           NA  Gyroscope       No           Yes               Mean             NA  -0.87475955                 50
 8:         1   LAYING   Time           NA  Gyroscope       No           Yes Standard Deviation             NA  -0.81901017                 50
 9:         1   LAYING   Time           NA  Gyroscope      Yes            No               Mean              X  -0.10727095                 50
10:         1   LAYING   Time           NA  Gyroscope      Yes            No               Mean              Y  -0.04151729                 50
11:         1   LAYING   Time           NA  Gyroscope      Yes            No               Mean              Z  -0.07405012                 50
12:         1   LAYING   Time           NA  Gyroscope      Yes            No Standard Deviation              X  -0.91860852                 50
13:         1   LAYING   Time           NA  Gyroscope      Yes            No Standard Deviation              Y  -0.96790724                 50
14:         1   LAYING   Time           NA  Gyroscope      Yes            No Standard Deviation              Z  -0.95779016                 50
15:         1   LAYING   Time           NA  Gyroscope      Yes           Yes               Mean             NA  -0.96346103                 50
```