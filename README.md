# README

Dataset and associated R scripts underpinning the following scientific publication:


Validating predicted juvenile distribution of the Critically Endangered flapper skate (Dipturus intermedius) using genetic sampling (in revision).



Skate_data.csv

These data represent haul information from the 2021 Irish Groundfish Survey, which is a fisheries stock assessment carried out annually.

The haul information are openly available through the Database of Trawl Surveys (DATRAS) download portal, managed by International  Council for Exploration of the Seas (ICES). For each haul, the presence or absence of the flapper skate (D. intermedius) is recorded, species ID was confirmed with genetic sampling onboard the survey. 


The environmental variables include distance to coast (m), depth (m), bottom temperature (°C) and surface current velocity (m/s). Surface current velocity was used as a proxy for bottom current velocity data due to limited data availability for the study area. A shapefile of the European coastline was sourced from the European Environment Agency (EEA, 2018). Distance to coast from each haul location was calculated using the ‘Near’ tool in ArcGIS Pro (Version 3.0.1). Depth data were obtained from the General Bathymetric Chart of the Oceans (GEBCO; GEBCO Compilation Group, 2020), while surface current velocity (x and y) and bottom temperature data were retrieved from the Irish Marine Institute’s ERDDAP data portal for November 2021.




prediction_data.csv

2000 location points were generated using the ‘Create Fishnet’ tool in ArcGIS Pro, with the Skate_data.csv extent as the boundary. Environmental data were extracted for these points, and null values, where the grid overlapped with land, were removed, leaving a total of 538 points. 



main_script.R

Script to build and run Bernoulli GLM, GAM and GAM + SPDE. Model validation + prediction steps are also included. Method and code from Zuur et al., 2017.



exploration.R

R script for data exploration according to methods outlined in Zuur et al., 2010.



environmentaldataplots.R

R script to produce data exploration plots for variables, including boxplots for differences between groups (with statistical tests), and normality plots.



extra_inla_fun.R

Functions that are called in the other scripts -

inla.mesh2sp - Convert inla.mesh to sp objects (Taken from google groups: https://groups.google.com/g/r-inla-discussion-group/c/z1n1exlZrKM/m/8vYNr2D8BwAJ) 
make_prediction_map - Use this function to create a dataframe containing INLA predictions with their locations.
append_prediction_map - Use this function to append a vector onto an existing df with the same number of rows. 
AddHoleToPolygon - Shoot hole in polygon (spdf) using another polygon (spdf) Available at: https://stackoverflow.com/questions/29624895/how-to-add-a-hole-to-a-polygon-within-a-spatialpolygonsdataframe



shapefile_mesh.R

Script to prepare a boundary dataframe for the INLA barrier model, using a shapefile.



References

Zuur, A.F., Ieno, E.N. and Elphick, C.S., 2010. A protocol for data exploration to avoid common statistical problems. Methods in ecology and evolution, 1(1), pp.3-14.

Zuur, A.F., Ieno, E.N. and Saveliev, A.A., 2017. Spatial, temporal and spatial–temporal ecological data analysis with R-INLA. Highland Statistics Ltd, 1.


