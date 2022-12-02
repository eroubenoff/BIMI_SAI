# Replication code for Roubenoff, Slootjes, and Bloemraad
## Spatial and Sociodemographic Vulnerability: Quantifying Accessibility to Health Care and Legal Services for Immigrants in California, Arizona, and Nevada

![Bay Area Fig](Bay_area.png)

This repository contains R code for replicating all figures and analysis.
[We have deposited our data in the Harvard Dataverse](https://doi.org/10.7910/DVN/RCQXN1). 
The core files are access restricted; please contact the authors (Ethan Roubenoff; eroubenoff@berkeley.edu) for access.
To replicate our results, follow the following instructions:

1) Clone this repository to your home directory.

2) Request permission and download all data from the Harvard Dataverse. The dataverse repository
contains two folders: `data` and `figs`. In this git repository
are two corresponding empty folders. To replicate our findings, 
you must copy the contents of both folders from the 
Dataverse files to the repository folders. 

3) To replicate our results, run files `01_calc_clinic_sj`, `02_bimi_index`, and `03_plots`.
Maps are generated using QGIS, and the corresponding project files are present in the 
dataverse repository.

# Important note about replication:

For this project, we calculated travel-time buffers around census tracts
and clinic locations. We used OpenStreetMap and the OpenSourceRoutingMachine
to do so. Replicating these buffers is not a trivial operation, so we 
have included the calculated buffers within the Dataverse repository `data` folder,
and the following steps are not needed to run the analysis.

To replicate the buffers, you must first install OSM and OSRM following
the instructions on their respective websites. [This quide](https://benjaminberhault.com//post/2018/12/08/set-up-an-osrm-server-on-ubuntu.html) 
was very helpful (Note: we have only 
tested this on linux/mac osx, and are unsure how the process will differ
for Windows). Then, make sure 
that you have downloaded the Dataverse repository and that the 
`osm` folder is present. 
The OSM file `CANVAZ.osm` is quite large and has been compressed. First, run: 
```
gzip -d CANVAZ.osm.gz
```
Then, create a OSRM server instance
(we recommend doing this inside of a `tmux` instance).
The command for doing so is:

```
tmux 
osrm-routed CANVAZ.osm
```


Then, to calculate the buffers, run `00_load_clinic_catchment_areas` and `00_load_tract_catchment_areas`,
which will write the output to `data/`.
