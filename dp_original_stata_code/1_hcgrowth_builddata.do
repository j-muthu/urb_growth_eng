/*-----------------------------------------------------------------------
1_hcgrowth_buildddata.do

Stata do file to perform data construction for
'Urban growth and its aggregate implications',
by Gilles Duranton and Diego Puga, Econometrica
-----------------------------------------------------------------------*/

* initialise
clear
set more off
set varabbrev off

cd builddata

** US nation totals: GDP per capita, schooling, CPI
do us_nation_totals

** City population data
do citypop_msa

** City-centres and college towns
* city centres obtained from from the google geocoding api
* set global flag GeocodeAgain = 1 to geocode again (google may provide slightly different coordinates)
* set global flag GeocodeAgain = 0 to replicate exactly using the provided data/intermediate/msa1999_geocoded.csv
if ("$GeocodeAgain" == "1") {
	* get main city name
	use ../../data/processed/county2msa1999, clear
	keep msa msa_name
	duplicates drop
	split msa_name, parse(,)
	split msa_name1, parse(-)
	gen state = substr(msa_name2,1,3)
	gen msa_maincity = msa_name11 + "," + state if (strlen(state) == 3)
	replace msa_maincity = msa_name11 if (strlen(state) != 3)
	keep msa msa_name msa_maincity
	sort msa
	outsheet using ../../data/intermediate/msa1999.csv, replace comma
	* get city centres from google
	cd ../../data/intermediate
	$PythonRunScript ../../code/builddata/python/1_batch_geocoding.py
	cd ../../code/builddata
	erase ../../data/intermediate/msa1999.csv
}
import delimited using ../../data/intermediate/msa1999_geocoded.csv, asdouble clear
rename (latitude longitude) (msa_lat msa_lon)
keep msa msa_name msa_maincity msa_lat msa_lon
order msa msa_name msa_maincity msa_lat msa_lon
label var msa "MSA/CMSA/NECMA FIPS code (1999 definitions)"
label var msa_name "MSA/CMSA/NECMA name"
label var msa_maincity "Main city in MSA/CMSA/NECMA"
label var msa_lat "MSA/CMSA/NECMA centre latitude"
label var msa_lon "MSA/CMSA/NECMA centre longitude"
label data "'Urban growth and its aggregate implications' Duranton & Puga. City centres"
compress
sort msa
desc
save ../../data/processed/controls_citycentre, replace
* college town indicator
do controls_collegetown

** Block-group controls
if ("$DisableGIS" == "0") {
	* run python scripts
	cd ../../data/intermediate
	$PythonRunScript ../../code/builddata/python/2_controls_blockg.py
	cd ../../code/builddata
}
* assemble block-group controls
do controls_blockg

** ACS
do housing_acs
erase ../../data/processed/controls_blockg_2012.dta

** MSA controls
* calculate city edge (requires running housing_acs.do before, then used by python_nlcd_fringe.py)
do controls_cityedge
if ("$DisableGIS" == "0") {
	* run python scripts
	cd ../../data/intermediate
	$PythonRunScript ../../code/builddata/python/3_slope.py
	$PythonRunScript ../../code/builddata/python/4_barriers.py
	$PythonRunScript ../../code/builddata/python/5_nlcd_fringe.py
	$PythonRunScript ../../code/builddata/python/6_controls_msa.py
	cd ../../code/builddata
}
* delete dask leftovers
cap erase ../../data/intermediate/dask-worker-space/global.lock
cap erase ../../data/intermediate/dask-worker-space/purge.lock
cap rmdir ../../data/intermediate/dask-worker-space
* delete aux files from 4_barriers.py
cap erase ../../data/intermediate/slope15_reclass_aux.tif
cap erase ../../data/intermediate/northamerica_mask.tif
cap erase ../../data/intermediate/protected_aux.tif
cap erase ../../data/intermediate/protected_aux2.tif
cap erase ../../data/intermediate/water_aux.tif
cap erase ../../data/intermediate/allbarriers_aux.tif
foreach shp_ext in cpg dbf prj shp shx {
	cap erase ../../data/intermediate/aquifers.`shp_ext'
	cap erase ../../data/intermediate/foreign.`shp_ext'
	cap erase ../../data/intermediate/protected.`shp_ext'
}
* delete aux files from 5_nlcd_fringe.py
cap erase ../../data/intermediate/nlcd_reclassified_urban.tif
foreach shp_ext in cpg dbf prj shp shx {
	cap erase ../../data/intermediate/quad_boundaries_aux.`shp_ext'
}
* delete aux files from 6_controls_msa.py
cap erase ../../data/intermediate/allbarriers.tif
cap erase ../../data/intermediate/aquifers.tif
cap erase ../../data/intermediate/foreign.tif
cap erase ../../data/intermediate/protected.tif
cap erase ../../data/intermediate/rug_int.tif
cap erase ../../data/intermediate/slope15_reclass.tif
cap erase ../../data/intermediate/slope15.tif
cap erase ../../data/intermediate/water.tif
cap erase ../../data/intermediate/wetland.tif
foreach shp_ext in cpg dbf prj shp shx {
	cap erase ../../data/intermediate/nlcd_devmedhigh.`shp_ext'
}
* assemble msa controls
do controls_msa

** MSA housing data
* calculate in python values for vacant agricultural land on urban fringe
if ("$DisableGIS" == "0") {
	* run python script
	cd ../../data/intermediate
	$PythonRunScript ../../code/builddata/python/7_nlcd_values.py
	cd ../../code/builddata
}
* delete dask leftovers
cap erase ../../data/intermediate/dask-worker-space/global.lock
cap erase ../../data/intermediate/dask-worker-space/purge.lock
cap rmdir ../../data/intermediate/dask-worker-space
* delete aux files from 7_nlcd_values.py
foreach img_ext in ige img rde rrd xml {
	cap erase ../../data/intermediate/nlcd_2011_land_cover_l48_20210604.`img_ext'
	cap erase ../../data/intermediate/nlcd_2011_impervious_descriptor_l48_20210604.`img_ext'
}
cap erase ../../data/intermediate/land_values_agriculture.tif
cap erase ../../data/intermediate/land_values_agriculture_aux.tif
cap erase ../../data/intermediate/nlcd_agriculture.tif
cap erase ../../data/intermediate/places_fmv_vacant_int.tif
cap erase ../../data/intermediate/places_fmv_vacant_res.tif
foreach shp_ext in cpg dbf prj shp shx {
	cap erase ../../data/intermediate/msa_fringe.`shp_ext'
}
* assemble msa-level housing data
do housing_msa

** NHTS
* only if block-group location  data is available or if fake location data will be generated 
if ("$NHTSBGUnavailable" != "1" | "$NHTSGenerateFakeLocations" == "1") {
	do travel_nhts_hh
	do travel_nhts_trip
}
erase ../../data/processed/controls_blockg_2000.dta

** NLSY79
do nlsy_panel
use ../../data/processed/nlsy_panel, clear

** CPS
do ipums_cps_msa

cd ..
