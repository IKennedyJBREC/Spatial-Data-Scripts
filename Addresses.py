import arcpy
from arcpy import os
from csv import reader
import numpy as np

workspace = os.getcwd()
data = input("File Path: ")
locator_path = "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/ArcGIS World Geocoding Service"

#Tidy data in R prior to reading in...Check if lon is negative/lat is positive
#Geocode using Mailing Address
arcpy.geocoding.GeocodeAddresses(data, locator_path, "'Address or Place' MILL_STREET1 VISIBLE NONE;Address2 <None> VISIBLE NONE;Address3 <None> VISIBLE NONE;Neighborhood <None> VISIBLE NONE;City MILL_CITY_x VISIBLE NONE;County <None> VISIBLE NONE;State MILL_STATECD VISIBLE NONE;ZIP MILL_ZIP_CD VISIBLE NONE;ZIP4 <None> VISIBLE NONE;Country <None> VISIBLE NONE", r"C:\Users\kenne\Downloads\TPO_Test\TPO_Test.gdb\Mailing_Geocoded", "STATIC", "US", "ADDRESS_LOCATION", "Subaddress;'Point Address';'Street Address';'Distance Marker';Intersection;'Street Name'", "MINIMAL")

MAddress = "Mailing_Geocoded"
PAddress = "Physical_Geocoded"
LAddress = "Lat_Lon_Geocoded"

# Get the fields from the input
fields = arcpy.ListFields(MAddress)

# Create a fieldinfo object
fieldinfo = arcpy.FieldInfo()

for field in fields:
    fieldinfo.addField(field.name, field, "VISIBLE", "")
    next
    
#Using fieldinfo, create an unmatched mailing spreadsheet named 'Unmatched_Mailing'. 'Mailing_Geocoded_Unmatched' is an
#intermediate file and can be disregarded
arcpy.MakeTableView_management(MAddress, "Mailing_Geocoded_Unmatched", "", "", fieldinfo)
arcpy.CopyRows_management("Mailing_Geocoded_Unmatched", "C:/Users/kenne/Downloads/TPO_Test/Unmatched_Mailing.xlsx")

#Delete matched rows from 'Unmatched_Mailing'
arcpy.management.SelectLayerByAttribute('Unmatched_Mailing', "NEW_SELECTION", "Status = 'M' Or Score > 80", None)
arcpy.management.DeleteRows('Unmatched_Mailing')
#Delete fields from 'Unmatched_Mailing'
arcpy.management.DeleteField("Unmatched_Mailing", "Status;Score;Match_type;Match_addr;Addr_type", "DELETE_FIELDS")



#Geocode unmatched mailing addresses with physical addresses
with arcpy.EnvManager(scratchWorkspace=r"C:\Users\kenne\Downloads\TPO_Test\TPO_Test.gdb", outputCoordinateSystem='GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]', workspace=r"C:\Users\kenne\Downloads\TPO_Test\TPO_Test.gdb"):
    arcpy.geocoding.GeocodeAddresses("Unmatched_Mailing", locator_path, "'Address or Place' PHYSICAL_S VISIBLE NONE;Address2 <None> VISIBLE NONE;Address3 <None> VISIBLE NONE;Neighborhood <None> VISIBLE NONE;City PHYSICAL_C VISIBLE NONE;County <None> VISIBLE NONE;State PHYSICAL_1 VISIBLE NONE;ZIP PHYSICAL_Z VISIBLE NONE;ZIP4 <None> VISIBLE NONE;Country <None> VISIBLE NONE", "Physical_Geocoded", "STATIC", "US", "ADDRESS_LOCATION", "Subaddress;'Point Address';'Street Address';'Distance Marker';Intersection;'Street Name'", "MINIMAL")
#arcpy.management.AlterField(PAddress, "Pro_Rad", "Pro_Rad_Mean_byState", "Pro_Rad_Mean_byState", "DOUBLE", 8, "NON_NULLABLE", "DO_NOT_CLEAR")

# Get the fields from the input
physical = arcpy.ListFields(PAddress)

# Create a fieldinfo object
physicalinfo = arcpy.FieldInfo()

#Using physicalinfo, create an unmatched physical spreadsheet named 'Unmatched_Physical'. 'Physical_Geocoded_Unmatched'
#is an intermediate file and can be disregarded
for field in physical:
    physicalinfo.addField(field.name, field, "VISIBLE", "")
    next

#Create 'Unmatched_Physical'
arcpy.MakeTableView_management(PAddress, "Physical_Geocoded_Unmatched", "", "", physicalinfo)
arcpy.CopyRows_management("Physical_Geocoded_Unmatched", "C:/Users/kenne/Downloads/TPO_Test/Unmatched_Physical.xlsx")

#Delete matched rows from 'Unmatched_Physical'
arcpy.management.SelectLayerByAttribute("Unmatched_Physical", "NEW_SELECTION", "Status = 'M' Or Score > 50", None)
arcpy.management.DeleteRows("Unmatched_Physical")

#Delete Unmatched Rows from M/P Geocodes
arcpy.management.SelectLayerByAttribute(PAddress, "NEW_SELECTION", "Status = 'U' Or Score < 80", None)
arcpy.management.DeleteRows(PAddress)
arcpy.management.SelectLayerByAttribute(MAddress, "NEW_SELECTION", "Status = 'U' Or Score < 80", None)
arcpy.management.DeleteRows(MAddress)

#Add new fields for Lat/Lon conversion...Can revert back to previous syntax if fixed in R (check if numeric or text)
#arcpy.management.AddFields("Unmatched_Physical", "MILL_LAT1 FLOAT MILL_LAT1 # # #;MILL_LON1 FLOAT MILL_LON1 # # #")
#Calculate new fields from MILL_LAT/MILL_LON strings
#arcpy.management.CalculateField("Unmatched_Physical", "MILL_LAT1", "!MILL_LAT!", "PYTHON3", '', "TEXT", "NO_ENFORCE_DOMAINS")
#arcpy.management.CalculateField("Unmatched_Physical", "MILL_LON1", "!MILL_LON!", "PYTHON3", '', "TEXT", "NO_ENFORCE_DOMAINS")

#Geocode unmatched points with Lat/Lon, store as Lat_Lon_Geocoded...
with arcpy.EnvManager(outputCoordinateSystem='GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]', extent="-179.147369141751 -14.5525569230741 179.778457675472 71.3525731140656"):
    arcpy.management.XYTableToPoint("Unmatched_Physical", LAddress, "MILL_LON", "MILL_LAT", None, 'GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]];-400 -400 1000000000;-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision')

#Transfer unmatched addresses to Unmatched_Final (not matched after Mailing, Physical, or Lat/Lon geocodes)
arcpy.management.SelectLayerByAttribute("Unmatched_Physical", "NEW_SELECTION", "MILL_LAT IS NULL Or MILL_LON IS NULL Or MILL_LAT = '0' Or MILL_LON = '0'", None)
arcpy.conversion.TableToTable("Unmatched_Physical", r"C:\Users\kenne\Downloads\TPO_Test", "Unmatched_Final.dbf", '', 'Status "Status" true true false 1 Text 0 0,First,#,Unmatched_Physical,Status,0,1;Score "Score" true true false 19 Double 0 0,First,#,Unmatched_Physical,Score,-1,-1;Match_type "Match_type" true true false 2 Text 0 0,First,#,Unmatched_Physical,Match_type,0,2;Match_addr "Match_addr" true true false 254 Text 0 0,First,#,Unmatched_Physical,Match_addr,0,254;Addr_type "Addr_type" true true false 20 Text 0 0,First,#,Unmatched_Physical,Addr_type,0,20;MILL_NBR "MILL_NBR" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_NBR,0,254;MILL_STATE "MILL_STATE" true true false 19 Double 0 0,First,#,Unmatched_Physical,MILL_STATE,-1,-1;MILL_NAME "MILL_NAME" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_NAME,0,254;MILL_STREE "MILL_STREE" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_STREE,0,254;MILL_CITY_ "MILL_CITY_" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_CITY_,0,254;MILL_ZIP_C "MILL_ZIP_C" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_ZIP_C,0,254;MTC "MTC" true true false 19 Double 0 0,First,#,Unmatched_Physical,MTC,-1,-1;TOT_MCF "TOT_MCF" true true false 19 Double 0 0,First,#,Unmatched_Physical,TOT_MCF,-1,-1;MILL_LAT "MILL_LAT" true true false 19 Double 0 0,First,#,Unmatched_Physical,MILL_LAT,-1,-1;MILL_LON "MILL_LON" true true false 19 Double 0 0,First,#,Unmatched_Physical,MILL_LON,-1,-1;LAT_LON_CH "LAT_LON_CH" true true false 254 Text 0 0,First,#,Unmatched_Physical,LAT_LON_CH,0,254;MILL_STATU "MILL_STATU" true true false 19 Double 0 0,First,#,Unmatched_Physical,MILL_STATU,-1,-1;TPOID2021 "TPOID2021" true true false 254 Text 0 0,First,#,Unmatched_Physical,TPOID2021,0,254;AMOUNT_201 "AMOUNT_201" true true false 254 Text 0 0,First,#,Unmatched_Physical,AMOUNT_201,0,254;PROCUREMEN "PROCUREMEN" true true false 254 Text 0 0,First,#,Unmatched_Physical,PROCUREMEN,0,254;MILL_TYPE_ "MILL_TYPE_" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_TYPE_,0,254;AMOUNT_202 "AMOUNT_202" true true false 254 Text 0 0,First,#,Unmatched_Physical,AMOUNT_202,0,254;PROCUREM_1 "PROCUREM_1" true true false 254 Text 0 0,First,#,Unmatched_Physical,PROCUREM_1,0,254;MILL_TYPE1 "MILL_TYPE1" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_TYPE1,0,254;AMOUNT_203 "AMOUNT_203" true true false 254 Text 0 0,First,#,Unmatched_Physical,AMOUNT_203,0,254;PROCUREM_2 "PROCUREM_2" true true false 254 Text 0 0,First,#,Unmatched_Physical,PROCUREM_2,0,254;Pro_Rad "Pro_Rad" true true false 254 Text 0 0,First,#,Unmatched_Physical,Pro_Rad,0,254;Pro_Rad_Me "Pro_Rad_Me" true true false 19 Double 0 0,First,#,Unmatched_Physical,Pro_Rad_Me,-1,-1;Pro_Rad__1 "Pro_Rad__1" true true false 19 Double 0 0,First,#,Unmatched_Physical,Pro_Rad__1,-1,-1;Pro_Rad__2 "Pro_Rad__2" true true false 19 Double 0 0,First,#,Unmatched_Physical,Pro_Rad__2,-1,-1;PHYSICAL_S "PHYSICAL_S" true true false 254 Text 0 0,First,#,Unmatched_Physical,PHYSICAL_S,0,254;PHYSICAL_C "PHYSICAL_C" true true false 254 Text 0 0,First,#,Unmatched_Physical,PHYSICAL_C,0,254;PHYSICAL_1 "PHYSICAL_1" true true false 254 Text 0 0,First,#,Unmatched_Physical,PHYSICAL_1,0,254;PHYSICAL_Z "PHYSICAL_Z" true true false 254 Text 0 0,First,#,Unmatched_Physical,PHYSICAL_Z,0,254', '')

#Delete Unmatched Rows from Lat/Lon Geocodes
arcpy.management.SelectLayerByAttribute(LAddress, "NEW_SELECTION", "MILL_LAT = 0 Or MILL_LON = 0 Or MILL_LAT IS NULL Or MILL_LON IS NULL", None)
arcpy.management.DeleteRows(LAddress)

Merge = "Geocoded_Merge"
#Merge Mailing, Physical, and Lat/Lon Geocodes
arcpy.management.Merge("Mailing_Geocoded;Physical_Geocoded;Lat_Lon_Geocoded", Merge, 'MILL_NBR "MILL_NBR" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,MILL_NBR,0,254,Mailing_Geocoded,MILL_NBR,0,255,Physical_Geocoded,MILL_NBR,0,254;MILL_STATE "MILL_STATE" true true false 8 Double 0 0,First,#,Lat_Lon_Geocoded,MILL_STATE,-1,-1,Physical_Geocoded,MILL_STATE,-1,-1;MILL_NAME "MILL_NAME" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,MILL_NAME,0,254,Mailing_Geocoded,MILL_NAME,0,255,Physical_Geocoded,MILL_NAME,0,254;MILL_STREE "MILL_STREE" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,MILL_STREE,0,254,Physical_Geocoded,MILL_STREE,0,254;MILL_CITY_ "MILL_CITY_" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,MILL_CITY_,0,254,Physical_Geocoded,MILL_CITY_,0,254;MILL_ZIP_C "MILL_ZIP_C" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,MILL_ZIP_C,0,254,Physical_Geocoded,MILL_ZIP_C,0,254;MTC "MTC" true true false 8 Double 0 0,First,#,Lat_Lon_Geocoded,MTC,-1,-1,Mailing_Geocoded,MTC,-1,-1,Physical_Geocoded,MTC,-1,-1;TOT_MCF "TOT_MCF" true true false 8 Double 0 0,First,#,Lat_Lon_Geocoded,TOT_MCF,-1,-1,Mailing_Geocoded,TOT_MCF,-1,-1,Physical_Geocoded,TOT_MCF,-1,-1;MILL_LAT "MILL_LAT" true true false 8 Double 0 0,First,#,Lat_Lon_Geocoded,MILL_LAT,-1,-1,Mailing_Geocoded,MILL_LAT,-1,-1,Physical_Geocoded,MILL_LAT,-1,-1;MILL_LON "MILL_LON" true true false 8 Double 0 0,First,#,Lat_Lon_Geocoded,MILL_LON,-1,-1,Mailing_Geocoded,MILL_LON,-1,-1,Physical_Geocoded,MILL_LON,-1,-1;LAT_LON_CH "LAT_LON_CH" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,LAT_LON_CH,0,254,Physical_Geocoded,LAT_LON_CH,0,254;MILL_STATU "MILL_STATU" true true false 8 Double 0 0,First,#,Lat_Lon_Geocoded,MILL_STATU,-1,-1,Physical_Geocoded,MILL_STATU,-1,-1;TPOID2021 "TPOID2021" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,TPOID2021,0,254,Mailing_Geocoded,TPOID2021,0,255,Physical_Geocoded,TPOID2021,0,254;AMOUNT_201 "AMOUNT_201" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,AMOUNT_201,0,254,Physical_Geocoded,AMOUNT_201,0,254;PROCUREMEN "PROCUREMEN" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,PROCUREMEN,0,254,Physical_Geocoded,PROCUREMEN,0,254;MILL_TYPE_ "MILL_TYPE_" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,MILL_TYPE_,0,254,Physical_Geocoded,MILL_TYPE_,0,254;AMOUNT_202 "AMOUNT_202" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,AMOUNT_202,0,254,Physical_Geocoded,AMOUNT_202,0,254;PROCUREM_1 "PROCUREM_1" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,PROCUREM_1,0,254,Physical_Geocoded,PROCUREM_1,0,254;MILL_TYPE1 "MILL_TYPE1" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,MILL_TYPE1,0,254,Physical_Geocoded,MILL_TYPE1,0,254;AMOUNT_203 "AMOUNT_203" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,AMOUNT_203,0,254,Physical_Geocoded,AMOUNT_203,0,254;PROCUREM_2 "PROCUREM_2" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,PROCUREM_2,0,254,Physical_Geocoded,PROCUREM_2,0,254;Pro_Rad "Pro_Rad" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,Pro_Rad,0,254,Mailing_Geocoded,Pro_Rad,0,255,Physical_Geocoded,Pro_Rad,0,254;Pro_Rad_Me "Pro_Rad_Me" true true false 8 Double 0 0,First,#,Lat_Lon_Geocoded,Pro_Rad_Me,-1,-1,Physical_Geocoded,Pro_Rad_Me,-1,-1;Pro_Rad_Mean_byState "Pro_Rad_Mean_byState" true true false 8 Double 0 0,First,#,Lat_Lon_Geocoded,Pro_Rad_Mean_byState,-1,-1,Mailing_Geocoded,Pro_Rad_Mean_byState,-1,-1,Physical_Geocoded,Pro_Rad_Mean_byState,-1,-1;Pro_Rad__2 "Pro_Rad__2" true true false 8 Double 0 0,First,#,Lat_Lon_Geocoded,Pro_Rad__2,-1,-1,Physical_Geocoded,Pro_Rad__2,-1,-1;PHYSICAL_S "PHYSICAL_S" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,PHYSICAL_S,0,254,Physical_Geocoded,PHYSICAL_S,0,254;PHYSICAL_C "PHYSICAL_C" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,PHYSICAL_C,0,254,Physical_Geocoded,PHYSICAL_C,0,254;PHYSICAL_1 "PHYSICAL_1" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,PHYSICAL_1,0,254,Physical_Geocoded,PHYSICAL_1,0,254;PHYSICAL_Z "PHYSICAL_Z" true true false 254 Text 0 0,First,#,Lat_Lon_Geocoded,PHYSICAL_Z,0,254,Physical_Geocoded,PHYSICAL_Z,0,254;MILL_STATECD "MILL_STATECD" true true false 8 Double 0 0,First,#,Mailing_Geocoded,MILL_STATECD,-1,-1;MILL_STREET1 "MILL_STREET1" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_STREET1,0,255;MILL_CITY_x "MILL_CITY#x" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_CITY_x,0,255;MILL_ZIP_CD "MILL_ZIP_CD" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_ZIP_CD,0,255;LAT_LON_CHANGE "LAT_LON_CHANGE" true true false 255 Text 0 0,First,#,Mailing_Geocoded,LAT_LON_CHANGE,0,255;MILL_STATUS_CD "MILL_STATUS_CD" true true false 8 Double 0 0,First,#,Mailing_Geocoded,MILL_STATUS_CD,-1,-1;AMOUNT_2019 "AMOUNT_2019" true true false 255 Text 0 0,First,#,Mailing_Geocoded,AMOUNT_2019,0,255;PROCUREMENT_RADIUS_2019 "PROCUREMENT_RADIUS_2019" true true false 255 Text 0 0,First,#,Mailing_Geocoded,PROCUREMENT_RADIUS_2019,0,255;MILL_TYPE_CD_2020 "MILL_TYPE_CD_2020" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_TYPE_CD_2020,0,255;AMOUNT_2020 "AMOUNT_2020" true true false 255 Text 0 0,First,#,Mailing_Geocoded,AMOUNT_2020,0,255;PROCUREMENT_RADIUS_2020 "PROCUREMENT_RADIUS_2020" true true false 255 Text 0 0,First,#,Mailing_Geocoded,PROCUREMENT_RADIUS_2020,0,255;MILL_TYPE_CD_2021 "MILL_TYPE_CD_2021" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_TYPE_CD_2021,0,255;AMOUNT_2021 "AMOUNT_2021" true true false 255 Text 0 0,First,#,Mailing_Geocoded,AMOUNT_2021,0,255;PROCUREMENT_RADIUS_2021 "PROCUREMENT_RADIUS_2021" true true false 255 Text 0 0,First,#,Mailing_Geocoded,PROCUREMENT_RADIUS_2021,0,255;Pro_Rad_Mean "Pro_Rad_Mean" true true false 8 Double 0 0,First,#,Mailing_Geocoded,Pro_Rad_Mean,-1,-1;Pro_Rad_Mean_byMTC "Pro_Rad_Mean_byMTC" true true false 8 Double 0 0,First,#,Mailing_Geocoded,Pro_Rad_Mean_byMTC,-1,-1;PHYSICAL_STREET "PHYSICAL_STREET" true true false 255 Text 0 0,First,#,Mailing_Geocoded,PHYSICAL_STREET,0,255;PHYSICAL_CITY "PHYSICAL_CITY" true true false 255 Text 0 0,First,#,Mailing_Geocoded,PHYSICAL_CITY,0,255;PHYSICAL_STATE "PHYSICAL_STATE" true true false 255 Text 0 0,First,#,Mailing_Geocoded,PHYSICAL_STATE,0,255;PHYSICAL_ZIP "PHYSICAL_ZIP" true true false 255 Text 0 0,First,#,Mailing_Geocoded,PHYSICAL_ZIP,0,255', "NO_SOURCE_INFO")



#populate new field with string values of procurement radius
arcpy.management.CalculateField(Merge, "Pro_Rad_Final", 'str(!Pro_Rad!) + " MILES"', "PYTHON3", '', "TEXT", "NO_ENFORCE_DOMAINS")

Buffer = "Geocoded_Merge_ProRad" 

#buffer addresses using procurement radius.
arcpy.analysis.Buffer(Merge, Buffer, "Pro_Rad_Final", "FULL", "ROUND", "NONE", None, "PLANAR")

#clip buffers to USA outline.
arcpy.analysis.Clip(Buffer, "USA", "Merged_Buff_Clip", None)


#Once clipped to buffer, need to split features by attributes
with arcpy.EnvManager(scratchWorkspace=r"C:\Users\kenne\Downloads\TPO_Test\TPO_Test.gdb", outputCoordinateSystem='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]', workspace=r"C:\Users\kenne\Downloads\TPO_Test\TPO_Test.gdb"):
    arcpy.analysis.SplitByAttributes("Merged_Buff_Clip", r"C:\Users\kenne\Downloads\TPO_Test\TPO_Test.gdb", "ORIG_FID")

#Find # of mills geocoded and store as MillNumber
with arcpy.da.SearchCursor("Merged_Buff_Clip", "OBJECTID") as cursor:
    x = []
    for i in cursor:
        x.append(i)
MillNumber = len(x)
MillNumber
    
#Then need to convert each shapefile to raster using TOT_MCF field
for i in range(0,MillNumber):
    with arcpy.EnvManager(outputCoordinateSystem='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]', cellSize="MAXOF"):
        arcpy.conversion.FeatureToRaster('T'+str(i+1), "TOT_MCF", 'Raster_T' + str(i+1), 0.00579837955199991)

#Create string containing names of all rasters to be used in MosaictoNewRaster, store as MosaicString
MosaicString = ""
for i in range(0,MillNumber):
    if i == 0:
        MosaicString = ('Raster_T'+str(i+1))
    else:
        MosaicString+=(';Raster_T'+str(i+1))


#Then need to convert each vector to a raster using Mosaic to New Raster
arcpy.management.MosaicToNewRaster(MosaicString, r"C:\Users\kenne\Downloads\TPO_Test\TPO_Test.gdb", "Summed_Raster", 'GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]', "32_BIT_UNSIGNED", None, 1, "SUM", "FIRST")

#After this change symbology, sum rasters, and convert to vector (if preferred)
