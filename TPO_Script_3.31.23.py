# -*- coding: utf-8 -*-
"""
Created on Fri Mar 31 19:09:53 2023

@author: ikenn
"""

import arcpy
from arcpy import os
from csv import reader
import numpy as np
import re

workspace = os.getcwd()
data = input("File Path: ")
locator_path = "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/ArcGIS World Geocoding Service"

#Tidy data in R prior to reading in...Check if lon is negative/lat is positive
#Geocode using Mailing Address
arcpy.geocoding.GeocodeAddresses(data, locator_path, "'Address or Place' MILL_STREET1 VISIBLE NONE;Address2  VISIBLE NONE;Address3  VISIBLE NONE;Neighborhood  VISIBLE NONE;City MILL_CITY VISIBLE NONE;County  VISIBLE NONE;State MILL_STATE VISIBLE NONE;ZIP MILL_ZIP_CD VISIBLE NONE;ZIP4  VISIBLE NONE;Country  VISIBLE NONE", r"C:\Users\ikenn\OneDrive - University of Massachusetts\Documents\MyProject2\MyProject2.gdb\Mailing_Geocoded", "STATIC", "US", "ADDRESS_LOCATION", "Subaddress;'Point Address';'Street Address';'Distance Marker';Intersection;'Street Name'", "MINIMAL")

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
arcpy.CopyRows_management("Mailing_Geocoded_Unmatched", "C:/Users/ikenn/OneDrive - University of Massachusetts/Downloads/TPO_Test/Unmatched_Mailing.xlsx")

#Delete matched rows from 'Unmatched_Mailing'
arcpy.management.SelectLayerByAttribute('Unmatched_Mailing', "NEW_SELECTION", "Status = 'M' Or Score > 80", None)
arcpy.management.DeleteRows('Unmatched_Mailing')
#Delete fields from 'Unmatched_Mailing'
arcpy.management.DeleteField("Unmatched_Mailing", "Status;Score;Match_type;Match_addr;Addr_type", "DELETE_FIELDS")

#Geocode unmatched mailing addresses with physical addresses
with arcpy.EnvManager(scratchWorkspace=r"C:\Users\ikenn\OneDrive - University of Massachusetts\Downloads\TPO_Test\TPO_Test.gdb", workspace=r"C:\Users\ikenn\OneDrive - University of Massachusetts\Documents\MyProject2\MyProject2.gdb"):
    arcpy.geocoding.GeocodeAddresses("Unmatched_Mailing", locator_path, "'Address or Place' PHYSICAL_A VISIBLE NONE;Address2  VISIBLE NONE;Address3  VISIBLE NONE;Neighborhood  VISIBLE NONE;City PHYSICAL_C VISIBLE NONE;County  VISIBLE NONE;State PHYSICAL_S VISIBLE NONE;ZIP PHYSICAL_Z VISIBLE NONE;ZIP4  VISIBLE NONE;Country  VISIBLE NONE", "Physical_Geocoded", "STATIC", "US", "ADDRESS_LOCATION", "Subaddress;'Point Address';'Street Address';'Distance Marker';Intersection;'Street Name'", "MINIMAL")
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
arcpy.CopyRows_management("Physical_Geocoded_Unmatched", "C:/Users/ikenn/OneDrive - University of Massachusetts/Downloads/TPO_Test/Unmatched_Physical.xlsx")

#Delete matched rows from 'Unmatched_Physical'
arcpy.management.SelectLayerByAttribute("Unmatched_Physical", "NEW_SELECTION", "Status = 'M' Or Score > 50", None)
arcpy.management.DeleteRows("Unmatched_Physical")

#Delete Unmatched Rows from M/P Geocodes
arcpy.management.SelectLayerByAttribute(PAddress, "NEW_SELECTION", "Status = 'U' Or Score < 80", None)
arcpy.management.DeleteRows(PAddress)
arcpy.management.SelectLayerByAttribute(MAddress, "NEW_SELECTION", "Status = 'U' Or Score < 80", None)
arcpy.management.DeleteRows(MAddress)

arcpy.management.CalculateField("Unmatched_Physical", "MILL_LON1", "!MILL_LON!", "PYTHON3", '', "DOUBLE", "NO_ENFORCE_DOMAINS")
arcpy.management.CalculateField("Unmatched_Physical", "MILL_LAT1", "!MILL_LAT!", "PYTHON3", '', "DOUBLE", "NO_ENFORCE_DOMAINS")

#Geocode unmatched points with Lat/Lon, store as Lat_Lon_Geocoded...Change variable names depending on if lat/lon are numeric/need conversion
arcpy.management.XYTableToPoint("Unmatched_Physical", LAddress, "MILL_LON1", "MILL_LAT1")

#Transfer unmatched addresses to Unmatched_Final (not matched after Mailing, Physical, or Lat/Lon geocodes)
arcpy.management.SelectLayerByAttribute("Lat_Lon_Geocoded", "NEW_SELECTION", "MILL_LAT1 = 0 Or MILL_LON1 = 0 Or MILL_LAT1 IS NULL Or MILL_LON1 IS NULL", None)

#Change geodatabase based on Region
arcpy.conversion.TableToTable("Unmatched_Physical", r"C:\Users\ikenn\OneDrive - University of Massachusetts\Documents\MyProject2\MyProject2.gdb", "Unmatched_Final", '', 'MILL_NAME "MILL_NAME" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_NAME,0,254;MILL_STREE "MILL_STREE" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_STREE,0,254;MILL_CITY "MILL_CITY" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_CITY,0,254;MILL_ZIP_C "MILL_ZIP_C" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_ZIP_C,0,254;MILL_STATE "MILL_STATE" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_STATE,0,254;PHYSICAL_A "PHYSICAL_A" true true false 254 Text 0 0,First,#,Unmatched_Physical,PHYSICAL_A,0,254;PHYSICAL_C "PHYSICAL_C" true true false 254 Text 0 0,First,#,Unmatched_Physical,PHYSICAL_C,0,254;PHYSICAL_S "PHYSICAL_S" true true false 254 Text 0 0,First,#,Unmatched_Physical,PHYSICAL_S,0,254;PHYSICAL_Z "PHYSICAL_Z" true true false 254 Text 0 0,First,#,Unmatched_Physical,PHYSICAL_Z,0,254;MILL_LAT "MILL_LAT" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_LAT,0,254;MILL_LON "MILL_LON" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_LON,0,254;MILL_TYPE_ "MILL_TYPE_" true true false 254 Text 0 0,First,#,Unmatched_Physical,MILL_TYPE_,0,254;TPOID "TPOID" true true false 254 Text 0 0,First,#,Unmatched_Physical,TPOID,0,254;Region "Region" true true false 254 Text 0 0,First,#,Unmatched_Physical,Region,0,254;TOT_MCF "TOT_MCF" true true false 19 Double 0 0,First,#,Unmatched_Physical,TOT_MCF,-1,-1;Pro_Rad "Pro_Rad" true true false 19 Double 0 0,First,#,Unmatched_Physical,Pro_Rad,-1,-1', '')

#Delete Unmatched Rows from Lat/Lon Geocodes
arcpy.management.SelectLayerByAttribute(LAddress, "NEW_SELECTION", "MILL_LAT1 = 0 Or MILL_LON1 = 0 Or MILL_LAT1 IS NULL Or MILL_LON1 IS NULL", None)
arcpy.management.DeleteRows(LAddress)

Merge = "Geocoded_Merge"
#Merge Mailing, Physical, and Lat/Lon Geocodes
arcpy.management.Merge("Mailing_Geocoded;Physical_Geocoded;Lat_Lon_Geocoded", r"C:\Users\ikenn\OneDrive - University of Massachusetts\Documents\MyProject2\MyProject2.gdb\Geocoded_Merge", 'MILL_NAME "MILL_NAME" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_NAME,0,255,Physical_Geocoded,MILL_NAME,0,254,Lat_Lon_Geocoded,MILL_NAME,0,254;MILL_STREET1 "MILL_STREET1" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_STREET1,0,255;MILL_CITY "MILL_CITY" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_CITY,0,255,Physical_Geocoded,MILL_CITY,0,254,Lat_Lon_Geocoded,MILL_CITY,0,254;MILL_ZIP_CD "MILL_ZIP_CD" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_ZIP_CD,0,255;MILL_STATE "MILL_STATE" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_STATE,0,255,Physical_Geocoded,MILL_STATE,0,254,Lat_Lon_Geocoded,MILL_STATE,0,254;PHYSICAL_ADDRESS "PHYSICAL_ADDRESS" true true false 255 Text 0 0,First,#,Mailing_Geocoded,PHYSICAL_ADDRESS,0,255;PHYSICAL_CITY "PHYSICAL_CITY" true true false 255 Text 0 0,First,#,Mailing_Geocoded,PHYSICAL_CITY,0,255;PHYSICAL_STATE "PHYSICAL_STATE" true true false 255 Text 0 0,First,#,Mailing_Geocoded,PHYSICAL_STATE,0,255;PHYSICAL_ZIP "PHYSICAL_ZIP" true true false 255 Text 0 0,First,#,Mailing_Geocoded,PHYSICAL_ZIP,0,255;MILL_LAT "MILL_LAT" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_LAT,0,255,Physical_Geocoded,MILL_LAT,0,254,Lat_Lon_Geocoded,MILL_LAT,0,254;MILL_LON "MILL_LON" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_LON,0,255,Physical_Geocoded,MILL_LON,0,254,Lat_Lon_Geocoded,MILL_LON,0,254;MILL_TYPE_CD "MILL_TYPE_CD" true true false 255 Text 0 0,First,#,Mailing_Geocoded,MILL_TYPE_CD,0,255;TPOID "TPOID" true true false 255 Text 0 0,First,#,Mailing_Geocoded,TPOID,0,255,Physical_Geocoded,TPOID,0,254,Lat_Lon_Geocoded,TPOID,0,254;Region "Region" true true false 255 Text 0 0,First,#,Mailing_Geocoded,Region,0,255,Physical_Geocoded,Region,0,254,Lat_Lon_Geocoded,Region,0,254;TOT_MCF "TOT_MCF" true true false 8 Double 0 0,First,#,Mailing_Geocoded,TOT_MCF,-1,-1,Physical_Geocoded,TOT_MCF,-1,-1,Lat_Lon_Geocoded,TOT_MCF,-1,-1;Pro_Rad "Pro_Rad" true true false 8 Double 0 0,First,#,Mailing_Geocoded,Pro_Rad,-1,-1,Physical_Geocoded,Pro_Rad,-1,-1,Lat_Lon_Geocoded,Pro_Rad,-1,-1;MILL_STREE "MILL_STREE" true false false 254 Text 0 0,First,#,Physical_Geocoded,MILL_STREE,0,254,Lat_Lon_Geocoded,MILL_STREE,0,254;MILL_ZIP_C "MILL_ZIP_C" true false false 254 Text 0 0,First,#,Physical_Geocoded,MILL_ZIP_C,0,254,Lat_Lon_Geocoded,MILL_ZIP_C,0,254;PHYSICAL_A "PHYSICAL_A" true false false 254 Text 0 0,First,#,Physical_Geocoded,PHYSICAL_A,0,254,Lat_Lon_Geocoded,PHYSICAL_A,0,254;PHYSICAL_C "PHYSICAL_C" true false false 254 Text 0 0,First,#,Physical_Geocoded,PHYSICAL_C,0,254,Lat_Lon_Geocoded,PHYSICAL_C,0,254;PHYSICAL_S "PHYSICAL_S" true false false 254 Text 0 0,First,#,Physical_Geocoded,PHYSICAL_S,0,254,Lat_Lon_Geocoded,PHYSICAL_S,0,254;PHYSICAL_Z "PHYSICAL_Z" true false false 254 Text 0 0,First,#,Physical_Geocoded,PHYSICAL_Z,0,254,Lat_Lon_Geocoded,PHYSICAL_Z,0,254;MILL_TYPE_ "MILL_TYPE_" true false false 254 Text 0 0,First,#,Physical_Geocoded,MILL_TYPE_,0,254,Lat_Lon_Geocoded,MILL_TYPE_,0,254', "NO_SOURCE_INFO")

Merge = "Geocoded_Merge"
#populate new field with string values of procurement radius
arcpy.management.CalculateField(Merge, "Pro_Rad_Final", 'str(!Pro_Rad!) + " MILES"', "PYTHON3", '', "TEXT", "NO_ENFORCE_DOMAINS")

Buffer = "Geocoded_Merge_ProRad" 

#buffer addresses using procurement radius.
arcpy.analysis.Buffer(Merge, Buffer, "Pro_Rad_Final", "FULL", "ROUND", "NONE", None, "PLANAR")

#clip buffers to USA outline.
arcpy.analysis.Clip(Buffer, "USA", "Merged_Buff_Clip", None)
arcpy.management.CalculateField("Merged_Buff_Clip", "ID_Val", "!OBJECTID!", "PYTHON3", '', "TEXT", "NO_ENFORCE_DOMAINS")

arcpy.analysis.SplitByAttributes("Merged_Buff_Clip", r"C:\Users\ikenn\Documents\TravelTimeLayout\Unmatched.gdb", "TPOID")
#After this step, add all shapefiles from folder

#Find # of mills geocoded and store as MillNumber
#"OBJECT_ID"
with arcpy.da.SearchCursor("Merged_Buff_Clip", "TPOID") as cursor:
    x = []
    for i in cursor:
        x.append(i)
MillNumber = len(x)
MillNumber

IDFormat = r'\d+-2021-\d+-\d+'
IDFormat2 = r'\d+_2021_\d+_\d+'

b = []
for i in x:
    b.append(str(i))
    next

c = []
for i in b:
    c.append(re.findall(IDFormat, i))
    next

d = np.array(c)
e = len(d)

f=[]
for i in range(e):
    f.append(re.sub("-", "_", str(c[i])))
    next

Final = []
for i in f:
    Final.append(re.findall(IDFormat2, i))
    next

FinalArray = np.array(Final)

# Traditional Clip
for i in range(e):
        arcpy.analysis.Clip(filepath+"\\T"+FinalArray[i,0]+"0", filepath+"\\T"+FinalArray[i,0], filepath + '\\T' + FinalArray[i,0] + "_Clip" , None)
# Or Pairwise Clip:
# filepath2 = r"C:\Users\ikenn\Documents\TravelTimeLayout\TravelTimeLayout.gdb"
# r"C:\Users\ikenn\OneDrive - University of Massachusetts\Documents\MyProject2\MyProject2.gdb"
# for i in range(e):
#     with arcpy.EnvManager(scratchWorkspace=r"C:\Users\ikenn\Documents\TravelTimeLayout\TravelTimeLayout.gdb", outputCoordinateSystem='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]', workspace=r"C:\Users\ikenn\OneDrive - University of Massachusetts\Documents\MyProject2\MyProject2.gdb"):
#         arcpy.analysis.PairwiseClip(filepath2+"\\T"+FinalArray[i,0], filepath2+"\\T"+FinalArray[i,0]+"New", filepath2+"\\T"+FinalArray[i,0]+"_Clip", None)

for i in range(e):
        with arcpy.EnvManager(outputCoordinateSystem='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]'):
            arcpy.conversion.PolygonToRaster(filepath+"\\T"+FinalArray[i,0]+"_Clip", "TOT_MCF", filepath + '\\Raster_T'+FinalArray[i,0], "CELL_CENTER", "NONE", 0.004, "DO_NOT_BUILD")

#Create string containing names of all rasters to be used in MosaictoNewRaster, store as MosaicString
MosaicString = ""
for i in range(e):
    if i == 0:
        MosaicString = ('Raster_T'+FinalArray[i,0])
    else:
        MosaicString+=(';Raster_T'+FinalArray[i,0])
        
#Then need to convert each vector to a raster using Mosaic to New Raster
arcpy.management.MosaicToNewRaster(MosaicString, filepath, "Summed_Raster", 'GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]', "32_BIT_UNSIGNED", None, 1, "SUM", "FIRST")