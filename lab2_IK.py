# Name: Ian Kennedy
# Date of Creation: 9/29/21

import math

from csv import reader
import polygon_area

points = []
latitude =[]
longitude =[]
with open ('mosquito_locations.csv','r') as raw_points:
    csv_reader = reader(raw_points)
    header = next(csv_reader)  
    
    for row in csv_reader:
        points.append(list(map(float,row[1:])))

#--------TASK 1----#
def perimeter(coord):
    distance = 0
    #write your code here to calculate the perimeter
    for i in points:
        latitude.append(math.radians(i[0]))
        longitude.append(math.radians(i[1]))
    for i in range(len(latitude)-1):
        distance += 6371.01 * math.acos((math.sin(latitude[i])) * (math.sin(latitude[i+1])) + math.cos(latitude[i]) * math.cos(latitude[i+1]) * math.cos(longitude[i+1]-longitude[i]))
    return distance
print("Perimeter of the polygon is: %.3f" %perimeter(points))

#--------TASK 2----#
for i in points:
    latitude.append(i[0])
    longitude.append(i[1])
x_coord = latitude
y_coord = longitude

area =(polygon_area.line_integral(x_coord,y_coord))/1000000
print("Area of the polygon is: %.3f" %area)


print(latitude)