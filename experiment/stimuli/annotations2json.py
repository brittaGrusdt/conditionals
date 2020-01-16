import csv
import json


csvfile = open('./scenes_luh_annotations.csv')
jsonfile = open('annotations.json', 'w')
reader = csv.DictReader(csvfile)


for row in reader:
    json.dump(row, jsonfile)
    jsonfile.write('\n')
