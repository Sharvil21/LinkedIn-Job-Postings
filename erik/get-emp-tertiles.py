#!/usr/bin/env python3
import sys
import csv
import numpy as np

with open(sys.argv[1], 'r') as csv_file:
    csv_reader = csv.reader(csv_file)

    # Skip the header row
    # Write the row to the CSV file
    #next(csv_reader)

    e, f = [], []
    for i, row in enumerate(csv_reader):

        if i == 0: continue

        employees = row[1]
        followers = row[2]


        e.append(int(employees))
        f.append(int(followers))

# Calculate lower tertile (33rd percentile)
lower_tertile = np.percentile(f, 33)

# Calculate middle tertile (67th percentile)
middle_tertile = np.percentile(f, 67)

print("FOLLOW_LOW = ", lower_tertile)
print("FOLLOW_MED = ", middle_tertile)

# Calculate lower tertile (33rd percentile)
lower_tertile = np.percentile(e, 33)

# Calculate middle tertile (67th percentile)
middle_tertile = np.percentile(e, 67)

print("EMP_LOW = ", lower_tertile)
print("EMP_MED = ", middle_tertile)
