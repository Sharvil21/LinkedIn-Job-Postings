#!/usr/bin/env python3
import sys
import csv
import numpy as np

with open(sys.argv[1], 'r') as csv_file:
    csv_reader = csv.reader(csv_file)

    # Skip the header row
    # Write the row to the CSV file
    #next(csv_reader)

    a, v, s = [], [], []
    for i, row in enumerate(csv_reader):

        if i == 0: continue

        applies = row[10]
        views = row[13]
        salary = row[27]


        if applies:
            a.append(float(applies))

        if views:
            v.append(float(views))

        if salary:
            s.append(float(salary))


# Calculate lower tertile (33rd percentile)
lower_tertile = np.percentile(a, 33)

# Calculate middle tertile (67th percentile)
middle_tertile = np.percentile(a, 67)

print("APPLIES_LOW = ", lower_tertile)
print("APPLIES_MED = ", middle_tertile)

# Calculate lower tertile (33rd percentile)
lower_tertile = np.percentile(v, 33)

# Calculate middle tertile (67th percentile)
middle_tertile = np.percentile(v, 67)

print("VIEWS_LOW = ", lower_tertile)
print("VIEWS_MED = ", middle_tertile)

# Calculate lower tertile (33rd percentile)
lower_tertile = np.percentile(s, 33)

# Calculate middle tertile (67th percentile)
middle_tertile = np.percentile(s, 67)

print("SALARY_LOW = ", lower_tertile)
print("SALARY_MED = ", middle_tertile)
