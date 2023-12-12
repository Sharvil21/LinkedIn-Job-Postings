#!/usr/bin/env python3
import sys
import csv

FOLLOW_LOW =  4075.7100000000005
FOLLOW_MED =  31740.590000000004
EMP_LOW =  68.0
EMP_MED =  864.4300000000003

APPLIES_LOW =  3.0
APPLIES_MED =  14.0
VIEWS_LOW =  10.0
VIEWS_MED =  53.0
SALARY_LOW =  65000.0
SALARY_MED =  121339.62000000004

# Open the CSV file in 'a' (append) mode to add a new row
with open(sys.argv[2], 'a', newline='') as csv_file:
    csv_writer = csv.writer(csv_file)

    with open(sys.argv[1], 'r') as csv_file:
        csv_reader = csv.reader(csv_file)

        # Skip the header row
        # Write the row to the CSV file
        #next(csv_reader)

        for i, row in enumerate(csv_reader):

            # There's no salary info, just write the row back out
            if i == 0:
                row.extend(["class_employees", "class_followers"])

                csv_writer.writerow(row)
                continue


            extra = []

            employees=int(row[1])

            if employees < EMP_LOW:
                extra.append("Low")
            elif employees < EMP_MED:
                extra.append("Medium")
            else:
                extra.append("High")

            followers=int(row[2])

            if followers < FOLLOW_LOW:
                extra.append("Low")
            elif followers < FOLLOW_MED:
                extra.append("Medium")
            else:
                extra.append("High")

            row.extend(extra)

            csv_writer.writerow(row)
