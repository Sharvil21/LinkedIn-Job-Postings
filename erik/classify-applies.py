#!/usr/bin/env python3
import sys
import csv


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
                row.append("class_views", "class_applies", "class_salary")

                csv_writer.writerow(row)
                continue


            applies = row[10]
            views = row[13]
            salary = row[27]

