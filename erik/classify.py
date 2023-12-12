#!/usr/bin/env python3
import sys
import csv

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
                row.extend(["class_views", "class_applies", "class_salary"])

                csv_writer.writerow(row)
                continue


            extra = []
            applies = row[10]

            try:
                applies = float(applies)

                if applies < APPLIES_LOW:
                    extra.append("Low")
                elif applies < APPLIES_MED:
                    extra.append("Medium")
                else:
                    extra.append("High")
            except:
                extra.append("Unspecified")

            views = row[13]
            try:
                views = float(views)

                if views < VIEWS_LOW:
                    extra.append("Low")
                elif views < VIEWS_MED:
                    extra.append("Medium")
                else:
                    extra.append("High")
            except:
                extra.append("Unspecified")

            salary = row[27]
            try:
                salary = float(salary)

                if salary < SALARY_LOW:
                    extra.append("Low")
                elif salary < SALARY_MED:
                    extra.append("Medium")
                else:
                    extra.append("High")
            except:
                extra.append("Unspecified")

            row.extend(extra)

            csv_writer.writerow(row)
