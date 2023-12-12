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

            # 4 - max salary
            # 5 - med salary
            # 6 - min salary
            # 7 - pay period
            # 8 - work-type (contract, PT, FT)

            # Try max first
            salary = row[4]

            # Then median
            if not salary:
                salary = row[5]

            # then minimum
            if not salary:
                salary = row[6]
         
            try:
                salary = float(salary)
            except:

                # There's no salary info, just write the row back out
                if i == 0:
                    row.append("clean_salary")
                else:
                    row.append("")

                csv_writer.writerow(row)
                continue

            # pay period
            pp = row[7]

            # work type
            wt = row[8]


            if pp.lower() == "yearly":
                # nothing to see here
                pass
            elif pp.lower() == "monthly":
                salary *= 12
            elif pp.lower() == "hourly" and wt.lower() == "full-time":
                # 52 weeks * 40 hours a week
                salary = 40 * 52 * salary
            elif pp.lower() == "hourly" and wt.lower() == "contract":
                # 52 weeks * 40 hours a week
                salary = 40 * 52 * salary
            elif pp.lower() == "hourly" and wt.lower() == "part-time":
                # 52 weeks * 20 hours a week
                salary = 20 * 52 * salary
            elif pp.lower() == "hourly" and wt.lower() == "internship":
                # 12 weeks * 40 hours a week
                salary = 12 * 40 * salary
            elif pp.lower() == "hourly" and wt.lower() == "temporary":
                # 12 weeks * 40 hours a week
                salary = 12 * 40 * salary
            elif pp.lower() == "hourly" and wt.lower() == "other":
                # 12 weeks * 40 hours a week
                salary = 12 * 40 * salary
            else:
                print(pp, wt)
                exit()

            row.append(salary)
            csv_writer.writerow(row)


