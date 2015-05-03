import csv
import itertools
with open('citiesToCitiesPass_test.csv') as f:
	reader = csv.reader(f)
	data = []
	tmp = []
	for row in reader:
		firstSplit = row[0].split(",")
		secondSplit = row[1].split(",")
		tmp = firstSplit + secondSplit + [row[2]] + [row[3]] + [row[4]] + [row[5]] + [row[6]] + ["\n"]
		if len(tmp[1]) == 2:
			tmp[1] = "USA"
		if len(tmp[3]) == 2:
			tmp[3] = "USA"
		print(tmp)
		data = data + tmp


# resultFile = open("output.csv",'wb')
# wr = csv.writer(resultFile, dialect='excel')
# wr.writerow(data)