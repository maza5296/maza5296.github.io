import csv
import itertools
with open('citiesToCitiesPass_test.csv') as f:
    data = ""
    tmp = ""
    quote = '"'
    newline = ["\n"]
    newlinestr = "\n"
    reader = csv.reader(f)
    for row in reader:
    	str_first = row[0].replace(",",'","')
    	str_second = row[1].replace(",", '","')
    	tmp = quote + str_first + '","'+ str_second + '","' + str(row[2]) + '","' + str(row[3]) + '","' + str(row[4]) + '","' + str(row[5]) + '","' + str(row[6]) + quote
    	print(tmp)
    	data = data + "\n" + tmp

out = open("myfile.csv","w")
out.write(data)
out.close