import json, csv
import pandas as pd
from pandas import DataFrame
import datetime
from datetime import datetime

infilename = '/Users/catherinedornfeld/Desktop/UW-Madison/CPL/PDC_Boston/events-feb-4-last.json'
infile = open(infilename,'rb')
mydata = []
for line in infile:
    thisline = json.loads(line)
    timestamp = int(thisline["timestamp"])/1000
    humTime = datetime.fromtimestamp(
        timestamp
    ).strftime('%Y-%m-%d %H:%M:%S')
    thisline["humanTime"] = humTime
    mydata.append(thisline)

myData = DataFrame(mydata)
##change write path
myData.to_csv("/Users/catherinedornfeld/Desktop/UW-Madison/CPL/PDC_Boston/lead_feb4_r2.csv", sep=",")
#
# print myData