import pygn
import json
import os
import time
import csv

# Credentials
clientID = '1247529661-426ED72417CF162A47EBA9EFF7EA2439'
userID = pygn.register(clientID)

# Read list of albums
file = 'album_list/album_list.csv'
with open(file,'r') as infile:
    csvreader = csv.reader(infile)
    albums = [(row[0],row[1]) for row in csvreader if row[0] != 'artist' and row[1] != 'name']
        

# Get song data
for (artist,album) in albums:
    outfile='json/'+artist+'-'+album+'.json'
    if not os.path.exists(outfile):
        print(outfile)
        time.sleep(0.05)

        results = pygn.search(clientID=clientID,
                               userID=userID, 
                               artist=artist,
                               album=album)

        with open(outfile,'w') as outfile:
            json.dump(results,outfile)

