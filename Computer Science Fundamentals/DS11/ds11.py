def hawkID():
    return("ntdickson")

def createMarkerString(currentTweetIndex, tweetLatLonList, mapCenterLatLon):

    returnString = "&markers=color:red|"
    
    if tweetLatLonList[currentTweetIndex] == None:
        latC = mapCenterLatLon[0]
        lonC = mapCenterLatLon[1]
    else:
        latC = tweetLatLonList[currentTweetIndex][0]
        lonC = tweetLatLonList[currentTweetIndex][1]

    returnString += (str(latC) + "," + str(lonC) + "&markers=color:yellow|size:small|")

    for index in range(0, len(tweetLatLonList)):
        if (index != currentTweetIndex) and (tweetLatLonList[index] != None):
            currLat = tweetLatLonList[index][0]
            currLon = tweetLatLonList[index][1]

            returnString += (str(currLat) + "," + str(currLon) + "|")

    returnString = returnString[0:len(returnString)-1]
    return returnString
    
