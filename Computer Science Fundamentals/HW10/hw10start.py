import tkinter
import math
import ssl
from urllib.request import urlopen, urlretrieve
from urllib.parse import urlencode, quote_plus
import json
from twitteraccess import authTwitter, searchTwitter
import webbrowser

def hawkID():
    return("ntdickson")
   
#
# In HW10 and 11, you will use two Google services, Google Static Maps API
# and Google Geocoding API.  Both require use of an API key.
# 
# When you have the API key, put it between the quotes in the string below
GOOGLEAPIKEY = "AIzaSyC-ACxbrTipNoqbv5E7vw35HRZJIuLUEbE"

# To run the HW10 program, call the last function in this file: HW10().

# The Globals class demonstrates a better style of managing "global variables"
# than simply scattering the globals around the code and using "global x" within
# functions to identify a variable as global.
#
# We make all of the variables that we wish to access from various places in the
# program properties of this Globals class.  They get initial values here
# and then can be referenced and set anywhere in the program via code like
# e.g. Globals.zoomLevel = Globals.zoomLevel + 1
#
class Globals:
   
   rootWindow = None
   mapLabel = None

   defaultLocation = "Mt. Fuji, Japan"
   mapLocation = defaultLocation
   mapFileName = 'googlemap.gif'
   mapSize = 400
   zoomLevel = 9
   searchTerms = None

   entryField = None
   plusButton = None
   minusButton = None
   choiceVar = None
   mapType = None
   searchTermsField = None

   tweets = []
   tweetIndex = 0
   tweetTextField = None
   tweetScrollLabel = None
   tweetLatLonList = []

   urls = []
   urlIndex = 0
   urlTextField = None
   urlScrollLabel = None

   lat = None
   lng = None
   
# Given a string representing a location, return 2-element tuple
# (latitude, longitude) for that location 
#
# See https://developers.google.com/maps/documentation/geocoding/
# for details about Google's geocoding API.
#
#
def geocodeAddress(addressString):
   urlbase = "https://maps.googleapis.com/maps/api/geocode/json?address="
   geoURL = urlbase + quote_plus(addressString)
   geoURL = geoURL + "&key=" + GOOGLEAPIKEY

   # required (non-secure) security stuff for use of urlopen
   ctx = ssl.create_default_context()
   ctx.check_hostname = False
   ctx.verify_mode = ssl.CERT_NONE
   
   stringResultFromGoogle = urlopen(geoURL, context=ctx).read().decode('utf8')
   jsonResult = json.loads(stringResultFromGoogle)
   if (jsonResult['status'] != "OK"):
      print("Status returned from Google geocoder *not* OK: {}".format(jsonResult['status']))
      result = (0.0, 0.0) # this prevents crash in retrieveMapFromGoogle - yields maps with lat/lon center at 0.0, 0.0
   else:
      loc = jsonResult['results'][0]['geometry']['location']
      result = (float(loc['lat']),float(loc['lng']))
   return result

# Contruct a Google Static Maps API URL that specifies a map that is:
# - is centered at provided latitude lat and longitude long
# - is "zoomed" to the Google Maps zoom level in Globals.zoomLevel
# - Globals.mapSize-by-Globals.mapsize in size (in pixels), 
# - will be provided as a gif image
#
# See https://developers.google.com/maps/documentation/static-maps/
#
# YOU WILL NEED TO MODIFY THIS TO BE ABLE TO
# 1) DISPLAY A PIN ON THE MAP
# 2) SPECIFY MAP TYPE - terrain vs road vs ...
#
def getMapUrl():
   lat, lng = geocodeAddress(Globals.mapLocation)
   urlbase = "http://maps.google.com/maps/api/staticmap?"

   if Globals.tweetLatLonList != []:
       markers = createMarkerString(Globals.tweetIndex, Globals.tweetLatLonList, (Globals.lat, Globals.lng))
   else:
        markers = "&markers=color:red%7C{},{}".format(lat, lng)
        
   args = "center={},{}&zoom={}&size={}x{}&maptype={}{}&format=gif".format(lat,lng,Globals.zoomLevel,Globals.mapSize,Globals.mapSize,Globals.mapType,markers)
   args = args + "&key=" + GOOGLEAPIKEY
   mapURL = urlbase + args
   print(mapURL)
   return mapURL

# Retrieve a map image via Google Static Maps API, storing the 
# returned image in file name specified by Globals' mapFileName
#
def retrieveMapFromGoogle():
   url = getMapUrl()
   urlretrieve(url, Globals.mapFileName)

########## 
#  basic GUI code

def displayMap():
   retrieveMapFromGoogle()    
   mapImage = tkinter.PhotoImage(file=Globals.mapFileName)
   Globals.mapLabel.configure(image=mapImage)
   # next line necessary to "prevent (image) from being garbage collected" - http://effbot.org/tkinterbook/label.htm
   Globals.mapLabel.mapImage = mapImage
   
def readEntriesSearchTwitterAndDisplayMap():
   #### you should change this function to read from the location from an Entry widget
   #### instead of using the default location

    authTwitter()
    
    Globals.tweetIndex = 0
    Globals.tweetLatLonList = []
    Globals.tweets = []

    Globals.mapLocation = Globals.entryField.get()
    displayMap()

    Globals.searchTerms = Globals.searchTermsField.get()
    lat, lng = geocodeAddress(Globals.mapLocation)

    Globals.lat = lat
    Globals.lng = lng

    tweetsDict = searchTwitter(Globals.searchTerms, 10, 2, (lat, lng))

    for tweet in tweetsDict:
      
        urlsDict = tweet['entities']['urls']

        urlsList = []
        if urlsDict != []:
            for key in urlsDict[0]:
                if key != 'indices':
                    urlsList.append(urlsDict[0][key])
        try:
            urlsList.append(tweet["entities"]["media"][0]["url"])
        except:
            pass

        coordinates = tweet['coordinates']
        if coordinates != None:
            coordsToAppend = coordinates['coordinates']
        else:
           coordsToAppend = None
           
        Globals.tweetLatLonList.append(coordsToAppend)

        Globals.tweets.append({"text":tweet["text"], "name":tweet["user"]["name"], "screen_name":tweet["user"]["screen_name"], "urls":urlsList})
    
    displayTweet()

def createMarkerString(currentTweetIndex, tweetLatLonList, mapCenterLatLon):

    returnString = "&markers=color:red|"
    
    if tweetLatLonList[currentTweetIndex] == None:
        latC = mapCenterLatLon[1]
        lonC = mapCenterLatLon[0]
    else:
        latC = tweetLatLonList[currentTweetIndex][1]
        lonC = tweetLatLonList[currentTweetIndex][0]

    returnString += (str(latC) + "," + str(lonC) + "&markers=color:yellow|size:small|")

    for index in range(0, len(tweetLatLonList)):
        if (index != currentTweetIndex) and (tweetLatLonList[index] != None):
            currLat = tweetLatLonList[index][1]
            currLon = tweetLatLonList[index][0]

            returnString += (str(currLat) + "," + str(currLon) + "|")

    returnString = returnString[0:len(returnString)-1]
    return returnString
        

def zoomIn():
   if Globals.zoomLevel <= 21:
      Globals.zoomLevel += 1
   displayMap()

def zoomOut():
   if Globals.zoomLevel >= 1:
      Globals.zoomLevel -= 1
   displayMap()

def radioButtonChosen():
    if Globals.choiceVar.get() == 1:
        Globals.mapType = "roadmap"
        readEntriesSearchTwitterAndDisplayMap()
        
    elif Globals.choiceVar.get() == 2:
        Globals.mapType = "satellite"
        readEntriesSearchTwitterAndDisplayMap()
        
    elif Globals.choiceVar.get() == 3:
        Globals.mapType = "terrain"
        readEntriesSearchTwitterAndDisplayMap()

    elif Globals.choiceVar.get() == 4:
       Globals.mapType = "hybrid"
       readEntriesSearchTwitterAndDisplayMap()

def leftTweet():
    if Globals.tweetIndex > 0:
        Globals.tweetIndex -= 1
    else:
        Globals.tweetIndex = len(Globals.tweets)-1
    displayTweet()

def rightTweet():
    if Globals.tweetIndex < len(Globals.tweets)-1:
        Globals.tweetIndex += 1
    else:
        Globals.tweetIndex = 0
    displayTweet()


def displayTweet():

    createMarkerString
    Globals.tweetTextField.delete(1.0, "end")
    Globals.urlIndex = 0
    
    if len(Globals.tweets) > 0:
        Globals.tweetTextField.insert(1.0,"Text: " + Globals.tweets[Globals.tweetIndex]["text"])
        Globals.tweetTextField.insert(1.0,"Name: " + Globals.tweets[Globals.tweetIndex]["name"] + "\n\n")
        Globals.tweetTextField.insert(1.0,"Handle: @" + Globals.tweets[Globals.tweetIndex]["screen_name"] + "\n")

        tweetString = "Tweet {} of {}".format(Globals.tweetIndex+1, len(Globals.tweets))
        Globals.tweetScrollLabel.configure(text=tweetString)

    else:
        tweetString = "Tweet {} of {}".format(Globals.tweetIndex, len(Globals.tweets))
        Globals.tweetScrollLabel.configure(text=tweetString)
    displayURL()
    displayMap()

def leftURL():
    if Globals.urlIndex > 0:
        Globals.urlIndex -= 1
    else:
        Globals.urlIndex = len(Globals.tweets[Globals.tweetIndex]["urls"])-1
    displayURL()

def rightURL():
    if Globals.urlIndex < len(Globals.tweets[Globals.tweetIndex]["urls"])-1:
        Globals.urlIndex += 1
    else:
        Globals.urlIndex = 0
    displayURL()

def displayURL():
    Globals.urlTextField.delete(1.0, "end")
    
    if len(Globals.tweets[Globals.tweetIndex]["urls"]) > 0:
        Globals.urlTextField.insert(1.0,Globals.tweets[Globals.tweetIndex]["urls"][Globals.urlIndex])

        urlString = "URL {} of {}".format(Globals.urlIndex+1, len(Globals.tweets[Globals.tweetIndex]["urls"]))
        Globals.urlScrollLabel.configure(text=urlString)

    else:
        urlString = "URL {} of {}".format(Globals.urlIndex, len(Globals.tweets[Globals.tweetIndex]["urls"]))
        Globals.urlScrollLabel.configure(text=urlString)

def webSearch():
    if len(Globals.tweets)>0:
        if len(Globals.tweets[Globals.tweetIndex]["urls"]) > 0:
            webbrowser.open(Globals.tweets[Globals.tweetIndex]["urls"][0])
    
def initializeGUIetc():

   Globals.rootWindow = tkinter.Tk()
   Globals.rootWindow.title("HW10")

   mainFrame = tkinter.Frame(Globals.rootWindow) 
   mainFrame.pack()

   middleFrame = tkinter.Frame(Globals.rootWindow)
   middleFrame.pack()

   bottomFrame = tkinter.Frame(Globals.rootWindow)
   bottomFrame.pack()

   bottomLeftFrame = tkinter.Frame(bottomFrame)
   bottomLeftFrame.pack(side = 'left')

   radioButtonFrame1 = tkinter.Frame(bottomFrame)
   radioButtonFrame1.pack(side='left')

   radioButtonFrame2 = tkinter.Frame(bottomFrame)
   radioButtonFrame2.pack(side='left')

   tweetFrame = tkinter.Frame(Globals.rootWindow)
   tweetFrame.pack(side='bottom')

   tweetScrollFrame = tkinter.Frame(tweetFrame)
   urlScrollFrame = tkinter.Frame(tweetFrame)
   
   # until you add code, pressing this button won't change the map (except
   # once, to the Beijing location "hardcoded" into readEntryAndDisplayMap)
   # you need to add an Entry widget that allows you to type in an address
   # The click function should extract the location string from the Entry widget
   # and create the appropriate map.
   readEntryAndDisplayMapButton = tkinter.Button(mainFrame, text="Show me the map!", command=readEntriesSearchTwitterAndDisplayMap)
   readEntryAndDisplayMapButton.pack()

   # we use a tkinter Label to display the map image
   Globals.mapLabel = tkinter.Label(mainFrame, width=Globals.mapSize, bd=2, relief=tkinter.FLAT)
   Globals.mapLabel.pack()

   entryLabel = tkinter.Label(middleFrame, text="Enter the location: ")
   entryLabel.pack(side = 'left')

   Globals.entryField = tkinter.Entry(middleFrame)
   Globals.entryField.pack(side = 'left')

   searchTermsLabel = tkinter.Label(middleFrame, text="Search terms: ")
   searchTermsLabel.pack(side = 'left')

   Globals.searchTermsField = tkinter.Entry(middleFrame)
   Globals.searchTermsField.pack(side = 'left')
   
   Globals.plusButton = tkinter.Button(bottomLeftFrame, text='+', command=zoomIn)
   Globals.plusButton.pack(side = 'top')

   Globals.minusButton = tkinter.Button(bottomLeftFrame, text='-', command=zoomOut)
   Globals.minusButton.pack(side = 'bottom')
   
   Globals.choiceVar = tkinter.IntVar()

   standardButton = tkinter.Radiobutton(radioButtonFrame1, text='Standard view', variable=Globals.choiceVar, value = 1, command=radioButtonChosen)
   standardButton.pack()
   standardButton.select()

   satelliteButton = tkinter.Radiobutton(radioButtonFrame1, text='Satellite view', variable=Globals.choiceVar, value = 2, command=radioButtonChosen)
   satelliteButton.pack()

   terrainButton = tkinter.Radiobutton(radioButtonFrame2, text='Terrain view', variable=Globals.choiceVar, value = 3, command=radioButtonChosen)
   terrainButton.pack()

   hybridButton = tkinter.Radiobutton(radioButtonFrame2, text='Hybrid view', variable=Globals.choiceVar, value = 4, command=radioButtonChosen)
   hybridButton.pack()

   Globals.tweetTextField = tkinter.Text(tweetFrame, borderwidth = 3, height = 5)
   Globals.tweetTextField.insert(1.0, 'Current tweet')
   Globals.tweetTextField.pack(side='top')
   
   leftTweetButton = tkinter.Button(tweetScrollFrame, text='<', command=leftTweet)
   leftTweetButton.pack(side='left')

   Globals.tweetScrollLabel = tkinter.Label(tweetScrollFrame, text='Tweet {} of {}'.format(Globals.tweetIndex, len(Globals.tweets)))
   Globals.tweetScrollLabel.pack(side='left')

   rightTweetButton = tkinter.Button(tweetScrollFrame, text='>', command=rightTweet)
   rightTweetButton.pack(side='left')

   tweetScrollFrame.pack(side='top')
   
   Globals.urlTextField = tkinter.Text(tweetFrame, borderwidth = 3, height = 5)
   Globals.urlTextField.insert(1.0, 'Current url')
   Globals.urlTextField.pack(side='top')

   leftURLButton = tkinter.Button(urlScrollFrame, text='<', command=leftURL)
   leftURLButton.pack(side='left')

   Globals.urlScrollLabel = tkinter.Label(urlScrollFrame, text='URL {} of {}'.format(Globals.urlIndex, len(Globals.urls)))
   Globals.urlScrollLabel.pack(side='left')

   rightURLButton = tkinter.Button(urlScrollFrame, text='>', command=rightURL)
   rightURLButton.pack(side='left')

   urlScrollFrame.pack(side='top')

   webSearchButton = tkinter.Button(tweetFrame, text = "Web Search", command=webSearch)
   webSearchButton.pack(side='top')

def HW10():
    initializeGUIetc()
    displayMap()
    Globals.rootWindow.mainloop()
