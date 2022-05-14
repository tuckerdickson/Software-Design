def q1(infoDict, listOfLists):
    
    result = []
    
    for sublist in listOfLists:
        
        greenCount = 0
        redCount = 0
        blueCount = 0

        countDict = {}

        if(len(sublist) > 0):
            largestElement = sublist[0]
            smallestElement = sublist[0]
        
            for number in sublist:

                if(infoDict.get(number, "NULL") == "red"):
                    redCount += 1
                elif(infoDict.get(number, "NULL") == "blue"):
                    blueCount += 1
                else:
                    greenCount += 1

                if(number > largestElement):
                    largestElement = number
                elif(number < smallestElement):
                    smallestElement = number

                if(number in countDict):
                    countDict[number] += 1
                else:
                    countDict[number] = 1

            keyList = list(countDict.keys())
            mostOccurring = keyList[0]

            for item in countDict:
                if countDict[item] > mostOccurring:
                    mostOccurring = item
                elif countDict[item] == mostOccurring:
                    if(item < mostOccurring):
                        mostOccurring = item

            if((redCount > blueCount) and (redCount > greenCount)):
                subresult = largestElement
            elif((blueCount > redCount) and (blueCount > greenCount)):
                subresult = smallestElement
            else:
                subresult = mostOccurring

            result.append(subresult)

        else:
            result.append(None)

    return result








# 2b.   The vast majority of the 20 most-occurring ham words are words that occur very frequently in conversation.
#       Most of them are either subject words like i, you and me, possessive words like my and your, or other common 2-3 letter words
#       like to, the, a, and, in, is, it, of, for, etc. Most of the 20 most-occurring ham words are 2-3 letters; only 3 of them are 4 letters.
#
#       The list of 20 most-occurring spam words overlaps a little bit with common words like a, you, your, the, and for, it also contains
#       several words that don't occur very frequently in normal speech, such as call, free, now, txt, mobile, and 2. Something else that I
#       found interesting about the most-occurring spam list was that it contianed several "texting contractions" such as 2, txt, u, and ur.
#       This is in contrast to the ham list, which only contained 'u'.
#
def q2(filename, minWordLengthToConsider = 1):
    # open the file
    file = open(filename,encoding='utf-8')

    hamCount = 0
    spamCount = 0

    hamAverageSum = 0
    spamAverageSum = 0

    hamList = []
    spamList = []

    hamDict = {}
    spamDict = {}

    # iterate over every line in the file
    for line in file:

        # split the line into a list of individual words
        lineAsList = line.split()
            
        # increment ham/spam count accordingly
        # add list to appropriate dictionary
        if lineAsList[0] == "ham":
            hamCount += 1
            lineAsList = lineAsList[1:]
            hamList.append(lineAsList)
            
        elif lineAsList[0] == "spam":
            spamCount += 1
            lineAsList = lineAsList[1:]
            spamList.append(lineAsList)

    # add all of the words in hamList to hamDict
    for line in hamList:
        hamAverageSum += len(line)
        
        for word in line:
            lowerWord = word.lower()
            newWord = lowerWord.strip("?!;:,.' ")
            
            if newWord in hamDict:
                hamDict[newWord] += 1
            else:
                hamDict[newWord] = 1

    # add all of the words in spamList to spamDict
    for line in spamList:
        spamAverageSum += len(line)
        
        for word in line:
            lowerWord = word.lower()
            newWord = lowerWord.strip("?!;:,.' ")
            
            if newWord in spamDict:
                spamDict[newWord] += 1
            else:
                spamDict[newWord] = 1

    sortedHam = sorted(hamDict.items(), key = lambda item : item[1], reverse = True)
    sortedSpam = sorted(spamDict.items(), key = lambda item : item[1], reverse = True)

    totalHamWords = sum(hamDict.values())
    totalSpamWords = sum(spamDict.values())

    uniqueHamWords = len(hamDict)
    uniqueSpamWords = len(spamDict)

    print("Total ham messages:", hamCount)
    print("Total spam messages:", spamCount, "\n")

    print("Total ham words:", totalHamWords)
    print("Total spam words:", totalSpamWords, "\n")

    print("Number of unique ham words:", uniqueHamWords)
    print("Number of unique spam words:", uniqueSpamWords, "\n")

    print("Average number of words per ham message:", float(hamAverageSum) / float(hamCount))
    print("Average number of words per spam message:", float(spamAverageSum) / float(spamCount), "\n")

    print("20 most-occurring ham words:")
    print("\t", "Word:", "\t\t", "Occurrences:", "\t", "Relative Frequency:\n")
    i = 0
    count = 0
    while count < 20:
        if len(sortedHam[i][0]) >= minWordLengthToConsider:
            print("\t", sortedHam[i][0], "\t\t", sortedHam[i][1], "\t\t", (float(sortedHam[i][1]) / float(totalHamWords)) * 100)
            count += 1
        i += 1
    print("\n")

    print("20 most-occurring spam words:")
    print("\t", "Word:", "\t\t", "Occurrences:", "\t", "Relative Frequency:\n")
    i = 0
    count = 0
    while count < 20:
        if len(sortedSpam[i][0]) >= minWordLengthToConsider:
            print("\t", sortedSpam[i][0], "\t\t", sortedSpam[i][1], "\t\t", (float(sortedSpam[i][1]) / float(totalSpamWords)) * 100)
            count += 1
        i += 1
    print("\n")
        
       




    
    
