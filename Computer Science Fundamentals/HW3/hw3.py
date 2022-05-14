def q1(inputString, minLetter):

    inputString = inputString.lower()
    
    smallestLetter = None
    smallestLetterIndex = None

    secondSmallestLetter = None
    secondSmallestLetterIndex = None
    
    thirdSmallestLetter = None
    thirdSmallestLetterIndex = None
    
    mostCommonLetter = None
    mostCommonLetterNum = None


    currIndex = 0
    while(currIndex < len(inputString)):
        currLetter = inputString[currIndex]

        if(currLetter >= minLetter):

            if(smallestLetter == None):
                
                smallestLetter = currLetter
                smallestLetterIndex = currIndex

            elif(secondSmallestLetter == None):
                
                if(currLetter < smallestLetter):
                    
                    secondSmallestLetter = smallestLetter
                    secondSmallestLetterIndex = smallestLetterIndex

                    smallestLetter = currLetter
                    smallestLetterIndex = currIndex

                elif(currLetter > smallestLetter):

                    secondSmallestLetter = currLetter
                    secondSmallestLetterIndex = currIndex

            elif(thirdSmallestLetter == None):

                if(currLetter < smallestLetter):

                    thirdSmallestLetter = secondSmallestLetter
                    thirdSmallestLetterIndex = secondSmallestLetterIndex

                    secondSmallestLetter = smallestLetter
                    secondSmallestLetterIndex = smallestLetterIndex

                    smallestLetter = currLetter
                    smallestLetterIndex = currIndex

                elif((currLetter > smallestLetter) and (currLetter < secondSmallestLetter)):

                    thirdSmallestLetter = secondSmallestLetter
                    thirdSmallestLetterIndex = secondSmallestLetterIndex

                    secondSmallestLetter = currLetter
                    secondSmallestLetterIndex = currIndex

                elif(currLetter > secondSmallestLetter):

                    thirdSmallestLetter = currLetter
                    thirdSmallestLetterIndex = currIndex
                    
            elif(smallestLetter != None) and (secondSmallestLetter != None) and (thirdSmallestLetter != None):

                if(currLetter < smallestLetter):
                    
                    thirdSmallestLetter = secondSmallestLetter
                    thirdSmallestLetterIndex = secondSmallestLetterIndex
                
                    secondSmallestLetter = smallestLetter
                    secondSmallestLetterIndex = smallestLetterIndex
                
                    smallestLetter = currLetter
                    smallestLetterIndex = currIndex
                    
                elif(currLetter > smallestLetter) and (currLetter < secondSmallestLetter):
                    thirdSmallestLetter = secondSmallestLetter
                    thirdSmallestLetterIndex = secondSmallestLetterIndex
                    
                    secondSmallestLetter = currLetter
                    secondSmallestLetterIndex = currIndex

                elif(currLetter > secondSmallestLetter) and (currLetter < thirdSmallestLetter):
                    thirdSmallestLetter = currLetter
                    thirdSmallestLetterIndex = currIndex
            
        currIndex += 1

    letterCountList = []

    for char in inputString:

        alreadyInList = False
        
        if(len(letterCountList) == 0):
            
            letterCountList = letterCountList + [[char,1]]

        else:

            for nestedList in letterCountList:
                
                if(char == nestedList[0]):
                
                    nestedList[1] = nestedList[1] + 1

                    alreadyInList = True

            if(not alreadyInList):

                letterCountList += [[char, 1]]

    if(len(letterCountList) > 0):
        mostFrequentLetter = letterCountList[0][0]
        mostFrequentLetterOccurrences = letterCountList[0][1]
    else:
        mostFrequentLetter = None
        mostFrequentLetterOccurrences = None

    for nestedList in letterCountList:
        
        char = nestedList[0]
        charOccurrences = nestedList[1]
        
        if(charOccurrences > mostFrequentLetterOccurrences):
            mostFrequentLetter = char
            mostFrequentLetterOccurrences = charOccurrences

        elif(charOccurrences == mostFrequentLetterOccurrences):
            if(char > mostFrequentLetter):
                mostFrequentLetter = char
                mostFrequentLetterOccurrences = charOccurrences
                    
    return (smallestLetter, smallestLetterIndex, thirdSmallestLetter, thirdSmallestLetterIndex, mostFrequentLetter, mostFrequentLetterOccurrences)



def q2(L, goalX, goalY):

    xMin = absoluteValue(goalX - L[0][0])
    yMin = absoluteValue(goalY - L[0][1])

    closestXelement = L[0]
    closestYelement = L[0]
               
    for item in L:

        if(absoluteValue(item[0] - goalX) < xMin):
            xMin = absoluteValue(item[0] - goalX)
            closestXelement = item

        if(absoluteValue(item[1] - goalY) < yMin):
            yMin = absoluteValue(item[1] - goalY)
            closestYelement = item


    if(xMin < yMin):
        closestXY = closestXelement
        XorY = "XMIN"
    else:
        closestXY = closestYelement
        XorY = "YMIN"

    return closestXY, XorY





def absoluteValue(number):
    
    if(number < 0):
        number = number * -1

    return number





def q3(L):

    returnList = []
    sumList = []
    numPositiveLists = 0
    greatestElement = None
    
    for nestedList in L:

        sum = 0
        
        positiveCount = 0
        negativeCount = 0
        
        for element in nestedList:

            sum += element

            if(element > 0):
                positiveCount += 1
            elif(element < 0):
                negativeCount += 1

            if(greatestElement == None):
                greatestElement = element
            elif(element > greatestElement):
                greatestElement = element
                
        sumList += [sum]

        if(positiveCount > negativeCount):
            numPositiveLists += 1

    return [sumList, numPositiveLists, greatestElement]
                          
                          
                          
