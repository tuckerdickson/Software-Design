def q1(origString, repeatCount, lettersToRepeat):

    # create an empty string that will eventually be returned
    newString = ""

    # loop through original string, seeing if each individual character is in lettersToRepeat
    for i in origString:
        # if it is, add it to newString repeatCount number of times
        if (i.lower() in lettersToRepeat.lower()):
            newString += (i * repeatCount)
        # otherwise, just add it once
        else:
            newString += i
            
    return newString




                
def q2(num, string1, string2):

    # count will hold the number of times corresponding characters from the two strings don't equal eachother
    count = 0

    # if the strings aren't the same length, return false
    if (len(string1) != len(string2)):
        return False
    # if they are the same length, enter the else
    else:
        # compare each string character-by-character, incrementing count each time they don't line up
        for i in range(0, len(string1)):
            if (string1[i] != string2[i]):
                count += 1

    # if the final count equals num, return True, otherwise return False
    if (count == num):
        return True
    else:
        return False




        
def q3(inputString, minLetter, lettersToIgnore, specialLetter):

    smallestChar = None
    highestIndex = 0
    specialLetterCount = 0

    # iterate through every index in the inputString
    currentIndex = 0
    while currentIndex < len(inputString):
        # reinitialize currentChar to the character at the current index
        currentChar = inputString[currentIndex]

        # check if the current character is greater than minLetter
        if currentChar > minLetter:

            # check if the smallest character so far equals none
            # or if the current character is less than or equal to the smallest character so far
            if (smallestChar == None) or (currentChar <= smallestChar):

                # check to see if the current character isn't in lettersToIgnore
                if (currentChar not in lettersToIgnore):

                    # if all 3 of the above conditions are met, set the smallest character to far to the current character
                    # and set the highest index to the current index
                    smallestChar = currentChar
                    highestIndex = currentIndex

        # if the current character equals the specialLetter, increment the special letter count
        if currentChar == specialLetter:
            specialLetterCount = specialLetterCount + 1

        currentIndex = currentIndex + 1

    # if highestIndex still equals 0 at this point, no smallestChar was found
    # reinitialize highestIndex to None
    if highestIndex == 0:
        highestIndex = None
        
    # if specialLetter occurs and even number of times return False, otherwise it's odd so return true
    if (specialLetterCount % 2 == 0):
        odd = False
    else:
        odd = True

    return smallestChar, highestIndex, odd

            
