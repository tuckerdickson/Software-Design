def hawkID():
    return("ntdickson") 



def q1(n):
    
    if n == 1:
        return [1]
    else:
        return  q1(n-1) + [n**2] + q1(n-1)




def q2(n, listOfStrings):

    if len(listOfStrings) == 0:
        return 0
    elif len(listOfStrings) == 1:
        if len(listOfStrings[0]) < n:
            return 1
        else:
            return 0

    if(len(listOfStrings[-1]) < n):
        add = 1
    else:
        add = 0

    return add + q2(n, listOfStrings[:-1])





def q3(item1, item2):

    type1 = type(item1)
    type2 = type(item2)

    returnVal = True

    if type1 != type2:
        if((type1 not in [int, float]) or (type2 not in [int, float])):
            returnVal = False

    if (type1 == list) and (type2 == list):
        if len(item1) != len(item2):
            returnVal = False

        for index in range(0,len(item1)):
            returnVal = q3(item1[index], item2[index])
            if returnVal == False:
                return False

    return returnVal
            
            

