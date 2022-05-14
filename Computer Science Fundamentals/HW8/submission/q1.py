def hawkID():
    return("ntdickson") 

def q1(numerator, denominator):

    c = 2                   # the variable that represents whole-number denominators; start with 2
    denoms = []       # list that holds all of the egyptian fractions
    printString = f"{numerator}/{denominator} = "

    # we haven't found all of the egyptian fractions until n/d -1/c == 0
    while (numerator*c - denominator) != 0:

        # we only want fractions that are less than n/d (ie: n/d > 1/c  ---> nc - d > 0)
        if (numerator*c - denominator) > 0:

            # adjust the numerator and denominator (subtract off 1/c
            numerator = numerator*c - denominator
            denominator = denominator*c

            # append c to our list of denominators
            denoms.append(c)

        # increment c
        c += 1

    # we will only break out of the while loop if n/d - 1/c == 0, meaning that 1/c is the last egyptian fraction
    # add c to denoms list
    denoms.append(c)

    for d in denoms:
        printString = printString + f"1/{d} + "

    print(printString[0 : len(printString) - 2])
    return denoms
