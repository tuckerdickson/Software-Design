
#
# HawkID function
#
def hawkID():
    return("ntdickson")


def computeTripData(distanceK, vehSpeedMPS, vehKPL, gasCostPerLiter, breakfastCostPerDay, lunchCostPerDay, dinnerCostPerDay, hotelCostPerNight):
    # calculate the length of the trip in hours
    distanceInMeters = distanceK * 1000
    vehSpeedMPH = vehSpeedMPS * 3600
    lengthInHours = distanceInMeters / vehSpeedMPH

    # calculate the cost of gas for the trip
    gasCostPerKilometer = gasCostPerLiter / vehKPL
    totalGasCost = gasCostPerKilometer * distanceK

    # calculate the number of hotel nights and the total cost of hotels
    numHotelNights = computeHotelNights(lengthInHours)
    costOfHotels = numHotelNights * hotelCostPerNight

    # calculate the number of breakfasts
    numBreakfasts = computeNumBreakfasts(lengthInHours)
    costOfBreakfasts = numBreakfasts * breakfastCostPerDay

    # calculate the number of lunches
    numLunches = computeNumLunches(lengthInHours)
    costOfLunches = numLunches * lunchCostPerDay

    # calculate the number of dinners
    numDinners = computeNumDinners(lengthInHours)
    costOfDinners = numDinners * dinnerCostPerDay

    # calculate the total cost of the trip
    totalCost = format(totalGasCost + costOfHotels + costOfBreakfasts + costOfLunches + costOfDinners, '0.2f')

    return lengthInHours, totalGasCost, totalCost, numBreakfasts, numLunches, numDinners, numHotelNights


def computeHotelNights(lengthOfTrip):
    # calculate the total days of driving
    numHotelNights = lengthOfTrip // 8;

    # take into account rest days
    numRestDays = lengthOfTrip // 40
    numHotelNights = numHotelNights + numRestDays

    # if the length of the trip is a multiple of 8, subtract one night
    if(lengthOfTrip % 8 == 0):
        numHotelNights = numHotelNights - 1

    # if the length of the trip is a multiple of 40, subtract another
    if(lengthOfTrip % 40 == 0):
        numHotelNights = numHotelNights - 1

    return int(numHotelNights)


def computeNumBreakfasts(lengthOfTrip):
    # number of breakfasts will usually be the integer division of hours and 8
    numBreakfasts = lengthOfTrip // 8;

    # except when the hours are a multiple of 8
    # need to subtract 1 to account for no breakfast on the first day
    if(lengthOfTrip % 8 == 0):
        numBreakfasts = numBreakfasts - 1
        
    return int(numBreakfasts)


def computeNumLunches(lengthOfTrip):
    # first account for the number of full days
    numLunches = lengthOfTrip // 8

    # if the last day is more than 4 hours, add a lunch
    hoursLeftover = lengthOfTrip % 8
    if(hoursLeftover > 4.0):
        numLunches = numLunches + 1

    return int(numLunches)


def computeNumDinners(lengthOfTrip):
    # first account for the number of full days
    numDinners = lengthOfTrip // 8
    
    # if the length of the trip is a multiple of 8, subtract one dinner
    if(lengthOfTrip % 8 == 0):
        numDinners = numDinners - 1

    return int(numDinners)


def printTripSummary(vehName, distanceM, vehSpeedMPH, vehMPG, gasCostPerGallon, breakfastCostPerDay, lunchCostPerDay, dinnerCostPerDay, hotelCostPerNight):
    # compute the distance in kilometers
    distanceInKilometers = distanceM * 1.609344

    # compute the speed in meters/second
    vehSpeedMilesPerSecond = vehSpeedMPH / 3600.0
    vehSpeedMetersPerSecond = vehSpeedMilesPerSecond * 1609.344

    # compute the car's fuel efficiency in kilometers/liter
    vehMilesPerLiter = vehMPG / 3.785411784
    vehKilometersPerLiter = vehMilesPerLiter * 1.609344
        
    # compute the cost of gas per liter
    gasCostPerLiter = gasCostPerGallon / 3.785411784

    lengthInHours, totalGasCost, totalCost, numBreakfasts, numLunches, numDinners, numHotelNights = computeTripData(distanceInKilometers, vehSpeedMetersPerSecond, vehKilometersPerLiter, gasCostPerLiter, breakfastCostPerDay, lunchCostPerDay, dinnerCostPerDay, hotelCostPerNight)

    # print and return
    print(vehName + " trip of " + str(distanceM) + " miles. Hotel Nights: " + str(numHotelNights) + ", Total cost: $" + str(totalCost))
    return vehName + " trip of " + str(distanceM) + " miles. Hotel Nights: " + str(numHotelNights) + ", Total cost: $" + str(totalCost)


