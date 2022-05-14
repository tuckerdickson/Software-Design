def hawkID():
    return("ntdickson") 

class Box:

    def __init__(self, centerX = 0.0, centerY = 0.0, centerZ = 0.0, width = 1.0, height = 1.0, depth = 1.0):
       self.centerX = centerX
       self.centerY = centerY
       self.centerZ = centerZ
       self.width = width
       self.height = height
       self.depth = depth

    def setCenter(self, x, y, z):
        self.centerX = x
        self.centerY = y
        self.centerZ = z

    def setWidth(self, width):
        self.width = width
        
    def setHeight(self, height):
        self.height = height
        
    def setDepth(self, depth):
        self.depth = depth
        
    def volume(self):
        return(self.width * self.height * self.depth)
    
    def surfaceArea(self):
        return((2 * self.width * self.height) + (2 * self.width * self.depth) + (2 * self.height * self.depth))

    def touches(self, otherBox):
        selfUpper = self.centerY + (self.height / 2)
        selfLower = self.centerY - (self.height / 2)
        selfRight = self.centerX + (self.width / 2)
        selfLeft = self.centerX - (self.width / 2)
        selfDeep = self.centerZ + (self.depth / 2)
        selfShallow = self.centerZ - (self.depth / 2)

        otherUpper = otherBox.centerY + (otherBox.height / 2)
        otherLower = otherBox.centerY - (otherBox.height / 2)
        otherRight = otherBox.centerX + (otherBox.width / 2)
        otherLeft = otherBox.centerX - (otherBox.width / 2)
        otherDeep = otherBox.centerZ + (otherBox.depth / 2)
        otherShallow = otherBox.centerZ - (otherBox.depth / 2)

        if((selfRight < otherLeft) or (selfLeft > otherRight)):
           return False

        if((selfUpper < otherLower) or (selfLower > otherUpper)):
           return False

        if((selfDeep < otherShallow) or (selfShallow > otherDeep)):
            return False
        
        return True

    def contains(self, otherBox):
        selfUpper = self.centerY + (self.height / 2)
        selfLower = self.centerY - (self.height / 2)
        selfRight = self.centerX + (self.width / 2)
        selfLeft = self.centerX - (self.width / 2)
        selfDeep = self.centerZ + (self.depth / 2)
        selfShallow = self.centerZ - (self.depth / 2)

        otherUpper = otherBox.centerY + (otherBox.height / 2)
        otherLower = otherBox.centerY - (otherBox.height / 2)
        otherRight = otherBox.centerX + (otherBox.width / 2)
        otherLeft = otherBox.centerX - (otherBox.width / 2)
        otherDeep = otherBox.centerZ + (otherBox.depth / 2)
        otherShallow = otherBox.centerZ - (otherBox.depth / 2)

        if((selfLeft < otherLeft) and (selfRight > otherRight)):
            if((selfLower < otherLower) and (selfUpper > otherUpper)):
                if((selfShallow < otherShallow) and (selfDeep > otherDeep)):
                    return True;
        return False;
    
    def __repr__(self):
        return f"< {self.width:g}-by-{self.height:g}-by-{self.depth:g} 3D box with center at ({self.centerX}, {self.centerY}, {self.centerZ}) >"





class NimGame:

    def __init__(self, heaps):
        isValid = True
        
        if len(heaps) > 0:
           for ball in heaps:
               if not ball > 0:
                   isValid = False
        else:
            isValid = False

        if isValid:
            self.heaps = heaps
            print("Nim game initialized with {} heaps".format(len(heaps)))
        else:
            print("Invalid parameters. Try again.")

    def __repr__(self):
        
        returnString = "< Nim game with {} heaps.".format(len(self.heaps))
        
        for number in range(0, len(self.heaps)):
            returnString = returnString + "\n\tHeap {}: {} balls".format(number, self.heaps[number])
            
        returnString = returnString + "\n>"
        
        return returnString

    def remove(self, heap, numToRemove):

        if not type(heap) == int:
            print("Your input of {} is invalid. Please try again.".format(heap))
            return

        # if no ValueError is thrown, make sure heap is between 0 and the final index of self.heaps
        if not (heap < len(self.heaps) and (heap > -1)):
            print("There is no heap {}. Please try again.".format(heap))
            return
        

        if not type(numToRemove) == int:
            print("Your input of {} is invalid. Please try again.".format(numToRemove))
            return

        # make sure numToRemove is between 1 and the number corresponding to the specific heap
        if not ((numToRemove <= self.heaps[heap]) and (numToRemove > 0)):
            print("You can't take {} balls from heap {}. Please try again.".format(numToRemove, heap))
            return
        

        # we should have valid input at this point; modify the heap as requested by the user
        self.heaps[heap] = self.heaps[heap] - numToRemove
        print("\nYou took {} balls from heap {}.".format(numToRemove, heap))

        if self.gameOver():
            print("You win!")
            return
        
        # computer's turn
        positiveHeapIndices = []
        
        for heap in range(0, len(self.heaps)):
            if  self.heaps[heap] > 0:
                positiveHeapIndices.append(heap)

        if len(positiveHeapIndices) == 1:
            print("Computer took {} balls from heap {}.".format(self.heaps[positiveHeapIndices[0]], positiveHeapIndices[0]))
            print("Computer wins!")
            self.heaps[positiveHeapIndices[0]] = 0
            return
            
        if len(positiveHeapIndices) > 1:
            for index in positiveHeapIndices:
                if self.heaps[index] > 1:
                    
                    numBalls = self.heaps[index] -1
                    self.heaps[index] = 1
                    print("Computer took {} balls from heap {}.".format(numBalls, index))
                    return

        self.heaps[positiveHeapIndices[0]] = 0
        print("Computer took {} balls from heap {}.".format(1, self.heaps[positiveHeapIndices[0]]))
            

    def gameOver(self):
        gameOver = True
        
        for heap in self.heaps:
            if not heap == 0:
                gameOver = False

        return gameOver

        

class Animal ():
    
    numAnimals = 0

    def __init__ (self, name = 'NoName', numLegs = 0):
        self.name = name
        self.numLegs = numLegs
        Animal.numAnimals = Animal.numAnimals + 1
        self.id = Animal.numAnimals

    def setName(self, name):
        self.name = name
        
    def getName(self):
        return self.name
    
    def getNumLegs(self):
        return self.numLegs
   
    def speak(self):
        print("...")

    def __repr__(self):
        return ('<{} the animal. ID:{}>'.format(self.name, self.id))

class Cat(Animal):
    def __init__(self, name = 'noname', furColor = 'unknown'):
        Animal.__init__(self, name, 4)
        self.color = furColor
    
    def speak(self):
        print('meow')

    def getFurColor(self):
        return self.color

    def __repr__(self):
        return ('<{} the {} cat. ID: {}>'.format(self.name, self.color, self.id))

class Dog(Animal):
    
    def __init__(self, name = 'rover'):
        Animal.__init__(self, name, 4)
    
    def speak(self):
        print('woof')
        
    def fetch(self):
        print("I'm fetching ...")

    def __repr__(self):
        return '<{} the dog. ID:{}>'.format(self.name, self.id)

class Bird(Animal):

    def __init__(self, name = "bubba", typeOfBird = "pigeon", flightless = True):
        Animal.__init__(self, name, 2)

        self.typeOfBird = typeOfBird
        self.flightless = flightless

    def speak(self):
        print("tweet-tweet")

    def canFly(self):
        if self.flightless:
            return("No, the {} cannot fly.".format(self.typeOfBird))
        else:
            return("Yes, the {} can fly.".format(self.typeOfBird))

    def __repr__(self):
        return "<{} the {}. ID:{}>".format(self.name, self.typeOfBird, self.id)
        
def testAnimal():
    
    c1 = Cat("Milo")
    c2 = Cat(furColor = "black")
    d1 = Dog()
    d2 = Dog()
    b1 = Bird()
    b2 = Bird("Zazu", "penguin", True)
    
    for animal in [c1, c2, d1, d2, b1, b2]:
        print(animal)
        animal.speak()
    d1.fetch()
    print(c2.getFurColor())
    print(b2.canFly())
            
