import tkinter
import random

def hawkID():
    return("ntdickson")

class StateVars:
    def __init__(self):
        self.maxVal = 999
        self.num1 = 0
        self.num2 = 0
        self.answer = 0
        self.numIncorrect = 0
        self.questionHistory = []

        # end of game stats
        self.numberSolved = 0
        self.incorrectGuessesList = []
        

    def setNums(self, num1, num2, answer):
        self.num1 = num1
        self.num2 = num2
        self.answer = answer

    def setIncorrectNum(self, numIncorrect):
        self.numIncorrect = numIncorrect

    def setNumberSolved(self, number):
        self.numberSolved = number

    def setIncorrectGuesses(self, number):
        self.incorrectGuesses = number

    def addToHistory(self, questionList):
        self.questionHistory.append(questionList)
        
    def addToIncorrectGuesses(self, numGuesses):
        self.incorrectGuessesList.append(numGuesses)

    def problemInHistory(self, questionList):
        if questionList in self.questionHistory:
            return True
        else:
            self.addToHistory(questionList)
            return False


def generateProblem():

    answerEntryField.configure(state = "normal")
    answerEntryField.delete(0, "end")
    
    num1 = random.randrange(0, stateVars.maxVal)
    num2 = random.randrange(0, stateVars.maxVal)
    
    if operation.get() == "+":
        op = "+"
        addition(num1, num2)
        
    elif operation.get() == "-":
        op = "-"
        subtraction(num1, num2)
        
    elif operation.get() == "*":
        op = "*"
        multiplication(num1, num2)
        
    elif operation.get() == "/":
        op = "/"
        division(num1, num2)
        
    else:
        op = randomOperation(num1, num2)

    displayProblem(op)

def displayProblem(op):
    problemLabel.configure(text = f"{stateVars.num1} {op} {stateVars.num2} = ")

def addition(num1, num2):
    while stateVars.problemInHistory([num1, "+", num2]):
        num1 = random.randrange(0,stateVars.maxVal)
        num2 = random.randrange(0,stateVars.maxVal)
    answer = num1 + num2
    stateVars.setNums(num1, num2, answer)

def subtraction(num1, num2):
    if num1 > num2:
        maxNum = num1
        minNum = num2
    else:
        minNum = num1
        maxNum = num2

    while stateVars.problemInHistory([maxNum, "-", minNum]):
        num1 = random.randrange(0,stateVars.maxVal)
        num2 = random.randrange(0,stateVars.maxVal)
        
        if num1 > num2:
            maxNum = num1
            minNum = num2
        else:
            minNum = num1
            maxNum = num2
        
    answer =  maxNum - minNum
    stateVars.setNums(maxNum, minNum, answer)

def multiplication(num1, num2):
    num1 = num1 % 100
    num2 = num2 % 100

    while stateVars.problemInHistory([num1, "*", num2]):
        num1 = random.randrange(0, stateVars.maxVal % 100)
        num2 = random.randrange(0, stateVars.maxVal % 100)
        
    answer = num1 * num2
    stateVars.setNums(num1, num2, answer)
    
def division(num1, num2):
    while(num2 == 0) or (num1 * num2 > 999) or (stateVars.problemInHistory([num1*num2, "/", num2])):
        num1 = random.randrange(0,stateVars.maxVal)
        num2 = random.randrange(0,stateVars.maxVal)
    answer = num1
    num1 = answer * num2
    stateVars.setNums(num1, num2, answer)

def randomOperation(num1, num2):
    indicator = random.randrange(0, 4)
    if indicator == 0:
        addition(num1, num2)
        return "+"
    
    elif indicator == 1:
        subtraction(num1, num2)
        return "-"
    
    elif indicator == 2:
        multiplication( num1, num2)
        return "*"
    
    elif indicator == 3:
        division(num1, num2)
        return "/"

def checkAnswer():
    userAnswer = int(answerEntryField.get())
    if userAnswer == stateVars.answer:
        message = "That's the correct answer!"
        answerEntryField.configure(state = "disabled")
        stateVars.addToIncorrectGuesses(stateVars.numIncorrect+1)
        stateVars.setIncorrectNum(0)
        numIncorrectLabel.configure(text = "")
        stateVars.setNumberSolved(stateVars.numberSolved + 1)
    else:
        message = "That's incorrect. Try again."
        stateVars.setIncorrectNum(stateVars.numIncorrect + 1)
        numIncorrectLabel.configure(text = f"Number of incorrect answers: {stateVars.numIncorrect}")

    correctnessLabel.configure(text = message)

def closeWindow():
    sumGuesses = 0
    for num in stateVars.incorrectGuessesList:
        sumGuesses += num

    averageGuessesPerSolved = sumGuesses / stateVars.numberSolved
    
    print(f"Number of problems attempted: {len(stateVars.questionHistory)}")
    print(f"Number of problems solved: {str(stateVars.numberSolved)}")
    print(f"Average number of total guesses per solved problem: {averageGuessesPerSolved}")
    mainWindow.destroy()
    

# window, widget and layout specifications
mainWindow = tkinter.Tk()

# first frame
topFrame = tkinter.Frame(mainWindow)

operation = tkinter.StringVar()
rb1 = tkinter.Radiobutton(topFrame, text = "+", variable = operation, value = "+")
rb2 = tkinter.Radiobutton(topFrame, text = "-", variable = operation, value = "-")
rb3 = tkinter.Radiobutton(topFrame, text = "*", variable = operation, value = "*")
rb4 = tkinter.Radiobutton(topFrame, text = "/", variable = operation, value = "/")
rb5 = tkinter.Radiobutton(topFrame, text = "Any", variable = operation, value = "Any")

rb1.pack(side = "left")
rb2.pack(side = "left")
rb3.pack(side = "left")
rb4.pack(side = "left")
rb5.pack(side = "left")

rb1.deselect()
rb2.deselect()
rb3.deselect()
rb4.deselect()
rb5.select()

topFrame.pack(side = "top")

generateProblemButton = tkinter.Button(mainWindow, text = "Generate new problem", command = generateProblem)
generateProblemButton.pack(side = "top")

problemFrame = tkinter.Frame(mainWindow)

problemLabel = tkinter.Label(problemFrame, text = "The problem goes here")
problemLabel.pack(side = "left")

answerEntryField = tkinter.Entry(problemFrame)
answerEntryField.pack(side = "right")

problemFrame.pack(side = "top")

checkAnswerButton = tkinter.Button(mainWindow, text = "Check answer", command = checkAnswer)
checkAnswerButton.pack(side = "top")

correctnessLabel = tkinter.Label(mainWindow)
correctnessLabel.pack(side = "top")

numIncorrectLabel = tkinter.Label(mainWindow)
numIncorrectLabel.pack(side = "top")

quitButton = tkinter.Button(mainWindow, text = "Quit", command = closeWindow)
quitButton.pack(side = "top")

stateVars = StateVars()
mainWindow.mainloop()
