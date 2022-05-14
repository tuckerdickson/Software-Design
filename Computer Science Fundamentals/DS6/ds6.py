# CS1210 Fall 2021, Discussion section assignment 6
#
# Complete both functions below and submit this file to the
# DS6 assignment item in the lecture ICON.
#
# ********** BOTH FUNCTIONS MUST BE RECURSIVE! **********

# Q1. Complete recursive function replaceVowels. Given
#
# - inputString, a string of zero or more lower case letters
# - replacement, a string of zero or more characters
#
# return a new string in which every vowel of inputString has been
# replaced with replacement.
#
# For this problem consider vowels to be a, e, i, o, and u.
#
# For example:
# >>> replaceVowels("hello", "z")
# "hzllz"
# >>> replaceVowels("hi", "aa")
# "haa"
# >>> replaceVowels("goodbye", "")
# "gdby"
#
def replaceVowels (inputString, replacement):
    vowels = ['a', 'e', 'i', 'o', 'u']
    result = ""

    if len(inputString) == 0:
        return ""
    
    if inputString[0] not in vowels:
        result = result + inputString[0]
    else:
        result = result + replacement
    
    return result + replaceVowels(inputString[1:], replacement)

# Q2. Given a list, print all the "contents" of the lists.  Here, "content"
# means all non-list items with in the list (maybe deeply nested within
# the list.
#
# For example,
# >>> printContents([1,2,'a'])
# 1
# 2
# a
#
# >>> printContents([1, [], 'a', [[[[3,4]]]], [[5],99]])
# 1
# a
# 3
# 4
# 5
# 99
#
# The function must be recursive but may also contain a loop.
#
# Solution outline:
# Iterate over the items of the list:
#   If an item is not a list, print it.
#   Otherwise, since recursively call this function on the item.
#
def printContents(inputList):
    for item in inputList:
        if type(item) != list:
            print(item)
        else:
            printContents(item)
    return



    

 
