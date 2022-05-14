//
// Created by Tucker Dickson on 5/1/21.
//

#include "MyList.h"
#include "Node.h"
#include <iostream>

using namespace std;

// constructor
MyList::MyList() {
    currentPtr = nullptr;
}

// destructor
MyList::~MyList() {
    this->clear();
}

//----------------------------------------------------------
// Description: This function prints out the data values of
//              a doubly-linked list in ascending order
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void MyList::printAscending() {

    // if the list is empty, let the user know
    if (currentPtr == nullptr) {
        cout << "This list is empty..." << endl;
        return;
    }

    // move the pointer to the start of the list
    while (currentPtr->getPrevNodePtr() != nullptr) {
        currentPtr = currentPtr->getPrevNodePtr();
    }

    // print out the first data value before entering the loop
    cout << currentPtr->getData() << " ";

    // print out the rest of the data values until the end of the list is reached
    while (currentPtr->getNextNodePtr() != nullptr) {
        currentPtr = currentPtr->getNextNodePtr();
        cout << currentPtr->getData() << " ";
    }
}

//----------------------------------------------------------
// Description: This function prints out the data values of
//              a doubly-linked list in descending order
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void MyList::printDescending() {

    // if the list is empty, let the user know
    if (currentPtr == nullptr) {
        cout << "This list is empty..." << endl;
        return;
    }

    // if the list only contains one element, print that element
    if (currentPtr->getPrevNodePtr() == nullptr && currentPtr->getNextNodePtr() == nullptr) {
        cout << currentPtr->getData() << endl;
        return;
    }

    // move current pointer to the end of the list
    while (currentPtr->getNextNodePtr() != nullptr) {
        currentPtr = currentPtr->getNextNodePtr();
    }

    // iterate through the list in reverse order, printing out each element as you go
    while (currentPtr->getPrevNodePtr() != nullptr) {
        cout << currentPtr->getData() << " ";
        currentPtr = currentPtr->getPrevNodePtr();
    }
        cout << endl;
}

//----------------------------------------------------------
// Description: This function inserts a value into a
//              doubly-linked list (in ascending order)
// Inputs: int value
//         this is the numerical value to be inserted
// Outputs: None
// ----------------------------------------------------------
void MyList::insert(int value) {

    // dynamically allocate new node so that it can exist beyond the scope of this function
    // assign the new node's data the value passed
    Node * newNode = new Node;
    newNode->setData(value);

    // make sure memory was successfully allocated for newNode
    if (newNode == nullptr) {
        cout << "Error: Memory could not be allocated for a new Node..." << endl;
        return;
    }

    // if currentPtr doesn't point to anything (empty list), insert node
    if (currentPtr == nullptr) {
        currentPtr = newNode;
        return;
    }

    // move currentPtr to the first node in the list
    while (currentPtr->getPrevNodePtr() != nullptr) {
        currentPtr = currentPtr->getPrevNodePtr();
    }

    // move currentPtr down the list until the end is reached or we find the spot to insert
    while (currentPtr->getNextNodePtr() != nullptr && currentPtr->getData() < value) {
        currentPtr = currentPtr->getNextNodePtr();
    }

    // if currentPtr is at the beginning of the list, insert the new node at the start
    if (currentPtr->getPrevNodePtr() == nullptr) {
        if (value < currentPtr->getData()) {
            newNode->setNextNodePtr(currentPtr);
            currentPtr->setPrevNodePtr(newNode);
            return;
        }
    }

    // if currentPtr is at the end of the list and if value is greater than currentPtr->getData(),
    // insert new node at the end of the list
    if (currentPtr->getNextNodePtr() == nullptr) {
        if (value > currentPtr->getData()) {
            newNode->setPrevNodePtr(currentPtr);
            currentPtr->setNextNodePtr(newNode);
            return;
        }
    }

    // if this point is reached, the new node needs inserted somewhere in the middle
    // so insert new node in the middle
    newNode->setNextNodePtr(currentPtr);
    newNode->setPrevNodePtr(currentPtr->getPrevNodePtr());

    currentPtr->getPrevNodePtr()->setNextNodePtr(newNode);
    currentPtr->setPrevNodePtr(newNode);

}

//----------------------------------------------------------
// Description: This function removes a value from a
//              doubly-linked list
// Inputs: int value
//         this is the numerical value to be removed
// Outputs: None
// ----------------------------------------------------------
void MyList::remove(int value) {

    Node * temp;

    // if the list is empty, let the user know
    if (currentPtr == nullptr) {
        cout << "This list is empty..." << endl;
        return;
    }

    // if there's only one element in the list, and if value matches its data, delete it
    else if (currentPtr->getPrevNodePtr() == nullptr && currentPtr->getNextNodePtr() == nullptr) {
        if (currentPtr->getData() == value) {
            delete currentPtr;
            currentPtr = nullptr;
            return;
        }
    }

    // move currentPtr to the start of the list
    while (currentPtr->getPrevNodePtr() != nullptr) {
        currentPtr = currentPtr->getPrevNodePtr();
    }

    // iterate through the list, stopping once either value is encountered or the end is reached
    while (currentPtr->getNextNodePtr() != nullptr && currentPtr->getData() != value) {
        currentPtr = currentPtr->getNextNodePtr();
    }

    // if the value isn't in the list, let the user know
    if (currentPtr->getNextNodePtr() == nullptr && currentPtr->getData() != value) {
        cout << "No node exists with data = " << value << endl;
        return;
    }

    // if currentPtr is at the start of the list, remove it from the list and delete
    if (currentPtr->getPrevNodePtr() == nullptr) {
        currentPtr->getNextNodePtr()->setPrevNodePtr(nullptr);
        temp = currentPtr->getNextNodePtr();
        delete currentPtr;
        currentPtr = temp;
        return;
    }

    // if currentPtr is at the end of the list, remove it from the list and delete
    if (currentPtr->getNextNodePtr() == nullptr) {
        currentPtr->getPrevNodePtr()->setNextNodePtr(nullptr);
        temp = currentPtr->getPrevNodePtr();
        delete currentPtr;
        currentPtr = temp;
        return;
    }

    // if we're here, currentPtr is somewhere in the middle...
    currentPtr->getPrevNodePtr()->setNextNodePtr(currentPtr->getNextNodePtr());
    currentPtr->getNextNodePtr()->setPrevNodePtr(currentPtr->getPrevNodePtr());
    temp = currentPtr->getNextNodePtr();
    delete currentPtr;
    currentPtr = temp;
}

//----------------------------------------------------------
// Description: This function removes all nodes from a
//              linked list, printing out each one to the
//              console as it goes
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void MyList::clear() {

    Node * temp;

    // if the list is empty, let the user know
    if (currentPtr == nullptr) {
        cout << "This list is empty..." << endl << endl;
        return;
    }

    // move currentPtr to the start of the list
    while (currentPtr->getPrevNodePtr() != nullptr) {
        currentPtr = currentPtr->getPrevNodePtr();
    }

    // iterate through the list, deleting each node as you go
    while (currentPtr != nullptr) {
        cout << "Deleting Node with data = " << currentPtr->getData() << "..." << endl;
        temp = currentPtr->getNextNodePtr();
        delete currentPtr;
        currentPtr = temp;
    }
}