//
// Created by Tucker Dickson on 5/1/21.
//

#include <iostream>
#include "Node.h"

using namespace std;

// Constructor
Node::Node(const int &data, Node *next, Node *prev) :
    data{data},
    nextNodePtr{next},
    prevNodePtr{prev}
{}

// Setters
void Node::setData(const int &newData) {
    data = newData;
}

void Node::setNextNodePtr(Node *newNextPtr) {
    nextNodePtr = newNextPtr;
}

void Node::setPrevNodePtr(Node *newPrevPtr) {
    prevNodePtr = newPrevPtr;
}

// Getters
int Node::getData() const {
    return data;
}

Node * Node::getNextNodePtr() const {
    return nextNodePtr;
}

Node * Node::getPrevNodePtr() const {
    return prevNodePtr;
}

//----------------------------------------------------------
// Description: This function simply prints the data member
//              of a single node
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void Node::print() const {
    cout << this->data << endl;
}