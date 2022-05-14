//
// Created by Tucker Dickson on 2/21/21.
//

#include <iostream>
#include "Student.h"
using namespace  std;

Student::Student(const string &firstName, const string &lastName, const bool &rightHanded) :
    firstName{firstName},
    lastName{lastName},
    rightHanded{rightHanded}
{}

void Student::setFirstName (const string &firstName) {
    this->firstName = firstName;
}

void Student::setLastName(const string &lastName) {
    this->lastName = lastName;
}

void Student::setRightHanded(const bool &rightHanded) {
    this->rightHanded = rightHanded;
}

string Student::getFirstName() const {
    return firstName;
}

string Student::getLastName() const {
    return lastName;
}

bool Student::getRightHanded() const {
    return rightHanded;
}

//----------------------------------------------------------
// Description: This function prints a students first name,
//              last name, and dominant hand to the console
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void Student::print() const {
    cout << "Student name: " << firstName << " " << lastName << endl;
    if (rightHanded) {
        cout << "Right handed" << endl << endl;
    }
    else {
        cout << "Left handed" << endl << endl;
    }
}