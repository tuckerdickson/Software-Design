//
// Created by Tucker Dickson on 2/22/21.
//

#include <iostream>
#include "Chair.h"
using namespace std;

Chair::Chair(const string &row, const int &number, const bool &rightHanded, const Student &student) :
        row{row},
        number{number},
        rightHanded{rightHanded},
        student{student}
{}

void Chair::setRow(const string &row) {
    this->row = row;
}

void Chair::setNumber(const int &number) {
    this->number = number;
}

void Chair::setRightHanded(const bool &rightHanded) {
    this->rightHanded = rightHanded;
}

void Chair::setStudent(const Student &student) {
    this->student = student;
}

string Chair::getRow() const {
    return row;
}

int Chair::getNumber() const {
    return number;
}

bool Chair::getRightHanded() const {
    return rightHanded;
}

Student Chair::getStudent() const {
    return student;
}

//----------------------------------------------------------
// Description: This function prints a chairs row, number,
//              whether it's a right handed or left handed
//              chair, and it's student to the console
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void Chair::print() const {
    cout << "Row: " << row << endl;
    cout << "Number: " << number << endl;
    if (rightHanded) {
        cout << "Right handed" << endl;
    }
    else {
        cout << "Left handed" << endl;
    }
    cout << student.getFirstName() << " " << student.getLastName() << endl << endl;
}


