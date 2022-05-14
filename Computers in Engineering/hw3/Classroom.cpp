//
// Created by Tucker Dickson on 2/22/21.
//

#include <iostream>
#include <fstream>
#include <vector>
#include "Classroom.h"

using namespace std;

Classroom::Classroom(const vector<Chair> &chairList) :
        chairList{chairList}
{}

void Classroom::setChairList(const vector<Chair> &chairList) {
    this->chairList = chairList;
}

vector<Chair> Classroom::getChairList() const {
    return chairList;
}

//----------------------------------------------------------
// Description: This function parses through the elements
//              of the chairList member vector, and calls
//              the Chair::print() function for each chair
//              in chairList
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void Classroom::print() const {
    for (int i = 0; i < chairList.size(); i++) {
        chairList.at(i).print();
    }
}

//----------------------------------------------------------
// Description: This function opens and reads a "classroom
//              layout" .txt file, and for each chair in
//              the layout file, it creates and appends a
//              new chair object to the chairList member
//              vector. It closes the file once it's
//              reached the end of the file
// Inputs: const string &filename: this string contains the
//              name of the layout .txt file that should be
//              read
// Outputs: None
// ----------------------------------------------------------
void Classroom::buildChairs(const string &filename) {
    filename.c_str();

    ifstream chairLayout(filename);     // read in classroom layout

    if (chairLayout.fail()) {       // check for failure
        cout << "Error: could not open layout file...";
        return;
    }

    // temporary variables to store read-in information
    string row;
    int start;
    int end;
    Chair currChair;
    Student blankStudent;

    chairLayout >> row;     // read in row
    while (!chairLayout.eof()) {
        chairLayout >> start;       // read in first seat
        chairLayout >> end;     // read in last seat

        currChair.setRow(row);      // set row for current chair
        for (int i = start; i <= end; i++) {
            currChair.setRow(row);
            currChair.setNumber(i);     // set number for current chair
            if (i == end) {
                currChair.setRightHanded(false);    // if last chair, set left handed
            }
            else {
                currChair.setRightHanded(true);     // else set right handed
            }
            currChair.setStudent(blankStudent);     // assign blank student

            chairList.push_back(currChair);     // put current chair onto chairList
        }
        chairLayout >> row;
    }
    chairLayout.close();
}

//----------------------------------------------------------
// Description: This function takes a vector of Student
//              objects as a parameter. It parses through
//              the vector, assigning each student in
//              the class list to either a rightHanded
//              vector or a leftHanded vector. It then
//              randomly reorders both vectors. After the
//              vectors are reordered, it assigns each
//              from each list (starting from the back)
//              to a seat corresponding to that student's
//              handedness. If there aren't enough right or
//              left handed seats for all of the students,
//              it prints an error message with the names
//              of the students who weren't seated.
// Inputs: const vector<Student> &classList: a vector of
//              Student objects that contains the first
//              name, last name, and handedness of each
//              student in a class.
// Outputs: None
// ----------------------------------------------------------
void Classroom::assignStudents(const vector<Student> &classList) {
    vector<Student> rightHanded;
    vector<Student> leftHanded;

    for (int i = 0; i < classList.size(); i++) {     // parse through classList
        if (classList.at(i).getRightHanded()) {
            rightHanded.push_back(classList.at(i));     // if right-handed, move to rightHanded
        }
        else {
            leftHanded.push_back(classList.at(i));      // else, move to leftHanded
        }
    }

    // temporary holding variables
    Student temp;
    unsigned long newPosition;

    for (int i = 0; i < rightHanded.size(); i++) {
        // generate new position for Student at position i
        newPosition = rand() % rightHanded.size();

        // assign the attributes of Student at newPosition to a temp student object
        temp.setFirstName(rightHanded.at(newPosition).getFirstName());
        temp.setLastName(rightHanded.at(newPosition).getLastName());

        // assign the attributes of Student at i to newPosition
        rightHanded.at(newPosition).setFirstName(rightHanded.at(i).getFirstName());
        rightHanded.at(newPosition).setLastName(rightHanded.at(i).getLastName());

        // assign the attributes of the Student who was originally at newPosition (now in temp)
        // to Student at i
        rightHanded.at(i).setFirstName(temp.getFirstName());
        rightHanded.at(i).setLastName(temp.getLastName());
    }

    for (int i = 0; i < leftHanded.size(); i++) {
        // generate new position for Student at position i
        newPosition = rand() % leftHanded.size();

        // assign the attributes of Student at newPosition to a temp student object
        temp.setFirstName(leftHanded.at(newPosition).getFirstName());
        temp.setLastName(leftHanded.at(newPosition).getLastName());

        // assign the attributes of Student at i to newPosition
        leftHanded.at(newPosition).setFirstName(leftHanded.at(i).getFirstName());
        leftHanded.at(newPosition).setLastName(leftHanded.at(i).getLastName());

        // assign the attributes of the Student who was originally at newPosition (now in temp)
        // to Student at i
        leftHanded.at(i).setFirstName(temp.getFirstName());
        leftHanded.at(i).setLastName(temp.getLastName());
    }

    int count = 0;
    while (!rightHanded.empty() && count < chairList.size()) {
        if (chairList.at(count).getRightHanded()) {             // if right handed desk...
            chairList.at(count).setStudent(rightHanded.back());     // assign last student in rightHanded to desk
            rightHanded.pop_back();     // remove the last student from rightHanded
            count++;
        }
        else {
            count++;
        }
    }
    if (!rightHanded.empty()) {     // if not enough right handed seats, display error message...
        cout << endl;
        cout << "ERROR: Not enough right handed seats..." << endl;
        cout << "\tRight-handed student(s) without a seat:" << endl;
        for (int i = 0; i < rightHanded.size(); i++) {      // display all students who weren't assigned
            cout << "\t\t" << rightHanded.at(i).getFirstName() << " " << rightHanded.at(i).getLastName() << endl;
        }
        cout << endl;
    }


    count = 0;
    while (!leftHanded.empty() && count < chairList.size()) {
        if (chairList.at(count).getRightHanded() == false) {        // if chair is left handed...
            chairList.at(count).setStudent(leftHanded.back());      // assign last student in leftHanded to chair
            leftHanded.pop_back();      // remove last student from leftHanded
            count++;
        }
        else {
            count++;
        }
    }
    if (!leftHanded.empty()) {      // display error message if not enough chairs
        cout << endl;
        cout << "ERROR: Not enough left handed seats..." << endl;
        cout << "\tLeft-handed student(s) without a seat:" << endl;
        for (int i = 0; i < leftHanded.size(); i++) {
            cout << "\t\t" << leftHanded.at(i).getFirstName() << " " << leftHanded.at(i).getLastName() << endl;
        }
        cout << endl;
    }
}