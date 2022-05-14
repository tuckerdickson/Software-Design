#include <iostream>
#include <fstream>
#include <vector>
#include <ctime>
#include "Student.h"
#include "Chair.h"
#include "Classroom.h"

using namespace std;

//----------------------------------------------------------
// Description: This function parses through a vector of
//              type Student and calls the print() member
//              function for each student
// Inputs: vector<Student> classList: a vector that contains
//              an object of type Student for each student
//              in the class
// Outputs: None
// ----------------------------------------------------------
void printClassList (vector<Student> classList) {
    for (int i = 0; i < classList.size(); i++) {
        classList.at(i).print();
    }
}

int main() {

    /*--------------------- Part 1: Create and Display a Class List ---------------------*/

    // initialize student vector
    vector<Student> classList;

    // read in student data
    ifstream studentData("student.txt");

    // check for failure
    if (studentData.fail()) {
        cout << "Error: could not open student.txt...";
        return -1;
    }

    // temporary variables to store read-in data
    int count = 0;
    string first;
    string last;
    char hand;
    Student student;

    studentData >> first;       // read in first name
    while (!studentData.eof()) {
        if (!studentData.fail()) {
            student.setFirstName(first);        // set first name of student
        }

        studentData >> last;        // read in last name
        if (!studentData.fail()) {
            student.setLastName(last);      // set last name of student
        }

        studentData >> hand;        // read in handedness
        if (!studentData.fail()) {
            if (hand == 'R') {
                student.setRightHanded(true);       // set right handed
            }
            else {
                student.setRightHanded(false);      // set left handed
            }
        }

        classList.push_back(student);       // add student to classList
        count++;

        studentData >> first;
    }

    studentData.close();

    cout << endl << "*----------------------------------------------------------------------*" << endl;
    cout <<         "|                              CLASS LIST                              |" << endl;
    cout <<         "*----------------------------------------------------------------------*" << endl << endl;
    printClassList(classList);

    /*--------------------- Part 2: Create and Display First Classroom ---------------------*/

    srand(time(0)); // seed random number generator

    Classroom room1;
    string filename = "layoutCBW128.txt";

    room1.buildChairs(filename);
    room1.assignStudents(classList);

    cout << endl << "*------------------------------------------------------------------------*" << endl;
    cout <<         "|                              CLASSROOM #1                              |" << endl;
    cout <<         "*------------------------------------------------------------------------*" << endl << endl;
    room1.print();

    /*--------------------- Part 3: Create and Display Second Classroom ---------------------*/

    Classroom room2;
    filename = "layoutEPB109.txt";

    room2.buildChairs(filename);
    room2.assignStudents(classList);

    cout << endl << "*------------------------------------------------------------------------*" << endl;
    cout <<         "|                              CLASSROOM #2                              |" << endl;
    cout <<         "*------------------------------------------------------------------------*" << endl << endl;
    room2.print();

    /*--------------------- Part 4: Create and Display Third Classroom ---------------------*/

    Classroom room3;
    filename = "layoutPHAR100A.txt";

    room3.buildChairs(filename);
    room3.assignStudents(classList);

    cout << endl << "*------------------------------------------------------------------------*" << endl;
    cout <<         "|                              CLASSROOM #3                              |" << endl;
    cout <<         "*------------------------------------------------------------------------*" << endl << endl;
    room3.print();

    return 0;
}
