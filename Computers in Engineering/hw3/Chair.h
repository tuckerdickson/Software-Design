//
// Created by Tucker Dickson on 2/22/21.
//

#ifndef HW3_CHAIR_H
#define HW3_CHAIR_H

#include <string>
#include "Student.h"
using namespace std;

class Chair {
public:
    explicit Chair (const string &row = "No row",
                    const int &number = 0,
                    const bool &rightHanded = true,
                    const Student &student = Student());
    void setRow(const string &row);
    void setNumber(const int &number);
    void setRightHanded(const bool &rightHanded);
    void setStudent(const Student &student);
    string getRow() const;
    int getNumber() const;
    bool getRightHanded() const;
    Student getStudent() const;
    void print() const;
private:
    string row;
    int number;
    bool rightHanded;
    Student student;
};


#endif //HW3_CHAIR_H
