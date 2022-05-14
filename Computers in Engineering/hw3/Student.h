//
// Created by Tucker Dickson on 2/21/21.
//

#ifndef HW3_STUDENT_H
#define HW3_STUDENT_H

#include <string>
using namespace std;

class Student {
public:
    explicit Student (const string &firstName = "NO",
                      const string &lastName = "NAME",
                      const bool &rightHanded = true);
    void setFirstName (const string &firstName);
    void setLastName (const string &lastName);
    void setRightHanded (const bool &rightHanded);
    string getFirstName() const;
    string getLastName() const;
    bool getRightHanded() const;
    void print() const;
private:
    string firstName;
    string lastName;
    bool rightHanded;
};

#endif //HW3_STUDENT_H
