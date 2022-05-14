//
// Created by Tucker Dickson on 2/22/21.
//

#ifndef HW3_CLASSROOM_H
#define HW3_CLASSROOM_H

#include <vector>
#include "Chair.h"
using namespace std;

class Classroom {
public:
    explicit Classroom (const vector<Chair> &chairList = {});
    void setChairList (const vector<Chair> &chairList);
    vector<Chair> getChairList() const;
    void print() const;
    void buildChairs (const string &filename);
    void assignStudents (const vector<Student> &classList);
private:
    vector<Chair> chairList;
};


#endif //HW3_CLASSROOM_H
