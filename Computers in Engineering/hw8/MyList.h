//
// Created by Tucker Dickson on 5/1/21.
//

#ifndef HW8_MYLIST_H
#define HW8_MYLIST_H

#include "Node.h"

class MyList {
public:
    // constructor
    explicit MyList();

    // destructor
    ~MyList();

    // print methods
    void printAscending();
    void printDescending();

    // insert
    void insert(int value);

    // remove
    void remove(int value);

    // clear
    void clear();

private:
    Node * currentPtr;
};


#endif //HW8_MYLIST_H
