//
// Created by Tucker Dickson on 5/1/21.
//

#ifndef HW8_NODE_H
#define HW8_NODE_H


class Node {
public:
    // constructor
    explicit Node (const int &data = 0,
                   Node * next = nullptr,
                   Node * prev = nullptr);

    // setters
    void setData (const int &newData);
    void setNextNodePtr (Node * newNextPtr);
    void setPrevNodePtr (Node * newPrevPtr);

    // getters
    int getData() const;
    Node * getNextNodePtr() const;
    Node * getPrevNodePtr() const;

    // print
    void print() const;

private:
    int data;
    Node * nextNodePtr;
    Node * prevNodePtr;
};


#endif //HW8_NODE_H
