#include <iostream>
#include "Node.h"
#include "MyList.h"

using namespace std;

int main() {

    cout << endl;
    cout << "*----------------------------------- TESTING NODE CONSTRUCTOR ----------------------------------*" << endl;
    cout << endl;

    // create a node, passing no parameters to constructor
    cout << "\tCreate Node with default parameters:" << endl << endl;
    Node node1;
    cout << "\t\tExpected data: 0" << endl;
    cout << "\t\tActual data: " << node1.getData() << endl << endl;
    cout << "\t\tExpected nextNodePtr: nullptr" << endl;
    cout << "\t\tActual nextNodePtr: ";
    if (node1.getNextNodePtr() == nullptr) {
        cout << "nullptr" << endl << endl;
    }
    cout << "\t\tExpected prevNodePtr: nullptr" << endl;
    cout << "\t\tActual prevNodePtr: ";
    if (node1.getPrevNodePtr() == nullptr) {
        cout << "nullptr" << endl << endl;
    }

    // create a node, passing an argument for data
    cout << "\tCreate Node with, passing 64 to the constructor:" << endl << endl;
    Node node64(64);
    cout << "\t\tExpected data: 64" << endl;
    cout << "\t\tActual data: " << node64.getData() << endl << endl;
    cout << "\t\tExpected nextNodePtr: nullptr" << endl;
    cout << "\t\tActual nextNodePtr: ";
    if (node1.getNextNodePtr() == nullptr) {
        cout << "nullptr" << endl << endl;
    }
    cout << "\t\tExpected prevNodePtr: nullptr" << endl;
    cout << "\t\tActual prevNodePtr: ";
    if (node1.getPrevNodePtr() == nullptr) {
        cout << "nullptr" << endl << endl;
    }

    cout << endl;
    cout << "*---------------------------- TESTING NODE GETTERS, SETTERS & PRINT ----------------------------*" << endl;
    cout << endl;

    cout << "\tFor the node that has a value of 0 for data, set data to 10\n"
            "\t\tthen print the result using Node::getData() and Node::print():" << endl << endl;
    node1.setData(10);
    cout << node1.getData() << endl;
    node1.print();
    cout << endl;

    cout << "\tNow, set the nextNodePtr of the node with data = 10 to point to the node with data = 64.\n"
            "\t\tAlso, set the prevNodePtr of the node with data = 64 to point to the node with data = 10";
            cout << endl << endl;

    cout << "\tUse Node::getNextNodePtr, Node::getPrevNodePtr, and Node::print() to print out the data for both nodes";
    cout << endl << endl;

    node1.setNextNodePtr(&node64);
    node64.setPrevNodePtr(&node1);

    node1.getNextNodePtr()->print();
    node64.getPrevNodePtr()->print();
    cout << endl;

    cout << endl;
    cout << "*----------------------------------- TESTING MyList::insert() ----------------------------------*" << endl;
    cout << endl;

    cout << "\tInsert a node with data = 5 into a list called myList, then print myList:" << endl << endl;
    MyList myList;
    myList.insert(5);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tInsert a node with data = 3 into myList, then print myList:" << endl << endl;
    myList.insert(3);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tInsert a node with data = 7 into myList, then print myList:" << endl << endl;
    myList.insert(7);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tInsert a node with data = 4 into myList, then print myList:" << endl << endl;
    myList.insert(4);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tInsert a node with data = 6 into myList, then print myList:" << endl << endl;
    myList.insert(6);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tInsert a node with data = 1 into myList, then print myList:" << endl << endl;
    myList.insert(1);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tInsert a node with data = 2 into myList, then print myList:" << endl << endl;
    myList.insert(2);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tInsert a node with data = 9 into myList, then print myList:" << endl << endl;
    myList.insert(9);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tInsert a node with data = 8 into myList, then print myList:" << endl << endl;
    myList.insert(8);
    myList.printAscending();
    cout << endl << endl;

    cout << endl;
    cout << "*----------------------------------- TESTING MyList::remove() ----------------------------------*" << endl;
    cout << endl;

    cout << "\tRemove a node with data = 5 from myList, then print myList:" << endl << endl;
    myList.remove(5);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tRemove a node with data = 9 from myList, then print myList:" << endl << endl;
    myList.remove(9);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tRemove a node with data = 1 from myList, then print myList:" << endl << endl;
    myList.remove(1);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tTry to remove a node with data = 1 from myList again, then print list:" << endl << endl;
    myList.remove(1);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tRemove a node with data = 8 from myList, then print myList:" << endl << endl;
    myList.remove(8);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tRemove a node with data = 2 from myList, then print myList:" << endl << endl;
    myList.remove(2);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tRemove a node with data = 6 from myList, then print myList:" << endl << endl;
    myList.remove(6);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tTry to remove a node with data = 10 from myList, then print myList:" << endl << endl;
    myList.remove(10);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tCreate a new list called myList2, then try to remove a value:" << endl << endl;
    MyList myList2;
    myList2.remove(0);
    cout << endl << endl;

    cout << endl;
    cout << "*------------------------------ TESTING MyList::printAscending() -------------------------------*" << endl;
    cout << endl;

    cout << "\tAdd data values 0, 6, 9, and 13 to myList, then print:" << endl << endl;
    myList.insert(0);
    myList.insert(6);
    myList.insert(9);
    myList.insert(13);
    myList.printAscending();
    cout << endl << endl;

    cout << "\tTry to print out myList2:" << endl << endl;
    myList2.printAscending();
    cout << endl;

    cout << "\tAdd data values 1, 2, and 3 to myList2, then print:" << endl << endl;
    myList2.insert(1);
    myList2.insert(2);
    myList2.insert(3);
    myList2.printAscending();
    cout << endl << endl;

    cout << "\tClear the values 1 and 2 from myList2, then print again:" << endl << endl;
    myList2.remove(1);
    myList2.remove(2);
    myList2.printAscending();
    cout << endl;

    cout << "\tClear 3 from myList2, then print again:" << endl << endl;
    myList2.remove(3);
    myList2.printAscending();

    cout << endl;
    cout << "*------------------------------ TESTING MyList::printDescending() ------------------------------*" << endl;
    cout << endl;

    cout << "\tPrint myList in descending order:" << endl << endl;
    myList.printDescending();
    cout << endl;

    cout << "\tAdd a node with data = 8 to myList, then print in descending order:" << endl << endl;
    myList.insert(8);
    myList.printDescending();
    cout << endl;

    cout << "\tTry to print myList2 in descending order:" << endl << endl;
    myList2.printDescending();
    cout << endl;

    cout << "\tAdd one element to myList2, then try to print in descending order:" << endl << endl;
    myList2.insert(1);
    myList2.printDescending();
    cout << endl;

    cout << endl;
    cout << "*----------------------------------- TESTING MyList::clear() -----------------------------------*" << endl;
    cout << endl;

    cout << "\tClear myList, then print out the list:" << endl << endl;
    myList.clear();
    cout << endl;
    myList.printAscending();
    cout << endl;

    cout << "\tTry to clear myList again, without inserting any more values:" << endl << endl;
    myList.clear();
    cout << endl;

    cout << "\tAdd one node to myList, clear the node, then print the list:" << endl << endl;
    myList.insert(7);
    myList.clear();
    cout << endl;
    myList.printAscending();
    cout << endl;

    cout << "\tAdd seven nodes to myList2, clear all the nodes, then print the list:" << endl << endl;
    myList2.insert(83);
    myList2.insert(43);
    myList2.insert(61);
    myList2.insert(59);
    myList2.insert(56);
    myList2.insert(72);
    myList2.insert(19);
    myList2.clear();
    cout << endl;
    myList2.printAscending();
    cout << endl;

    cout << "\tTry to clear myList2 again, without inserting any more values:" << endl << endl;
    myList2.clear();
    cout << endl;

    cout << endl;
    cout << "*----------------------------------- TESTING MyList Destructor ---------------------------------*" << endl;
    cout << endl;

    cout << "\tWhen the program ends, the destructor will be called twice;\n "
            "\t\tonce for myList (which contains seven nodes)\n"
            "\t\tand once again for myList2 (which is empty)";

    myList.insert(49);
    myList.insert(392);
    myList.insert(32);
    myList.insert(034);
    myList.insert(923);
    myList.insert(128);
    myList.insert(53);

    cout << endl << endl;

    return 0;
}
