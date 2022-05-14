// Fig. 21.3: List.java
// ListNode and List class declarations.
package datastructures;

/**
 * This class represents the individual nodes in a list. This package was taken from "Java: How to Program,
 * 11th Edition" (Deitel). This example can be found at textbook_examples_11th_edition.ch21.fig21_12.src.
 */
class ListNode<T> {
    // package access members; List can access these directly

    /**
     * This public T object represents the data stored in a node. This is the actual information that we will be
     * interested in accessing in our program. This member variable is public so that objects of the List class are able
     * to directly access it.
     */
    T data;

    /**
     * This public ListNode object refers to the next node in the list. If this node is the last node in the list,
     * nextNode will be null. This member variable is public so that objects of the List class are able to directly
     * access it.
     */
    ListNode<T> nextNode;

    /**
     * This is one of two constructors for the ListNode class. This constructor takes in one argument, an object of type
     * T, and passes it to the two-argument constructor as data along with null for nextNode.
     *
     * @param object This T object represents the data of the new node being created.
     */
    ListNode(T object) {
        this(object, null);
    }

    /**
     * This is the second of the two constructors for this class. This constructor takes in two arguments, an object of
     * type T and a ListNode object, and uses them to initialize data and nextNode, respectively.
     *
     * @param object This T object represents the data of the new node being created, and will be used to initialize data.
     * @param node This ListNode object refers to the next node in the list, and will be used to initialize nextNode.
     *            If the new node being added is the last node in the list, node will be null.
     */
    ListNode(T object, ListNode<T> node) {
        data = object;
        nextNode = node;
    }

    /**
     * This method acts as the getter method for the data instance variable. It doesn't take any parameters and it returns
     * data.
     *
     * @return This method returns an object of type T representing data.
     */
    T getData() {
        return data;
    }

    /**
     * This method acts as the getter method for the nextNode instance variable. It doesn't take any parameters and it returns
     * nextNode.
     *
     * @return This method returns a ListNode object representing nextNode.
     */
    ListNode<T> getNext() {
        return nextNode;
    }
} // end class ListNode<T>

/**
 * This class represents a list data structure. This package was taken from "Java: How to Program,
 * 11th Edition" (Deitel). This example can be found at textbook_examples_11th_edition.ch21.fig21_12.src.
 */
public class List<T> {

    /**
     * This private ListNode object refers to the first node in the list.
     */
    private ListNode<T> firstNode;

    /**
     * This private ListNode object refers to the last node in a list.
     */
    private ListNode<T> lastNode;

    /**
     * This private String represents the name of the list; used for printing.
     */
    private String name;

    // constructor creates empty List with "list" as the name

    /**
     * This is the first of two constructors in the List class. It is a no-argument constructor which simply creates an
     * empty list with "list" as the name. It does this by calling the one-argument constructor and passing it "list".
     */
    public List() {
        this("list");
    }

    /**
     * This is the second of two constructors in the List class. It takes in one String as an argument and uses it to
     * create an empty list with the given name.
     *
     * @param listName This String is used to initialize the name of the List being created.
     */
    public List(String listName) {
        name = listName;
        firstNode = lastNode = null;
    }

    /**
     * This method is responsible for inserting a new node at the front of the list with the data passed in. If the list
     * is empty, it simply creates a new node and assigns firstNode and lastNode to it. Otherwise, it creates a new node,
     * passing in firstNode as the nextNode, and then assigns firstNode to the new node.
     *
     * @param insertItem This object of type T represents the data that will be contained in the new node.
     */
    public void insertAtFront(T insertItem) {
        if (isEmpty()) // firstNode and lastNode refer to same object
            firstNode = lastNode = new ListNode<T>(insertItem);
        else // firstNode refers to new node
            firstNode = new ListNode<T>(insertItem, firstNode);
    }

    /**
     * This method is responsible for inserting a new node at the back of the list with the data passed in. If the list
     * is empty, it simply creates a new node and assigns firstNode and lastNode to it. Otherwise, it creates a new node,
     * (by calling the one-argument constructor, meaning the nextNode will be null), and assigns the new node to lastNode
     * and lastNode's nextNode.
     *
     * @param insertItem This object of type T represents the data that will be contained in the new node.
     */
    public void insertAtBack(T insertItem) {
        if (isEmpty()) // firstNode and lastNode refer to same object
            firstNode = lastNode = new ListNode<T>(insertItem);
        else // lastNode's nextNode refers to new node
            lastNode = lastNode.nextNode = new ListNode<T>(insertItem);
    }

    /**
     * This method is responsible for removing the fist node from the front of the list. If the list is empty, it throws
     * an EmptyListException, passing in the name of the list to the constructor. Otherwise it captures the data from
     * the first node in a variable, then updates the firstNode to be firstNode's nextNode (or null if there is only
     * one node in the list). It then returns the data of the removed node.
     *
     * @return This method returns an object of type T, representing the data of the node removed from the list.
     * @throws EmptyListException This method throws an EmptyListException if the list is already empty.
     */
    public T removeFromFront() throws EmptyListException {
        if (isEmpty()) // throw exception if List is empty
            throw new EmptyListException(name);

        T removedItem = firstNode.data; // retrieve data being removed

        // update references firstNode and lastNode
        if (firstNode == lastNode)
            firstNode = lastNode = null;
        else
            firstNode = firstNode.nextNode;

        return removedItem; // return removed node data
    } // end method removeFromFront

    /**
     * This method is responsible for removing the last node from the end of the list. If the list is empty, it throws
     * an EmptyListException, passing in the name of the list to the constructor. Otherwise it captures the data from
     * the last node in a variable, then updates the lastNode to be the node prior to lastNode (or null if there is only
     * one node in the list). It then returns the data of the removed node.
     *
     * @return This method returns an object of type T, representing the data of the node removed from the list.
     * @throws EmptyListException This method throws an EmptyListException if the list is already empty.
     */
    public T removeFromBack() throws EmptyListException {
        if (isEmpty()) // throw exception if List is empty
            throw new EmptyListException(name);

        T removedItem = lastNode.data; // retrieve data being removed

        // update references firstNode and lastNode
        if (firstNode == lastNode)
            firstNode = lastNode = null;
        else // locate new last node
        {
            ListNode<T> current = firstNode;

            // loop while current node does not refer to lastNode
            while (current.nextNode != lastNode)
                current = current.nextNode;

            lastNode = current; // current is new lastNode
            current.nextNode = null;
        }

        return removedItem; // return removed node data
    }

    /**
     * This method is responsible for determining whether this list is currently empty. It does this by checking to see
     * if the firstNode is null.
     *
     * @return This method returns a boolean value indicating whether the list is currently empty (true if it is).
     */
    public boolean isEmpty() {
        return firstNode == null; // return true if list is empty
    }

    /**
     * This method is responsible for printing out the contents of the List. If the list is empty, it prints out a message
     * saying so. Otherwise, it prints out the data for each of the elements in the list.
     */
    public void print() {
        if (isEmpty()) {
            System.out.printf("Empty %s%n", name);
            return;
        }

        System.out.printf("The %s is: ", name);
        ListNode<T> current = firstNode;

        // while not at end of list, output current node's data
        while (current != null) {
            System.out.printf("%s ", current.data);
            current = current.nextNode;
        }

        System.out.println();
    }
} // end class List<T>

/**************************************************************************
 * (C) Copyright 1992-2014 by Deitel & Associates, Inc. and               *
 * Pearson Education, Inc. All Rights Reserved.                           *
 *                                                                        *
 * DISCLAIMER: The authors and publisher of this book have used their     *
 * best efforts in preparing the book. These efforts include the          *
 * development, research, and testing of the theories and programs        *
 * to determine their effectiveness. The authors and publisher make       *
 * no warranty of any kind, expressed or implied, with regard to these    *
 * programs or to the documentation contained in these books. The authors *
 * and publisher shall not be liable in any event for incidental or       *
 * consequential damages in connection with, or arising out of, the       *
 * furnishing, performance, or use of these programs.                     *
 *************************************************************************/
