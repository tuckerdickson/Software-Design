// Fig. 21.12: StackComposition.java
// StackComposition uses a composed List object.
package datastructures;

/**
 * This class is responsible for constructing the stack objects that will be used in our algorithm. This package
 * was taken from "Java: How to Program, 11th Edition" (Deitel). This example can be found at
 * textbook_examples_11th_edition.ch21.fig21_12.src.
 */
public class StackComposition<T> {

    /**
     * This private List of T objects represents the actual stack that we will be pushing to and popping from.
     */
    private List<T> stackList;

    /**
     * This is the sole constructor for the StackComposition class. It doesn't take any parameters, and it simply
     * initializes stackList to be a new List.
     */
    public StackComposition() {
        stackList = new List<T>("stack");
    }

    /**
     * This method is responsible for adding objects to  our stack. It takes in one T object as a parameter, and then
     * passes that T to the List.insertAtFront method.
     *
     * @param object This method takes one T object as a parameter. This is the object that will be pushed onto the
     *               stack.
     */
    public void push(T object) {
        stackList.insertAtFront(object);
    }

    // remove object from stack
    public T pop() throws EmptyListException {
        return stackList.removeFromFront();
    }

    // determine if stack is empty
    public boolean isEmpty() {
        return stackList.isEmpty();
    }

    // output stack contents
    public void print() {
        stackList.print();
    }
} // end class StackComposition


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
