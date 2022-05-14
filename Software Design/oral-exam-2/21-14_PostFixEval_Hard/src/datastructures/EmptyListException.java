// Fig. 21.4: EmptyListException.java
// Class EmptyListException declaration.
package datastructures;

/**
 * This class is responsible for handing exceptions that arise when invalid inputs are entered by the user. This package
 * was taken from "Java: How to Program, 11th Edition" (Deitel). This example can be found at
 * textbook_examples_11th_edition.ch21.fig21_12.src.
 */
public class EmptyListException extends RuntimeException {

    /**
     * This is the first of two constructors in this class. This is the no-argument constructor, so it takes no
     * arguments. It simply makes a call to the other (one-argument) constructor, passing it the String "List".
     */
    public EmptyListException() {
        this("List"); // call other EmptyListException constructor
    }

    /**
     * This is the second of the two constructors in this class. It takes one String as an argument and makes a call
     * to the superclass (RuntimeException) constructor, passing it a message.
     * @param name
     */
    public EmptyListException(String name) {
        super(name + " is empty"); // call superclass constructor
    }
} // end class EmptyListException

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
