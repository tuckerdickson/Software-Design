import datastructures.EmptyListException;
import datastructures.StackComposition;

import javax.naming.OperationNotSupportedException;
import java.util.ArrayList;
import java.util.Scanner;

/**
 * This class serves as the driver class for this program. It contains the main method, as well as the
 * evaluatePostfixExpression method and the calculate method. This class does the heavy lifting of this program; it is
 * responsible for processing the user's input and deciding how to interact with the stack based on the input.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/24/2021
 */
public class PostfixEvaluator {

    /**
     * This method is responsible for breaking apart the StringBuffer containing the user input and deciding how to
     * process the information inside. It accomplishes this by separating the StringBuffer at spaces between characters,
     * inserting those chunks into an ArrayList, and then looping over the ArrayList, pushing onto the stack or calling
     * the calculate method as fit. It takes in one parameter, a StringBuffer object containing user input and returns
     * a double representing the result.
     *
     * @param buffer This StringBuffer object contains the user-input postfix expression
     * @return This method returns a double representing the result of the postfix expression
     */
    public static double evaluatePostfixExpression(StringBuffer buffer) {
        // create a new ArrayList to hold all of the individual operations/numbers from the StringBuffer
        ArrayList<String> list = new ArrayList<>();

        // append the flag to the StringBuffer
        buffer.append(" )");

        // initialize a marker variable to mark the new starting index when calling substring
        int marker = 0;

        // loop over all of the characters in the StringBuffer, looking for spaces
        for (int i = 0; i < buffer.length(); i++) {
            if(buffer.charAt(i) == ' ') {
                // everytime a space is encountered, add a substring from index marker to index i to the ArrayList
                list.add(buffer.substring(marker, i));

                // reset marker
                marker = i + 1;
            }
        }

        // create a new StackComposition object (that holds Doubles)
        StackComposition<Double> stack = new StackComposition<>();

        // iterate over all of the Strings in the ArrayList
        for(String str : list) {
            // try to convert the current String to a double
            // if successful, push the new Double onto the stack
            try {
                Double value = Double.parseDouble(str);
                stack.push(value);

            // if the current string can't be converted to a double, it is probably an operator
            // try pushing the result returned from a call to the calculate method onto the stack
            } catch (NumberFormatException nfe) {
                try {
                    stack.push(calculate(str, stack));

                // this catch block will catch many of the exceptions thrown as a result of incorrect notation
                // ie: 56 +
                } catch (EmptyListException ele) {
                    System.out.println("ERROR: Invalid input.");
                    return -999;

                // this catch block will catch any characters that the program isn't designed to handle
                } catch (OperationNotSupportedException onse) {
                    System.out.println("ERROR: Operation not supported.");
                    return -999;
                }

            }
        }
        // at this point, the result should be the first and only element in the stack; pop and return
        return stack.pop();
    }

    /**
     * This function is responsible for doing most of the interaction with the stack, and thus preforming most of the
     * calculations in the program. It takes in two parameters, a String representing an operator, and a
     * StackComposition object representing the stack we're using.
     *
     * @param operation This String represents an operation (+-/*^%) in the user-input postfix expression.
     * @param stack This StackComposition object represents the stack that we're pushing onto/popping from.
     * @return This method returns a double representing the result of a single computation.
     *
     * @throws OperationNotSupportedException This method throws an OperationNotSupportedException whenever it is passed
     *                                          an operation that it doesn't recognize.
     */
    public static double calculate(String operation, StackComposition<Double> stack) throws OperationNotSupportedException {
        // if addition, add the top two elements of the stack and return
        if (operation.equals("+")) {
            return (stack.pop() + stack.pop());
        }

        // if subtraction, subtract the second element from the first element and return.
        else if (operation.equals("-")) {
            double subtractor = stack.pop();
            double subtracted = stack.pop();
            return subtracted - subtractor;
        }

        // if multiplication, multiply the top two elements and return.
        else if (operation.equals("*")) {
            return (stack.pop() * stack.pop());
        }

        // if division, divide the second element by the top element and return.
        else if (operation.equals("/")) {
            double divisor = stack.pop();
            double dividend = stack.pop();
            return dividend / divisor;
        }

        // if exponentiation, raise the first element to the second element and return.
        else if (operation.equals("^")) {
            double base = stack.pop();
            double exponent = stack.pop();
            return Math.pow(exponent, base);
        }

        // if modulus, mod the top element by the second element and return.
        else if (operation.equals("%")) {
            double modder = stack.pop();
            double modded = stack.pop();
            return modded % modder;
        }

        // otherwise the operation is unknown, throw and exception.
        else {
            throw new OperationNotSupportedException();
        }
    }

    /**
     * This method acts as the driver for this program. It is responsible for obtaining input from the user, and then
     * passing that input to the evaluatePostfixExpression method and printing the result returned.
     *
     * @param args This is the default argument for all main methods in Java.
     */
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        String input = "";

        System.out.println("*_______________________*");
        System.out.println("|\tPostfix Evaluator\t|");
        System.out.println("*-----------------------*");
        System.out.println("\n");
        System.out.println("\tNOTE: When entering postfix expressions, leave a space between any operators and numerical values.");
        System.out.println("\t\tFor example: 12 + 5\t--->\t12 5 +");

        // break loop if input is -1
        while(!input.equals("-1")) {

            // read in postfix expression; pass it to evaluatePostfixExpression
            StringBuffer buffer = new StringBuffer("");
            System.out.print("\nPlease enter a postfix expression (or -1 to exit): ");
            input = scanner.nextLine();
            buffer.append(input);

            double result = evaluatePostfixExpression(buffer);

            // print result
            System.out.println("\tResult: " + result);
        }
    }
}
