/**
 * This class is responsible for printing out a summary of an individual order at the end of the program, or that
 * a delivery truck has delivered its final order if the end of file flag is detected. It has no private instance
 * variables, and it contains one method, printInfo().
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/12/2021
 */
public class Print {

    /**
     * This is the sole method in the Print class. It is responsible for printing a summary of an order, or that a
     * delivery truck has stopped delivering when the end of file flag is detected. This is a static method, meaning
     * that it can be called without creating a Print object. It is also synchronized, meaning that only one thread can
     * access it at a time, ensuring that the console output isn't jumbled.
     *
     * @param order This array of Strings refers to either an order or the end-of-file flag.
     */
    public synchronized static void printInfo(String[] order) {
        // if the length of order is greater than three, order refers to an actual order, so print it out
        if (order.length > 3) {
            System.out.println("Address: " + order[0]);
            System.out.println("\t" + order[1] + ", " + order[2]);
            System.out.println("\t" + order[3]);

            System.out.println("Name: " + order[4]);
            System.out.println("Item: " + order[5]);
            System.out.println("Category: " + order[6]);
            System.out.println("Shipping Center: " + order[7]);
            System.out.println("\tSection: " + order[8]);
            System.out.println("\tTruck: " + order[9]);
            System.out.println();
            System.out.println();
        }
        // otherwise, order refers to the end-of-file flag, meaning there are no orders left to deliver
        else {
            System.out.println("Truck " + order[2] + " of Shipping Center " + order[0] + " has delivered its " +
                    "final order.");
            System.out.println();
        }
    }
}
