import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * This class represents the Delivery Truck node. Its primary responsibility is to receive orders from the Shipping Dock
 * node, and print out a summary of the order in groups of four, with a brief pause between orders. This class implements
 * the Runnable interface, which declares one method, run().
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/12/2021
 */
public class Truck implements Runnable {
    /**
     * This private int represents the number of this Truck.
     */
    private int truckNum;

    /**
     * This private String array refers to the current order being processed.
     */
    private String[] orderData;

    /**
     * This private int refers to the input buffer (shared with a Shipping Dock node).
     */
    private SynchronizedBuffer inputBuffer;

    /**
     * This private ArrayList containing String arrays will be used to collect orders four at a time to print out
     */
    private ArrayList<String[]> database = new ArrayList<String[]>();

    /**
     * This SecureRandom object will be used to generate a random integer between 0 and 10000 to determine sleep time.
     */
    private static final SecureRandom generator = new SecureRandom();

    /**
     * This is the sole constructor for the Truck class. It takes in two parameters, an int and a SynchronizedBuffer, and
     * uses them to initialize truckNum and inputBuffer, respectively.
     *
     * @param truckNum This int will be used to initialize truckNum.
     * @param inputBuffer This SynchronizedBuffer will be used to initialize inputBuffer.
     */
    public Truck(int truckNum, SynchronizedBuffer inputBuffer) {
        this.truckNum = truckNum;
        this.inputBuffer = inputBuffer;
    }

    /**
     * This method is declared in the Runnable interface. Its main purpose is to get orders from the input buffer and
     * pass them to the Print.printInfo() method to be printed.
     */
    @Override
    public void run() {

        // initialize length to be 3; count to be 0 so that we can enter both while loops below
        int length = 3;
        int count = 0;

        // keep looping while length is greater than two
        while(length > 2) {
            // keep looping while count is less than four
            while (count < 4) {

                // try to get an order from the input buffer; reinitialize length and increment count
                try {
                    orderData = inputBuffer.blockingGet();
                    length = orderData.length;
                    count += 1;

                    // create a new String array that is a copy of the order array, plus one index tacked on the end
                    String[] newOrderData = Arrays.copyOf(orderData, orderData.length + 1);

                    // initialize the new index to sectionNum
                    newOrderData[newOrderData.length-1] = Integer.toString(truckNum);

                    // if the length of the modified order is less than seven, it's the eof flag
                    // set count to five to break out of the inner while loop after the current iteration
                    if(newOrderData.length < 7) {
                        count = 5;
                    }

                    // add the order to the database
                    database.add(newOrderData);

                } catch (InterruptedException interruptedException) {
                    Thread.currentThread().interrupt();
                }
            }

            // at this point, we either have four orders in the database, or less than four orders if the eof flag was
            // encountered
            // either way, we want to pass all of those to the Print.printInfo() method
            for(String[] order : database) {
                // first, sleep for 0-10 seconds
                try {
                    Thread.sleep(generator.nextInt(10000));
                } catch (InterruptedException interruptedException) {
                    interruptedException.printStackTrace();
                }

                // print the order
                Print.printInfo(order);
            }

            // clear the database and  set count to zero before either (a) beginning the next iteration of the outer loop
            // or (b) breaking the outer loop (if the eof flag was encountered)
            database.clear();
            count = 0;
        }
    }
}
