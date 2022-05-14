import java.security.SecureRandom;
import java.util.Arrays;

/**
 * This class represents the Section nodes. It responsible for receiving orders from the Shipping Center node, appending
 * the Section number to the order, and then sending the order to the Shipping Dock node, after a brief pause. This
 * class implements the Runnable interface, which defines one method, run().
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/12/2021
 */
public class Section implements Runnable {

    /**
     * This private double represents the section number of the specific Section object. This double follows the
     * convention: <Shipping Center Number> . <Section Number>
     */
    private int sectionNum;

    /**
     * This private array of Strings will be used to refer to individual orders when getting from the input buffer
     * and putting to the output buffer.
     */
    private String[]orderData;

    /**
     * This private SynchronizedBuffer will refer to the input buffer, the buffer between a Shipping Center node
     * and this Section node.
     */
    private final SynchronizedBuffer inputBuffer;

    /**
     * This private SynchronizedBuffer will refer to the output buffer, the buffer between this Section node and a
     * Shipping Dock node.
     */
    private final SynchronizedBuffer outputBuffer;

    /**
     * This private SecureRandom object will be used to generate a random number, used to determine the wait time before
     * putting and order to the output buffer.
     */
    private static final SecureRandom generator = new SecureRandom();

    /**
     * This is the sole constructor for the Section class. It takes in three parameters, a double and two
     * SynchronizedBuffers, and uses them to initialize sectionNum, inputBuffer, and outputBuffer, respectively.
     *
     * @param sectionNum This double will be used to initialize sectionNum.
     * @param inputBuffer This SynchronizedBuffer will be used to initialize inputBuffer.
     * @param outputBuffer This SynchronizedBuffer will be used to initialize outputBuffer.
     */
    public Section(int sectionNum, SynchronizedBuffer inputBuffer, SynchronizedBuffer outputBuffer) {
        this.sectionNum = sectionNum;
        this.inputBuffer = inputBuffer;
        this.outputBuffer = outputBuffer;
    }

    /**
     * This method is declared in the Runnable interface. It is responsible for getting orders from the input buffer
     * (shared with Shipping Center nodes), appending the Section number to the order, waiting between 0 and 5 seconds
     * (random), and then putting the new order to the output buffer (shared with Shipping Dock nodes).
     */
    @Override
    public void run() {
        // initialize length to two so we can enter while loop
        int length = 2;

        // while the length of the order is greater than one, keep looping
        while(length > 1) {

            // try to get an order from the input buffer; reinitialize length to the length of the order array
            try {
                orderData = inputBuffer.blockingGet();
                length = orderData.length;

                // create a new String array that is a copy of the order array, plus one index tacked on the end
                String[] newOrderData = Arrays.copyOf(orderData, orderData.length + 1);

                // initialize the new index to sectionNum
                newOrderData[newOrderData.length-1] = Integer.toString(sectionNum);

                // try to put the modified order in the output buffer
                try {
                    Thread.sleep(generator.nextInt(5000));
                    outputBuffer.blockingPut(newOrderData);
                } catch(InterruptedException interruptedException) {
                    Thread.currentThread().interrupt();
                }

            } catch (InterruptedException interruptedException) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
