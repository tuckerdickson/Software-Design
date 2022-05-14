import java.util.Arrays;

/**
 * This class represents the Shipping Center nodes. Its primary responsibility is to get order information from its
 * buffer shared with the Web Server, and then put that line into one of two output buffers (depending on the item
 * category), to be picked up by the Section node. This class implements the Runnable interface, which defines one
 * method, run().
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/12/2021
 */
public class ShippingCenter implements Runnable {
    /**
     * This private int represents the number of the Shipping Center. This variable is set by the
     * constructor. For this assignment, it will always be either 1 or 2.
     */
    private int scNum;

    /**
     * This private array of Strings represents an individual order being processed. This variable will be used when
     * getting the order from the Web Services shared buffer and when putting the order in the Section shared buffer.
     */
    private String[] shippingCenterData;

    /**
     * This private SynchronizedBuffer represents a Shipping Center's sole input buffer (shared with Web Services).
     * It will be used to receive orders from Web Services.
     */
    private final SynchronizedBuffer inputBuffer;

    /**
     * This private SynchronizedBuffer represents one the output buffers. All orders with categories beginning with
     * A-P will be placed in this buffer, to be received by Section 1.
     */
    private final SynchronizedBuffer outputBuffer1;

    /**
     * This private SynchronizedBuffer represents one the output buffers. All orders with categories beginning with
     * Q-Z will be placed in this buffer, to be received by Section 2.
     */
    private final SynchronizedBuffer outputBuffer2;

    /**
     * This is the sole constructor the the ShippingCenter class. It takes in three parameters: an int and three
     * SynchronizedBuffers. The int is used to initialize scNum, and the three SynchronizedBuffers are used to initialize
     * inputBuffer, outputBuffer1, and outputBuffer2, respectively.
     *
     * @param scNum This int will be used to initialize scNum.
     * @param inputBuffer This SynchronizedBuffer will be used to initialize inputBuffer.
     * @param outputBuffer1 This SynchronizedBuffer will be used to initialize outputBuffer1.
     * @param outputBuffer2 This SynchronizedBuffer will be used to initialize outputBuffer2.
     */
    public ShippingCenter(int scNum, SynchronizedBuffer inputBuffer,
                          SynchronizedBuffer outputBuffer1, SynchronizedBuffer outputBuffer2) {
        this.scNum = scNum;
        this.inputBuffer = inputBuffer;
        this.outputBuffer1 = outputBuffer1;
        this.outputBuffer2 = outputBuffer2;
    }

    /**
     * This method is declared in the Runnable interface. It is responsible for getting orders from the Web Server
     * shared buffer, and then passing those order to one of two Sections, depending on the first letter of the
     * item category.
     */
    @Override
    public void run() {

        // initialize length to 1 so that we can enter the while loop
        int length = 1;

        // keep looping while the length of the input array is greater than 0
        while(length > 0) {

            // try to get an order from the input buffer; set length to the length of the order
            try {
                shippingCenterData = inputBuffer.blockingGet();
                length = shippingCenterData.length;

                // create a new String array that is a copy of the order array, plus one index tacked on the end
                String[] newOrderData = Arrays.copyOf(shippingCenterData, shippingCenterData.length + 1);

                // initialize the new index to sectionNum
                newOrderData[newOrderData.length-1] = Integer.toString(scNum);

                // if the length of the order is greater than zero, proceed
                if(length > 0) {

                    // if the first letter of the item category is between A and P, try to put the order in the
                    // section 1 buffer
                    if((newOrderData[6].charAt(0) >= 'A' && newOrderData[6].charAt(0) <= 'P') ||
                            (newOrderData[6].charAt(0) >= 'a' && newOrderData[6].charAt(0) <= 'p')) {
                        try {
                            outputBuffer1.blockingPut(newOrderData);
                        } catch (InterruptedException interruptedException) {
                            Thread.currentThread().interrupt();
                        }
                    }

                    // otherwise, try to put the order in the section 2 buffer
                    else {
                        try {
                            outputBuffer2.blockingPut(newOrderData);
                        } catch (InterruptedException interruptedException) {
                            Thread.currentThread().interrupt();
                        }
                    }
                }
                else {
                    // when the end-of-file flag is read in, length gets set to zero, so all of the above code is skipped over
                    // try to pass this zero-length array onto each of the sections
                    try {
                        outputBuffer1.blockingPut(newOrderData);
                        outputBuffer2.blockingPut(newOrderData);
                    } catch (InterruptedException interruptedException) {
                        Thread.currentThread().interrupt();
                    }
                }
            } catch (InterruptedException interruptedException) {
                Thread.currentThread().interrupt();
            }
        }
    }
}
