/**
 * This class represents the shipping dock node. It is responsible for receiving orders from the section nodes, and then
 * sending those orders to one of two delivery trucks, depending on which is available. This class implements the
 * Runnable interface, which declares one method, run().
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/12/2021
 */
public class ShippingDock implements Runnable {

    /**
     * This private String array will be used to refer to the orders being received from the Section node.
     */
    private String[] orderData;

    /**
     * This private SynchronizedBuffer will be used to refer to the input buffer (shared with a Section node).
     */
    private SynchronizedBuffer inputBuffer;

    /**
     * This private SynchronizedBuffer will be used to refer to one of the output buffers (shared with a Truck node).
     */
    private SynchronizedBuffer outputBuffer1;

    /**
     * This private SynchronizedBuffer will be used to refer to the other output buffer (shared with a Truck node).
     */
    private SynchronizedBuffer outputBuffer2;

    /**
     * This is the sole constructor for the ShippingDock class. It takes in three SynchronizedBuffers as parameters
     * and uses them to initialize inputBuffer, outputBuffer1, and outputBuffer2, respectively.
     *
     * @param inputBuffer This SynchronizedBuffer will be used to initialize inputBuffer.
     * @param outputBuffer1 This SynchronizedBuffer will be used to initialize outputBuffer1.
     * @param outputBuffer2 This SynchronizedBuffer will be used to initialize outputBuffer2.
     */
    public ShippingDock(SynchronizedBuffer inputBuffer,
                        SynchronizedBuffer outputBuffer1, SynchronizedBuffer outputBuffer2) {
        this.inputBuffer = inputBuffer;
        this.outputBuffer1 = outputBuffer1;
        this.outputBuffer2 = outputBuffer2;
    }

    /**
     * This method is declared in the Runnable interface. Its main purpose is to get orders from the input buffer, and
     * send those orders to the first available output buffer. If both are available, outputBuffer1 will be prioritized.
     */
    @Override
    public void run() {
        // initialize length to three so we can enter the while loop
        int length = 3;
        // since there are two different threads feeding into this buffer, we need to keep track of the number of eof
        // flags
        int eofCount = 0;

        // keep looping while length is greater than 2
        while(eofCount < 2) {
            // try to get an order from the input buffer; use it to reinitialize length
            try {
                orderData = inputBuffer.blockingGet();
                length = orderData.length;

                // if length is greater than two try to put the order in the first available output buffer
                if(length > 2) {

                    // keep looping while both output buffers are full
                    while(outputBuffer1.isOccupied() && outputBuffer2.isOccupied()) {
                        // do nothing
                    }

                    // if outputBuffer1 has free space, try to put the order in
                    if(!outputBuffer1.isOccupied()) {
                        try {
                            outputBuffer1.blockingPut(orderData);
                        } catch(InterruptedException interruptedException) {
                            Thread.currentThread().interrupt();
                        }
                    }
                    // if outputBuffer2 has free space, try to put the order in
                    else if(!outputBuffer2.isOccupied()) {
                        try {
                            outputBuffer2.blockingPut(orderData);
                        } catch(InterruptedException interruptedException) {
                            Thread.currentThread().interrupt();
                        }
                    }
                }
                // if the length-zero end-of-file flag is read in, the above if block will get skipped over and we'll break out
                // of the while
                else {
                    // increment the eof flag count
                    eofCount += 1;
                    // if this is the second eof flag, try to pass the eof flag onto the Shipping Dock node
                    try {
                        if(eofCount == 2) {
                            outputBuffer1.blockingPut(orderData);
                            outputBuffer2.blockingPut(orderData);
                        }
                    } catch (InterruptedException interruptedException) {
                        Thread.currentThread().interrupt();
                    }
                }
            } catch (InterruptedException interruptedException) {
                interruptedException.printStackTrace();
            }
        }
    }
}
