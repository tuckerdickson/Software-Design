import java.util.concurrent.ArrayBlockingQueue;

/**
 * This class is responsible for implementing the shared buffer objects that will be used as a means
 * to pass information between the various nodes in the program.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/12/2021
 */
public class SynchronizedBuffer {

    /**
     * This private ArrayBlockingQueue object represents the buffer that two nodes will use to communicate.
     */
    private ArrayBlockingQueue<String[]> buffer;

    /**
     * This private boolean indicates whether this buffer is currently occupied (it contains a value).
     */
    private boolean occupied;

    /**
     * This is the sole constructor for the SynchronizedBuffer class. It doesn't take any parameters, and it initializes
     * buffer to a new ArrayBlockingQueue that takes arrays of Strings (with a capacity of one) and occupied to false.
     */
    public SynchronizedBuffer() {
        buffer = new ArrayBlockingQueue<String[]>(1);
        occupied = false;
    }

    /**
     * This method will be used by a "producer" node to send an order into a shared buffer.
     *
     * @param data This array of Strings refers to an individual order being put into a shared buffer.
     * @throws InterruptedException This exception will be thrown in the case that a thread is interrupted.
     */
    public void blockingPut(String[] data) throws InterruptedException {
        occupied = true;
        buffer.put(data);
    }

    /**
     * This method will be ud to a "consumer" node to receive an order from a shared buffer.
     *
     * @return This method returns an array of Strings referring to an individual order.
     * @throws InterruptedException This exception will be thrown in the case that a thread is interrupted
     */
    public String[] blockingGet() throws InterruptedException {
        String[] read = buffer.take();
        occupied = false;
        return read;
    }

    /**
     * This method checks to see if this buffer is "occupied" (i.e. it contains an object). If it is, the method returns
     * true, otherwise it returns false.
     *
     * @return This method returns a boolean value indicating whether a buffer is occupied at the moment.
     */
    public boolean isOccupied() {
        return occupied;
    }

}
