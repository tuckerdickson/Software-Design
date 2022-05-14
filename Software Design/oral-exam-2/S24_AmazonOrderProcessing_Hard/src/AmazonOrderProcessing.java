import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * This class serves as the driver class for the AmazonOrderProcessing program. It doesn't contain any private instance
 * variables, and it contains one method, main. The primary purposes of this class are to create Synchronized buffer
 * objects which link the various nodes throughout the program, and call the ExecutorService.execute() method, which
 * then calls the run() method within the specified class.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/11/2021
 */
public class AmazonOrderProcessing {

    /**
     * This is the sole method in the AmazonOrderProcessing class. Its only purposes are to create SynchronizedBuffer
     * objects which link the various nodes throughout the program, and to call ExecutorService.execute(), which in
     * turn calls the run() method in the specified object.
     *
     * @param args This is the required argument for all main functions.
     */
    public static void main(String[] args) {

        // these buffers will link the Amazon Web Server node to the two Shipping center nodes
        SynchronizedBuffer webServicesOutputBuffer1 = new SynchronizedBuffer();
        SynchronizedBuffer webServicesOutputBuffer2 = new SynchronizedBuffer();

        // these buffers will link the two shipping centers to each of their two Sections, respectively
        // they are named according to the convention: shippingCenterOutputBuffer<Shipping Center #>_<Section #>
        SynchronizedBuffer shippingCenterOutputBuffer1_1 = new SynchronizedBuffer();
        SynchronizedBuffer shippingCenterOutputBuffer1_2 = new SynchronizedBuffer();
        SynchronizedBuffer shippingCenterOutputBuffer2_1 = new SynchronizedBuffer();
        SynchronizedBuffer shippingCenterOutputBuffer2_2 = new SynchronizedBuffer();

        // these buffers will link the four Sections to the two Shipping docks
        // both sections on each side of the tree will share one output buffer
        SynchronizedBuffer sectionOutputBuffer1 = new SynchronizedBuffer();
        SynchronizedBuffer sectionOutputBuffer2 = new SynchronizedBuffer();

        // these buffers will link the two shipping docks to the four delivery trucks
        SynchronizedBuffer dockOutputBuffer1_1 = new SynchronizedBuffer();
        SynchronizedBuffer dockOutputBuffer1_2 = new SynchronizedBuffer();
        SynchronizedBuffer dockOutputBuffer2_1 = new SynchronizedBuffer();
        SynchronizedBuffer dockOutputBuffer2_2 = new SynchronizedBuffer();

        // create a new thread pool with two threads
        ExecutorService executorService = Executors.newCachedThreadPool();

        // call ExecutorService.execute(); pass it a new AmazonWebServerObject
        // this call will in turn call the run() method within the new AmazonWebServerObject
        executorService.execute(new AmazonWebServer(webServicesOutputBuffer1, webServicesOutputBuffer2));

        // call ExecutorService.execute() for the two shipping centers;
        executorService.execute(new ShippingCenter(1, webServicesOutputBuffer1, shippingCenterOutputBuffer1_1, shippingCenterOutputBuffer1_2));
        executorService.execute(new ShippingCenter(2, webServicesOutputBuffer2, shippingCenterOutputBuffer2_1, shippingCenterOutputBuffer2_2));

        // call ExecutorService.execute() for the four Sections
        executorService.execute(new Section(1, shippingCenterOutputBuffer1_1, sectionOutputBuffer1));
        executorService.execute(new Section(2, shippingCenterOutputBuffer1_2, sectionOutputBuffer1));
        executorService.execute(new Section(1, shippingCenterOutputBuffer2_1, sectionOutputBuffer2));
        executorService.execute(new Section(2, shippingCenterOutputBuffer2_2, sectionOutputBuffer2));

        // call ExecutorService.execute() for the two Shipping Docks
        executorService.execute(new ShippingDock(sectionOutputBuffer1, dockOutputBuffer1_1, dockOutputBuffer1_2));
        executorService.execute(new ShippingDock(sectionOutputBuffer2, dockOutputBuffer2_1, dockOutputBuffer2_2));

        // call ExecutorService.execute() for the four Delivery Trucks
        executorService.execute(new Truck(1, dockOutputBuffer1_1));
        executorService.execute(new Truck(2, dockOutputBuffer1_2));
        executorService.execute(new Truck(1, dockOutputBuffer2_1));
        executorService.execute(new Truck(2, dockOutputBuffer2_2));

        executorService.shutdown();
    }

}
