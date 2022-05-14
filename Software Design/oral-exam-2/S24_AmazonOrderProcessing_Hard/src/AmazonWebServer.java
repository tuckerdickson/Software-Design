import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

/**
 * This class represents the Amazon Web Server node. It is responsible for reading in a .csv file containing a list of
 * orders, and then distributing those orders to one of two Shipping Centers (based on which city the order is being
 * shipped to). This class implements the Runnable interface, which declares one method, run().
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/11/2021
 */
public class AmazonWebServer implements Runnable{
    /**
     * This private String represents the name of the file containing the orders to be read in. The file name (and path)
     * are hard-coded in the constructor.
     */
    private String fileName;

    /**
     * This private SynchronizedBuffer object will be used as the buffer linking the Web Server to Shipping Center 1.
     * All of the orders being shipped to LA, San Francisco, Seattle, and Denver will be put in this buffer, eventually
     * to be picked up by Shipping Center 1.
     */
    private final SynchronizedBuffer sc1SharedBuffer;

    /**
     * This private SynchronizedBuffer object will be used as the buffer linking the Web Server to Shipping Center 2.
     * All of the orders being shipped anywhere other than the cities listed above will be put in this buffer, eventually
     * to be picked up by Shipping Center 2.
     */
    private final SynchronizedBuffer sc2SharedBuffer;

    /**
     * This is the sole constructor for the AmazonWebServer class. It takes in two arguments, each SynchronizedBuffer
     * objects. It uses thse two objects to initialize sc1SharedBuffer and sc2SharedBuffer, respectively. It also
     * sets fileName to the appropriate file path so that the orders file can be read in later.
     *
     * @param sc1SharedBuffer This SynchronizedBuffer object will be used to initialize sc1SharedBuffer.
     * @param sc2SharedBuffer This SynchronizedBuffer object will be used to initialize sc2SharedBuffer.
     */
    public AmazonWebServer(SynchronizedBuffer sc1SharedBuffer, SynchronizedBuffer sc2SharedBuffer) {
        // THIS FILEPATH ONLY WORKS IF THE WORKING DIRECTORY IS ntdickson_swd
        this.fileName = "./oral_exam2/S24_AmazonOrderProcessing_Hard/files/S24_AmazonOrderProcessing_OrdersFile.csv";
        this.sc1SharedBuffer = sc1SharedBuffer;
        this.sc2SharedBuffer = sc2SharedBuffer;
    }

    /**
     * This method is declared in the Runnable interface. It gets called from main when the ExecutorService.execute()
     * method is called and passed in an AmazonWebServer object. This method is responsible for reading in the orders
     * from the orders file one-by-one, converting the order (a String) to an array, and then putting that array
     * into the correct buffer, based on the city it's being shipped to.
     */
    @Override
    public void run() {
        // create a new File object, passing fileName to the constructor
        File fileIn = new File(fileName);
        // this Scanner will be used to read in from the file
        Scanner reader;

        // if the File object successfully found the file, proceed
        if(fileIn.exists()) {

            // try to initialize the Scanner, using the File object created above
            try {
                reader = new Scanner(fileIn);
            } catch (FileNotFoundException exception) {
                exception.printStackTrace();
                return;
            }

            // read in the first line (we can discard this one as it's just labels)
            String currLine = reader.nextLine();

            // go until there aren't anymore lines
            while(reader.hasNextLine()) {
                // get the current line
                currLine = reader.nextLine();

                // initialize an array of Strings (size 7) to hold info from line
                String[] lineArray = new String[7];

                // data in the orders file are separated by commas, so we'll keep track of them
                int commaCount = 0;

                // while there are still commas in the current line
                while(currLine.contains(",")) {
                    // find the index of the next comma
                    int index = currLine.indexOf(',');

                    // commaCount should equal the index of the next empty spot in our array
                    // grab the information before the comma and put it in the array
                    lineArray[commaCount] = currLine.substring(0,index);

                    // "chop off" the information that we just grabbed off of the front
                    currLine = currLine.substring(index + 1);

                    // increment commaCount
                    commaCount += 1;
                }

                // at this point, there should be one data entry left (since rows don't end with commas)
                // commaCount should be 6, so simply assign what's left of the current row to lineArray[commaCount]
                lineArray[commaCount] = currLine;

                // if the city is LA, San Francisco, Seattle, or Denver, try putting the array in sc1SharedBuffer
                if (lineArray[1].equals("Los Angeles" ) || lineArray[1].equals("San Francisco") ||
                        lineArray[1].equals("Seattle") || lineArray[1].equals("Denver")) {
                    try {
                        sc1SharedBuffer.blockingPut(lineArray);
                        //System.out.println("WS sending " + lineArray[lineArray.length-1] + " to SC1");
                    } catch (InterruptedException interruptedException) {
                        Thread.currentThread().interrupt();
                    }
                }
                // otherwise, put it in sc2SharedBuffer
                else {
                    try {
                        sc2SharedBuffer.blockingPut(lineArray);
                        //System.out.println("WS sending " + lineArray[lineArray.length-1] + " to SC2");
                    } catch (InterruptedException interruptedException) {
                        interruptedException.printStackTrace();
                    }

                }
            }

            // at this point, the file should be completely processed and all orders should have been put to one of
            // the two buffers
            // System.out.println("Finished reading file; AmazonWebServer thread terminating");

            // create a new empty String array and put it in both buffers
            // this array will serve as a flag to the Shipping Centers that the Web Server has completed its task
            String[] eofArray = new String[0];
            try {
                sc1SharedBuffer.blockingPut(eofArray);
                sc2SharedBuffer.blockingPut(eofArray);
            } catch (InterruptedException interruptedException) {
                Thread.currentThread().interrupt();
            }
        }
        // if this else executes, the file should not be located
        else{
            System.out.println("Could not locate S24_AmazonOrderProcessing_OrdersFile.csv");
            System.out.println("Exiting AmazonWebServer thread");
        }

    }
}
