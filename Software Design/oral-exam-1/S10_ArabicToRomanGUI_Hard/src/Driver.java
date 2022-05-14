import javax.swing.JFrame;

/**
 * The Driver class is the driver of this program. It sets in motion the creation of a TextAreaFrame, which will
 * ultimately be used to build the Arabic-to-Roman converter that the user will interface with.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 8/28/2021
 */
public class Driver {

    /**
     * The main method is the sole method in the Driver class. It creates a TextAreaFrame and sets the close operation,
     * size, and visibility of the new TextAreaFrame.
     *
     * @param args This is the default parameter for a main method in Java
     */
    public static void main(String[] args) {

        // create a new TextAreaFrame object called textAreaFrame
        TextAreaFrame textAreaFrame = new TextAreaFrame();

        // set the closing operation to EXIT_ON_CLOSE
        textAreaFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        // set the size of textAreaFrame
        textAreaFrame.setSize(425, 200);

        // set textAreaFrame to be visible
        textAreaFrame.setVisible(true);

    }
}
