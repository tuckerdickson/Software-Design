import javax.swing.*;
import javax.swing.border.Border;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

/**
 * This purpose of this class is to create a frame that includes two text areas, one for Roman numeral input and output,
 * and one for Arabic numeral input and output. Each of these text areas detects user input, and displays the equivalent
 * numeral in the other text area in real-time. This class is a subclass of the JFrame class, and it implements the KeyListener
 * interface.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 9/10/2021
 */
public class TextAreaFrame extends JFrame implements KeyListener{

    /**
     * This private instance variable will reference the JTextArea object that the user will type Arabic numerals in
     */
    private final JTextArea textAreaArabic;

    /**
     * This private instance variable will reference the JTextArea object that the user will type Roman numerals in
     */
    private final JTextArea textAreaRoman;

    /**
     * This is the sole constructor for the TextAreaFrame class. It creates a Box object and two JTextArea objects,
     * and utilizes the Roman and Arabic classes to convert between Roman Numerals and Arabic numerals
     */
    public TextAreaFrame() {

        // call the JFrame constructor, passing in the assignment name as the title
        super("S10_ArabicToRomanGUI_Hard");

        // create a Box container; will be used to organize GUI components (the two text areas in this case)
        Box box = Box.createHorizontalBox();

        // create a Border object; set color to light gray and thickness to 10
        Border border = BorderFactory.createLineBorder(Color.LIGHT_GRAY,10);

        // create a JTextArea object called textAreaArabic; pass the prompt string, rows, and columns to the constructor
        String arabicPrompt = "Enter an Arabic Numeral here";
        textAreaArabic = new JTextArea(arabicPrompt, 10, 15);
        // use the Border object created earlier to set the border for textAreaArabic
        textAreaArabic.setBorder(border);
        // use the JTextArea.addKeyListener method to add a listener that will detect key strokes in textAreaArabic
        // because our TextAreaFrame class implements the KeyListener interface, we can pass 'this' as the KeyListener
            // param ('this' is a reference to this TextAreaFrame object)
        textAreaArabic.addKeyListener(this);
        // add textAreaArabic to the box
        box.add(textAreaArabic);

        // create a JTextArea object called textAreaRoman; pass the prompt string, rows, and columns to the constructor
        String romanPrompt = "Enter a Roman Numeral here";
        textAreaRoman = new JTextArea(romanPrompt,10, 15);
        // use the Border object created earlier to set the border for textAreaRoman
        textAreaRoman.setBorder(border);
        // use the JTextArea.addKeyListener method to add a listener that will detect key strokes in textAreaRoman
        // because our TextAreaFrame class implements the KeyListener interface, we can pass 'this' as the KeyListener
            // param ('this' is a reference to this TextAreaFrame object)
        textAreaRoman.addKeyListener(this);
        // add textAreaRoman to the box
        box.add(textAreaRoman);

        // add the box to the frame
        add(box);
    }

    /**
     * This is TextAreaFrame's overridden version of the keyPressed method, declared in the KeyListener interface. This
     * method is never used in this program.
     *
     * @param event This parameter is of type KeyEvent, and it is used to store information about a specific keyboard event
     */
    @Override
    public void keyPressed(KeyEvent event) {
        // due to how this method processes key strokes, the user input does not produce the correct output
        // not using
    }

    /**
     * This is TextAreaFrame's overridden version of the keyTyped method, declared in the KeyListener interface. This
     * method is never used in this program.
     *
     * @param event This parameter is of type KeyEvent, and it is used to store information about a specific keyboard event
     */
    @Override
    public void keyTyped(KeyEvent event) {
        // due to how this method processes key strokes, the user input does not produce the correct output
        // not using
    }

    /**
     * This is TextAreaFrame's overridden version of the keyReleased method, declared in the KeyListener interface. This
     * method is called by the KeyListeners in textAreaRoman and textAreaArabic every time a keyboard event is detected
     * in either JTextArea, respectively. The method takes one parameter, a KeyEvent called event, which it uses to
     * determine the source of the key event. If the source is textAreaArabic, the method uses the Arabic class methods
     * to compute the equivalent Roman numeral and display it in textAreaRoman. If the source is textAreaRoman, the method
     * uses the Roman class methods to compute the equivalent Arabic numeral and display it in textAreaArabic.
     *
     * @param event This parameter is of type KeyEvent. It stores information about the key event, such as the source,
     *              which is used within the method to determine how to interpret the input.
     */
    @Override
    public void keyReleased(KeyEvent event) {
        // if the source of the KeyEvent was textAreaArabic, get the text in textAreaArabic, use it to create a new
            // Arabic object, get the computed Roman numeral equivalent from that Arabic object, and use that to set
            // the output text in textAreaRoman
        if(event.getSource() == textAreaArabic) {
            Arabic arabic = new Arabic(textAreaArabic.getText());
            textAreaRoman.setText(arabic.getRoman());
        }
        // if the source of the KeyEvent was textAreaRoman, get the text in textAreaRoman, use it to create a new
            // Roman object, get the computed Arabic numeral equivalent from that Roman object, and use that to set
            // the output text in textAreaArabic
        else if(event.getSource() == textAreaRoman) {
            Roman roman = new Roman(textAreaRoman.getText());
            textAreaArabic.setText(roman.getArabic());
        }
    }
}
