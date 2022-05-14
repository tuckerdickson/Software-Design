import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.event.ActionEvent;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Line;

import java.util.ArrayList;

/**
 * This class is responsible for handing all of the backend logic behind the GUI components. This class contains thirteen
 * private member variables, most of which are GUI components. It also contains two methods, processGuess and restart.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 11/06/21
 */
public class HangmanController {

    /**
     * This HangmanModel object will store all of the information about the current game, including the total letters
     * guessed, the incorrect letters guessed, the number of lives the user has remaining, and the word. It is also
     * provides methods to access those variables, as well as indicate whether the game is over.
     */
    private HangmanModel model = new HangmanModel();

    /**
     * This private TextField is the GUI component that the user will use to enter their guesses.
     */
    @FXML
    private TextField guessTextField;

    /**
     * This private TextField is the GUI component that will display the number of lives that the user has remaining
     */
    @FXML
    private TextField livesLeftTextField;

    /**
     * This private TextArea is the GUI component that will display all of the incorrect guesses that the user has made.
     */
    @FXML
    private TextArea incorrectGuessesTextArea;

    /**
     * This private TextField is the GUI component that will display the word (or partial word) that the user is trying
     * to guess.
     */
    @FXML
    private TextField wordTextField;

    /**
     * This private Label is the GUI component that will be displayed if the user has lost a game.
     */
    @FXML
    private Label gameOverLabel;

    /**
     * This private Label is the GUI component that will be displayed if the user has won a game.
     */
    @FXML
    private Label winnerLabel;

    /**
     * This private Circle is the GUI component that will be displayed on the user's first incorrect guess.
     */
    @FXML
    private Circle head;

    /**
     * This private Line is the GUI component that will be displayed on the user's second incorrect guess.
     */
    @FXML
    private Line body;

    /**
     * This private Line is the GUI component that will be displayed on the user's sixth incorrect guess.
     */
    @FXML
    private Line rightLeg;

    /**
     * This private Line is the GUI component that will be displayed on the user's fifth incorrect guess.
     */
    @FXML
    private Line leftLeg;

    /**
     * This private Line is the GUI component that will be displayed on the user's fourth incorrect guess.
     */
    @FXML
    private Line rightArm;

    /**
     * This private Line is the GUI component that will be displayed on the user's third incorrect guess.
     */
    @FXML
    private Line leftArm;

    /**
     * This method is responsible for processing every guess that the user makes. Its jobs include reading in the guess,
     * adding the guess to the totalGuesses (and possibly incorrectGuesses) ArrayList, adjusting the lives and hangman
     * figure accordingly, and outputting the adjusted word and guesses to the screen.
     *
     * @param event This method takes an ActionEvent as a parameter. This ActionEvent is created whenever the user
     *              clicks on the 'Go' button.
     */
    @FXML
    void processGuess(ActionEvent event) {
        // read in the text from the guessTextField and convert it to lower case
        String guess = guessTextField.getText().toLowerCase();

        // call the clear method on guessTextField, wordTextField, and incorrectGuessesTextArea to get rid of any output
        // left over from last turn
        guessTextField.clear();
        wordTextField.clear();
        incorrectGuessesTextArea.clear();

        // only proceed if the user actually entered something into guessTextField
        if (guess.length() > 0) {

            // loop over each character in the guess string (this will usually only be one character)
            for (int i = 0; i < guess.length(); i++) {

                // convert each character to a String (so that we can pass them to String.contains()
                String guessChar = String.valueOf(guess.charAt(i));

                // if the winning word doesn't contain the current character and the incorrectGuesses ArrayList doesn't
                // already contain the character, add it to incorrectGuesses and decrement numLives

                // the second condition is included so that the user won't get penalized for making the same incorrect
                // guess twice
                if ((!model.getWord().contains(guessChar)) && (!model.getIncorrectGuesses().contains(guessChar))) {
                    model.addIncorrectGuess(guessChar);
                    model.setNumLives(model.getNumLives() - 1);
                }

                // if the totalGuesses ArrayList doesn't already contain the character, add it
                if (!model.getTotalGuesses().contains(guessChar)) {
                    model.addGuess(guessChar);
                }
            }
        }

        // the following for loop is used to display the winning word with _ where there are characters that haven't
        // been guessed yet

        // loop over the winning word character by character
        for (int i = 0; i < model.getWord().length(); i++) {

            // store the current character in a String so that we can use String.contains()
            String currChar = String.valueOf(model.getWord().charAt(i));

            // if the current character (of the winning word) hasn't been guessed yet, append an _ to wordTextField
            if (!model.getTotalGuesses().contains(currChar)) {
                wordTextField.setText(wordTextField.getText() + "_");

            // otherwise, the character has been guessed already to append it to wordTextField
            } else {
                wordTextField.setText(wordTextField.getText() + currChar);
            }
        }

        // if the user has less than 6 lives remaining, display the head
        if (model.getNumLives() < 6) {
            head.setVisible(true);
        }
        // if the user has less than 5 lives remaining, display the body
        if (model.getNumLives() < 5) {
            body.setVisible(true);
        }
        // if the user has less than 4 lives remaining, display the leftArm
        if (model.getNumLives() < 4) {
            leftArm.setVisible(true);
        }
        // if the user has less than 3 lives remaining, display the rightArm
        if (model.getNumLives() < 3) {
            rightArm.setVisible(true);
        }
        // if the user has less than 2 lives remaining, display the leftLeg
        if (model.getNumLives() < 2) {
            leftLeg.setVisible(true);
        }
        // if the user has less than 1 life remaining, display the rightLeg
        if (model.getNumLives() < 1) {
            rightLeg.setVisible(true);
        }

        // only enter conditional if the game isn't over yet
        if (!model.gameOver()) {
            // if the user has won the game, disable guessTextField and display the winnerLabel
            if (model.didWin()) {
                winnerLabel.setVisible(true);
                guessTextField.setEditable(false);
                guessTextField.setDisable(true);
            // the game isn't over and the user hasn't won, so display all of the incorrect guesses so far in
            // incorrectGuessTextArea
            } else {
                ArrayList<String> incorrectGuesses = model.getIncorrectGuesses();
                for (String str : incorrectGuesses) {
                    incorrectGuessesTextArea.setText(incorrectGuessesTextArea.getText() + str + " ");
                }
            }

        // if this else is executed, the user lost; disable guessTextField and display the gameOverLabel
        } else {
            gameOverLabel.setVisible(true);
            guessTextField.setEditable(false);
            guessTextField.setDisable(true);
        }

        // display the remaining lives regardless
        livesLeftTextField.setText(String.valueOf(model.getNumLives()));
    }

    /**
     * This method is responsible for restarting the game. This method is called whenever the user clicks on the
     * "Restart" button.
     *
     * @param event This method takes in an ActionEvent as a parameter. This action event is created whenever the user
     *              clicks on the "Restart" button.
     */
    @FXML
    void restart(ActionEvent event) {

        // clear any text out of wordTextField and guessTextField
        wordTextField.clear();
        guessTextField.clear();

        // make winnerLabel and gameOverLabel invisible
        winnerLabel.setVisible(false);
        gameOverLabel.setVisible(false);

        // make guessTextField editable again
        guessTextField.setEditable(true);
        guessTextField.setDisable(false);

        // make the hangman invisible
        head.setVisible(false);
        body.setVisible(false);
        leftLeg.setVisible(false);
        leftArm.setVisible(false);
        rightLeg.setVisible(false);
        rightArm.setVisible(false);

        // call HangmanModel.clear() to clear all of the previous game info
        model.clear();
    }


}

