import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;

/**
 * This class acts as the driver of this program. It's primary purpose is to "set the stage" so to speak for the GUI
 * application. This class contains two methods, the main method and start, and no private instance variables.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 11/6/2021
 */
public class Hangman extends Application {

    /**
     * This method is the first step in the process of creating this GUI. It only contains one line of code, in which
     * is makes a call to the launch method (from the Application superclass). This causes an instance of Application to
     * be constructed and sets into the motion the building of the GUI.
     *
     * @param args This array of Strings contains the command-line arguments passed to the program upon invocation.
     */
    public static void main(String[] args) {
        launch(args);
    }

    /**
     * This method is responsible for locating the root fxml file containing all of the instructions for constructing
     * the GUI, creating a new Scene object with the path to the root, and then setting that Scene as the primary stage
     * of the application. If FXMLLoader.load() fails to locate the fxml file, it will throw an IOException.
     *
     * @param primaryStage This Stage object represents the primary stage that the GUI application will be built on.
     */
    @Override
    public void start(Stage primaryStage) {
        try {
            // locate the fxml file, load it into the Parent root
            Parent root = FXMLLoader.load(getClass().getResource("Hangman.fxml"));

            // create a new Scene, pass it the root containing the fxml file
            Scene scene = new Scene(root);

            // set the title of the stage
            primaryStage.setTitle("Hangman");

            // set the scene of the stage
            primaryStage.setScene(scene);

            // make the stage visible
            primaryStage.show();

        // if FXMLLoader fails to locate the fxml file, it will throw an IOException
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}
