import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * This class serves as the driver for the S1_Computus_Hard program. Ultimately, the class serves two main purposes:
 * to print out the date of easter given a specific year, and display the number of times easter occurs on each day over
 * a 5700000-year cycle. The class consists of two methods, main and printEasterLoop. The printEasterLoop method is
 * responsible for printing the cycle data. The main method is responsible for displaying a menu and either printing
 * the date of easter given a specific year, or calling printEasterLoop, depending on user input.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 8/28/2021
 *
 */
public class Driver {

    /**
     * This function is responsible for printing out the number of times easter occurs on each calendar day over a
     * 5700000-year cycle. It does this by creating an Easter object, setting the year, and then repeatedly retrieving
     * the date that Easter occurred that year and incrementing the year, all the while storing dates in a 2-D array.
     */
    public static void printEasterLoop() {
        // initialize array object with months (for output later)
        String[] monthArray = new String[] { "", "January ", "February", "March ", "April ", "May ", "June ", "July ",
                "August ", "September ", "October ", "November ", "December " };

        // create a 2-D array that will hold the number of occurrences for each specific day
        // make it one element larger in each dimension for ease of indexing later on
        int[][] easterOccurrencesByDay = new int[13][32];

        // create an Easter object that we will use to calculate Easter dates
        Easter e = new Easter(0);

        // loop through 5700000 times, each time adding a year and incrementing the element in easterOccurrencesByDay
        // that corresponds to the specific day that easter occurs that year
        for (int i = 0; i < 5700000; i++) {
            easterOccurrencesByDay[e.getMonth()][e.getDay()] += 1;
            e.setYear(e.getYear() + 1);
        }

        // now that easterOccurrencesByDay has been "filled", we need to loop through it and display the results
        System.out.print("\n");
        for (int i = 1; i < 13; i++) {
            for (int j = 1; j < 32; j++) {
                // only print out the current element if there are occurrences of Easter on that day
                if (easterOccurrencesByDay[i][j] != 0) {
                    System.out.println(monthArray[i] + j + " - " + easterOccurrencesByDay[i][j]);
                }
            }
        }
        System.out.print("\n");
    }

    /**
     * This method is responsible for driving the S1_Computus_Hard module. Upon execution, this method displays a menu
     * prompting the user to enter either a 1, 2, or 3, depending on if they want to calculate Easter for a specific
     * year, display the number of occurrences of Easter for each calendar day over a 57000000-year cycle, or terminate
     * the program, respectively. If the user elects to compute Easter on a specific year (1), they will be prompted to
     * enter a year. The method will keep looping back to the menu until the user elects to terminate the program (3).
     *
     * @param args The default parameter for main methods.
     */
    public static void main(String[] args) {
        Scanner outerInput = new Scanner(System.in);    // used for user input at the menu (1, 2, or 3)
        Scanner yearInput = new Scanner(System.in);     // used for user input when computing easter for a specific year

        boolean outerLoop = true;       // used keep looping back to the menu
        int menuInput;                  // the int that holds the user-input at the menu level
        int year;                       // the int used to hold the user-input when calculating easter

        System.out.println("S1_Computus\n\n");

        // keep looping while outerLoop is true
        while(outerLoop) {

            // display the options and prompt the user for input
            System.out.println("\t(1) Compute Easter\n");
            System.out.println("\t(2) Print Easter Cycle\n");
            System.out.println("\t(3) Exit\n\n");
            System.out.print("Please enter an option (1-3): ");

            // read in the user's input
            try {
                // try to read in an int
                menuInput = outerInput.nextInt();
            }
            catch(InputMismatchException e) {
                // if the user enters something other than an int, read it in and then assign the value 4 to menuInput
                outerInput.next();
                menuInput = 4;
            }

            // if the user enters a 1, prompt them to enter a year
            if(menuInput == 1) {
                System.out.print("\nPlease enter a year: ");
                year = yearInput.nextInt();
                // create a new Easter object, pass it the year, and print the date of easter
                Easter e = new Easter(year);
                e.printEaster();
            }
            // if the user enters a 2, call the printEasterLoop method
            else if(menuInput == 2) {
                printEasterLoop();
            }
            // if the user enters a 3, break out of the loop and terminate the program
            else if(menuInput == 3) {
                System.out.println("Goodbye\n");
                outerLoop = false;
            }
            // otherwise, display an invalid input message
            else {
                System.out.println("Invalid input...\n");
            }
        }
    }

}
