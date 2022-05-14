import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * This class acts as the driver class for the Scoreboard program. It is responsible for creating a menu to interface
 * with the user, and utilizing the various other classes in the project to simulate a scoreboard. It doesn't contain
 * any private instance variables, and it only contains one method, main.
 */
public class Scoreboard {

    /**
     * This is the sole method in the Scoreboard class. It is responsible for displaying a command-line user interface
     * which simulates a scoreboard. It does this by tying together the various other classes in this project. This
     * method takes one parameter and doesn't return anything.
     *
     * @param args This array of Strings is the mandatory parameter for any main function.
     */
    public static void main(String[] args) {

        boolean keepGoing = true;   // this boolean will control the outer-most while loop (game type menu)

        int gameType;               // this int will store the user-input value representing their choice of game
        Scanner gameTypeScanner = new Scanner(System.in);   // create a new Scanner to read in the choice of game type

        String homeTeamString;      // this String will store the user-input name of the home team
        Scanner homeTeamScanner = new Scanner(System.in);   // create a new Scanner to read in the home team name
        Team homeTeam;              // this Team object will represent the home team

        String awayTeamString;      // this String will store the user-input name of the away team
        Scanner awayTeamScanner = new Scanner(System.in);   // create a new Scanner to read in the away team name
        Team awayTeam;              // this Team object will represent the away team

        Game game;                  // create a new Game object to represent the game about to be played

        int menuChoice;             // this int will store the user-input choice at the menu within the game
        Scanner menuChoiceScanner = new Scanner(System.in); // create a new Scanner object to read in the menu choice

        // keep looping while keepGoing is true
        while(keepGoing) {

            // display the game type menu
            System.out.println("Select the type of game:");
            System.out.println("\t1. Football");
            System.out.println("\t2. Basketball");
            System.out.println("\t3. Soccer");
            System.out.println("\t4. Hockey");
            System.out.println("\t5. Exit Scoreboard");

            // prompt the user for input
            System.out.print("Enter choice: ");

            try {
                // try to read in an int
                gameType = gameTypeScanner.nextInt();
            }
            catch(InputMismatchException e) {
                // if the user enters something other than an int, read it in and then assign the value 6 to menuInput
                // this will trigger the below conditional to alert the user of invalid input
                gameTypeScanner.next();
                gameType = 6;
            }

            // execute the following if statement if the user input is between 1 and 4 (they've opted for a game)
            if((gameType > 0) && (gameType < 5)) {

                // prompt user to enter a home team name, then use a Scanner to read in input
                // use input to create a new Team object
                System.out.print("\nEnter Home Team: ");
                homeTeamString = homeTeamScanner.next();
                homeTeam = new Team(homeTeamString);

                // prompt user to enter an away team name, then use a Scanner to read in input
                // use input to create a new Team object
                System.out.print("Enter Away Team: ");
                awayTeamString = awayTeamScanner.next();
                awayTeam = new Team(awayTeamString);

                // need to initialize game outside of an if/else if once
                // otherwise the IDE won't let me call startGame below
                game = new Football(homeTeam, awayTeam);

                // reinitialize game accordingly, using the concept of Polymorphism to dynamically 'change' game
                // from a Game object to a Football/Basketball/Soccer/Hockey object
                if(gameType == 1) {
                    game = new Football(homeTeam, awayTeam);
                }
                else if(gameType == 2) {
                    game = new Basketball(homeTeam, awayTeam);
                }
                else if(gameType == 3) {
                    game = new Soccer(homeTeam, awayTeam);
                }
                else if(gameType == 4) {
                    game = new Hockey(homeTeam, awayTeam);
                }

                // call game.startGame() to set the scores to zero and the period to 1
                game.startGame();

                // print out each team's score (both 0  at this point) and the current period (1)
                System.out.print(homeTeam.getName() + " - " + homeTeam.getScore() + ", ");
                System.out.println(awayTeam.getName() + " - " + awayTeam.getScore());
                System.out.println("Current " + game.getNameOfPeriod() + ": " + game.getCurrentPeriodOfPlay() + '\n');

                // keep looping over the scoring/period menu while the game is still going
                while(!game.isGameOver()) {
                    System.out.println("Menu:");

                    // loop over all of the scoring methods for the particular type of game, printing out each as an
                    // option for the home team following the format:
                    // number. homeTeamName scoringMethodName
                    for(int i = 0; i < game.getScoringMethods().length; i += 1) {
                        System.out.println((i + 1) + ". " + game.getHomeTeam().getName() + " " +
                                game.getScoringMethods()[i].getMethodName());
                    }

                    // loop over all of the scoring methods for the particular type of game, printing out each as an
                    // option for the away team following the format:
                    // number. awayTeamName scoringMethodName
                    for(int j = 0; j < game.getScoringMethods().length; j += 1) {
                        System.out.println((j + game.getScoringMethods().length + 1) + ". " +
                                game.getAwayTeam().getName() + " " + game.getScoringMethods()[j].getMethodName());
                    }

                    // calculate the final number in the list by multiplying the game's ScoringMethods array by 2 and
                    // adding one
                    int nextNumber = (2 * game.getScoringMethods().length) + 1;

                    // print out the final option: ending the current period of play
                    System.out.println(nextNumber + ". End " + game.getNameOfPeriod());

                    // prompt user for input
                    System.out.print("\nEnter choice: ");

                    try {
                        // try to read in an integer
                        menuChoice = menuChoiceScanner.nextInt();
                    } catch (InputMismatchException e){
                        // if the user doesn't input an integer, catch it with a Scanner.next() call and set menuChoice
                        // to an invalid integer to be caught later
                        menuChoiceScanner.next();
                        menuChoice = (2 * game.getScoringMethods().length) + 2;
                    }

                    // if the choice is one of the home team scoring options, add the appropriate score to the home team
                    if((menuChoice > 0) && (menuChoice <= game.getScoringMethods().length)) {
                        game.addScore(homeTeam, game.getScoringMethods()[menuChoice - 1]);
                    }
                    // if the choice is one of the away team scoring options, add the appropriate score to the away team
                    else if((menuChoice > game.getScoringMethods().length) && (menuChoice <= 2 * game.getScoringMethods().length)) {
                        game.addScore(awayTeam, game.getScoringMethods()[menuChoice - game.getScoringMethods().length - 1]);
                    }
                    // if the choice is to end the period of play, end the period of play
                    else if(menuChoice == ((2 * game.getScoringMethods().length) + 1)) {
                        game.endCurrentPeriod();
                    }
                    // otherwise, the input is invalid
                    else {
                        System.out.println("\nInvalid input...\n");
                    }

                    // if the game isn't over at this point, print out the scores and the current period
                    if(!game.isGameOver()) {
                        System.out.print(homeTeam.getName() + " - " + homeTeam.getScore() + ", ");
                        System.out.println(awayTeam.getName() + " - " + awayTeam.getScore());
                        System.out.println("Current " + game.getNameOfPeriod() + ": " + game.getCurrentPeriodOfPlay() + '\n');
                    }
                    // otherwise, the game is over so include a message saying so, as well as the winner
                    else {
                        System.out.println("\nGame is over.");
                        System.out.print(homeTeam.getName() + " - " + homeTeam.getScore() + ", ");
                        System.out.println(awayTeam.getName() + " - " + awayTeam.getScore());
                        System.out.println("Current " + game.getNameOfPeriod() + ": Final");
                        System.out.println("Winner: " + game.getWinner().getName() + '\n');
                    }

                }
            }
            // if the user inputs 5, set keepGoing to false to break out of the outer while loop
            else if(gameType == 5) {
                keepGoing = false;
            }
            // if this else executes, the "Game Type" input was invalid
            else {
                System.out.println("\nInvalid input...\n");
            }
        }
    }
}
