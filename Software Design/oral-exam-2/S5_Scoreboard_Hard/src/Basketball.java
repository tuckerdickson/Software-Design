/**
 * This class is used to implement a game of Basketball. It is a subclass of the Game class, and thus it defines the
 * two abstract methods from Game. It also includes a two-argument constructor that makes a call to the super-class
 * constructor.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/3/2021
 */
public class Basketball extends Game {

    /**
     * This is the sole constructor for the Basketball class. It takes in two parameters, Team objects homeTeam and
     * awayTeam, and then passes those two Teams, along with the int 10 and the String "quarter" to the super-class
     * constructor.
     *
     * @param homeTeam This Team object refers to the home team in the game, and it will be used to set Game's homeTeam
     *                 private instance variable.
     * @param awayTeam This Team object refers to the away team in the game, and it will be used to set Game's awayTeam
     *                 private instance variable.
     */
    public Basketball(Team homeTeam, Team awayTeam) {
        // use homeTeam and awayTeam to make a call to super-class (Game) constructor
        super(homeTeam, awayTeam, 10, "quarter");
    }

    /**
     * This method is the Basketball class's overridden version of the Game class's abstract getScoringMethods() method.
     * It doesn't take any parameters, but it returns an array of ScoringMethod objects, each of which represents a
     * scoring method in basketball.
     *
     * @return This method returns an array of ScoringMethod objects, each of which represents a scoring method in the
     * game of basketball (one for a 3-pointer, one for a 2-pointer, and one for a free-throw).
     */
    @Override
    public ScoringMethod[] getScoringMethods() {

        // create three ScoringMethods (one for 3-pointer, one for 2-pointer, and one for free-throws)
        ScoringMethod threePointer = new ScoringMethod("three-point shot", 3);
        ScoringMethod twoPointer = new ScoringMethod("two-point shot", 2);
        ScoringMethod freeThrow = new ScoringMethod("free throw", 1);

        // create an array of scoring methods and add to it the three ScoringMethods just created
        ScoringMethod[] scoringMethods = {threePointer, twoPointer, freeThrow};

        // return the array of ScoringMethods
        return scoringMethods;
    }

    /**
     * This method is the Basketball class's overridden version of the Game class's abstract isGameOver() method. This
     * method takes in no parameters, but it returns a boolean value indicating whether or not a basketball game is over.
     *
     * @return This method returns a boolean value indicating whether or not a basketball game is over (true means the game
     * is over).
     */
    @Override
    public boolean isGameOver() {
        // if the current quarter is greater than 4, the game is over so return true
        if (this.getCurrentPeriodOfPlay() > 4) {
            return true;
        }
        // otherwise the game isn't over yet so return false
        else {
            return false;
        }
    }
}
