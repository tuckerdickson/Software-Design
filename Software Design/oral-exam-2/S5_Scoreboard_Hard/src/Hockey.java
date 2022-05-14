/**
 * This class is used to implement a game of Hockey. It is a subclass of the Game class, and thus it defines the
 * two abstract methods from Game. It also includes a two-argument constructor that makes a call to the super-class
 * constructor.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/3/2021
 */
public class Hockey extends Game {

    /**
     * This is the sole constructor for the Hockey class. It takes in two parameters, Team objects homeTeam and
     * awayTeam, and then passes those two Teams, along with the int 20 and the String "period" to the super-class
     * constructor.
     *
     * @param homeTeam This Team object refers to the home team in the game, and it will be used to set Game's homeTeam
     *                 private instance variable.
     * @param awayTeam This Team object refers to the away team in the game, and it will be used to set Game's awayTeam
     *                 private instance variable.
     */
    public Hockey(Team homeTeam, Team awayTeam) {
        super(homeTeam, awayTeam, 20, "period");
    }

    /**
     * This method is the Hockey class's overridden version of the Game class's abstract getScoringMethods() method.
     * It doesn't take any parameters, but it returns an array of ScoringMethod objects, each of which represents a
     * scoring method in Hockey.
     *
     * @return This method returns an array of ScoringMethod objects, each of which represents a scoring method in the
     * game of hockey (in this case, a goal is the only scoring method).
     */
    @Override
    public ScoringMethod[] getScoringMethods() {

        // create a new ScoringMethod object to represent a goal
        ScoringMethod goal = new ScoringMethod("goal", 1);

        // create an array of ScoringMethod objects and add the goal ScoringMethod
        ScoringMethod[] scoringMethods = {goal};

        // return the array of ScoringMethods
        return scoringMethods;
    }

    /**
     * This method is the Hockey class's overridden version of the Game class's abstract isGameOver() method. This
     * method takes in no parameters, but it returns a boolean value indicating whether or not a hockey game is over.
     *
     * @return This method returns a boolean value indicating whether or not a hockey game is over (true means the game
     * is over).
     */
    @Override
    public boolean isGameOver() {
        // if the current period of play (period) is greater than 3, the game is over so return true
        if (this.getCurrentPeriodOfPlay() > 3) {
            return true;
        }
        // otherwise the game is still going so return false
        else {
            return false;
        }
    }
}
