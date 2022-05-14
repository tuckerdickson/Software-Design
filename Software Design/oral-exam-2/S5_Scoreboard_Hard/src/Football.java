/**
 * This class is used to implement a game of Football. It is a subclass of the Game class, and thus it defines the
 * two abstract methods from Game. It also includes a two-argument constructor that makes a call to the super-class
 * constructor.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/3/2021
 */
public class Football extends Game {

    /**
     * This is the sole constructor for the Football class. It takes in two parameters, Team objects homeTeam and
     * awayTeam, and then passes those two Teams, along with the int 15 and the String "quarter" to the super-class
     * constructor.
     *
     * @param homeTeam This Team object refers to the home team in the game, and it will be used to set Game's homeTeam
     *                 private instance variable.
     * @param awayTeam This Team object refers to the away team in the game, and it will be used to set Game's awayTeam
     *                 private instance variable.
     */
    public Football(Team homeTeam, Team awayTeam) {
        super(homeTeam, awayTeam, 15, "quarter");
    }

    /**
     * This method is the Football class's overridden version of the Game class's abstract getScoringMethods() method.
     * It doesn't take any parameters, but it returns an array of ScoringMethod objects, each of which represents a
     * scoring method in Football.
     *
     * @return This method returns an array of ScoringMethod objects, each of which represents a scoring method in the
     * game of football (one for a touchdown, one for a field goal, one for a safety, one for a two-point conversion,
     * and one for an extra-point).
     */
    @Override
    public ScoringMethod[] getScoringMethods() {

        // create a new ScoringMethod object for each individual scoring method in the game of football
        ScoringMethod touchdown = new ScoringMethod("touchdown", 6);
        ScoringMethod fieldGoal = new ScoringMethod("field goal", 3);
        ScoringMethod safety = new ScoringMethod("safety", 2);
        ScoringMethod twoPointConversion = new ScoringMethod("two-point conversion", 2);
        ScoringMethod pointAfter = new ScoringMethod("extra-point", 1);

        // create an array of ScoringMethod objects and add the five ScoringMethod objects created above
        ScoringMethod[] scoringMethods = {touchdown, fieldGoal, safety, twoPointConversion, pointAfter};

        // return the array of ScoringMethods
        return scoringMethods;
    }

    /**
     * This method is the Football class's overridden version of the Game class's abstract isGameOver() method. This
     * method takes in no parameters, but it returns a boolean value indicating whether or not a football game is over.
     *
     * @return This method returns a boolean value indicating whether or not a football game is over (true means the game
     * is over).
     */
    @Override
    public boolean isGameOver() {
        // if the current period of play (quarter) is greater than 4, the game is over so return true
        if (this.getCurrentPeriodOfPlay() > 4) {
            return true;
        }
        // otherwise, the game is still going so return false
        else {
            return false;
        }
    }
}
