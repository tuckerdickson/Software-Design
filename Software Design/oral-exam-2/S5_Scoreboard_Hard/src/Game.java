/**
 * This abstract class is the super-class for Basketball, Soccer, Hockey, and Football. It contains all of the
 * concrete classes used to start a game, end a period, add a score, determine the winner, and more. It also contains
 * two abstract methods used to get a specific sport's scoring methods, and to determine if the game is over.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/3/2021
 */
public abstract class Game {

    /**
     * This private instance variable refers to the Team object that represents the home team.
     */
    private Team homeTeam;

    /**
     * This private instance variable refers to the Team object that represents the away team.
     */
    private Team awayTeam;

    /**
     * This private instance variable represents the current period of play of the game.
     */
    private int currentPeriodOfPlay;

    /**
     * This private instance variable represents the length of the game-specific period of play.
     */
    private int lengthOfPeriod;

    /**
     * This private instance variable represents the game-specific name of the period of play.
     */
    private String nameOfPeriod;

    /**
     * This is the sole constructor for the Game class. It takes in four parameters and simply uses them to set the
     * four corresponding private instance variables.
     *
     * @param homeTeam This parameter is a Team object that will be used to set the homeTeam private instance variable.
     * @param awayTeam This parameter is a Team object that will be used to set the awayTeam private instance variable.
     * @param lengthOfPeriod This parameter is an int that will be used to set the lengthOfPeriod private instance variable.
     * @param nameOfPeriod This parameter is a String that will be used to set the nameOfPeriod private instance variable.
     */
    public Game(Team homeTeam, Team awayTeam, int lengthOfPeriod, String nameOfPeriod) {
        this.homeTeam = homeTeam;
        this.awayTeam = awayTeam;
        this.lengthOfPeriod = lengthOfPeriod;
        this.nameOfPeriod = nameOfPeriod;
    }

    /**
     * This is the setter method for the homeTeam private instance variable. It takes one parameter and uses it to set
     * homeTeam. This method is never called.
     *
     * @param homeTeam This parameter is the Team object that will be used to set the homeTeam private instance variable.
     */
    public void setHomeTeam(Team homeTeam) { this.homeTeam = homeTeam; }

    /**
     * This is the setter method for the awayTeam private instance variable. It takes one parameter and uses it to set
     * awayTeam. This method is never called.
     *
     * @param awayTeam This parameter is the Team object that will be used to set the awayTeam private instance variable.
     */
    public void setAwayTeam(Team awayTeam) { this.awayTeam = awayTeam; }

    /**
     * This is the getter method for the homeTeam private instance variable. It doesn't take any parameters and it
     * returns homeTeam.
     *
     * @return This method returns the homeTeam private instance variable.
     */
    public Team getHomeTeam() { return homeTeam; }

    /**
     * This is the getter method for the awayTeam private instance variable. It doesn't take any parameters and it
     * returns awayTeam.
     *
     * @return This method returns the awayTeam private instance variable.
     */
    public Team getAwayTeam() { return awayTeam; }

    /**
     * This is the getter method for the currentPeriodOfPlay private instance variable. It doesn't take any parameters
     * and it returns currentPeriodOfPlay.
     *
     * @return This method returns the currentPeriodOfPlay private instance variable.
     */
    public int getCurrentPeriodOfPlay() { return currentPeriodOfPlay; }

    /**
     * This is the getter method for the lengthOfPeriod private instance variable. It doesn't take any parameters and it
     * returns lengthOfPeriod. This method is never called.
     *
     * @return This method returns the lengthOfPeriod private instance variable.
     */
    public int getLengthOfPeriod() { return lengthOfPeriod; }

    /**
     * This is the getter method for the nameOfPeriod private instance variable. It doesn't take any parameters and it
     * returns nameOfPeriod.
     *
     * @return This method returns the nameOfPeriod private instance variable.
     */
    public String getNameOfPeriod() { return nameOfPeriod; }

    /**
     * This method is called when a new game is started in main. It simply sets the home and away scores to zero and
     * sets the current period to 1. This method doesn't take any parameters or return anything.
     */
    public void startGame() {
        homeTeam.setScore(0);
        awayTeam.setScore(0);
        currentPeriodOfPlay = 1;
    }

    /**
     * This method is called whenever the user decides to end the current period of play. It simply increments the
     * currentPeriodOfPlay private instance variable. This method doesn't take any parameters or return anything.
     */
    public void endCurrentPeriod() {
        currentPeriodOfPlay = currentPeriodOfPlay + 1;
    }

    /**
     * This method is responsible for determining the winner (or lack of) between homeTeam and awayTeam. This method
     * doesn't take in any parameters, but it returns a Team object.
     *
     * @return This method returns the Team object corresponding to the winner of the game.
     */
    public Team getWinner() {
        // create a new team object
        Team winner;

        // if homeTeam's score is greater than awayTeam's score, assign homeTeam to winner
        if(homeTeam.getScore() > awayTeam.getScore()) {
            winner = homeTeam;
        }
        // if awayTeam's score is greater than homeTeam's score, assign awayTeam to winner
        else if (awayTeam.getScore() > homeTeam.getScore()) {
            winner = awayTeam;
        }
        // if this else is executed, that means the two scores are equal
        // in this case, assign winner to a new Team object with the name "Tie"
        else {
            winner = new Team("Tie");
        }

        // return the winner
        return winner;
    }

    /**
     * This method is responsible for adding a score to a Team. It takes two parameters, one referring to the team
     * whose score is to be increased, and one referring to the score to increase by.
     *
     * @param team This Team object refers to the Team whose score needs to be increased.
     * @param scoringMethod This ScoringMethod object refers to the ScoringMethod that is to be used to increase the
     *                      score.
     */
    public void addScore(Team team, ScoringMethod scoringMethod) {
        // get the team's current score, get the points from the ScoringMethod passed in
        // sum the two and use the sum to set the team's score
        team.setScore(team.getScore() + scoringMethod.getMethodPoints());
    }

    /**
     * This abstract method is responsible for returning the various scoring methods for a specific type of game.
     * It returns the scoring methods in the form of an array of ScoringMethods.
     *
     * @return This method returns an array of ScoringMethods that contains all of the scoring methods for a particular game.
     */
    public abstract ScoringMethod[] getScoringMethods();

    /**
     * This abstract method is responsible for indicating whether or not a game is over. It does this by returning a
     * boolean value.
     *
     * @return This method returns a boolean value that represents whether or not a game is over (true means game is over)
     */
    public abstract boolean isGameOver();
}
