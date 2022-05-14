/**
 * This class is used to represent a team in a certain game. A Team object includes a name and a score.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/4/2021
 */
public class Team {

    /**
     * This private instance variable refers to the name of the team.
     */
    private String name;

    /**
     * This private instance variable refers to the score of the team.
     */
    private int score;

    /**
     * This is the sole constructor for the Team class. It takes one parameter and uses it to initialize the name
     * private instance variable.
     *
     * @param name This String is used to initialize the name private instance variable.
     */
    public Team(String name) {
        this.name = name;
    }

    /**
     * This method acts as the setter for the name private instance variable. It takes in one parameter and uses it to
     * set name.
     *
     * @param name This String will be used to set the name private instance variable.
     */
    public void setName(String name) { this.name = name; }

    /**
     * This method acts as the setter for the score private instance variable. It takes in one parameter and uses it to
     * set score.
     *
     * @param score This int will be used to set the score private instance variable.
     */
    public void setScore(int score) { this.score = score; }

    /**
     * This method acts as the getter for the name private instance variable. It doesn't take any parameters and returns
     * a String representing name.
     *
     * @return This method returns a String representing the name private instance variable.
     */
    public String getName() { return name; }

    /**
     * This method acts as the getter for the score private instance variable. It doesn't take any parameters and returns
     * an int representing score.
     *
     * @return This method returns an int representing the score private instance variable.
     */
    public int getScore() { return score; }
}
