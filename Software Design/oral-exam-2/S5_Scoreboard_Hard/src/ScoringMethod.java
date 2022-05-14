/**
 * This class is used to implement a new ScoringMethod type, which can represent a game-specific scoring method,
 * including the name of the scoring method (for example: "touchdown") as well as the value of the scoring method
 * (for example, a touchdown is worth 6 points).
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/4/2021
 */
public class ScoringMethod {
    /**
     * This private instance variable is a String that refers to the name of a specific scoring method. For example,
     * in the game of soccer, this variable would represent the String "goal".
     */
    private String methodName;

    /**
     * This private instance variable is an int that represents the value of a specific scoring method. For example,
     * if the scoring method being created was a touchdown, this variable would contain the int 6.
     */
    private int methodPoints;

    /**
     * This is the sole constructor for the ScoringMethod class. It takes in two parameters, one for each private
     * instance variable, and simply initializes both variables.
     *
     * @param methodName This String will be used to set the methodName private instance variable
     * @param methodPoints This int will be used to set the methodPoints private instance variable
     */
    public ScoringMethod(String methodName, int methodPoints) {
        this.methodName = methodName;
        this.methodPoints = methodPoints;
    }

    /**
     * This method acts as the getter method for the methodName private instance variable. It simply returns methodName.
     *
     * @return This method returns a String representing the methodName private instance variable
     */
    public String getMethodName() { return methodName; }

    /**
     * This method acts as the getter method for the methodPoints private instance variable. It simply returns
     * methodPoints.
     *
     * @return This method returns an int representing the methodPoints private instance variable
     */
    public int getMethodPoints() { return methodPoints; }
}
