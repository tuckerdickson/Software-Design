/**
 * This class is the template for creating the Food objects in the Thanksgiving class. This class has one three-argument
 * constructor and seven instance methods (four getter methods and three setter methods). It also contains four private
 * instance variables.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/24/21
 */
public class Food {

    /**
     * This private double represents the volume occupied by this food object.
     */
    private double volume;

    /**
     * This private double represents the enjoyment value of this food object.
     */
    private double enjoyment;

    /**
     * This private String represents the name of this food object (ie: turkey).
     */
    private final String name;

    /**
     * This private double represents the enjoyment-to-volume ratio of this food object, expressed as
     * evRatio = enjoyment / volume
     */
    private double evRatio;

    /**
     * This is the sole constructor for the Food class. It takes in three arguments and uses them to initialize volume,
     * enjoyment, and name, respectively. It also initializes evRatio by diving enjoyment by volume.
     *
     * @param volume This double is used to initialize the volume instance variable.
     * @param enjoyment This double is used to initialize the enjoyment instance variable.
     * @param name This String is used to initialize the name instance variable.
     */
    public Food(double volume, double enjoyment, String name) {
        this.volume = volume;
        this.enjoyment = enjoyment;
        this.name = name;
        evRatio = enjoyment / volume;
    }

    /**
     * This method acts as the "getter" method for the volume instance variable. It doesn't take any parameters and it
     * returns volume.
     *
     * @return This method returns the volume instance variable.
     */
    public double getVolume() { return volume; }

    /**
     * This method acts as the "getter" method for the enjoyment instance variable. It doesn't take any parameters and it
     * returns enjoyment.
     *
     * @return This method returns the enjoyment instance variable.
     */
    public double getEnjoyment() { return enjoyment; }

    /**
     * This method acts as the "getter" method for the name instance variable. It doesn't take any parameters and it
     * returns name.
     *
     * @return This method returns the name instance variable.
     */
    public String getName() { return name; }

    /**
     * This method acts as the "getter" method for the evRatio instance variable. It doesn't take any parameters and it
     * returns evRatio.
     *
     * @return This method returns the evRatio instance variable.
     */
    public double getEvRatio() { return evRatio; }

    /**
     * This method acts as the "setter" method for the volume instance variable. It takes in one double as a parameter
     * and uses it to set volume. If the value of the parameter is less than zero, an error message is printed and
     * volume gets initialized to one.
     *
     * @param volume This double is used to initialize the volume instance variable.
     */
    public void setVolume(double volume) {
        if(volume > 0) {
            this.volume = volume;
        }
        else {
            System.out.println("ERROR: " + name + " volume must be greater than zero. Initializing to 1.");
            this.volume = 1;
        }
    }

    /**
     * This method acts as the "setter" method for the enjoyment instance variable. It takes in one double as a parameter
     * and uses it to set enjoyment. If the value of the parameter is less than zero, an error message is printed and
     * enjoyment gets initialized to one.
     *
     * @param enjoyment This double is used to initialize the enjoyment instance variable.
     */
    public void setEnjoyment(double enjoyment) {
        if(enjoyment >= 0) {
            this.enjoyment = enjoyment;
        }
        else {
            System.out.println("ERROR: " + name + " enjoyment must be greater than or equal to zero. Initializing to 1.");
            this.enjoyment = 1;
        }
    }

    /**
     * This method acts as the "setter" method for the evRatio instance variable. Unlike most setters, this method does
     * not take any parameters; it simply sets evRatio to the quotient of enjoyment and volume.
     */
    public void setEvRatio() { evRatio = enjoyment / volume; }

}
