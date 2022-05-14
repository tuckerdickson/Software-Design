import java.util.ArrayList;
import java.util.Collections;
import java.util.InputMismatchException;
import java.util.Scanner;

/**
 * The Thanksgiving class acts as the driver class for this program. It is also responsible for computing the optimal
 * enjoyment. It contains two methods, the main method and optimizeEnjoyment, and no private instance variables.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/23/21
 */
public class Thanksgiving {

    /**
     * This method is responsible for computing and returning the optimal enjoyment given an ArrayList of Food objects.
     * It takes in two parameters, the list of Food objects and the stomach capacity of the user. It sorts the list of
     * Foods based on the enjoyment-to-volume ratios of the individual Food objects (in descending order), and then
     * iterates over the sorted list, adding to the optimal enjoyment as long as there is volume to fill. It then returns
     * the optimal enjoyment.
     *
     * @param foods This ArrayList of Food objects contains the seven different foods present at the Thanksgiving dinner,
     *              each with their corresponding volumes and enjoyments already set.
     * @param capacity This double represents the capacity of the users stomach. This is the space to be filled.
     * @return This method returns a double representing the optimal enjoyment given the specific Foods list and
     *          capacity
     */
    public static double optimizeEnjoyment(ArrayList<Food> foods, double capacity) {

        // initialie optimalEnjoyment to zero so that we can add to it later
        double optimalEnjoyment = 0;

        // sort the list based on the evRatio of the Food objects in descending order (highest evRatio at front)
        Collections.sort(foods, new FoodComparator());

        // iterate over all of the Food objects in the list
        for(Food f : foods) {

            // loop while the capacity is greater than or equal to the volume of the current Food object
            while(capacity >= f.getVolume()) {

                // update the capacity by subtracting off the volume of the current Food
                capacity = capacity - f.getVolume();

                // update the optimalEnjoyment by adding on the enjoyment of the current Food
                optimalEnjoyment = optimalEnjoyment + f.getEnjoyment();

                //System.out.println("Consumed 1 " + f.getName() + "\n\tRemaining capacity: " + capacity);
            }
        }
        return optimalEnjoyment;
    }

    /**
     * This method acts as the driver of this program. It is responsible for creating seven Food objects, using those
     * objects to construct an ArrayList, getting user input to obtain the capacity, as well as the volume and enjoyment
     * for each of the seven food objects, making a call to optimizeEnjoyment, and then printing out the value returned.
     *
     * @param args This is the default argument for any main program in Java.
     */
    public static void main(String[] args) {

        // create a new scanner to get input from the user
        Scanner scanner = new Scanner(System.in);

        // create a new Food object for each food at the dinner
        // initial values for volume and enjoyment don't matter as they will be reset later on
        Food turkey = new Food(1,1,"turkey");
        Food pumpkinPie = new Food(1,1,"pumpkin pie");
        Food mashedPotatoes = new Food(1,1,"mashed potatoes");
        Food gravy = new Food(1,1,"gravy");
        Food stuffing = new Food(1,1,"stuffing");
        Food cranberries = new Food(1,1,"cranberries");
        Food casserole = new Food(1,1,"casserole");

        // create a new ArrayList of Food objects to contain the seven food objects
        ArrayList<Food> listOfFoods = new ArrayList<>();

        // add the seven food objects to the ArrayList
        listOfFoods.add(turkey);
        listOfFoods.add(pumpkinPie);
        listOfFoods.add(mashedPotatoes);
        listOfFoods.add(gravy);
        listOfFoods.add(stuffing);
        listOfFoods.add(cranberries);
        listOfFoods.add(casserole);


        // create temporary storage variables for capacity, volume and enjoyment
        double capacity = 0;
        double volume;
        double enjoyment;

        // try to read a double into capacity; print an error message and terminate the program if an
        // InputMismatchException is thrown
        System.out.print("\nPlease enter your stomach capacity: ");
        try {
            capacity = scanner.nextDouble();
        } catch (InputMismatchException inputMismatchException) {
            System.out.println("ERROR: INVALID INPUT");
            System.exit(1);
        }

        // make sure capacity is greater than or equal to zero
        if (capacity < 0) { System.exit(1); }

        // iterate over each of the seven Food objects in our list
        for(Food food : listOfFoods) {

            // try to read a double into volume; use it to set the volume of the current Food object
            System.out.print("\nPlease enter volume of " + food.getName() + ": ");
            try {
                volume = scanner.nextDouble();
                food.setVolume(volume);
            // print an error message and terminate the program if an InputMismatchException is thrown
            } catch (InputMismatchException inputMismatchException) {
                System.out.println("ERROR: INVALID INPUT");
                System.exit(1);
            }

            // try to read a double into enjoyment; use it to set the enjoyment of the current Food object
            System.out.print("Please enter enjoyment of " + food.getName() + ": ");
            try {
                enjoyment = scanner.nextDouble();
                food.setEnjoyment(enjoyment);
            // print an error message and terminate the program if an InputMismatchException is thrown
            } catch (InputMismatchException inputMismatchException) {
                System.out.println("ERROR: INVALID INPUT");
                System.exit(1);
            }

            // call the Food.setEvRatio method for the current Food object
            // this will use the volume and enjoyment to set the evRatio
            food.setEvRatio();
        }

        // make a call to optimizeEnjoyment, passing the listOfFoods and capacity; print results
        double maxEnjoyment = optimizeEnjoyment(listOfFoods, capacity);
        System.out.println("The optimal amount of enjoyment is: " + maxEnjoyment);
    }
}

