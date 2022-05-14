import org.junit.jupiter.api.Test;
import java.util.ArrayList;
import static org.junit.jupiter.api.Assertions.*;

/**
 * This class is responsible for testing the optimizeEnjoyment method in the Thanksgiving class. It contains five methods,
 * each a call to optimizeEnjoyment but with different values for capacity, volume, and enjoyment.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 10/24/21
 */
class ThanksgivingTest {

    /**
     * This method is the first of five tests for Thanksgiving.optimizeEnjoyment(). It does not take any parameters and
     * doesn't return anything.
     */
    @Test
    void testOptimizeEnjoyment1() {

        double capacity = 20;

        Food turkey = new Food(3,4,"turkey");
        Food pumpkinPie = new Food(2,2,"pumpkin pie");
        Food mashedPotatoes = new Food(4,5,"mashed potatoes");
        Food gravy = new Food(10,10,"gravy");
        Food stuffing = new Food(2,3,"stuffing");
        Food cranberries = new Food(7,5,"cranberries");
        Food casserole = new Food(12,17,"casserole");

        ArrayList<Food> listOfFoods = new ArrayList<>();

        listOfFoods.add(turkey);
        listOfFoods.add(pumpkinPie);
        listOfFoods.add(mashedPotatoes);
        listOfFoods.add(gravy);
        listOfFoods.add(stuffing);
        listOfFoods.add(cranberries);
        listOfFoods.add(casserole);

        assertEquals(30,Thanksgiving.optimizeEnjoyment(listOfFoods,capacity));
    }

    /**
     * This method is the second of five tests for Thanksgiving.optimizeEnjoyment(). It does not take any parameters and
     * doesn't return anything.
     */
    @Test
    void testOptimizeEnjoyment2() {

        double capacity = 41;

        Food turkey = new Food(3,4,"turkey");
        Food pumpkinPie = new Food(2,2,"pumpkin pie");
        Food mashedPotatoes = new Food(4,5,"mashed potatoes");
        Food gravy = new Food(1,1,"gravy");
        Food stuffing = new Food(2,3,"stuffing");
        Food cranberries = new Food(10,14,"cranberries");
        Food casserole = new Food(15,24,"casserole");

        ArrayList<Food> listOfFoods = new ArrayList<>();

        listOfFoods.add(turkey);
        listOfFoods.add(pumpkinPie);
        listOfFoods.add(mashedPotatoes);
        listOfFoods.add(gravy);
        listOfFoods.add(stuffing);
        listOfFoods.add(cranberries);
        listOfFoods.add(casserole);

        assertEquals(64,Thanksgiving.optimizeEnjoyment(listOfFoods,capacity));
    }

    /**
     * This method is the third of five tests for Thanksgiving.optimizeEnjoyment(). It does not take any parameters and
     * doesn't return anything.
     */
    @Test
    void testOptimizeEnjoyment3() {

        double capacity = 7;

        Food turkey = new Food(3,5,"turkey");
        Food pumpkinPie = new Food(4,12,"pumpkin pie");
        Food mashedPotatoes = new Food(1,1,"mashed potatoes");
        Food gravy = new Food(2,5,"gravy");
        Food stuffing = new Food(1,1,"stuffing");
        Food cranberries = new Food(2,2,"cranberries");
        Food casserole = new Food(3,3,"casserole");

        ArrayList<Food> listOfFoods = new ArrayList<>();

        listOfFoods.add(turkey);
        listOfFoods.add(pumpkinPie);
        listOfFoods.add(mashedPotatoes);
        listOfFoods.add(gravy);
        listOfFoods.add(stuffing);
        listOfFoods.add(cranberries);
        listOfFoods.add(casserole);

        assertEquals(18,Thanksgiving.optimizeEnjoyment(listOfFoods,capacity));
    }

    /**
     * This method is the fourth of five tests for Thanksgiving.optimizeEnjoyment(). It does not take any parameters and
     * doesn't return anything.
     */
    @Test
    void testOptimizeEnjoyment4() {

        double capacity = 9;

        Food turkey = new Food(3,6,"turkey");
        Food pumpkinPie = new Food(4,9,"pumpkin pie");
        Food mashedPotatoes = new Food(1,0.5,"mashed potatoes");
        Food gravy = new Food(2,4,"gravy");
        Food stuffing = new Food(1,1,"stuffing");
        Food cranberries = new Food(2,2,"cranberries");
        Food casserole = new Food(3,3,"casserole");

        ArrayList<Food> listOfFoods = new ArrayList<>();

        listOfFoods.add(turkey);
        listOfFoods.add(pumpkinPie);
        listOfFoods.add(mashedPotatoes);
        listOfFoods.add(gravy);
        listOfFoods.add(stuffing);
        listOfFoods.add(cranberries);
        listOfFoods.add(casserole);

        assertEquals(19,Thanksgiving.optimizeEnjoyment(listOfFoods,capacity));
    }

    /**
     * This method is the fifth of five tests for Thanksgiving.optimizeEnjoyment(). It does not take any parameters and
     * doesn't return anything.
     */
    @Test
    void testOptimizeEnjoyment5() {

        double capacity = 5;

        Food turkey = new Food(6,7,"turkey");
        Food pumpkinPie = new Food(7,8,"pumpkin pie");
        Food mashedPotatoes = new Food(8,9,"mashed potatoes");
        Food gravy = new Food(9,10,"gravy");
        Food stuffing = new Food(10,11,"stuffing");
        Food cranberries = new Food(11,12,"cranberries");
        Food casserole = new Food(12,13,"casserole");

        ArrayList<Food> listOfFoods = new ArrayList<>();

        listOfFoods.add(turkey);
        listOfFoods.add(pumpkinPie);
        listOfFoods.add(mashedPotatoes);
        listOfFoods.add(gravy);
        listOfFoods.add(stuffing);
        listOfFoods.add(cranberries);
        listOfFoods.add(casserole);

        assertEquals(0,Thanksgiving.optimizeEnjoyment(listOfFoods,capacity));
    }
}