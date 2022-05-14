import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 * This class is used to test the computeEaster() method in the Easter class.
 * All Easter dates used were pulled from the following website:
 *
 * https://www.census.gov/srd/www/genhol/easter500.html
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 8/28/2021
 */
class EasterTest {

    /**
     * This method is used to test the computeEaster method in the Easter class. The method contains twenty test cases,
     * ranging year from 1600-2099.
     */
    @Test
    void testComputeEaster() {
        // Easter dates pulled from https://www.census.gov/srd/www/genhol/easter500.html

        // 1600 April 2
        Easter test = new Easter(1600);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),2);

        // 1635 April 8
        test.setYear(1635);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),8);

        // 1673 April 2
        test.setYear(1673);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),2);

        // 1702 April 16
        test.setYear(1702);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),16);

        // 1726 April 21
        test.setYear(1726);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),21);

        // 1744 April 5
        test.setYear(1744);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),5);

        // 1778 April 19
        test.setYear(1778);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),19);

        // 1803 April 10
        test.setYear(1803);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),10);

        // 1819 April 11
        test.setYear(1819);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),11);

        // 1861 March 31
        test.setYear(1861);
        assertEquals(test.getMonth(),3);
        assertEquals(test.getDay(),31);

        // 1883 March 25
        test.setYear(1883);
        assertEquals(test.getMonth(),3);
        assertEquals(test.getDay(),25);

        // 1900 April 15
        test.setYear(1900);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),15);

        // 1904 April 3
        test.setYear(1904);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),3);

        // 1921 March 27
        test.setYear(1921);
        assertEquals(test.getMonth(),3);
        assertEquals(test.getDay(),27);

        // 1943 April 25
        test.setYear(1943);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),25);

        // 1986 March 30
        test.setYear(1986);
        assertEquals(test.getMonth(),3);
        assertEquals(test.getDay(),30);

        // 1993 April 11
        test.setYear(1993);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),11);

        // 2021 April 4
        test.setYear(2021);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),4);

        // 2050 April 10
        test.setYear(2050);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),10);

        // 2099 April 12
        test.setYear(2099);
        assertEquals(test.getMonth(),4);
        assertEquals(test.getDay(),12);
    }

}