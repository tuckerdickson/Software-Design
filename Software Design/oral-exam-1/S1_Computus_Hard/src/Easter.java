/**
 * The main purpose of this class is to take in a year and compute the date that Easter occurred/will occur on
 * that year.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 8/28/2021
 */
public class Easter {

    /**
     * This private instance variable represents the day that Easter occurs on a given year; set by computeEaster()
     */
    private int day;

    /**
     * This private instance variable represents the month that Easter occurs on a given year; set by computeEaster()
     */
    private int month;

    /**
     * This private instance variable represents the year that will be used to calculate Easter; passed by the user
     */
    private int year;

    /**
     * This is the sole constructor for the Easter class. It takes an int called year as a parameter and uses it to set
     * the year instance variable. It then calls the computeEaster method to compute and set the day and month instance
     * variables.
     *
     * @param year This represents the year that will be used to compute the date of Easter. It will be used to set the
     *             year instance variable.
     */
    public Easter(int year){
        // set the year instance variable
        this.year = year;
        // call computeEaster method, which will eventually set day and month
        computeEaster();
    }

    /**
     * This method acts as the 'getter' for the day instance variable. It simply returns the int day.
     *
     * @return This function returns an int that represents the day that Easter occurs on a specific year
     */
    public int getDay(){
        return day;
    }

    /**
     * This method acts as the 'getter' for the month instance variable. It simply returns the int month.
     *
     * @return This function returns an int that represents the month that Easter occurs on a specific year
     */
    public int getMonth(){
        return month;
    }

    /**
     * This method acts as the 'getter' for the year instance variable. It simply returns the int year.
     *
     * @return This function returns an int that represents a year, previously set by the user.
     */
    public int getYear(){
        return year;
    }

    /**
     * This is the setter method for the day instance variable. This method is not used currently.
     *
     * @param day This parameter represents the day that will be used to set the day instance variable
     */
    public void setDay(int day) { this.day = day; }

    /**
     * This is the setter method for the month instance variable. This method is not used currently.
     *
     * @param month This parameter represents the month that will be used to set the month instance variable
     */
    public void setMonth(int month) { this.month = month; }

    /**
     * This method acts as the 'setter' for the year instance variable, as well as day and month indirectly. The
     * method takes in an int called year as a parameter and uses it to set the year instance variable. It then calls
     * the computeEaster(), which uses the year instance variable to compute the date of Easter on that year, and set
     * the day and month instance variables.
     *
     * @param year This parameter represents the year that the user wants to compute the date of Easter for
     */
    public void setYear(int year){
        this.year = year;
        computeEaster();
    }

    /**
     * This method uses the Meeus/Jones/Butcher algorithm to calculate the date of Easter given a specific year. It
     * uses the year instance variable to do this, and sets the day and month instance variables once it's computed the
     * date.
     */
    public void computeEaster(){
        // calculate the date of Easter using the Meeus/Jones/Butcher algorithm

        // decided to allow integer division to truncate any division operations instead of using a floor method
        int a = year % 19;
        int b = year / 100;
        int c = year % 100;
        int d = b / 4;
        int e  = b % 4;
        int f = (b + 8) / 25;
        int g = (b - f + 1) / 3;
        int h = ((19 * a) + b - d - g + 15) % 30;
        int i = c / 4;
        int k = c % 4;
        int l = (32 + (2 * e) + (2 * i) - h - k) % 7;
        int m = (a + (11 * h) + (22 * l)) / 451;
        // set day and month
        month = ((h + l - (7 * m) + 114) / 31);
        day = ((h + l - (7 * m) + 114) % 31) + 1;
    }

    /**
     * This method simply prints uses the day, month, and year instance variables to print out the date of Easter,
     * following the format below:
     *
     * Easter: Month Day, Year
     */
    public void printEaster(){
        // initialize array object with months (for output later)
        String[] monthArray = new String[] { "", "January ", "February", "March ", "April ", "May ", "June ", "July ",
                "August ", "September ", "October ", "November ", "December " };

        System.out.println("Easter: " + monthArray[month] + day + ", " + year + "\n");
    }
}
