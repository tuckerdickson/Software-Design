/**
 * The Arabic class provides a blueprint for creating, validating, and converting an Arabic numeral to a Roman numeral.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 8/28/2021
 */
public class Arabic {

    /**
     * This private instance variable represents the Arabic numeral input by the user in the Arabic text area
     */
    private int arabic;

    /**
     * This private instance variable represents the Roman numeral that is calculated in arabicToRoman() using the
     * Arabic numeral that is input by the user.
     */
    private String roman;

    /**
     * This is the sole constructor for the Arabic class. It takes a String as a parameter and passes that string
     * to the setArabic setter method.
     *
     * @param arabic This is a string that contains an Arabic numeral
     */
    public Arabic(String arabic) {
        setArabic(arabic);
    }

    /**
     * This is the setter method for the arabic instance variable. It takes a String as a parameter and checks to see
     * whether the string represents a valid Arabic numeral in the range 1-3999. If the string is valid, the method
     * uses it to set the arabic instance variable and then calls the arabicToRoman() method. If the string is not
     * valid, the method sets the arabic instance variable to an 'invalid' message.
     *
     * @param arabic This string contains an Arabic numeral
     */
    public void setArabic(String arabic) {
        // use the try block to attempt to convert the user-input string to an integer
        try {
            // get rid of whitespace
            arabic.trim();

            // attempt a string-to-int conversion
            this.arabic = Integer.parseInt(arabic);

            // if the conversion was successful, call the arabic to roman method
            arabicToRoman();
        }
        // if the conversion failed, set the roman instance variable to the following message
        catch (Exception exception) {
            roman = "Please enter a valid Arabic numeral.";
        }

        // if the arabic number is out of the bounds, let the user know
        if (this.arabic < 1 || this.arabic > 3999) {
            roman = "Please enter an Arabic numeral between 1 and 3,999";
        }
    }

    /**
     * This is the setter for the roman instance variable. Currently this method is never called.
     *
     * @param roman This String represents a Roman numeral that will be assigned to the roman instance variable.
     */
    public void setRoman(String roman) { this.roman = roman; }

    /**
     * This is the getter for the arabic instance variable. Currently this method is never called.
     *
     * @return This method returns an int that represents the user-input arabic instance variable.
     */
    public int getArabic() { return arabic; }

    /**
     * This is the getter method for the roman instance variable. It simply returns the roman instance variable.
     *
     * @return This method returns a String that contains the calculated roman instance variable.
     */
    public String getRoman() {
        return roman;
    }

    /**
     * This method uses the (valid) arabic instance variable to compute the corresponding Roman numeral. After
     * computing the Roman numeral, it sets the roman instance variable.
     */
    public void arabicToRoman() {
        String romanTemp = "";  // a temporary variable used for manipulation; will eventually be used to set roman I.V.
        int thousands;          // represents the number of thousands in the arabic numeral
        int hundreds;           // represents the number of hundreds in the arabic numeral
        int tens;               // represents the number of tens in the arabic numeral
        int ones;               // represents the number of ones in the arabic numeral

        // use integer division to compute the number of thousands, hundreds, tens, and ones in the given number
        thousands = arabic / 1000;
        hundreds = (arabic - (thousands * 1000)) / 100;
        tens = (arabic - (thousands * 1000) - (hundreds * 100)) / 10;
        ones = arabic % 10;

        // start by appending the appropriate number of Ms to the currently empty romanTemp string
        // one M corresponds to 1000
        for(int i = 0; i < thousands; i++){
            romanTemp += "M";
        }

        // next, move onto the hundreds section; this is a little bit trickier than the thousands
        // if hundreds is between 1 and 3, append the appropriate number of Cs
        // one C corresponds to 100
        if(hundreds >= 1 && hundreds <= 3) {
            for (int i = 0; i < hundreds; i++) {
                romanTemp += "C";
            }
        }
        // otherwise, if hundreds is equal to 4, append CD (CD represents 400)
        else if(hundreds == 4) {
            romanTemp += "CD";
        }
        // otherwise, if hundreds is between 5 and 8, do the following...
        else if(hundreds >= 5 && hundreds <= 8) {
            // one D corresponds to 500, so append a D regardless
            romanTemp += "D";
            // if hundreds is between 6 and 8, append the appropriate number of Cs after the D
            for(int i = 0; i < (hundreds - 5); i++) {
                romanTemp += "C";
            }
        }
        // otherwise, if hundreds equals 9, append CM (CM represents 900)
        else if(hundreds == 9) {
            romanTemp += "CM";
        }

        // next, move onto the tens; this is similar in structure to hundreds
        // if tens is between 1 and 3, append the appropriate number of Xs
        // one X corresponds to 10
        if(tens >= 1 && tens <= 3) {
            for (int i = 0; i < tens; i++) {
                romanTemp += "X";
            }
        }
        // otherwise, if tens is equal to 4, append XL (XL corresponds to 40
        else if(tens == 4) {
            romanTemp += "XL";
        }
        // otherwise, if tens is between 5 and 8, execute the following...
        else if(tens >= 5 && tens <= 8) {
            // L corresponds to 50, so append an L regardless
            romanTemp += "L";
            // if tens is between 6 and 8, append the appropriate number of Xs
            for(int i = 0; i < (tens - 5); i++) {
                romanTemp += "X";
            }
        }
        // otherwise, if tens equals 9, append XC (XC corresponds to 90)
        else if(tens == 9) {
            romanTemp += "XC";
        }

        // next, move onto the ones; this is similar in structure to hundreds and tens
        // if ones is between 1 and 3, append the appropriate number of Is
        // one I corresponds to 1
        if(ones >= 1 && ones <= 3) {
            for (int i = 0; i < ones; i++) {
                romanTemp += "I";
            }
        }
        // otherwise, if ones is equal to 4, append IV (IV corresponds to 4)
        else if(ones == 4) {
            romanTemp += "IV";
        }
        // otherwise, if ones is between 5 and 8, do the following...
        else if(ones >= 5 && ones <= 8) {
            // V corresponds to 5, so append a V regardless
            romanTemp += "V";
            // if ones is between 6 and 8, append the appropriate number of Is
            for(int i = 0; i < (ones - 5); i++) {
                romanTemp += "I";
            }
        }
        // otherwise, if ones equals 9, append IX (IX corresponds to 9)
        else if(ones == 9) {
            romanTemp += "IX";
        }

        // assign the completed ronTemp string to roman
        roman = romanTemp;
    }
}
