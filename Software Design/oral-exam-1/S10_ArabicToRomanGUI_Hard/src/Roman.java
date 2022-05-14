/**
 * The Roman class provides a blueprint for creating and validating a Roman numeral, as well as converting from Roman
 * numerals to Arabic numerals.
 *
 * @author Tucker Dickson
 * @version 1.0
 * @since 9/10/2021
 *
 */
public class Roman {

    /**
     * This private instance variable will represent the Roman numeral that is input by the user in the Roman text area
     */
    private String roman;

    /**
     * This private instance variable will represent the Arabic numeral that is calculated by romanToArabic(), using
     * the Roman numeral input by the user
     */
    private String arabic;

    /**
     * This is the sole constructor for the Roman class. It takes one parameter, a String called roman, and calls the
     * setter method for the roman instance variable, passing in roman as the parameter.
     *
     * @param roman This string represents a Roman numeral
     */
    public Roman(String roman) {
        setRoman(roman);
    }

    /**
     * This is the setter method for the roman instance variable. It takes a String called roman as a parameter and
     * verifies that it is a valid Roman numeral. If roman is a valid Roman numeral, the method uses it to set the
     * roman instance variable and calls the romanToArabic method to set the arabic instance variable. If roman is
     * not a valid Roman numeral, the method sets the roman instance variable to the String "Please enter a valid
     * Roman numeral between I and MMMCMXCIX"
     *
     * @param roman This String represents a Roman numeral
     */
    public void setRoman(String roman) {

        boolean isValid = true;     // the flag that will be thrown if an invalid numeral is detected
        int total = 0;              // will hold the decimal number calculated from the characters input by the user

        // get rid of whitespace and make the string upper-case
        roman = roman.trim();
        roman = roman.toUpperCase();

        String romanTemp = roman;   // a copy of roman that will be used for manipulation

        // first, find and account for any CM, CD, XC, XL, IX, or IV
        // note: these six sequences can only occur once in a valid Roman numeral
        // the first and last indexes will only be equal if that is the sole instance in the string
        if((romanTemp.indexOf("CM") != romanTemp.lastIndexOf("CM")) ||
                (romanTemp.indexOf("CD") != romanTemp.lastIndexOf("CD")) ||
                (romanTemp.indexOf("XC") != romanTemp.lastIndexOf("XC")) ||
                (romanTemp.indexOf("XL") != romanTemp.lastIndexOf("XL")) ||
                (romanTemp.indexOf("IX") != romanTemp.lastIndexOf("IX")) ||
                (romanTemp.indexOf("IV") != romanTemp.lastIndexOf("IV"))) {
            isValid = false;
        }

        // if the following else executes, that means there is either zero or one instances of the above sequences
        // find each instance, wipe it from romanTemp, and increment total accordingly
        else {
            if (romanTemp.contains("CM")) {
                total += 900;
                romanTemp = romanTemp.replaceFirst("CM", "");
            }
            if (romanTemp.contains("CD")) {
                total += 400;
                romanTemp = romanTemp.replaceFirst("CD", "");
            }
            if (romanTemp.contains("XC")) {
                total += 90;
                romanTemp = romanTemp.replaceFirst("XC", "");
            }
            if (romanTemp.contains("XL")) {
                total += 40;
                romanTemp = romanTemp.replaceFirst("XL", "");
            }
            if (romanTemp.contains("IX")) {
                total += 9;
                romanTemp = romanTemp.replaceFirst("IX", "");
            }
            if (romanTemp.contains("IV")) {
                total += 4;
                romanTemp = romanTemp.replaceFirst("IV", "");
            }
        }

        // with the odd-balls out of the way, all of the remaining characters represent constant positive decimal values
        // parse the remaining string, adding onto total accordingly for each instance of a valid character
        for (int i = 0; i < romanTemp.length(); i++) {
            if(romanTemp.charAt(i) == 'M') { total += 1000; }
            else if(romanTemp.charAt(i) == 'C') { total += 100; }
            else if(romanTemp.charAt(i) == 'D') { total += 500; }
            else if(romanTemp.charAt(i) == 'X') { total += 10; }
            else if(romanTemp.charAt(i) == 'L') { total += 50; }
            else if(romanTemp.charAt(i) == 'V') { total += 5; }
            else if(romanTemp.charAt(i) == 'I') { total += 1; }
        }

        // if total is greater than 3999, invalidate the input
        if(total > 3999) {
            isValid = false;
        }

        // now that we have a total, we can create a new arabic object, passing total as the arabic numeral
        // then, using Arabic.getRoman, we can compare the user-input roman numeral to the returned roman numeral
        Arabic arabic = new Arabic(Integer.toString(total));
        String computedRoman = arabic.getRoman();

        // if the user-input numeral and the computed numeral aren't the same, invalidate the input
        if(!computedRoman.equals(roman)){
            isValid = false;
        }

        // if isValid is still true, the user-input numeral is valid
        // set the roman instance variable, and call romanToArabic() to convert the numeral and set arabic
        if(isValid) {
            this.roman = roman;
            romanToArabic();
        }

        // otherwise, set arabic to the following message
        else {
            this.arabic = "Please enter a valid Roman numeral between I and MMMCMXCIX";
        }
    }

    /**
     * This is the setter function for the arabic instance variable. Currently, this function is never called.
     *
     * @param arabic This parameter represents and Arabic numeral that will be assigned to the arabic instance variable.
     */
    public void setArabic(String arabic) { this.arabic = arabic; }

    /**
     * This is the getter method for the arabic instance variable. It simply returns the value currently stored in
     * the arabic instance variable.
     *
     * @return This method returns a String that represents an arabic numeral
     */
    public String getArabic() {
        return this.arabic;
    }

    /**
     * This is the getter method for the roman instance variable. It simply returns the value currently stored in
     * the roman instance variable. Currently, this method is never called.
     *
     * @return This method returns a String that represents a roman numeral
     */
    public String getRoman() { return this.arabic; }

    /**
     * This method uses the roman instance variable to compute the equivalent Arabic numeral, and uses that computed
     * numeral to set the arabic instance variable.
     */
    public void romanToArabic() {
        int arabicTemp = 0;         // gets incremented as we parse the roman numeral; eventually used to set arabic
        String romanTemp = roman;   // a copy of the roman instance variable; used for manipulation

        // loop until there's nothing left in romanTemp
        while(romanTemp.length() > 0) {

            // start with the thousands (M)
            // for each 'M' detected, increment arabicTemp by 1000 and remove that specific 'M' from romanTemp
            if (romanTemp.charAt(0) == 'M') {
                arabicTemp += 1000;
                romanTemp = romanTemp.substring(1);
            }

            // go onto the hundreds
            // if there is a CM, increment arabicTemp by 900 and remove the CM
            // use the length function to avoid out_of_range exceptions
            else if ((romanTemp.length() > 1) && (romanTemp.charAt(0) == 'C') && (romanTemp.charAt(1) == 'M')) {
                arabicTemp += 900;
                romanTemp = romanTemp.substring(2);
            }

            // for each 'D' detected, increment arabicTemp by 500 and remove that specific 'D' from romanTemp
            else if (romanTemp.charAt(0) == 'D') {
                arabicTemp += 500;
                romanTemp = romanTemp.substring(1);
            }

            // if there is a CD, increment arabicTemp by 400 and remove the CD
            // use the length function to avoid out_of_range exceptions
            else if ((romanTemp.length() > 1) && (romanTemp.charAt(0) == 'C') && (romanTemp.charAt(1) == 'D')) {
                arabicTemp += 400;
                romanTemp = romanTemp.substring(2);
            }

            // for each 'C' detected, increment arabicTemp by 100 and remove that specific 'C' from romanTemp
            else if (romanTemp.charAt(0) == 'C') {
                arabicTemp += 100;
                romanTemp = romanTemp.substring(1);
            }

            // move onto the tens
            // if there is an XC, increment arabicTemp by 90 and remove the XC
            // use the length function to avoid out_of_range exceptions
            else if ((romanTemp.length() > 1) && (romanTemp.charAt(0) == 'X') && (romanTemp.charAt(1) == 'C')) {
                arabicTemp += 90;
                romanTemp = romanTemp.substring(2);
            }

            // for each 'L' detected, increment arabicTemp by 50 and remove that specific 'L' from romanTemp
            else if (romanTemp.charAt(0) == 'L') {
                arabicTemp += 50;
                romanTemp = romanTemp.substring(1);
            }

            // if there is an XL, increment arabicTemp by 40 and remove the XL
            // use the length function to avoid out_of_range exceptions
            else if ((romanTemp.length() > 1) && (romanTemp.charAt(0) == 'X') && (romanTemp.charAt(1) == 'L')) {
                arabicTemp += 40;
                romanTemp = romanTemp.substring(2);
            }

            // for each 'X' detected, increment arabicTemp by 10 and remove that specific 'X' from romanTemp
            else if (romanTemp.charAt(0) == 'X') {
                arabicTemp += 10;
                romanTemp = romanTemp.substring(1);
            }

            // move onto ones
            // if there is an IX, increment arabicTemp by 9 and remove the IX
            // use the length function to avoid out_of_range exceptions
            else if ((romanTemp.length() > 1) && (romanTemp.charAt(0) == 'I') && (romanTemp.charAt(1) == 'X')) {
                arabicTemp += 9;
                romanTemp = romanTemp.substring(2);
            }

            // for each 'V' detected, increment arabicTemp by 5 and remove that specific 'V' from romanTemp
            else if (romanTemp.charAt(0) == 'V') {
                arabicTemp += 5;
                romanTemp = romanTemp.substring(1);
            }

            // if there is an IV, increment arabicTemp by 4 and remove the IV
            // use the length function to avoid out_of_range exceptions
            else if ((romanTemp.length() > 1) && (romanTemp.charAt(0) == 'I') && (romanTemp.charAt(1) == 'V')) {
                arabicTemp += 4;
                romanTemp = romanTemp.substring(2);
            }

            // for each 'I' detected, increment arabicTemp by 1 and remove that specific 'I' from romanTemp
            else if (romanTemp.charAt(0) == 'I') {
                arabicTemp += 1;
                romanTemp = romanTemp.substring(1);
            }
        }

        // use arabicTemp to set the arabic instance variable
        arabic = Integer.toString(arabicTemp);
    }
}
