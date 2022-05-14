//----------------------------------------------------------
// Programmer: Tucker Dickson
// Date: February 2, 2021
// Name: main.cpp
// Description: This program simulates a Blackjack dice game
//              between a user and the computer
// ----------------------------------------------------------

#include <iostream>
#include <ctime>

using namespace std;

//----------------------------------------------------------
// Description: This function prints out the game
//              instructions to the console
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void Instructions () {
    cout << "The goal of this game is to be the player who rolls" << endl;
    cout << "the highest number without exceeding twenty-one." << endl;
    cout << endl;
    cout << "The player gets to roll first. They will start by rolling two dice" << endl;
    cout << "until they either elect to stop rolling, or reach seventeen." << endl;
    cout << "Each time a player rolls, the sum of the two dice is added to their score." << endl;
    cout << endl;
    cout << "If the player reaches seventeen, they must begin rolling with only one die." << endl;
    cout << "The player continues to roll until they either elect to stop, or reach twenty-one." << endl;
    cout << "At that point, the dealer begins their turn." << endl;
    cout << endl;
    cout << "If the player exceeds twenty-one, they are assigned a score of zero and the dealer wins." << endl;
    cout << "If the player elects to stop rolling before they reach twenty-one" << endl;
    cout << "then the dealer must top their score." << endl;
    cout << "The player with the highest score in the end wins." << endl;
    cout << endl;

}

//----------------------------------------------------------
// Description: This function prints a pictographic
//              representation of the first face of a die
//              to the console
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void OneFace () {
    cout << " -----" << endl;
    cout << "|     |" << endl;
    cout << "|  o  |" << endl;
    cout << "|     |" << endl;
    cout << " -----" << endl;
}

//----------------------------------------------------------
// Description: This function prints a pictographic
//              representation of the second face of a die
//              to the console
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void TwoFace () {
    cout << " -----" << endl;
    cout << "| o   |" << endl;
    cout << "|     |" << endl;
    cout << "|   o |" << endl;
    cout << " -----" << endl;
}

//----------------------------------------------------------
// Description: This function prints a pictographic
//              representation of the third face of a die
//              to the console
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void ThreeFace () {
    cout << " -----" << endl;
    cout << "| o   |" << endl;
    cout << "|  o  |" << endl;
    cout << "|   o |" << endl;
    cout << " -----" << endl;
}

//----------------------------------------------------------
// Description: This function prints a pictographic
//              representation of the fourth face of a die
//              to the console
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void FourFace () {
    cout << " -----" << endl;
    cout << "| o o |" << endl;
    cout << "|     |" << endl;
    cout << "| o o |" << endl;
    cout << " -----" << endl;
}

//----------------------------------------------------------
// Description: This function prints a pictographic
//              representation of the fifth face of a die
//              to the console
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void FiveFace () {
    cout << " -----" << endl;
    cout << "| o o |" << endl;
    cout << "|  o  |" << endl;
    cout << "| o o |" << endl;
    cout << " -----" << endl;
}

//----------------------------------------------------------
// Description: This function prints a pictographic
//              representation of the sixth face of a die
//              to the console
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void SixFace () {
    cout << " -----" << endl;
    cout << "| o o |" << endl;
    cout << "| o o |" << endl;
    cout << "| o o |" << endl;
    cout << " -----" << endl;
}

//----------------------------------------------------------
// Description: This function takes in an integer (the
//              'roll') and decides what face of the die
//              to print out
// Inputs: int rollNumber - this is the integer produced
//                          in main when the rand() function
//                          is called to simulate a roll
// Outputs: None
// ----------------------------------------------------------
void CallFace (int rollNumber) {
    if (rollNumber == 1) {
        OneFace();
    }
    else if (rollNumber == 2) {
        TwoFace();
    }
    else if (rollNumber == 3) {
        ThreeFace();
    }
    else if (rollNumber == 4) {
        FourFace();
    }
    else if (rollNumber == 5) {
        FiveFace();
    }
    else if (rollNumber == 6) {
        SixFace();
    }
}

//----------------------------------------------------------
// Description: This function simulates the roll of a
//              fair six-sided die, passes the roll to
//              CallFace(), and then returns the roll to
//              main
// Inputs: int die - This functions only parameter is an
//                  integer called die, which is what
//                  stores the roll
// Outputs: int die - This function returns an integer
//                  called die to main, which stores the
//                  value (1-6) of the roll
// ----------------------------------------------------------
int RollDie (int die) {
    die = (rand() % 6) + 1;
    CallFace(die);
    return die;
}

//----------------------------------------------------------
// Description: This function prompts the user to 'Press
//              enter to continue' and then uses a
//              cin.ignore() to temporarily halt the program
//              until the user presses enter
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void PressEnterToContinue () {
    cout << "\t\tPress enter to continue...";
    cin.ignore();
    cout << endl;
}

//----------------------------------------------------------
// Description: This function prints out a decorative
//              welcome message to the console
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void DecorativeWelcome () {
    cout << endl;
    cout << "/-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-\\" << endl;
    cout << "| Welcome to Tucker's BlackJack Dice Game! |" << endl;
    cout << "\\-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-/" << endl;
    cout << endl;
    PressEnterToContinue();
}

int main() {

    // initialize GameState
    enum GameState {
        PLAYER_TURN, DEALER_TURN, COMPARE_SCORES, CONTINUE, STOP_TURN, GAME_OVER
    };

    // set rand() seed
    srand(10);

    // print decorative welcome
    DecorativeWelcome();

    // print instructions
    Instructions();

    // prompt user for input and enter the outer game loop
    cout << "Do you want to play? (y/n): ";
    GameState gameVal = PLAYER_TURN;
    while (gameVal != GAME_OVER) {

        char input;
        cin >> input;
        cin.ignore();

        // if input is 'y', proceed to play the game
        if (input == 'y') {

            gameVal = PLAYER_TURN;

            cout << endl;
            cout << "Great! I'll let you go first." << endl;
            cout << "Go ahead and roll!" << endl;
            cout << endl;

            PressEnterToContinue();

            int playerOneScore = 0;     // variable that holds the player's score
            int playerTwoScore = 0;     // variable that holds the dealer's score
            int rollOne = 0;            // variable that holds the value of the first die
            int rollTwo = 0;            // variable that holds the value of the second die

            // simulate rolls of both dice with RollDie(), then add the sum to player's score
            rollOne = RollDie(rollOne);
            rollTwo = RollDie(rollTwo);
            playerOneScore += rollOne + rollTwo;

            // ask the player if they want to roll again
            cout << endl;
            cout << "You rolled a " << rollOne + rollTwo << "!" << endl;
            cout << "That brings your score to " << playerOneScore << "..." << endl;
            cout << "Do you want to roll again? (y/n): ";

            // the player's turn loop
            while (gameVal == PLAYER_TURN) {

                // get input from player
                cin >> input;
                cin.ignore();
                cout << endl;

                // if input is 'y', begin the game with the player's turn
                if (input == 'y') {
                    // if player score is less than 17, roll both dice and add to player's score
                    if (playerOneScore > 0 && playerOneScore < 17) {
                        rollOne = RollDie(rollOne);
                        rollTwo = RollDie(rollTwo);
                        playerOneScore += rollOne + rollTwo;

                        cout << endl;
                        cout << "You rolled a " << rollOne + rollTwo << "!" << endl;
                        cout << "That brings your score to " << playerOneScore << "!" << endl;
                    }
                    // if player score is 17 or greater, roll one die
                    else if (playerOneScore >= 17) {
                        rollOne = RollDie(rollOne);
                        playerOneScore += rollOne;

                        cout << endl;
                        cout << "You rolled a " << rollOne << "!" << endl;
                        cout << "That brings your score to " << playerOneScore << "!" << endl;
                        cout << endl;
                    }

                    // if player score is 21, end player turn and start dealer turn
                    if (playerOneScore == 21) {
                        cout << "Well done!" << endl;
                        cout << endl;
                        gameVal = DEALER_TURN;
                    }
                    // if player score is greater than 21, set player score to zero and end game
                    else if (playerOneScore > 21 ) {
                        cout << endl;
                        cout << "You exceeded 21!" << endl;
                        cout << "You're score is now 0 so I win this one." << endl;
                        cout << "Good game!" << endl;
                        cout << endl;

                        playerOneScore = 0;
                        gameVal = COMPARE_SCORES;
                    }
                    // if player score is less than 21, ask if they want to roll again
                    else {
                        cout << "Do you want to roll again? (y/n): ";
                    }
                }
                // if player inputs 'n', end the player's turn and start the dealer's turn
                else if (input == 'n') {
                    gameVal = DEALER_TURN;
                }
                // if input is neither 'y' or 'n', prompt the user to try again
                else if (input != 'y' && input != 'n') {
                    cout << "Invalid response... Try again (y/n):";
                }
            }

            // dealer turn loop
            while (gameVal == DEALER_TURN) {
                cout << endl;
                cout << "Now it's my turn!" << endl;

                playerTwoScore = 0;
                // initialize GameState rollVal to decide when to stop rolling
                GameState rollVal = CONTINUE;

                while (rollVal != STOP_TURN) {
                    cout << endl;
                    PressEnterToContinue();

                    // if dealer score is less than 17, roll both dice and add to dealer score
                    if (playerTwoScore >= 0 && playerTwoScore < 17) {
                        rollOne = RollDie(rollOne);
                        rollTwo = RollDie(rollTwo);
                        playerTwoScore += rollOne + rollTwo;

                        cout << endl;
                        cout << "I rolled a " << rollOne + rollTwo << "!" << endl;
                        cout << "That brings my score to " << playerTwoScore << "..." << endl;
                        cout << endl;
                    }
                    // if dealer score is 17 or greater, roll one die and add to dealer score
                    else if (playerTwoScore >= 17) {
                        rollOne = RollDie(rollOne);
                        playerTwoScore += rollOne;

                        cout << endl;
                        cout << "I rolled a " << rollOne << "!" << endl;
                        cout << "That brings my score to " << playerTwoScore << "..." << endl;
                        cout << endl;
                    }

                    if (playerTwoScore < 21) {
                        // if dealer score is less than 21 and less or equal to player score, roll again
                        if (playerTwoScore <= playerOneScore) {
                            cout << "I'd better roll again..." << endl;
                        }
                        // if dealer score is less than 21 and greater than player score, end dealer turn
                        else if (playerTwoScore > playerOneScore) {
                            cout << "I think I'll stop there." << endl;
                            gameVal = COMPARE_SCORES;
                            rollVal = STOP_TURN;
                        }
                    // if dealer score is greater than 21, set dealer score to zero and end turn
                    }
                    else if (playerTwoScore > 21) {
                        cout << endl;
                        cout << "Shoot! I've gone over 21!" << endl;
                        cout << "Looks like you win this one!" << endl;
                        cout << "Good game!" << endl;
                        cout << endl;

                        playerTwoScore = 0;
                        gameVal = COMPARE_SCORES;
                        rollVal = STOP_TURN;
                    }
                    else if (playerTwoScore == 21) {
                        gameVal = COMPARE_SCORES;
                        rollVal = STOP_TURN;
                    }
                }
            }

            // GameState after both player and dealer have rolled (or player scored over 21...)
            if (gameVal == COMPARE_SCORES) {
                if (playerOneScore != 0 && playerTwoScore != 0) {
                    if (playerOneScore > playerTwoScore) {
                        cout << endl;
                        cout << "Your final score is " << playerOneScore << endl;
                        cout << "and my final score is " << playerTwoScore << "." << endl;
                        cout << "It looks like this game belongs to you. Well done!" << endl;
                        cout << endl;
                    }
                    else if ((playerOneScore < playerTwoScore) || (playerOneScore == 21 && playerTwoScore == 21)) {
                        cout << endl;
                        cout << "Your final score is " << playerOneScore << endl;
                        cout << "and my final score is " << playerTwoScore << "." << endl;
                        cout << "I win this one. Better luck next time!" << endl;
                        cout << endl;
                    }
                    else {
                        cout << endl;
                        cout << "Your final score is " << playerOneScore << endl;
                        cout << "and my final score is " << playerTwoScore << "." << endl;
                        cout << "It appears this one is a draw. Good game!" << endl;
                        cout << endl;
                    }
                }
            }
            cout << "Do you want to play again? (y/n): ";
        }
        // if input is 'n', end game
        else if (input == 'n') {
            // end the game
            cout << "Goodbye!" << endl;
            cout << endl;
            gameVal = GAME_OVER;
        }
        // if input is neither 'y' nor 'n', prompt user to try again
        else if (input != 'y' && input != 'n') {
            cout << "Invalid response... Try again (y/n):";
        }
    }
    return 0;
}