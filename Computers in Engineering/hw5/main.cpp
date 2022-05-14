#include <SFML/Graphics.hpp>
#include <SFML/Audio.hpp>
#include <time.h>
#include <string>
#include <vector>

using namespace sf;

const int boardHeight = 25;                     // height of the game board
const int boardWidth = 40;                      // width of the game board

int grid[boardHeight][boardWidth] = {0};       // a value of zero means a blank square
int tileSize = 18;                             // tile size

class Enemy {
public:
    // Enemy constructor
    Enemy();

    // getters
    int getX() const;
    int getY() const;
    int getDx() const;
    int getDy() const;

    // setters
    void setX(const int x);
    void setY(const int y);
    void setDx(const int dx);
    void setDy(const int dy);

    void move();

private:
    int x;      // x location of the enemy
    int y;      // y location of the enemy
    int dx;     // speed in the x direction of the enemy
    int dy;     // speed in the y direction of the enemy
};

// Enemy constructor
Enemy::Enemy() {
    x = y = 300;            // all spinners start at location (300,300)
    dx = 4 - rand() % 8;    // set random speed of the enemy in the x direction
    dy = 4 - rand() % 8;    // set random speed of the enemy in the y direction
}

// getters
int Enemy::getX() const {
    return x;
}
int Enemy::getY() const {
    return y;
}
int Enemy::getDx() const {
    return dx;
}
int Enemy::getDy() const {
    return dy;
}

// setters
void Enemy::setX(const int x) {
    if (x >= 0 && x <= boardWidth) {
        this->x = x;
    }
}
void Enemy::setY(const int y) {
    if (y >= 0 && x <= boardHeight) {
        this->y = y;
    }
}
void Enemy::setDx(const int dx) {
    if (dx >= -3 && dx <= 4) {
        this->dx = dx;
    }
}
void Enemy::setDy(const int dy) {
    if (dy >= -3 && dy <= 4) {
        this->dy = dy;
    }
}

//----------------------------------------------------------
// Description: This function is responsible for making the
//              Enemy objects move. Once the enemy reaches a
//              boundary, the x and y directions of travel
//              are reversed
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void Enemy::move() {
    x += dx;
    if (grid[y / tileSize][x / tileSize] == 1) {        // if enemy hits blue tile traveling in x...
        dx = -dx;                                       // reverse x direction
        x += dx;
    }
    y += dy;
    if (grid[y / tileSize][x / tileSize] == 1) {
        dy = -dy;                                       // if enemy hits blue tile traveling in y...
        y += dy;                                        // reverse y direction
    }
}

//----------------------------------------------------------
// Description: Flood fill a region with -1 if the input
//              x,y is zero. Ultimately used to figure
//              out if enemy is enclosed in a rectangular
//              region.
//              (Recursive function)
// Inputs: This function takes two inputs...
//          int y: stores the y position of an enemy
//          int x: stores the x position of an enemy
// Outputs: None
// ----------------------------------------------------------
void drop(int y, int x) {
    // base case (simplest case) we know what the answer is without doing any work
    if (grid[y][x] == 0) {          // if the grid index is empty...
        grid[y][x] = -1;            // fill it in with -1 (no color - used to fill in regions that are 0)
    }

    // recursive step. breaks problem into simpler problems and calls drop on those problems
    if (grid[y - 1][x] == 0) {      // check if north neighbor is a 0
        drop(y - 1, x);
    }

    if (grid[y + 1][x] == 0) {      // check if south neighbor is a 0
        drop(y + 1, x);
    }

    if (grid[y][x - 1] == 0) {      // check if west neighbor is a 0
        drop(y, x - 1);
    }

    if (grid[y][x + 1] == 0) {      // check if east neighbor is a 0
        drop(y, x + 1);
    }
}

int main() {
    srand(time(0));

    // instantiate window object, name it 'Xonix Game!', set width and height
    RenderWindow window(VideoMode(boardWidth * tileSize, boardHeight * tileSize), "Xonix Game!");
    window.setFramerateLimit(60);   // max frame rate is 60 fps

    // instantiate texture objects for tiles, gameover and enemy
    Texture tilesTexture, gameoverTexture, enemyTexture;

    tilesTexture.loadFromFile("images/tiles.png");              // load tiles image from file
    if (!tilesTexture.loadFromFile("images/tiles.png")) {       // check for failure
        return EXIT_FAILURE;
    }

    gameoverTexture.loadFromFile("images/gameover.png");            // load gameover image from file
    if (!gameoverTexture.loadFromFile("images/gameover.png")) {     // check for failure
        return EXIT_FAILURE;
    }

    enemyTexture.loadFromFile("images/enemy.png");              // load enemy image from file
    if (!enemyTexture.loadFromFile("images/enemy.png")) {       // check for failure
        return EXIT_FAILURE;
    }

    /*
    music from https://www.zapsplat.com/?s=arcade+music&post_type=music&sound-effect-category-id=
    Standard License: "This License grants a single user the right to download and use our sound effects and music"
    */
    Music music;                                                        // instantiate music object
    if (!music.openFromFile("sounds/gameMusic.ogg")) {          // check if the file opened
        return EXIT_FAILURE;                                            // error if it didn't
    }

    /*
    sound effect from https://www.zapsplat.com/music/arcade-game-falling-tone-1/
    Standard License: "This License grants a single user the right to download and use our sound effects and music"
    */
    SoundBuffer buffer;                                                 // instantiate SoundBuffer object
    if (!buffer.loadFromFile("sounds/gameOver.ogg")) {          // check if file successfully loaded
        return EXIT_FAILURE;                                            // error if it didn't
    }
    Sound sound;
    sound.setBuffer(buffer);

    /*
    font from https://www.1001freefonts.com/planet-kosmos.font
    License: "free for personal use"
    */
    Font font;                                                      // instantiate font object
    if (!font.loadFromFile("fonts/font.TTF")) {             // check if file successfully loads
        return EXIT_FAILURE;                                        // if it doesn't, error
    }

    // this is the text that will be used to display the running score in the top left corner
    Text scoreText;
    scoreText.setFont(font);                                             // select the font
    scoreText.setString("Score: 0");                              // set the string
    scoreText.setFillColor(Color::Red);                                  // set the color
    scoreText.setCharacterSize(20);                                 // set size
    scoreText.setPosition(22,12);                                  // set origin

    /*
    EXTRA PART
    This is the text that will be used to display a list of the 5 highest scores in the given program run
    each time a game ends.
    */
    Text highScore;
    highScore.setFont(font);
    highScore.setFillColor(Color::White);
    highScore.setCharacterSize(25);
    highScore.setPosition(225,150);

    /*
    EXTRA PART
    This is just a rectangle that is placed behind the list of high scores to make it easier to read against
    an all blue background.
    */
    RectangleShape bigRectangle(Vector2f(300.f,275.f));
    bigRectangle.setPosition(200, 140);
    bigRectangle.setFillColor(Color::Red);
    bigRectangle.setOutlineThickness(7.f);
    bigRectangle.setOutlineColor(Color::Black);

    /*
    EXTRA PART
    This is just a rectangle that is placed behind the running score counter to make it easier to read against
    an all blue background.
    */
    RectangleShape smallRectangle(Vector2f(162.f, 18.f));
    smallRectangle.setPosition(18,18);
    smallRectangle.setFillColor(Color::Black);


    // instantiate sprite objects for tiles, game over and enemy
    Sprite sTile(tilesTexture), sGameover(gameoverTexture), sEnemy(enemyTexture);
    sGameover.setPosition(100, 0);
    sEnemy.setOrigin(20, 20);

    int enemyCount = 4;
    Enemy enemy[10];

    bool Game = true;
    int x = 0, y = 0;
    int dx = 0, dy = 0;                              // delta x and y for the player motion
    float timer = 0, delay = 0.07;                   // this line and the next control how fast sprites move
    float timer2 = 0, delay2 = 0.5;
    int score = 0;
    std::vector<int>highScores = {0,0,0,0,0};
    Clock clock1;
    Clock clock2;

    for (int i = 0; i < boardHeight; i++) {
        for (int j = 0; j < boardWidth; j++) {
            if (i == 0 || j == 0 || i == boardHeight - 1 || j == boardWidth - 1) {
                grid[i][j] = 1;                     // sets the border of the game board
            }
        }
    }

    music.setLoop(true);                        // make the music loop
    music.play();                                    // start playing music

    while (window.isOpen()) {                       // outer "game" loop; only breaks when window is closed
        float time = clock1.getElapsedTime().asSeconds();
        float time2 = clock2.getElapsedTime().asMilliseconds();
        clock1.restart();
        clock2.restart();
        timer += time;
        timer2+= time2;
        score = 0;

        Event event;                                // instantiate Event object
        while (window.pollEvent(event)) {       // 'event' loop; checks if an event is occurring
            if (event.type == Event::Closed) {      // check if the event type is Closed
                window.close();                    // close the window
            }

            if (event.type == Event::KeyPressed) {              // checks if a key is pressed
                if (event.key.code == Keyboard::Escape) {       // checks if the escape key is pressed
                    sound.stop();                               // stop the game over sound effect
                    music.play();                               // start the game music again
                    for (int i = 1; i < boardHeight - 1; i++) {
                        for (int j = 1; j < boardWidth - 1; j++) {
                            grid[i][j] = 0;                     // resets the game board to all zeros (restarts the game)
                        }
                    }

                    x = 10;     // reset player coordinates to (10, 0) after resetting game board
                    y = 0;
                    Game = true;
                }
            }
        }

        if (Keyboard::isKeyPressed(Keyboard::Left)) {       // if left key is pressed, move left 1
            dx = -1;
            dy = 0;
        }

        if (Keyboard::isKeyPressed(Keyboard::Right)) {      // if right key is pressed, move right 1
            dx = 1;
            dy = 0;
        }

        if (Keyboard::isKeyPressed(Keyboard::Up)) {        // if up key is pressed, move up 1
            dx = 0;
            dy = -1;
        }

        if (Keyboard::isKeyPressed(Keyboard::Down)) {       // if down key is pressed, move down 1
            dx = 0;
            dy = 1;
        }

        if (Game) {


            if (timer > delay) {                // check if time elapsed is greater than delay
                x += dx;                        // if this is the case, add delta x and delta y to x and y respectively
                y += dy;

                if (x < 0) {                    // set a left bound on the game board
                    x = 0;
                }

                if (x > boardWidth - 1) {      // set a right bound on the game board
                    x = boardWidth - 1;
                }

                if (y < 0) {                    // set a top bound on the game board
                    y = 0;
                }

                if (y > boardHeight - 1) {      // set a bottom bound on the game board
                    y = boardHeight - 1;
                }

                if (grid[y][x] == 2) {          // if player moves onto a green rectangle...
                    Game = false;               // end the game
                }

                if (grid[y][x] == 0) {          // after the player moves into a '0' rectangle...
                    grid[y][x] = 2;             // make it a '2' rectangle (will be set turned green later)
                }

                timer = 0;
            }

            for (int i = 0; i < enemyCount; i++) {      // loop through each of the enemies
                enemy[i].move();                        // make each of them move
            }

            if (grid[y][x] == 1) {                      // don't let player move into blue region
                dx = dy = 0;

                for (int i = 0;
                     i < enemyCount; i++) {          // call drop() for each enemy, passing the enemy's location
                    drop(enemy[i].getY() / tileSize, enemy[i].getX() / tileSize);
                }

                for (int i = 0; i < boardHeight; i++) {
                    for (int j = 0; j < boardWidth; j++) {
                        if (grid[i][j] == -1) {                 // if a grid index is -1...
                            grid[i][j] = 0;                     // assign it back to 0
                        } else {
                            grid[i][j] = 1;                     // otherwise, assign it 1 so that it can later be turned blue
                            //score++;
                        }
                    }
                }
            }

            for (int i = 0; i < enemyCount; i++) {
                if (grid[enemy[i].getY() / tileSize][enemy[i].getX() / tileSize] ==2) {   // if an enemy comes in contact with a green rectangle...
                    Game = false;                                                          // end the game
                }
            }

            /////////draw//////////
            window.clear();                             // clear the window

            for (int i = 0; i < boardHeight; i++) {
                for (int j = 0; j < boardWidth; j++) {
                    if (!grid[i][j] == 0) {

                        if (grid[i][j] == 1) {                          // set blue tile wherever grid == 1
                            sTile.setTextureRect(IntRect(0, 0, tileSize, tileSize));
                            score++;
                        }

                        if (grid[i][j] == 2) {                          // set green tile wherever grid == 2
                            sTile.setTextureRect(IntRect(54, 0, tileSize, tileSize));
                        }

                        sTile.setPosition(j * tileSize, i * tileSize);
                        window.draw(sTile);                             // draw colored tile to hidden buffer
                    }
                }
            }

            sTile.setTextureRect(IntRect(36, 0, tileSize, tileSize));   // grab the red tile
            sTile.setPosition(x * tileSize, y * tileSize);                                // set position of red tile
            window.draw(sTile);                                                                 // draw the red tile

            sEnemy.rotate(10);                                                  // make the enemies spin
            for (int i = 0; i < enemyCount; i++) {
                sEnemy.setPosition(enemy[i].getX(), enemy[i].getY());
                window.draw(sEnemy);                                                  // draw to hidden buffer
            }

            if (!Game) {                    // check if bool Game is false
                music.stop();               // stop the game music
                sound.play();               // play the game over sound


                if (score - 126 > highScores.at(0)) {
                    for (int i = 4; i > 0; i--) {
                        highScores.at(i) = highScores.at(i - 1);
                    }
                    highScores.at(0) = score - 126;
                }
                else if (score - 126 > highScores.at(1)) {
                    for (int i = 4; i > 1; i--) {
                        highScores.at(i) = highScores.at(i - 1);
                    }
                    highScores.at(1) = score - 126;
                }
                else if (score - 126 > highScores.at(2)) {
                    for (int i = 4; i > 2; i--) {
                        highScores.at(i) = highScores.at(i - 1);
                    }
                    highScores.at(2) = score - 126;
                }
                else if (score - 126 > highScores.at(3)) {
                    for (int i = 4; i > 3; i--) {
                        highScores.at(i) = highScores.at(i - 1);
                    }
                    highScores.at(3) = score - 126;
                }
                else if (score - 126 > highScores.at(4)) {
                    highScores.at(4) = score - 126;
                }

                highScore.setString("High Scores:\n" + std::to_string(highScores.at(0)) + "\n"
                                                        + std::to_string(highScores.at(1)) + "\n"
                                                        + std::to_string(highScores.at(2)) + "\n"
                                                        + std::to_string(highScores.at(3)) + "\n"
                                                        + std::to_string(highScores.at(4)) + "\n");

                window.draw(bigRectangle);       // draw black rectangle to hidden buffer
                window.draw(highScore);          // draw high scores to hidden buffer
                window.draw(sGameover);          // draw game over message to hidden buffer

            }

            window.draw(smallRectangle);        // draw the small rectangle to hidden buffer
            scoreText.setString("Score: " + std::to_string(score - 126));
            window.draw(scoreText);              // draw score to hidden buffer

            window.display();               // display hidden buffer
        }
    }

    return 0;
}
