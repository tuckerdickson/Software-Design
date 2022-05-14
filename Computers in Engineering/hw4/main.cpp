#include <SFML/Graphics.hpp>
#include <time.h>

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

    // instantiate sprite objects for tiles, game over and enemy
    Sprite sTile(tilesTexture), sGameover(gameoverTexture), sEnemy(enemyTexture);
    sGameover.setPosition(100, 100);
    sEnemy.setOrigin(20, 20);

    int enemyCount = 4;
    Enemy enemy[10];

    bool Game = true;
    int x = 0, y = 0;
    int dx = 0, dy = 0;                         // delta x and y for the player motion
    float timer = 0, delay = 0.07;              // this line and the next control how fast sprites move
    Clock clock;

    for (int i = 0; i < boardHeight; i++) {
        for (int j = 0; j < boardWidth; j++) {
            if (i == 0 || j == 0 || i == boardHeight - 1 || j == boardWidth - 1) {
                grid[i][j] = 1;                     // sets the border of the game board
            }
        }
    }

    while (window.isOpen()) {                       // outer "game" loop; only breaks when window is closed
        float time = clock.getElapsedTime().asSeconds();
        clock.restart();
        timer += time;

        Event event;                                // instantiate Event object
        while (window.pollEvent(event)) {       // 'event' loop; checks if an event is occurring
            if (event.type == Event::Closed) {      // check if the event type is Closed
                window.close();                    // close the window
            }

            if (event.type == Event::KeyPressed) {              // checks if a key is pressed
                if (event.key.code == Keyboard::Escape) {       // checks if the escape key is pressed
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

        if (!Game) {
            continue;
        }

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

            for (int i = 0; i < enemyCount; i++) {          // call drop() for each enemy, passing the enemy's location
                drop(enemy[i].getY() / tileSize, enemy[i].getX() / tileSize);
            }

            for (int i = 0; i < boardHeight; i++) {
                for (int j = 0; j < boardWidth; j++) {
                    if (grid[i][j] == -1) {                 // if a grid index is -1...
                        grid[i][j] = 0;                     // assign it back to 0
                    }
                    else {
                        grid[i][j] = 1;                     // otherwise, assign it 1 so that it can later be turned blue
                    }
                }
            }
        }

        for (int i = 0; i < enemyCount; i++) {
            if (grid[enemy[i].getY() / tileSize][enemy[i].getX() / tileSize] == 2) {    // if an enemy comes in contact with a green rectangle...
                Game = false;                                                           // end the game
            }
        }

        /////////draw//////////
        window.clear();                             // clear the window

        for (int i = 0; i < boardHeight; i++) {
            for (int j = 0; j < boardWidth; j++) {
                if (grid[i][j] == 0) {
                    continue;
                }

                if (grid[i][j] == 1) {                          // set blue tile wherever grid == 1
                    sTile.setTextureRect(IntRect(0, 0, tileSize, tileSize));
                }

                if (grid[i][j] == 2) {                          // set green tile wherever grid == 2
                    sTile.setTextureRect(IntRect(54, 0, tileSize, tileSize));
                }

                sTile.setPosition(j * tileSize, i * tileSize);
                window.draw(sTile);                 // draw colored tile to hidden buffer
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
            window.draw(sGameover);     // draw Game over sprite to hidden buffer
        }

        window.display();               // display hidden buffer
    }

    return 0;
}
