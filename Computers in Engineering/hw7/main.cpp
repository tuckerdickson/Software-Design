/* --------------------------------------------- EXTRA CREDIT! ---------------------------------------------*/
 /* For the extra credit part of this assignment I...
  *     1. Added a sound effect that plays every time the laser is fired
  *     2. Added a sound effect that plays every time an object explodes
  *     3. Added a score tracker at the top of the window
  *     4. Added a high score page that displays after the player dies
  */



#include <SFML/Graphics.hpp>
#include <SFML/Audio.hpp>
#include <time.h>
#include <list>
#include <cmath>
#include <vector>

using namespace sf;

const int W = 1200;                 // game board width
const int H = 800;                  // game board height

float DEGTORAD = 0.017453f;         // degrees to radians conversion factor

class Animation {
public:
    float Frame, speed;
    Sprite sprite;
    std::vector<IntRect> frames;

    Animation() {}

    Animation(Texture &t, int x, int y, int w, int h, int count, float Speed) {
        Frame = 0;
        speed = Speed;

        for (int i = 0; i < count; i++) {
            frames.push_back(IntRect(x + i * w, y, w, h));
        }

        sprite.setTexture(t);
        sprite.setOrigin(w / 2, h / 2);
        sprite.setTextureRect(frames[0]);
    }


    void update() {
        Frame += speed;
        int n = frames.size();
        if (Frame >= n) {
            Frame -= n;
        }
        if (n > 0) {
            sprite.setTextureRect(frames[int(Frame)]);
        }
    }

    bool isEnd() {
        return Frame + speed >= frames.size();
    }

};


class Entity {
public:
    float x, y;             // x and y locations of the entity
    float dx, dy;           // speed in the x and y directions of the entities
    float R;                // collision radius of the entity
    float angle;            // rotation of the entity
    bool life;              // if false, remove from the entities list
    std::string name;       // the name of the derived class
    Animation anim;         // object for displaying the animation

    Entity() {
        life = true;        // set the entity to be drawn on the screen
    }

    // used to set all of the properties of an entity object or any of its derived objects
    void settings(Animation &a, int X, int Y, float Angle = 0, int radius = 1) {
        anim = a;
        x = X;
        y = Y;
        angle = Angle;
        R = radius;
    }

    // virtual base class function
    virtual void update() {};

    void draw(RenderWindow &app) {
        anim.sprite.setPosition(x, y);
        anim.sprite.setRotation(angle + 90);
        app.draw(anim.sprite);

        CircleShape circle(R);
        circle.setFillColor(Color(255, 0, 0, 170));
        circle.setPosition(x, y);
        circle.setOrigin(R, R);
        //app.draw(circle);
    }

    // entity destructor
    virtual ~Entity() {};
};


class asteroid : public Entity {
public:
    static unsigned int asteroidCount;   // used to keep track of the number of existing asteroids

    // constructor
    asteroid() {
        dx = rand() % 8 - 4;        // randomly generated speed in the x direction
        dy = rand() % 8 - 4;        // randomly generated speed in the y direction
        name = "asteroid";          // name assigned to asteroid objects
        asteroidCount++;            // increment asteroidCount when an asteroid object is created
    }

    // destructor
    ~asteroid() {
        asteroidCount--;            // decrement asteroidCount when an asteroid object is destroyed
    }

    //----------------------------------------------------------
    // Description: This function is responsible for the
    //              movement of asteroid objects. It also
    //              sets bounds on where the bullet can travel
    //              (without being destroyed)
    //              This is a concrete function (not virtual)
    // Inputs: None
    // Outputs: None
    // ----------------------------------------------------------
    void update() {
        x += dx;                // update the x position of the asteroid
        y += dy;                // update the y position of the asteroid

        if (x > W) {
            x = 0;              // if asteroid goes off screen to right, bring it back on the left side
        }
        if (x < 0) {
            x = W;              // if asteroid goes off screen to left, bring it back on the right side
        }
        if (y > H) {
            y = 0;              // if asteroid goes off the bottom of the screen, bring it back to the top
        }
        if (y < 0) {
            y = H;              // if asteroid goes off the top of the screen, bring it back to the bottom
        }
    }

};

unsigned int asteroid::asteroidCount = 0;   // initialize asteroidCount to zero once

class bullet : public Entity {
public:
    bullet() {
        name = "bullet";        // name assigned to bullet objects
    }

    //----------------------------------------------------------
    // Description: This function is responsible for the
    //              movement of bullet objects. It also
    //              sets bounds on where the bullet can travel
    //              (without being destroyed)
    //              This is a concrete function (not virtual)
    // Inputs: None
    // Outputs: None
    // ----------------------------------------------------------
    void update() {
        // set direction and speed of bullet
        dx = cos(angle * DEGTORAD) * 6;
        dy = sin(angle * DEGTORAD) * 6;
        // angle+=rand()%6-3;
        x += dx;
        y += dy;

        // when the bullet reaches the edge of the screen, remove it from the display list
        if (x > W || x < 0 || y > H || y < 0) {
            life = false;
        }
    }

};

class ufo : public Entity {
public:
    ufo() {
        dx = 2;                 // set the speed in the x direction
        name = "ufo";           // name assigned to asteroid objects
    }

    //----------------------------------------------------------
    // Description: This function is responsible for the
    //              movement of ufo objects. It also
    //              sets bounds on where the ufo can travel
    //              (without being destroyed)
    //              This is a concrete function (not virtual)
    // Inputs: None
    // Outputs: None
    // ----------------------------------------------------------
    void update() {
        // update the x position of the ufo
        x += dx;

        // when the ufo reaches the edge of the screen, remove it from the display list
        if (x > (W+50)) {
            life = false;
        }
    }
};

class player : public Entity {
public:
    bool thrust;            // when this is true, player moves forward

    // constructor
    player() {
        name = "player";    // name assigned to player objects
    }

    //----------------------------------------------------------
    // Description: This function is responsible for the
    //              movement of the player object. It also
    //              sets bounds on where the player can travel
    //              This is a concrete function (not virtual)
    // Inputs: None
    // Outputs: None
    // ----------------------------------------------------------
    void update() {
        // when the up arrow is pressed, then thrust is true -> speed up the spaceship
        if (thrust) {
            // increase the speed by 20 percent in the angle direction
            dx += cos(angle * DEGTORAD) * 0.2;
            dy += sin(angle * DEGTORAD) * 0.2;
        } else {
            // slow down by 1 percent
            dx *= 0.99;
            dy *= 0.99;
        }

        // limit the speed of the ship to 15
        int maxSpeed = 15;
        float speed = sqrt(dx * dx + dy * dy);
        if (speed > maxSpeed) {
            dx *= maxSpeed / speed;
            dy *= maxSpeed / speed;
        }

        // move the player
        x += dx;
        y += dy;

        if (x > W) {
            x = 0;              // if player goes off screen to right, bring it back on the left side
        }
        if (x < 0)  {
            x = W;              // if player goes off screen to left, bring it back on the right side
        }
        if (y > H) {
            y = 0;              // if player goes off the bottom of the screen, bring it back to the top
        }
        if (y < 0) {
            y = H;              // if player goes off the top of the screen, bring it back to the bottom
        }
    }

};

//----------------------------------------------------------
// Description: This function checks to see if two entity
//              objects are in contact (or if they have
//              'collided')
// Inputs: This function has two inputs...
//         Entity *a is a pointer to an entity object
//         Entity *b is also a pointer to an entity object
// Outputs: This function returns a boolean which depends
//          on whether the objects are in contact
// ----------------------------------------------------------
bool isCollide(Entity *a, Entity *b) {
    return (b->x - a->x) * (b->x - a->x) +
           (b->y - a->y) * (b->y - a->y) <
           (a->R + b->R) * (a->R + b->R);
}

int main() {

    /*--------------------------------------- declare and initialize variables ---------------------------------------*/

    int ufoTimer = 0;       // used to determine when to make ufo appear
    int score = 0;          // holds the player's score
    bool game = true;       // state machine: displays high scores when game is false
    std::vector<int>highScoreVector = {0,0,0,0};    // holds the four highest scores
    srand(time(0));

    /*------------------------------------------------- Window Object ------------------------------------------------*/

    RenderWindow app(VideoMode(W, H), "Asteroids!");      // render a window object; call it Asteroids!
    app.setFramerateLimit(60);                                  // cap the frame rate

    /*---------------------------------------------- Music/Sound Objects ---------------------------------------------*/

    /*
    License: sound effects free for personal use
    http://static1.grsites.com/archive/sounds/scifi/scifi072.mp3
    */
    Music ufoSound;
    if (!ufoSound.openFromFile("Audio/ufoSound.ogg")) {    // load from file; check for failure
        return EXIT_FAILURE;                                        // if failure, return error
    }
    ufoSound.setLoop(true);                                    // tell the ufoSound object to loop

    /*------------------------------------------------------------------------------------------------------------------
    Standard License: This License grants a single user the right to download and use our sound effects and music
    Sound effects obtained from https://www.zapsplat.com
     https://www.zapsplat.com/music/science-fiction-laser-gun-made-with-metal-slinky-coil/
     ------------------------------------------------------------------------------------------------------------------*/
    SoundBuffer laserBuffer;
    if (!laserBuffer.loadFromFile("Audio/laserSound.ogg")) {
        return EXIT_FAILURE;
    }
    Sound laserSound;
    laserSound.setBuffer(laserBuffer);

    /*------------------------------------------------------------------------------------------------------------------
    Standard License: This License grants a single user the right to download and use our sound effects and music
    Sound effects obtained from https://www.zapsplat.com
     https://www.zapsplat.com/music/punchy-explosion-with-a-short-tail-5/
     ------------------------------------------------------------------------------------------------------------------*/
    SoundBuffer explosionBuffer;
    if (!explosionBuffer.loadFromFile("Audio/explosionSound.ogg")) {
        return EXIT_FAILURE;
    }
    Sound explosionSound;
    explosionSound.setBuffer(explosionBuffer);

    /*-------------------------------------------------- Font Object -------------------------------------------------*/

    /*
    License: "free for personal use"
    Font obtained from https://www.1001freefonts.com/planet-kosmos.font
    */
    Font font;                                                      // instantiate font object
    if (!font.loadFromFile("fonts/font.TTF")) {             // check if file successfully loads
        return EXIT_FAILURE;                                        // if it doesn't, error
    }

    /*------------------------------------------------- Text Objects -------------------------------------------------*/

    // this is the text that will be used to display the running score in the top left corner
    Text scoreText;
    scoreText.setFont(font);                                             // select the font
    scoreText.setString("Score: " + std::to_string(score));        // set the string
    scoreText.setFillColor(Color::Red);                                  // set the color
    scoreText.setOutlineColor(Color::White);                             // set outline color
    scoreText.setOutlineThickness(7.0f);                        // set outline thickness
    scoreText.setCharacterSize(40);                                 // set size
    scoreText.setPosition(40,30);                                  // set position

    // this is the text for the game over message after player dies
    Text gameOver;
    gameOver.setFont(font);
    gameOver.setString("Game Over");
    gameOver.setFillColor(Color::White);
    gameOver.setOutlineThickness(7.0f);
    gameOver.setOutlineColor(Color::Black);
    gameOver.setCharacterSize(70);
    gameOver.setPosition((W/3.5)-10,H/6);

    // this is the text for the "high scores:" message after player dies
    Text highScores;
    highScores.setFont(font);
    highScores.setString("High Scores:");
    highScores.setFillColor(Color::White);
    highScores.setOutlineThickness(7.0f);
    highScores.setOutlineColor(Color::Black);
    highScores.setCharacterSize(50);
    highScores.setPosition((W/3) - 20,(H/3) + 10);

    // this is the text for the game over message after player dies
    Text highScoreList;
    highScoreList.setFont(font);
    highScoreList.setFillColor(Color::White);
    highScoreList.setOutlineThickness(7.0f);
    highScoreList.setOutlineColor(Color::Black);
    highScoreList.setCharacterSize(40);
    highScoreList.setPosition(W/3, (H/2) - 25);

    /*------------------------------------------------ Texture Objects -----------------------------------------------*/

    Texture t1, t2, t3, t4, t5, t6, t7, t8, t9;
    if (!t1.loadFromFile("images/spaceship.png")) {
        return EXIT_FAILURE;
    }
    if (!t2.loadFromFile("images/background.jpg")) {
        return EXIT_FAILURE;
    }
    if (!t3.loadFromFile("images/explosions/type_C.png")) {
        return EXIT_FAILURE;
    }
    if (!t4.loadFromFile("images/rock.png")) {
        return EXIT_FAILURE;
    }
    if (!t5.loadFromFile("images/fire_blue.png")) {
        return EXIT_FAILURE;
    }
    if (!t6.loadFromFile("images/rock_small.png")) {
        return EXIT_FAILURE;
    }
    if (!t7.loadFromFile("images/explosions/type_B.png")) {
        return EXIT_FAILURE;
    }

    /*
    License: free with attribution
    Icon made by Freepik from flaticon.com
    https://www.flaticon.com/free-icon/ufo_3306608?term=ufo&page=1&position=24&page=1&position=24&related_id=3306608&origin=search
    */
    if (!t8.loadFromFile("images/ufo.png")) {
        return EXIT_FAILURE;
    }

    // enable the Texture class smooth filter  for t1, t2, and t8 (make individual pixels less noticeable)
    t1.setSmooth(true);
    t2.setSmooth(true);
    t8.setSmooth(true);

    // instantiate a sprite object using t2
    Sprite background(t2);

    /*----------------------------------------------- Animation Objects ----------------------------------------------*/

    // instantiate eight animation objects
    Animation sExplosion(t3, 0, 0, 256, 256, 48, 0.5);
    Animation sRock(t4, 0, 0, 64, 64, 16, 0.2);
    Animation sRock_small(t6, 0, 0, 64, 64, 16, 0.2);
    Animation sBullet(t5, 0, 0, 32, 64, 16, 0.8);
    Animation sPlayer(t1, 40, 0, 40, 40, 1, 0);
    Animation sPlayer_go(t1, 40, 40, 40, 40, 1, 0);
    Animation sExplosion_ship(t7, 0, 0, 192, 192, 64, 0.5);
    Animation sUfo(t8,0,0,128,128,1,0);

    /*------------------------------------------------- Shape Objects ------------------------------------------------*/

    // this rectangle will be used as a backdrop for the high score list
    RectangleShape bigRectangle(Vector2f(W/2,H/2));
    bigRectangle.setPosition(W/4, H/3);
    bigRectangle.setFillColor(Color::Red);
    bigRectangle.setOutlineThickness(7.f);
    bigRectangle.setOutlineColor(Color::Black);

    // this rectangle will be used as a backdrop for the game over message
    RectangleShape smallRectangle(Vector2f(W/2,H/8));
    smallRectangle.setPosition(W/4, (H/6) + 5);
    smallRectangle.setFillColor(Color::Red);
    smallRectangle.setOutlineThickness(7.f);
    smallRectangle.setOutlineColor(Color::Black);

    std::list<Entity *> entities;   // create a list of entity pointers

    // dynamically allocate memory for fifteen asteroids
    // give each new asteroid a random x position, random y position, random rotation angle, and radius of 25
    // push each asteroid onto the back of the entities list
    for (int i = 0; i < 15; i++) {
        asteroid *a = new asteroid();
        a->settings(sRock, rand() % W, rand() % H, rand() % 360, 25);
        entities.push_back(a);
    }

    // dynamically allocate memory for a new player object
    // set the player's x and y positions each to 200, rotation angle to 0, and radius to 20
    // push the player onto the back of the entities list
    player *p = new player();
    p->settings(sPlayer, 200, 200, 0, 20);
    entities.push_back(p);

    /*--------------------------------------------------- Main Loop --------------------------------------------------*/

    while (app.isOpen()) {
        ufoTimer++;
        Event event;                                            // instantiate event object
        while (app.pollEvent(event)) {                       // start event loop
            if (event.type == Event::Closed) {                  // if the user requests to close the window...
                app.close();                                    // then close the window
            }

            if (event.type == Event::KeyPressed) {
                if (event.key.code == Keyboard::Space) {        // if the user hits the space bar...
                    bullet *b = new bullet();                   // then dynamically allocate memory for a bullet object
                    b->settings(sBullet, p->x, p->y, p->angle, 10);
                    entities.push_back(b);                      // push the bullet  onto the end of the entities list
                    laserSound.play();                          // player laser sound
                }
            }

            // if the user hits the escape key, game gets set to true, and score and ufoTimer get set back to zero
            // primary purpose is to exit the high score page and play again
            if (event.type == Event::KeyPressed) {
                if (event.key.code == Keyboard::Escape) {
                    game = true;
                    score = 0;
                    ufoTimer = 0;
                }
            }
        }
        if (game) {
            if (Keyboard::isKeyPressed(Keyboard::Right)) {      // if the user presses the right key
                p->angle += 3;                                          // add three to angle (rotate player clockwise)
            }
            if (Keyboard::isKeyPressed(Keyboard::Left)) {       // if the user presses the left key
                p->angle -= 3;                                          // subtract three from angle (rotate counter-cw)
            }
            if (Keyboard::isKeyPressed(Keyboard::Up)) {         // if the user presses the up key
                p->thrust = true;                                       // set thrust to true (make player move forward)
            } else {
                p->thrust = false;                                  // otherwise thrust gets false; we don't move forward
            }

            // compare each of the objects in the entities list with a nested for-loop
            for (auto a:entities) {
                for (auto b:entities) {

                    // if the two objects being compared are an asteroid and a bullet...
                    if (a->name == "asteroid" && b->name == "bullet") {

                        // and if isCollide() returns true (if the objects come in contact)...
                        if (isCollide(a, b)) {

                            // set life to false for each object (so they can be removed from entities list later)
                            a->life = false;
                            b->life = false;

                            // dynamically allocate memory for a new Entity object
                            // call the new object "explosion", and set it's position to the position of the asteroid
                            // push explosion onto the back of the entities list
                            Entity *e = new Entity();
                            e->settings(sExplosion, a->x, a->y);
                            e->name = "explosion";
                            entities.push_back(e);

                            // iterate two times
                            for (int i = 0; i < 2; i++) {

                                // pass over if small asteroid
                                if (a->R == 15) {
                                    continue;
                                }

                                // if large asteroid: dynamically allocate memory for a new asteroid object
                                Entity *e = new asteroid();

                                // give the new asteroid small rock animation, the x and y locations of the existing asteroid
                                // a random rotation angle, and a radius of 15
                                e->settings(sRock_small, a->x, a->y, rand() % 360, 15);

                                // push the new (small) asteroid onto the back of the entities list
                                entities.push_back(e);
                            }

                        }
                    }

                    // if the two current objects are player and an asteroid...
                    if (a->name == "player" && b->name == "asteroid") {

                        // and if isCollide() is true (if the objects are in contact)...
                        if (isCollide(a, b)) {

                            // set player life to false
                            b->life = false;

                            // dynamically allocate memory for a new entity object
                            // give it the exploding ship animation, as well as the player's x and y locations
                            // name the entity explosion and push it onto the back of the entities list
                            Entity *e = new Entity();
                            e->settings(sExplosion_ship, a->x, a->y);
                            e->name = "explosion";
                            entities.push_back(e);

                            // stop the ufo music, clear entities list, and set game to false to stop the game
                            ufoSound.stop();
                            game = false;
                            for (auto e:entities) {
                                if (e->name == "asteroid" || e->name == "ufo" || e->name == "bullet") {
                                    // set explosion life to false (remove it from entities list)
                                    e->life = false;
                                }
                            }

                            // relocate the player to the center of the board and stop any prior movement
                            p->settings(sPlayer, W / 2, H / 2, 0, 20);
                            p->dx = 0;
                            p->dy = 0;
                        }
                    }

                    // if the two current objects are player and an ufo...
                    if (a->name == "player" && b->name == "ufo") {

                        // and if isCollide() is true (if the objects are in contact)...
                        if (isCollide(a, b)) {

                            // set player life to false
                            b->life = false;

                            // dynamically allocate memory for a new entity object
                            // give it the exploding ship animation, as well as the player's x and y locations
                            // name the entity explosion and push it onto the back of the entities list
                            Entity *e = new Entity();
                            e->settings(sExplosion_ship, a->x, a->y);
                            e->name = "explosion";
                            entities.push_back(e);

                            // stop the ufo music, clear entities list, and set game to false to stop the game
                            ufoSound.stop();
                            game = false;
                            for (auto e:entities) {
                                if (e->name == "asteroid" || e->name == "ufo" || e->name == "bullet") {
                                    // set explosion life to false (remove it from entities list)
                                    e->life = false;
                                }
                            }

                            // relocate the player to the center of the board and stop any prior movement
                            p->settings(sPlayer, W / 2, H / 2, 0, 20);
                            p->dx = 0;
                            p->dy = 0;
                        }
                    }

                    // if the two objects being compared are ufo and a bullet...
                    if (a->name == "ufo" && b->name == "bullet") {

                        // and if isCollide() returns true (if the objects come in contact)...
                        if (isCollide(a, b)) {

                            // set life to false for each object (so they can be removed from entities list later)
                            a->life = false;
                            b->life = false;

                            // dynamically allocate memory for a new Entity object
                            // call the new object "explosion", and set it's position to the position of the asteroid
                            // push explosion onto the back of the entities list
                            Entity *e = new Entity();
                            e->settings(sExplosion, a->x, a->y);
                            e->name = "explosion";
                            entities.push_back(e);

                        }
                    }
                    // stop the ufo music if the ufo goes off screen or explodes
                    if ((a->name == "ufo" && a->life == false) || (a->name == "ufo" && a->x > (W + 45))) {
                        ufoSound.stop();
                    }

                    // play explosion sound when asteroid, ufo, or player explodes
                    if ((b->name == "asteroid" && b->life == false) ||
                        (b->name == "ufo" && b->life == false) ||
                        (b->name == "player" && b->life == false)) {
                        explosionSound.play();
                    }
                }
                if (game) {
                    // increment the score if an asteroid is destroyed
                    // or add five if a ufo is destroyed
                    // or reset to zero if the player dies
                    if (a->name == "asteroid" && a->life == false) {
                        score++;
                    } else if (a->name == "ufo" && a->life == false) {
                        score += 5;
                    } else if (a->name == "player" && a->life == false) {
                        score = 0;
                    }
                }
            }

            // if the player object's thrust is true...
            if (p->thrust) {

                // set the player animation to sPlayer_go
                p->anim = sPlayer_go;
            } else {

                // if not thrust, set the player animation to sPlayer
                p->anim = sPlayer;
            }

            // iterate through the entities list
            // if the list contains an entity called "explosion"
            for (auto e:entities) {
                if (e->name == "explosion") {

                    // and if the animation has ended
                    if (e->anim.isEnd()) {

                        // set explosion life to false (remove it from entities list)
                        e->life = false;
                    }
                }
            }

            /*-------------------------------------- old asteroid generation --------------------------------------
            // generate new asteroids
            if (rand() % 150 == 0) {
                asteroid *a = new asteroid();
                a->settings(sRock, 0, rand() % H, rand() % 360, 25);
                entities.push_back(a);
            }
            -----------------------------------------------------------------------------------------------------*/

            /*---------------------------------------- NEW asteroid generation ---------------------------------------*/
            if (asteroid::asteroidCount == 0) {     // if all asteroids have been destroyed...

                // dynamically allocate memory for fifteen asteroids
                // give each new asteroid a random x position, random y position, random rotation angle, and radius of 25
                // push each asteroid onto the back of the entities list
                for (int i = 0; i < 15; i++) {
                    asteroid *a = new asteroid();
                    a->settings(sRock, rand() % W, rand() % H, rand() % 360, 25);
                    entities.push_back(a);
                }
            }

            // generate ufo
            if (ufoTimer > 1000) {
                ufo *u = new ufo();
                u->settings(sUfo, -45, rand() % H, 270, 50);
                entities.push_back(u);
                ufoTimer = 0;
                ufoSound.play();
            }


            // entities.begin() returns the first iterator of the entities list
            // entities.end() returns the iterator of the theoretical element after the last element in the list
            // i is an iterator (an iterator is an object that is like a pointer)
            for (auto i = entities.begin(); i != entities.end();) {
                Entity *e = *i;         // use the * on the iterator i to get the element of the list (an entity pointer)

                e->update();            // polymorphism; call the derived class update()
                e->anim.update();       // update the animation

                // remove an entity pointer from the list if life is false
                if (e->life == false) {
                    // erase() removes the element from the list and moves the iterator to the next position
                    i = entities.erase(i);
                    delete e;           // polymorphism, call the destructor for the derived class
                } else i++;               // move iterator to the next element in the list
            }



            //////draw//////
            app.draw(background);

            scoreText.setString("Score: " + std::to_string(score));
            app.draw(scoreText);

            // iterate through each object in the entities list and draw each one
            for (auto i:entities) {
                i->draw(app);
            }

            if (!game) {
                app.draw(bigRectangle);
                app.draw(smallRectangle);
                app.draw(gameOver);
                app.draw(highScores);

                if (score >= highScoreVector.at(0)) {
                    highScoreVector.at(3) = highScoreVector.at(2);
                    highScoreVector.at(2) = highScoreVector.at(1);
                    highScoreVector.at(1) = highScoreVector.at(0);
                    highScoreVector.at(0) = score;
                }
                else if (score >= highScoreVector.at(1)) {
                    highScoreVector.at(3) = highScoreVector.at(2);
                    highScoreVector.at(2) = highScoreVector.at(1);
                    highScoreVector.at(1) = score;
                }
                else if (score >= highScoreVector.at(2)) {
                    highScoreVector.at(3) = highScoreVector.at(2);
                    highScoreVector.at(2) = score;
                }
                else if (score >= highScoreVector.at(3)) {
                    highScoreVector.at(3) = score;
                }

                highScoreList.setString("1.        " + std::to_string(highScoreVector.at(0)) + "\n"
                                    + "2.       " + std::to_string(highScoreVector.at(1)) + "\n"
                                    + "3.       " + std::to_string(highScoreVector.at(2)) + "\n"
                                    + "4.       " + std::to_string(highScoreVector.at(3)) + "\n");
                app.draw(highScoreList);
            }

            app.display();
        }
    }

    return 0;
}
