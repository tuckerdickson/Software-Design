#include <iostream>
#include <string>
using namespace std;

class Business {
public:
    // constructor
    explicit Business(const string& name = "No Name",       // add a constructor for Business, include default values
                      const string& address = "No Address",
                      const int maxOccupancy = 0);

    // destructor
    ~Business();                                    // add a destructor to Business class

    // copy constructor
    Business(const Business& origBusiness);         // add a copy constructor to Business class

    // copy assignment operator
    Business& operator=(const Business& businessToCopy);        // add a copy assignment operator to business class

    // setters
    void SetName(const string &busName);
    void SetAddress(const string &busAddress);
    void SetMaxOccupancy(int maxOccupancyPar);       // add a setter for maxOccupancy

    // getters
    string GetName () const;                        // add getters
    string GetAddress () const;
    int GetMaxOccupancy () const;

    string GetDescription() const;
    void print() const;                              // add a print function to the business class

protected:
    int *maxOccupancy{nullptr};                     // add protected variable called maxOccupancy. Initialize to nullptr

private:
    string name;                                    // don't touch these
    string address;
};

class Restaurant : public Business {
public:
    // constructor
    explicit Restaurant(const string name = "No Name",              // add explicit constructor for Restaurant class
                        const string address = "No Address",        // add defaults for all members
                        const int maxOccupancy = 0,
                        const int rating = 0);

    // destructor
    ~Restaurant();                                                  // add a destructor to Restaurant class

    //copy constructor
    Restaurant(const Restaurant& origRestaurant);                   // add a copy constructor to Restaurant class

    // copy assignment operator
    Restaurant& operator=(const Restaurant& restaurantToCopy);      // add a copy assignment operator to Restaurant class

    // setter
    void SetRating(int userRating);

    // getter
    int GetRating() const;

    void print() const;                                             // add a print function to the Restaurant class

private:
    int rating;
};

/*-------------------------------------  Business class function definitions  -------------------------------------*/
// constructor
Business::Business(const string& name, const string& address, const int maxOccupancy) :
        name{name},
        address{address}
{
    this->maxOccupancy = new int;                   // allocate memory for maxOccupancy
    *(this->maxOccupancy) = -1;                     // dereference maxOccupancy and set to -1
}

// destructor
Business::~Business() {
    delete maxOccupancy;                            // release maxOccupancy
}

// copy constructor
Business::Business(const Business &origBusiness) {
    maxOccupancy = new int;                         // allocate memory for maxOccupancy
    *maxOccupancy = *(origBusiness.maxOccupancy);   // copy maxOccupancy (de-referenced) from origBusiness to the Business
                                                        // object being initialized
    name = origBusiness.name;                       // copy name
    address = origBusiness.address;                 // copy address
}

// copy assignment operator
Business& Business::operator=(const Business &businessToCopy) {
    if (this != &businessToCopy) {                          // don't self-assign
        delete maxOccupancy;                                // delete old maxOccupancy
        maxOccupancy = new int;                             // allocate new maxOccupancy
        *maxOccupancy = *(businessToCopy.maxOccupancy);     // copy maxOccupancy (de-referenced)

        name = businessToCopy.name;                         // copy name
        address = businessToCopy.address;                   // copy address

    }
    return *this;
}

// setters
void Business::SetName(const string &busName) {
    name = busName;
}
void Business::SetAddress(const string &busAddress) {
    address = busAddress;
}
void Business::SetMaxOccupancy(const int maxOccupancyPar) {
    if (maxOccupancy == nullptr) {
        maxOccupancy = new int;
    }
    *maxOccupancy = maxOccupancyPar;
}

// getters
string Business::GetName () const {
    return name;
}
string Business::GetAddress () const {
    return address;
}
int Business::GetMaxOccupancy () const {
    return *maxOccupancy;
}

// other
string Business::GetDescription() const {
    return name + " -- " + address;
}

void Business::print() const {
    cout << "Name: " << name << endl;           // print name, address, and maxOccupancy (de-referenced)
    cout << "Address: " << address << endl;
    cout << "Maximum Occupancy: " << *maxOccupancy << endl;
}

/*-------------------------------------  Restaurant class function definitions  -------------------------------------*/
// constructor
Restaurant::Restaurant(const string name, const string address, const int maxOccupancy, const int rating) :
        Business{name, address, maxOccupancy},  // call business constructor to initialize name, address, and maxOccupancy
        rating{rating}                          // set rating
{}

// destructor
Restaurant::~Restaurant() {}

// copy constructor
Restaurant::Restaurant(const Restaurant &origRestaurant) :
Business{origRestaurant.GetName(), origRestaurant.GetAddress(), origRestaurant.GetMaxOccupancy()},  // pass name, address, and maxOccupancy to Business constructor
rating{origRestaurant.GetRating()}                                  // set rating
{
    this->SetMaxOccupancy(origRestaurant.GetMaxOccupancy());        // reset maxOccupancy because business constructor sets it to -1
}

// copy assignment operator
Restaurant& Restaurant::operator=(const Restaurant &restaurantToCopy) {
    if (this != &restaurantToCopy) {                                // don't self-assign
        delete maxOccupancy;                                        // delete old maxOccupancy
        maxOccupancy = new int;                                     // allocate new maxOccupancy
        *maxOccupancy = *(restaurantToCopy.maxOccupancy);           // copy maxOccupancy (de-referenced)

        this->SetName(restaurantToCopy.GetName());                      // copy name
        this->SetAddress(restaurantToCopy.GetAddress());                // copy address
        rating = restaurantToCopy.rating;                               // copy rating
    }
    return *this;
}

// setter
void Restaurant::SetRating(int userRating) {
    rating = userRating;
}

// getter
int Restaurant::GetRating() const {
    return rating;
}

// print
void Restaurant::print() const {
    Business::print();                      // call business class print function
    cout << "Rating: " << rating << endl;
}


int main() {
    /*------------------------------------ original main() ------------------------------------*/
    Business someBusiness;
    Restaurant favoritePlace;

    someBusiness.SetName("ACME");
    someBusiness.SetAddress("4 Main St");

    favoritePlace.SetName("Friends Cafe");
    favoritePlace.SetAddress("500 W 2nd Ave");
    favoritePlace.SetRating(5);

    cout << someBusiness.GetDescription() << endl;
    cout << favoritePlace.GetDescription() << endl;
    cout << "  Rating: " << favoritePlace.GetRating() << endl;

    /*------------------------------------ additions to main() ------------------------------------*/

    cout << endl << endl;
    cout << "*------------------------------ Business Constructor Tests ------------------------------*" << endl;

    Business business2;
    business2.SetName("Subway");
    business2.SetAddress("1440 9th St W");
    business2.SetMaxOccupancy(7);
    cout << "INITIALIZE -> SET MEMBERS:" << endl;
    business2.print();
    cout << endl;

    Business business3("El Sol", "613 S Dubuque St", 3);
    cout << "INITIALIZE WITH PARAMETERS:" << endl;
    business3.print();
    cout << endl;

    Business business4;
    cout << "INITIALIZE WITH NO PARAMETERS:" << endl;
    business4.print();
    cout << endl;

    cout << "*------------------------------ Restaurant Constructor Tests ------------------------------*" << endl;

    Restaurant rest1;
    rest1.SetName("Joe's");
    rest1.SetAddress("Address 1");
    rest1.SetMaxOccupancy(100);
    rest1.SetRating(10);
    cout << "INITIALIZE -> SET MEMBERS:" << endl;
    rest1.print();
    cout << endl;

    Restaurant rest2("Q's", "Address 2", 50, 7);
    cout << "INITIALIZE WITH PARAMETERS:" << endl;
    rest2.print();
    cout << endl;

    Restaurant rest3;
    cout << "INITIALIZE WITH NO PARAMETERS:" << endl;
    rest3.print();
    cout << endl;

    cout << "*------------------------------ Business COPY Constructor Tests ------------------------------*" << endl;
    Business business5;
    business5.SetName("Tucker's");
    business5.SetAddress("Address b5");
    business5.SetMaxOccupancy(150);

    Business bCopy(business5);
    cout << "COPY BEFORE ORIGINAL DATA CHANGED" << endl;
    bCopy.print();
    cout << endl;

    business5.SetName("Tucker's NEW");
    business5.SetAddress("Address b5 NEW");
    business5.SetMaxOccupancy(300);

    cout << "COPY AFTER ORIGINAL DATA CHANGED" << endl;
    bCopy.print();
    cout << endl;

    cout << "*------------------------------ Restaurant COPY Constructor Tests ------------------------------*" << endl;
    Restaurant rest4;
    rest4.SetName("Reese's");
    rest4.SetAddress("Address r4");
    rest4.SetMaxOccupancy(125);
    rest4.SetRating(2);

    Restaurant rCopy(rest4);
    cout << "COPY BEFORE ORIGINAL DATA CHANGED" << endl;
    rCopy.print();
    cout << endl;

    rest4.SetName("Reese's NEW");
    rest4.SetAddress("Address r4 NEW");
    rest4.SetMaxOccupancy(250);
    rest4.SetRating(9);

    cout << "COPY AFTER ORIGINAL DATA CHANGED" << endl;
    rCopy.print();
    cout << endl;

    cout << "*--------------------------- Business COPY ASSIGNMENT OPERATOR Tests ---------------------------*" << endl;
    Business business6;
    business6.SetName("Kayle's");
    business6.SetAddress("Address b6");
    business6.SetMaxOccupancy(45);

    Business bCA;
    bCA = business6;
    cout << "COPY BEFORE ORIGINAL DATA CHANGED" << endl;
    bCA.print();
    cout << endl;

    business6.SetName("Kayle's NEW");
    business6.SetAddress("Address b6 NEW");
    business6.SetMaxOccupancy(90);

    cout << "COPY AFTER ORIGINAL DATA CHANGED" << endl;
    bCA.print();
    cout << endl;

    cout << "*--------------------------- Restaurant COPY ASSIGNMENT OPERATOR Tests ---------------------------*" << endl;
    Restaurant restaurant6;
    restaurant6.SetName("Emma's");
    restaurant6.SetAddress("Address r6");
    restaurant6.SetMaxOccupancy(35);
    restaurant6.SetRating(8);

    Restaurant rCA;
    rCA = restaurant6;
    cout << "COPY Before ORIGINAL DATA CHANGED" << endl;
    rCA.print();
    cout << endl;

    restaurant6.SetName("Emma's NEW");
    restaurant6.SetAddress("Address r6 NEW");
    restaurant6.SetMaxOccupancy(70);
    restaurant6.SetRating(16);

    cout << "COPY AFTER ORIGINAL DATA CHANGED" << endl;
    rCA.print();
    cout << endl;

    return 0;
}
