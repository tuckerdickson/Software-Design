//----------------------------------------------------------
// Programmer: Tucker Dickson
// Date: February 14, 2021
// Name: main.cpp
// Description: This program uses a Matrix class to
//              initialize and manipulate 2x2 matrices
//              in various ways.
// ----------------------------------------------------------

#include <iostream>

using namespace std;

class Matrix {
public:
    explicit Matrix(double element1 = 0,
                    double element2 = 0,
                    double element3 = 0,
                    double element4 = 0);
    void setElement1(double value);
    void setElement2(double value);
    void setElement3(double value);
    void setElement4(double value);
    double getElement1() const;
    double getElement2() const;
    double getElement3() const;
    double getElement4() const;
    void print() const;
    double determinant() const;
    void inverse();
    void transpose();
    void add(Matrix matrix2);
    void multiply(Matrix matrix2);
    void copy(Matrix original);
private:
    double m_element1;
    double m_element2;
    double m_element3;
    double m_element4;
};

//----------------------------------------------------------
// Description: This function assigns user-input values
//              to the four private data members
// Inputs: double element1: a double which holds the value
//                          which will be assigned to the
//                          first row, first column
//                  element2: a double which holds the value
//                          which will be assigned to the
//                          first row, second column
//                  element3: a double which holds the value
//                          which will be assigned to the
//                          second row, first column
//                  element4: a double which holds the value
//                          which will be assigned to the
//                          second row, second column
// Outputs: None
// ----------------------------------------------------------
Matrix::Matrix(const double element1, const double element2, const double element3, const double element4) {
    setElement1(element1);
    setElement2(element2);
    setElement3(element3);
    setElement4(element4);
}

void Matrix::setElement1(const double value) {
    m_element1 = value;
}

void Matrix::setElement2(const double value) {
    m_element2 = value;
}

void Matrix::setElement3(const double value) {
    m_element3 = value;
}

void Matrix::setElement4(const double value) {
    m_element4 = value;
}

double Matrix::getElement1() const {
    return m_element1;
}

double Matrix::getElement2() const {
    return m_element2;
}

double Matrix::getElement3() const {
    return m_element3;
}

double Matrix::getElement4() const {
    return m_element4;
}

//----------------------------------------------------------
// Description: This function prints out a 2x2 matrix to
//              the console
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void Matrix::print() const {
    cout << m_element1 << "\t" << m_element2 << endl;
    cout << m_element3 << "\t" << m_element4 << endl;
}

//----------------------------------------------------------
// Description: This function computes and returns the
//              determinant of a 2x2 matrix
// Inputs: None
// Outputs: This function returns a double which holds
//          the value of the determinant
// ----------------------------------------------------------
double Matrix::determinant() const {
    double determinant = (m_element1 * m_element4) - (m_element2 * m_element3);
    return determinant;
}

//----------------------------------------------------------
// Description: This function computes the inverse of
//              a 2x2 matrix and then assigns the 'inversed'
//              matrix to the original matrix
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void Matrix::inverse() {
    Matrix matrix(m_element1, m_element2, m_element3, m_element4);
    double det = matrix.determinant();

    if(det != 0) {
        m_element1 = matrix.m_element4 / det;
        m_element2 = (-1 * matrix.m_element2) / det;
        m_element3 = (-1 * matrix.m_element3) / det;
        m_element4 = matrix.m_element1 / det;
    }
    else {
        cout << "This matrix doesn't have an inverse..." << endl << endl;
    }
}

//----------------------------------------------------------
// Description: This function computes the transpose of
//              a 2x2 matrix and then assigns the transposed
//              matrix to the original matrix
// Inputs: None
// Outputs: None
// ----------------------------------------------------------
void Matrix::transpose() {
    Matrix matrix(m_element1, m_element2, m_element3, m_element4);

    m_element2 = matrix.m_element3;
    m_element3 = matrix.m_element2;
}

//----------------------------------------------------------
// Description: This function takes a 2x2 matrix and element
//              -by-element, adds the values of another 2x2
//              matrix
// Inputs: Matrix matrix2: a variable of type Matrix that
//                          contains the values to be added
//                          to the original matrix
// Outputs: None
// ----------------------------------------------------------
void Matrix::add(const Matrix matrix2) {
    m_element1 += matrix2.m_element1;
    m_element2 += matrix2.m_element2;
    m_element3 += matrix2.m_element3;
    m_element4 += matrix2.m_element4;
}

//----------------------------------------------------------
// Description: This function calculates the product of two
//              2x2 matrices, and assigns the resulting
//              matrix to the original matrix
// Inputs: Matrix matrix2: a variable of type Matrix that
//                          will be multiplied with the
//                          original matrix
// Outputs: None
// ----------------------------------------------------------
void Matrix::multiply(const Matrix matrix2) {
    Matrix origCopy(m_element1, m_element2, m_element3, m_element4);
    m_element1 = (origCopy.m_element1 * matrix2.m_element1) + (origCopy.m_element2 * matrix2.m_element3);
    m_element2 = (origCopy.m_element1 * matrix2.m_element2) + (origCopy.m_element2 * matrix2.m_element4);
    m_element3 = (origCopy.m_element3 * matrix2.m_element1) + (origCopy.m_element4 * matrix2.m_element3);
    m_element4 = (origCopy.m_element3 * matrix2.m_element2) + (origCopy.m_element4 * matrix2.m_element4);
}

//----------------------------------------------------------
// Description: This function takes a 2x2 matrix and element
//              -by-element, assigns the values (copies them)
//              to another 2x2 matrix
// Inputs: Matrix original: a variable of type Matrix that
//                          contains the values to be
//                          assigned to another matrix
// Outputs: None
// ----------------------------------------------------------
void Matrix::copy(const Matrix original) {
    m_element1 = original.m_element1;
    m_element2 = original.m_element2;
    m_element3 = original.m_element3;
    m_element4 = original.m_element4;
}

int main() {
    // test 1
    cout << "Begin Testing" << endl << endl;
    Matrix m1;

    /*------------------------------- getter/setter methods *-------------------------------*/
    cout << "*------------------------------- Setter and Getter Methods -------------------------------*" << endl;
    cout << "\tMatrix m1;" << endl;
    cout << "\tm1.setElement1(3);" << endl;
    m1.setElement1(3);
    cout << "\tm1.getElement1();" << endl;
    cout << "\t\tExpected Output: 3" << endl;
    cout << "\t\tTest Output: " << m1.getElement1() << endl;

    if (m1.getElement1() != 3) {
        cout << "\t\tTEST FAILED" << endl << endl;
    }
    else {
        cout << "\t\tTEST PASSED" << endl << endl;
    }

    cout << "\tm1.setElement2(-39);" << endl;
    m1.setElement2(-39);
    cout << "\tm1.getElement2();" << endl;
    cout << "\t\tExpected Output: -39" << endl;
    cout << "\t\tTest Output: " << m1.getElement2() << endl;

    if (m1.getElement2() != -39) {
        cout << "\t\tTEST FAILED" << endl << endl;
    }
    else {
        cout << "\t\tTEST PASSED" << endl << endl;
    }

    cout << "\tm1.setElement3(0);" << endl;
    m1.setElement3(0);
    cout << "\tm1.getElement3();" << endl;
    cout << "\t\tExpected Output: 0" << endl;
    cout << "\t\tTest Output: " << m1.getElement3() << endl;

    if (m1.getElement3() != 0) {
        cout << "\t\tTEST FAILED" << endl << endl;
    }
    else {
        cout << "\t\tTEST PASSED" << endl << endl;
    }

    cout << "\tm1.setElement4(101);" << endl;
    m1.setElement4(101);
    cout << "\tm1.getElement4();" << endl;
    cout << "\t\tExpected Output: 101" << endl;
    cout << "\t\tTest Output: " << m1.getElement4() << endl;

    if (m1.getElement4() != 101) {
        cout << "\t\tTEST FAILED" << endl << endl;
    }
    else {
        cout << "\t\tTEST PASSED" << endl << endl;
    }

    /*------------------------------- print method -------------------------------*/
    cout << "*------------------------------- Print method -------------------------------*" << endl;

    cout << "\tMatrix m2(-19, 92, 0, 3);" << endl;
    Matrix m2(-19, 92, 0, 3);
    cout << "\tm2.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "-19\t92\n0\t3" << endl;
    cout << "\t\tTest Output: " << endl;
    m2.print();
    cout << endl;

    cout << "\tMatrix m3(49, 6, -10);" << endl;
    Matrix m3(49, 6, -10);
    cout << "\tm3.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "49\t6\n-10\t0" << endl;
    cout << "\t\tTest Output: " << endl;
    m3.print();
    cout << endl;

    cout << "\tMatrix m4(0, 13);" << endl;
    Matrix m4(0,13);
    cout << "\tm4.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "0\t13\n0\t0" << endl;
    cout << "\t\tTest Output: " << endl;
    m4.print();
    cout << endl;

    cout << "\tMatrix m5;" << endl << endl;
    Matrix m5;
    cout << "\tm5.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "0\t0\n0\t0" << endl;
    cout << "\t\tTest Output: " << endl;
    m5.print();
    cout << endl;

    /*------------------------------- determinant method *-------------------------------*/
    cout << "*------------------------------- Determinant Method -------------------------------*" << endl;

    cout << "\tMatrix m6(1, -4, 0, 3);" << endl;
    Matrix m6(1, -4, 0, 3);
    cout << "\tdouble i = m6.determinant();" << endl;
    double i = m6.determinant();
    cout << "\tcout << i;" << endl;
    cout << "\t\tExpected Output: 3" << endl;
    cout << "\t\tTest Output: " << i << endl;

    if (i != 3) {
        cout << "\t\tTEST FAILED" << endl << endl;
    }
    else {
        cout << "\t\tTEST PASSED" << endl << endl;
    }

    cout << "\tMatrix m7(2, 1, -1);" << endl;
    Matrix m7(2, 1, -1);
    cout << "\tdouble i = m7.determinant();" << endl;
    i = m7.determinant();
    cout << "\tcout << i;" << endl;
    cout << "\t\tExpected Output: 1" << endl;
    cout << "\t\tTest Output: " << i << endl;

    if (i != 1) {
        cout << "\t\tTEST FAILED" << endl << endl;
    }
    else {
        cout << "\t\tTEST PASSED" << endl << endl;
    }

    cout << "\tMatrix m8(-3, 6);" << endl;
    Matrix m8(-3,6);
    cout << "\tdouble i = m8.determinant();" << endl;
    i = m8.determinant();
    cout << "\tcout << i;" << endl;
    cout << "\t\tExpected Output: 0" << endl;
    cout << "\t\tTest Output: " << i << endl;

    if (i != 0) {
        cout << "\t\tTEST FAILED" << endl << endl;
    }
    else {
        cout << "\t\tTEST PASSED" << endl << endl;
    }

    cout << "\tMatrix m9;" << endl;
    Matrix m9;
    cout << "\tdouble i = m9.determinant();" << endl;
    i = m9.determinant();
    cout << "\tcout << i;" << endl;
    cout << "\t\tExpected Output: 0" << endl;
    cout << "\t\tTest Output: " << i << endl;

    if (i != 0) {
        cout << "\t\tTEST FAILED" << endl << endl;
    }
    else {
        cout << "\t\tTEST PASSED" << endl << endl;
    }

    /*------------------------------- inverse method *-------------------------------*/
    cout << "*------------------------------- Inverse Method -------------------------------*" << endl;

    cout << "\tMatrix m10(5, 2, -7, -3);" << endl;
    Matrix m10(5, 2, -7, -3);
    cout << "\tm10.inverse();" << endl;
    m10.inverse();
    cout << "\tm10.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "3\t2\n-7\t-5" << endl;
    cout << "\t\tTest Output:" << endl;
    m10.print();
    if (m10.getElement1() == 3 && m10.getElement2() == 2 && m10.getElement3() == -7 && m10.getElement4() == -5) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m11(-3, 1, 5, -2);" << endl;
    Matrix m11(-3, 1, 5, -2);
    cout << "\tm11.inverse();" << endl;
    m11.inverse();
    cout << "\tm11.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "-2\t-1\n-5\t-3" << endl;
    cout << "\t\tTest Output:" << endl;
    m11.print();
    if (m11.getElement1() == -2 && m11.getElement2() == -1 && m11.getElement3() == -5 && m11.getElement4() == -3) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m12(4, 2, 2, 1);" << endl;
    Matrix m12(4, 2, 2, 1);
    cout << "\tm12.inverse();" << endl;
    cout << "\tm12.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "This matrix doesn't have an inverse..." << endl;
    cout << "4\t2\n2\t1" << endl;
    cout << "\t\tTest Output:" << endl;
    m12.inverse();
    m12.print();
    if (m12.getElement1() == 4 && m12.getElement2() == 2 && m12.getElement3() == 2 && m12.getElement4() == 1) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m13;" << endl;
    Matrix m13;
    cout << "\tm13.inverse();" << endl;
    cout << "\tm13.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "This matrix doesn't have an inverse..." << endl;
    cout << "0\t0\n0\t0" << endl;
    cout << "\t\tTest Output:" << endl;
    m13.inverse();
    m13.print();
    if (m13.getElement1() == 0 && m13.getElement2() == 0 && m13.getElement3() == 0 && m13.getElement4() == 0) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    /*------------------------------- transpose method *-------------------------------*/
    cout << "*------------------------------- Transpose Method -------------------------------*" << endl;

    cout << "\tMatrix m14(0, 4, 2, 1);" << endl;
    Matrix m14(0, 4, 2, 1);
    cout << "\tm14.transpose();" << endl;
    m14.transpose();
    cout << "\tm14.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "0\t2\n4\t1" << endl;
    cout << "\t\tTest Output:" << endl;
    m14.print();

    if (m14.getElement1() == 0 && m14.getElement2() == 2 && m14.getElement3() == 4 && m14.getElement4() == 1) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m15(-1, 3, 8, -5);" << endl;
    Matrix m15(-1, 3, 8, -5);
    cout << "\tm15.transpose();" << endl;
    m15.transpose();
    cout << "\tm15.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "-1\t8\n3\t-5" << endl;
    cout << "\t\tTest Output:" << endl;
    m15.print();

    if (m15.getElement1() == -1 && m15.getElement2() == 8 && m15.getElement3() == 3 && m15.getElement4() == -5) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m16(6, 2, -1);" << endl;
    Matrix m16(6, 2, -1);
    cout << "\tm16.transpose();" << endl;
    m16.transpose();
    cout << "\tm16.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "6\t-1\n2\t0" << endl;
    cout << "\t\tTest Output:" << endl;
    m16.print();

    if (m16.getElement1() == 6 && m16.getElement2() == -1 && m16.getElement3() == 2 && m16.getElement4() == 0) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m17(5, -3);" << endl;
    Matrix m17(5, -3);
    cout << "\tm17.transpose();" << endl;
    m17.transpose();
    cout << "\tm17.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "5\t0\n-3\t0" << endl;
    cout << "\t\tTest Output:" << endl;
    m17.print();

    if (m17.getElement1() == 5 && m17.getElement2() == 0 && m17.getElement3() == -3 && m17.getElement4() == 0) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

/*------------------------------- Add method *-------------------------------*/
    cout << "*------------------------------- Add Method -------------------------------*" << endl;

    cout << "\tMatrix m18(-4, 5, 2, -3);" << endl;
    Matrix m18(-4, 5, 2, -3);
    cout << "\tMatrix m19(6, 0, 3, -2);" << endl;
    Matrix m19(6, 0, 3, -2);
    cout << "\tm18.add(m19);" << endl;
    m18.add(m19);
    cout << "\tm18.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "2\t5\n5\t-5" << endl;
    cout << "\t\tTest Output:" << endl;
    m18.print();

    if (m18.getElement1() == 2 && m18.getElement2() == 5 && m18.getElement3() == 5 && m18.getElement4() == -5) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m20(1, 5, 9);" << endl;
    Matrix m20(1, 5, 9);
    cout << "\tMatrix m21(0, 0, 0, 1);" << endl;
    Matrix m21(0, 0, 0, 1);
    cout << "\tm20.add(m21);" << endl;
    m20.add(m21);
    cout << "\tm20.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "1\t5\n9\t1" << endl;
    cout << "\t\tTest Output:" << endl;
    m20.print();

    if (m20.getElement1() == 1 && m20.getElement2() == 5 && m20.getElement3() == 9 && m20.getElement4() == 1) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m22;" << endl;
    Matrix m22;
    cout << "\tMatrix m23(5, -2);" << endl;
    Matrix m23(5, -2);
    cout << "\tm22.add(m23);" << endl;
    m22.add(m23);
    cout << "\tm22.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "5\t-2\n0\t0" << endl;
    cout << "\t\tTest Output:" << endl;
    m22.print();

    if (m22.getElement1() == 5 && m22.getElement2() == -2 && m22.getElement3() == 0 && m22.getElement4() == 0) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m24(-3, -4, -9);" << endl;
    Matrix m24(-3, -4, -9);
    cout << "\tMatrix m25(-1, 0, 0, -1);" << endl;
    Matrix m25(-1, 0, 0, -1);
    cout << "\tm24.add(m25);" << endl;
    m24.add(m25);
    cout << "\tm24.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "-4\t-4\n-9\t-1" << endl;
    cout << "\t\tTest Output:" << endl;
    m24.print();

    if (m24.getElement1() == -4 && m24.getElement2() == -4 && m24.getElement3() == -9 && m24.getElement4() == -1) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    /*------------------------------- multiply method *-------------------------------*/
    cout << "*------------------------------- Multiply Method -------------------------------*" << endl;

    cout << "\tMatrix m26(4, 3, 1, 1);" << endl;
    Matrix m26(4, 3, 1, 1);
    cout << "\tMatrix m27(4, 3, 1, 1);" << endl;
    Matrix m27(4, 3, 1, 1);
    cout << "\tm26.inverse();" << endl;
    m26.inverse();
    cout << "\tm26.multiply(m27);" << endl;
    m26.multiply(m27);
    cout << "\t\tExpected Output:" << endl;
    cout << "1\t0\n0\t1" << endl;
    cout << "\t\tTest Output:" << endl;
    m26.print();

    if (m26.getElement1() == 1 && m26.getElement2() == 0 && m26.getElement3() == 0 && m26.getElement4() == 1) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m28(3, 1, 4, 2);" << endl;
    Matrix m28(3, 1, 4, 2);
    cout << "\tMatrix m29(3, 1, 4, 2);" << endl;
    Matrix m29(3, 1, 4, 2);
    cout << "\tm29.inverse();" << endl;
    m29.inverse();
    cout << "\tm28.multiply(m29);" << endl;
    m28.multiply(m29);
    cout << "\t\tExpected Output:" << endl;
    cout << "1\t0\n0\t1" << endl;
    cout << "\t\tTest Output:" << endl;
    m28.print();

    if (m28.getElement1() == 1 && m28.getElement2() == 0 && m28.getElement3() == 0 && m28.getElement4() == 1) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m30(5, 8, 3, 8);" << endl;
    Matrix m30(5, 8, 3, 8);
    cout << "\tMatrix m31(3, 8, 8, 9);" << endl;
    Matrix m31(3, 8, 8, 9);
    cout << "\tm30.multiply(m31);" << endl;
    m30.multiply(m31);
    cout << "\t\tExpected Output:" << endl;
    cout << "79\t112\n73\t96" << endl;
    cout << "\t\tTest Output:" << endl;
    m30.print();

    if (m30.getElement1() == 79 && m30.getElement2() == 112 && m30.getElement3() == 73 && m30.getElement4() == 96) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m30(12, -29, -42, 28);" << endl;
    Matrix m32(12, -29, -42, 28);
    cout << "\tMatrix m31(-52, -12, 40, 2);" << endl;
    Matrix m33(-52, -12, 40, 2);
    cout << "\tm32.multiply(m33);" << endl;
    m32.multiply(m33);
    cout << "\t\tExpected Output:" << endl;
    cout << "-1784\t-202\n3304\t560" << endl;
    cout << "\t\tTest Output:" << endl;
    m32.print();

    if (m32.getElement1() == -1784 && m32.getElement2() == -202 && m32.getElement3() == 3304 && m32.getElement4() == 560) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    /*------------------------------- copy method *-------------------------------*/
    cout << "*------------------------------- Copy Method -------------------------------*" << endl;

    cout << "\tMatrix m34(-4, 9, 0, 12);" << endl;
    Matrix m34(-4, 9, 0, 12);
    cout << "\tMatrix m35;" << endl;
    Matrix m35;
    cout << "\tm35.copy(m34);" << endl;
    m35.copy(m34);
    cout << "\tm35.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "-4\t9\n0\t12" << endl;
    cout << "\t\tTest Output:" << endl;
    m35.print();

    if (m34.getElement1() == m35.getElement1() && m34.getElement2() == m35.getElement2()
        && m34.getElement3() == m35.getElement3() && m34.getElement4() == m35.getElement4()) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m36(-2, -7, -34, -23);" << endl;
    Matrix m36(-2, -7, -34, -23);
    cout << "\tMatrix m37(45, 92, 0);" << endl;
    Matrix m37(45, 92, 0);
    cout << "\tm36.copy(m37);" << endl;
    m36.copy(m37);
    cout << "\tm36.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "45\t92\n0\t0" << endl;
    cout << "\t\tTest Output:" << endl;
    m36.print();

    if (m36.getElement1() == m37.getElement1() && m36.getElement2() == m37.getElement2()
        && m36.getElement3() == m37.getElement3() && m36.getElement4() == m37.getElement4()) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m38;" << endl;
    Matrix m38;
    cout << "\tMatrix m39(23, 65, -23, 12);" << endl;
    Matrix m39(23, 65, -23, 12);
    cout << "\tm39.copy(m38);" << endl;
    m39.copy(m38);
    cout << "\tm39.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "0\t0\n0\t0" << endl;
    cout << "\t\tTest Output:" << endl;
    m39.print();

    if (m38.getElement1() == m39.getElement1() && m38.getElement2() == m39.getElement2()
        && m38.getElement3() == m39.getElement3() && m38.getElement4() == m39.getElement4()) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "\tMatrix m40;" << endl;
    Matrix m40;
    cout << "\tMatrix m41(23, 65, -23, 12);" << endl;
    Matrix m41(23, 65, -23, 12);
    cout << "\tm40.copy(m41);" << endl;
    m40.copy(m41);
    cout << "\tm40.print();" << endl;
    cout << "\t\tExpected Output:" << endl;
    cout << "23\t65\n-23\t12" << endl;
    cout << "\t\tTest Output:" << endl;
    m40.print();

    if (m40.getElement1() == m41.getElement1() && m40.getElement2() == m41.getElement2()
        && m40.getElement3() == m41.getElement3() && m40.getElement4() == m41.getElement4()) {
        cout << "\t\tTEST PASSED" << endl << endl;
    }
    else {
        cout << "\t\tTEST FAILED" << endl << endl;
    }

    cout << "*----------------------------------------------------------------------------*" << endl;
    cout << endl;
    cout << "\t\t\t\t\tEnd Testing" << endl;
    return 0;
}
