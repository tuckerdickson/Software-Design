cmake_minimum_required(VERSION 3.17)
project(hw3)

set(CMAKE_CXX_STANDARD 14)

add_executable(hw3 main.cpp Student.cpp Student.h Chair.cpp Chair.h Classroom.cpp Classroom.h)

configure_file(student.txt student.txt COPYONLY)
configure_file(layoutCBW128.txt layoutCBW128.txt COPYONLY)
configure_file(layoutEPB109.txt layoutEPB109.txt COPYONLY)
configure_file(layoutPHAR100A.txt layoutPHAR100A.txt COPYONLY)