# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.19

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Disable VCS-based implicit rules.
% : %,v


# Disable VCS-based implicit rules.
% : RCS/%


# Disable VCS-based implicit rules.
% : RCS/%,v


# Disable VCS-based implicit rules.
% : SCCS/s.%


# Disable VCS-based implicit rules.
% : s.%


.SUFFIXES: .hpux_make_needs_suffix_list


# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /Applications/CLion.app/Contents/bin/cmake/mac/bin/cmake

# The command to remove a file.
RM = /Applications/CLion.app/Contents/bin/cmake/mac/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5/cmake-build-debug

# Include any dependencies generated for this target.
include CMakeFiles/Xonix.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/Xonix.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/Xonix.dir/flags.make

CMakeFiles/Xonix.dir/main.cpp.o: CMakeFiles/Xonix.dir/flags.make
CMakeFiles/Xonix.dir/main.cpp.o: ../main.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/Xonix.dir/main.cpp.o"
	/Library/Developer/CommandLineTools/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/Xonix.dir/main.cpp.o -c /Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5/main.cpp

CMakeFiles/Xonix.dir/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/Xonix.dir/main.cpp.i"
	/Library/Developer/CommandLineTools/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5/main.cpp > CMakeFiles/Xonix.dir/main.cpp.i

CMakeFiles/Xonix.dir/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/Xonix.dir/main.cpp.s"
	/Library/Developer/CommandLineTools/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5/main.cpp -o CMakeFiles/Xonix.dir/main.cpp.s

# Object files for target Xonix
Xonix_OBJECTS = \
"CMakeFiles/Xonix.dir/main.cpp.o"

# External object files for target Xonix
Xonix_EXTERNAL_OBJECTS =

Xonix: CMakeFiles/Xonix.dir/main.cpp.o
Xonix: CMakeFiles/Xonix.dir/build.make
Xonix: /usr/local/lib/libsfml-network.dylib
Xonix: /usr/local/lib/libsfml-audio.dylib
Xonix: /usr/local/lib/libsfml-graphics.dylib
Xonix: /usr/local/lib/libsfml-window.dylib
Xonix: /usr/local/lib/libsfml-system.dylib
Xonix: CMakeFiles/Xonix.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable Xonix"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/Xonix.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/Xonix.dir/build: Xonix

.PHONY : CMakeFiles/Xonix.dir/build

CMakeFiles/Xonix.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/Xonix.dir/cmake_clean.cmake
.PHONY : CMakeFiles/Xonix.dir/clean

CMakeFiles/Xonix.dir/depend:
	cd /Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5/cmake-build-debug && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5 /Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5 /Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5/cmake-build-debug /Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5/cmake-build-debug /Users/tuckerdickson/engr2730spring2021/ntdickson/homework/hw5/cmake-build-debug/CMakeFiles/Xonix.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/Xonix.dir/depend

