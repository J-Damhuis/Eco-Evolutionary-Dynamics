SOURCES += main.cpp

# C++14
CONFIG += c++14
CONFIG += resources_big
QMAKE_CXXFLAGS += -std=c++14

# High warning levels
# SFML goes bad with -Weffc++
QMAKE_CXXFLAGS += -Wall -Wextra -Wshadow -Wnon-virtual-dtor -pedantic -Werror

unix:!macx {
# Fix error: unrecognized option '--push-state--no-as-needed'
QMAKE_LFLAGS += -fuse-ld=gold
}

# Needed this to make std::cout work
# CONFIG += console
# NOTE you need to flush a cout to print it
# seems like std::endl is superior after all

# Debug and release settings
CONFIG += debug_and_release
CONFIG(release, debug|release) {

  DEFINES += NDEBUG

  # GNU/Linux
  unix:!macx {
    # gprof
    QMAKE_CXXFLAGS += -pg
    QMAKE_LFLAGS += -pg

    # helgrind, for helgrind and memcheck
    QMAKE_LFLAGS += -pthread -Wl,--no-as-needed
  }
}

CONFIG(debug, debug|release) {

  # GNU/Linux
  unix:!macx {
    # gcov
    QMAKE_CXXFLAGS += -fprofile-arcs -ftest-coverage
    LIBS += -lgcov

    # helgrind, for helgrind and memcheck
    QMAKE_LFLAGS += -pthread -Wl,--no-as-needed

    # UBSAN
    QMAKE_CXXFLAGS += -fsanitize=undefined
    QMAKE_LFLAGS += -fsanitize=undefined
    LIBS += -lubsan
  }
}

# High warning level, warnings are errors
QMAKE_CXXFLAGS += -Wall -Wextra -Wshadow -Wnon-virtual-dtor -pedantic
QMAKE_CXXFLAGS += -Werror

# Qt5
QT += core gui

# QResources give this error
QMAKE_CXXFLAGS += -Wno-unused-variable

# Fixes
#/usr/include/boost/math/constants/constants.hpp:277: error: unable to find numeric literal operator 'operator""Q'
#   BOOST_DEFINE_MATH_CONSTANT(half, 5.000000000000000000000000000000000000e-01, "5.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e-01")
#   ^
QMAKE_CXXFLAGS += -fext-numeric-literals

# Prevent Qt for failing with this error:
# qrc_[*].cpp:400:44: error: 'qInitResources_[*]__init_variable__' defined but not used
# [*]: the resource filename
QMAKE_CXXFLAGS += -Wno-unused-variable
