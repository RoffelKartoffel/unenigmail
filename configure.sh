mkdir build.release -p
cd build.release
qmake -makefile "TARGET = release_binary" ../src/unenigmail.pro
cd ..
