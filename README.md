# unenigmail

Unenigmail strips Enigmails GPG encryption from emails within a supplied mbox file. Since version 0.20 this project has been rewritten in C++ using Qt 5.7.
Note: This is only recommended if you store your emails in a secure environment. For example an mbox on your local disk with disk encrypion.

## Building
```sh
cd unenigmail
./configure
cd build.release
make -j
```

## Usage
```sh
Usage: ./unenigmail [options] file

Options:
  -h, --help  Displays this help.
  -v          Show verbose program output.

Arguments:
  file        Mbox to strip encryption from.
```
