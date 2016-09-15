# unenigmail

Unenigmail strips Enigmails GPG encryption from emails within a supplied mbox file. Since version 0.20 this project has been rewritten in C++ using Qt 5.7. The mails are read from the mbox and gpg encryption is piped through the commandline gpg application in order to decrypt it. If your key is protected by a passphrase then it is recommended that you run a gpg-agent in order to supply the passphares.

Note: Using unenigmail is only recommended if you store your emails in a secure environment. For example an mbox on your local disk with disk encrypion.

## Usage
```sh
Usage: ./unenigmail [options] file

Options:
  -h, --help  Displays this help.
  -v          Show verbose program output.

Arguments:
  file        Mbox to strip encryption from.
```

## Compiling the application
```sh
cd unenigmail
./configure
cd build.release
make -j
```
### Running the tests
```sh
cd build.release
make test
```
