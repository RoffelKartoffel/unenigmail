QT += core
QT -= gui

CONFIG += c++11

TARGET = unenigmail
CONFIG += console
CONFIG -= app_bundle

TEMPLATE = app

SOURCES += main.cpp \
    Mail.cpp \
    MailBoxReader.cpp \
    MailBoxWriter.cpp \
    Gpg.cpp \
    MailConverter.cpp \
    bluetiger9/quotedprintable.cpp

HEADERS += \
    Mail.h \
    MailBoxReader.h \
    MailBoxWriter.h \
    Gpg.h \
    MailConverter.h \
    Constants.h \
    bluetiger9/quotedprintable.h
