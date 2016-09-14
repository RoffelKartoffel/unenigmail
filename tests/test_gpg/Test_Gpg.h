#include "src/Gpg.h"

#include <QDebug>
#include <QtTest/QtTest>

using namespace unenigmail;


class Test_Gpg : public QObject
{
    Q_OBJECT
  private slots:
    void decryptionTest()
    {
        qDebug() << "Please be sure to import the required key before you run "
                    "this test!";

        QVector<QString> input;
        input += "-----BEGIN PGP MESSAGE-----";
        input += "";
        input += "hQEMAzEmlP6ldGvRAQgAqjFSGhJ0cC9Z3rPXssYDEijscGAoN08zXwOCcXV4b21C";
        input += "s6rQ4fdyGtW+YZ3VMOcBlT2RYGg4y4KNwZNhG3dN4K3PNNdXoAY9sCOphylWKKwG";
        input += "n8/xKeqBHK5CqoyHD9tZ23QWGFJ/AbdULq1K3/TD9WgUumtx58C6rUTHvdLP+Bnu";
        input += "XZIbPW0EJgWwtqS4t4woprtI4gm/p5/2mFQmCLNpNQo9/n0QX8lZd9rTH3FYLjO2";
        input += "pNTsJ+8Zi3a4HqCTb7gde0fYcxg5n1kFltWd80WCWVsunCvWqcaPxG2l6FzFI3bR";
        input += "XlQ8tyLDXrZ49m+onJyaegXMFkQT/S5pmHE1LrgaLdJOARUqRZA24TI6P1OG4hBV";
        input += "gOsTEayoiohsIcvzNah3EEiln8kKHlRaPicy5a3Nffholu44Kkk+SELuOXJnhkLp";
        input += "kL4bS2x3K1Xm3RpCIv/Y";
        input += "=e7EE";
        input += "-----END PGP MESSAGE-----";

        Gpg::Decrypted decrypted = Gpg::decrypt(input);
        QVERIFY(decrypted.isSuccessFull() == true);

        QVector<QString> output;
        for (QByteArray line : decrypted.mStdOut.split('\n'))
        {
            output += line;
        }

        QVector<QString> expected;
        expected += "multiline";
        expected += "gpg test";
        expected += "";

        QCOMPARE(output.size(), expected.size());

        for (int i = 0; i < output.size(); i++)
        {
            QCOMPARE(output.at(i), expected.at(i));
        }
    }
};

QTEST_MAIN(Test_Gpg)
