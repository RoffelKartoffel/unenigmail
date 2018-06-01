#include "src/MailBoxReader.h"
#include "src/MailBoxWriter.h"
#include "src/MailConverter.h"

#include <QtTest/QtTest>

using namespace unenigmail;


class Test_Mbox : public QObject
{
    Q_OBJECT
  private:
    QVector<Mail> mails_input;
    QVector<Mail> mails_expected;

    int countInHeader(Mail const &pMail, QString const &pString)
    {
        int count = 0;
        for (HeaderOption const &option : pMail.getHead())
        {
            for (QString const &line : option)
            {
                if (line.contains(pString))
                {
                    count++;
                }
            }
        }
        return count;
    }

    bool containsOnce(Mail const &pMail, QString const &pString)
    {
        return countInHeader(pMail, pString) == 1;
    }

    bool containsNone(Mail const &pMail, QString const &pString)
    {
        return countInHeader(pMail, pString) == 0;
    }


  private slots:

    void initTestCase()
    {
        MailBoxReader readerInput("mbox2");
        do
        {
            Mail mail = readerInput.next();
            if (mail.isNull())
            {
                break;
            }
            mails_input += mail;
        } while (true);
        QCOMPARE(mails_input.size(), 3);

        MailBoxReader readerExpected("mbox2");
        do
        {
            Mail mail = readerExpected.next();
            if (mail.isNull())
            {
                break;
            }
            mails_expected += mail;
        } while (true);
        QCOMPARE(mails_expected.size(), mails_input.size());
    }


    void simpleDecryption()
    {
        QCOMPARE(mails_input.size(), 3);

        Mail mailConverted;
        QCOMPARE(MailConverter::stripEncryptionFromMail(mails_input.at(0), mailConverted),
                 MailConverter::ReturnCode::DECRYPTED);

        QCOMPARE(MailConverter::stripEncryptionFromMail(mails_input.at(1), mailConverted),
                 MailConverter::ReturnCode::NOT_ENCRYPTED);

        QCOMPARE(MailConverter::stripEncryptionFromMail(mails_input.at(2), mailConverted),
                 MailConverter::ReturnCode::DECRYPTED);
    }


    void decryptionAndHeaderProcessing()
    {
        QCOMPARE(mails_input.size(), 3);

        Mail mailConverted;
        MailConverter::stripEncryptionFromMail(mails_input.at(0), mailConverted);
        QVERIFY(containsOnce(mailConverted,
                             "Content-Type: multipart/mixed; boundary=\"Ws2toQn5kRbSRMScofrJoVwRe7Jl1aLKf\""));
        QVERIFY(containsNone(mailConverted, "X-Enigmail"));
        QVERIFY(containsNone(mailConverted, "Content-Type: multipart/encrypted;"));


        MailConverter::stripEncryptionFromMail(mails_input.at(2), mailConverted);
        QVERIFY(containsOnce(mailConverted,
                             "Content-Type: multipart/mixed; boundary=\"2SaJxuSc0VQfGEOeLF82TMPoGEVHvsQ3m\""));
        QVERIFY(containsNone(mailConverted, "X-Enigmail"));
        QVERIFY(containsNone(mailConverted, "Content-Type: multipart/encrypted;"));
    }


    void overallOutput()
    {
        QTemporaryFile file;
        file.open();
        const QString tmpFileName = file.fileName();

        {
            MailConverter::stripEncryptionFromFile("mbox2", tmpFileName);
        }

        QFile expected("mbox2_output");
        QFile output(tmpFileName);
        QVERIFY(expected.open(QIODevice::ReadOnly));
        QVERIFY(output.open(QIODevice::ReadOnly));
        QCOMPARE(output.readAll(), expected.readAll());
    }
};

QTEST_MAIN(Test_Mbox)
