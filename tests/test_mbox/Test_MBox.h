#include "src/MailBoxReader.h"
#include "src/MailBoxWriter.h"

#include <QtTest/QtTest>

using namespace unenigmail;


class Test_Mbox : public QObject
{
    Q_OBJECT
  private slots:
    void readMails()
    {
        QVector<Mail> mails;
        MailBoxReader reader("mbox1");
        do
        {
            Mail mail = reader.next();
            if (mail.isNull())
            {
                break;
            }
            mails += mail;
        } while (true);
        QCOMPARE(mails.size(), 3);

        Mail mail = mails.at(0);
        QCOMPARE(mail.getHeadRef().size(), 26);
        QCOMPARE(mail.getBodyRef().size(), 21);

        mail = mails.at(1);
        QCOMPARE(mail.getHeadRef().size(), 32);
        QCOMPARE(mail.getBodyRef().size(), 12);

        mail = mails.at(2);
        QCOMPARE(mail.getHeadRef().size(), 37);
        QCOMPARE(mail.getBodyRef().size(), 21);
    }


    void readWriteMbox()
    {
        QVector<Mail> mails;
        MailBoxReader reader("mbox1");
        do
        {
            Mail mail = reader.next();
            if (mail.isNull())
            {
                break;
            }
            mails += mail;
        } while (true);

        QTemporaryFile file;
        file.open();
        const QString tmpFileName = file.fileName();

        {
            MailBoxWriter writer(tmpFileName);
            for (const Mail &mail : mails)
            {
                writer.write(mail);
            }
        }

        QFile expected("mbox1_output");
        QFile output(tmpFileName);
        QVERIFY(expected.open(QIODevice::ReadOnly));
        QVERIFY(output.open(QIODevice::ReadOnly));
        QCOMPARE(output.readAll(), expected.readAll());
    }
};

QTEST_MAIN(Test_Mbox)
