#include "MailConverter.h"

#include "Constants.h"
#include "Gpg.h"
#include "MailBoxReader.h"
#include "MailBoxWriter.h"
#include "bluetiger9/quotedprintable.h"

#include <QDebug>
#include <QStringBuilder>

using namespace unenigmail;


QString MailCoverter::fillLine(const QString &pInput, const QChar &pFiller,
                               const int pTargetLength)
{
    QString result = pInput;
    int missingChars = pTargetLength - result.size();
    while (missingChars-- > 0)
    {
        result = result % pFiller;
    }

    return result;
}


QVector<QString>
MailCoverter::generateGpgInfoFooter(const Gpg::Decrypted &pDecrypted)
{
    QVector<QString> footer;
    footer += QString();
    footer += fillLine("", '-', 90);
    footer += fillLine(QStringLiteral("--------- unenigmail ") + VERSION + " ",
                       '-', 90);
    footer += fillLine("", '-', 90);

    for (QByteArray const &line : pDecrypted.mStdErr.split('\n'))
    {
        footer += line;
    }

    footer += fillLine("", '-', 90);

    QVector<QString> encodedFooter;
    for (QString const &line : footer)
    {
        encodedFooter += QuotedPrintable::encode(line.toUtf8());
    }

    return encodedFooter;
}


bool MailCoverter::processDecryptedBody(Gpg::Decrypted const &pDecrypted,
                                        QVector<QString> &pInnerHead,
                                        QVector<QString> &pInnerBody)
{
    QVector<QString> decryptedLines;
    for (QByteArray const &line : pDecrypted.mStdOut.split('\n'))
    {
        decryptedLines += line;
    }


    const QString BOUNDARY(QStringLiteral("boundary=\""));
    QString currentBoundary;
    bool headFinished = false;
    for (int h = 0; h < decryptedLines.size(); h++)
    {
        QString const &line = decryptedLines.at(h);
        if (!headFinished &&
            line.startsWith(QLatin1String("Content-Type: multipart/mixed;")))
        {
            for (int i = h; i < decryptedLines.size(); i++)
            {
                h = i;

                QString const &boundaryLine = decryptedLines.at(i);
                if (boundaryLine.contains(BOUNDARY))
                {
                    currentBoundary = boundaryLine.mid(
                        boundaryLine.indexOf(BOUNDARY) + BOUNDARY.length(), -1);
                    currentBoundary =
                        currentBoundary.mid(0, currentBoundary.indexOf("\""));
                    break;
                }

                if (h == decryptedLines.size() - 1)
                {
                    qWarning()
                        << "Missing announced multipart boundary definition.";
                    return false;
                }
            }
        }
        else if (line.isEmpty())
        {
            headFinished = true;
        }
        else if (!currentBoundary.isEmpty() &&
                 line == QStringLiteral("--") + currentBoundary)
        {
            headFinished = false;
        }
        else if (!currentBoundary.isEmpty() &&
                 line ==
                     QStringLiteral("--") + currentBoundary +
                         QStringLiteral("--"))
        {
            int pos = h;
            if (pos < 0)
            {
                return false;
            }
            for (QString const &line : generateGpgInfoFooter(pDecrypted))
            {
                decryptedLines.insert(pos++, line);
            }
            break;
        }
    }

    if (currentBoundary.isEmpty())
    {
        qWarning() << "Decrypted GPG does not contain multipart header. This "
                      "is unhandled!";
        return false;
    }


    headFinished = false;
    for (QString const &line : decryptedLines)
    {
        if (!headFinished && line.isEmpty())
        {
            headFinished = true;
            continue;
        }

        if (headFinished)
        {
            pInnerBody += line;
        }
        else
        {
            pInnerHead += line;
        }
    }


    return true;
}


MailCoverter::ReturnCode
MailCoverter::stripEncryptionFromMail(Mail const &pMailInput, Mail &pMailOutput)
{
    pMailOutput = pMailInput;

    QVector<QString> &body = pMailOutput.getBodyRef();

    int begin = -1;
    int end = -1;
    for (int i = 0; i < body.size(); i++)
    {
        QString const &line = body.at(i);
        if (line == QLatin1String("-----BEGIN PGP MESSAGE-----"))
        {
            begin = i;
        }
        else if (line == QLatin1String("-----END PGP MESSAGE-----"))
        {
            end = i;
        }
    }

    if (begin == -1 || end == -1)
    {
        return NOT_ENCRYPTED;
    }

    QVector<QString> gpgEncrypted;
    for (int i = begin; i < end; i++)
    {
        gpgEncrypted += body.at(i);
    }

    Gpg::Decrypted decrypted = Gpg::decrypt(gpgEncrypted);
    if (!decrypted.isSuccessFull())
    {
        return ERROR;
    }

    bool encryptedMultipart = false;
    QVector<HeaderOption> &head = pMailOutput.getHeadRef();
    for (int i = 0; i < head.size(); i++)
    {
        HeaderOption option = head.at(i);
        if (option.startsWith(
                QLatin1String("Content-Type: multipart/encrypted;")))
        {
            encryptedMultipart = true;
            head.remove(i);
            i--;
        }
        else if (option.startsWith(QLatin1String("X-Enigmail")))
        {
            head.remove(i);
            i--;
        }
    }

    if (!encryptedMultipart)
    {
        qWarning()
            << "Found GPG witout multipart/encrypted. This is unhandled!";
        return ERROR;
    }
    // encrypted multipart means the whole body is replaced
    body.clear();

    QVector<QString> innerHead;
    QVector<QString> innerBody;
    if (!processDecryptedBody(decrypted, innerHead, innerBody))
    {
        qWarning() << "Failed to process decrypted part!";
        return ERROR;
    }

    pMailOutput.addHeaderOptions(innerHead);
    body += innerBody;

    return DECRYPTED;
}


MailCoverter::Statistic
MailCoverter::stripEncryptionFromFile(const QString &pPath)
{
    const QString pathTmp = pPath % QStringLiteral(".unenigmail");

    int processedTotal = 0;
    int processedDecrypted = 0;
    int processedFailed = 0;
    {
        MailBoxReader reader(pPath);
        MailBoxWriter writer(pathTmp);

        for (;;)
        {
            Mail mailOriginal = reader.next();
            if (mailOriginal.isNull())
            {
                break;
            }

            processedTotal++;

            Mail mailConverted;
            MailCoverter::ReturnCode ret =
                MailCoverter::stripEncryptionFromMail(mailOriginal,
                                                      mailConverted);
            if (ret == MailCoverter::ReturnCode::ERROR)
            {
                processedFailed++;
                qDebug() << "Failed to strip GPG from mail.";
            }

            if (ret == MailCoverter::ReturnCode::DECRYPTED)
            {
                processedDecrypted++;
                writer.write(mailConverted);
            }
            else
            {
                writer.write(mailOriginal);
            }
        }
    }

#ifndef QT_DEBUG
    if (processedDecrypted > 0)
    {
        QFile original(pPath);
        original.remove();
        QFile converted(pathTmp);
        converted.rename(pPath);
    }
    else
    {
        QFile converted(pathTmp);
        converted.remove();
    }
#else
    qInfo() << "** Debug Mode. Original file has not been overwritten.";
#endif

    return Statistic(processedTotal, processedDecrypted, processedFailed);
}
