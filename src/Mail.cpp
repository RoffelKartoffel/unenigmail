#include "Mail.h"

#include <QDebug>

using namespace unenigmail;

void Mail::addHeaderOptions(const QVector<QString> &pLines)
{
    for (int i = 0; i < pLines.size(); i++)
    {
        HeaderOption option;
        option += pLines.at(i);

        for (int j = i + 1; j < pLines.size(); j++)
        {
            QString const &line = pLines.at(j);
            if (line.at(0) != '\t' && line.at(0) != ' ')
            {
                break;
            }

            option += line;
            i = j;
        }

        mHead += option;
    }
}


Mail::Mail(QVector<QString> pText)
    : mValidStructure(false)
{
    bool headCompleted = false;

    QVector<QString> head;
    QVector<QString> body;
    for (QString const &line : pText)
    {
        if (line.isEmpty() && !headCompleted)
        {
            headCompleted = true;
            continue;
        }

        if (!headCompleted)
        {
            head += line;
        }
        else
        {
            body += line;
        }
    }

    if (head.isEmpty())
    {
        qWarning() << "No mail header found.";
        return;
    }
    if (!headCompleted)
    {
        qWarning() << "No mail body found.";
        return;
    }
    if (!head.at(0).startsWith(QLatin1String("From ")))
    {
        qWarning() << "Mail does not start with \"From \"";
        return;
    }

    mValidStructure = true;
    addHeaderOptions(head);
    mBody = body;
}


Mail::Mail()
    : mValidStructure(false)
{
}

bool Mail::isNull() const
{
    return !mValidStructure;
}


QVector<QString> Mail::assembleRawMail() const
{
    QVector<QString> mail;

    if (!mValidStructure)
    {
        return mail;
    }

    for (HeaderOption const &option : mHead)
    {
        for (QString const &line : option)
        {
            mail += line;
        }
    }

    mail += QString();
    mail += mBody;
    return mail;
}


QVector<HeaderOption> &Mail::getHeadRef()
{
    return mHead;
}


QVector<QString> &Mail::getBodyRef()
{
    return mBody;
}
