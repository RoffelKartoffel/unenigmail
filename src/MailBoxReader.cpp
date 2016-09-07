#include "MailBoxReader.h"

#include <QFile>
#include <QVector>

using namespace unenigmail;


MailBoxReader::MailBoxReader(const QString &pPath)
{
    QScopedPointer<QFile> file(new QFile());
    file->setFileName(pPath);
    if (!file->open(QIODevice::ReadOnly))
    {
        return;
    }

    mFile.reset(file.take());
}


bool MailBoxReader::isNull() const
{
    return mFile.isNull();
}


Mail MailBoxReader::next()
{
    QVector<QString> lines;

    if (this->isNull())
    {
        return Mail();
    }

    if (!mReadAhead.isNull())
    {
        lines += mReadAhead;
        mReadAhead = QString();
    }

    for (;;)
    {
        QString line = QString::fromLatin1(mFile->readLine());
        if (line.isNull())
        {
            mFile.reset();
            break;
        }

        if (line.endsWith('\n'))
        {
            line = line.left(line.length() - 1);
        }

        if (!lines.isEmpty() && line.startsWith(QLatin1String("From ")))
        {
            mReadAhead = line;
            break;
        }

        lines += line;
    }

    if (lines.isEmpty())
    {
        return Mail();
    }

    return Mail(lines);
}
