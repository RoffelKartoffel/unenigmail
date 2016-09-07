#include "MailBoxWriter.h"


using namespace unenigmail;


MailBoxWriter::MailBoxWriter(const QString &pPath)
{
    QScopedPointer<QFile> file(new QFile(pPath));
    if (!file->open(QIODevice::ReadWrite))
    {
        return;
    }

    mFile.reset(file.take());
}


bool MailBoxWriter::isNull() const
{
    return mFile.isNull();
}


void MailBoxWriter::write(const Mail &pMail)
{
    if (this->isNull())
    {
        return;
    }

    for (QString const &line : pMail.assembleRawMail())
    {
        mFile->write(line.toLatin1());
        mFile->write("\n");
    }
}
