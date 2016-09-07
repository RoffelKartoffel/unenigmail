#pragma once

#include "Mail.h"

#include <QFile>
#include <QScopedPointer>
#include <QString>

namespace unenigmail
{

class MailBoxReader
{
  private:
    QScopedPointer<QFile> mFile;

    QString mReadAhead;

  public:
    MailBoxReader(QString const &pPath);

    bool isNull() const;
    Mail next();
};
}
