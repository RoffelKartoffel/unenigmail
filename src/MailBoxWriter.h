#pragma once

#include "Mail.h"

#include <QFile>
#include <QScopedPointer>
#include <QString>

namespace unenigmail
{

class MailBoxWriter
{
  private:
    QScopedPointer<QFile> mFile;

  public:
    MailBoxWriter(QString const &pPath);
    bool isNull() const;

    void write(Mail const &pMail);
};
}
