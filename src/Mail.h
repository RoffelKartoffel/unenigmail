#pragma once

#include <QString>
#include <QVector>

namespace unenigmail
{

typedef QVector<QString> HeaderOption;

class Mail
{
  private:
    bool mValidStructure;
    QVector<HeaderOption> mHead;
    QVector<QString> mBody;

  public:
    Mail(QVector<QString> pText);
    Mail();

    bool isNull() const;
    QVector<QString> assembleRawMail() const;

    void addHeaderOptions(const QVector<QString> &pLines);
    QVector<HeaderOption> &getHeadRef();
    QVector<HeaderOption> getHead() const;
    QVector<QString> &getBodyRef();
};
}
