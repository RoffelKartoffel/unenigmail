#pragma once

#include "Gpg.h"
#include "Mail.h"

namespace unenigmail
{

class MailConverter
{
  private:
    static QString fillLine(QString const &pInput, QChar const &pFiller, const int pTargetLength);
    static QVector<QString> generateGpgInfoFooter(const Gpg::Decrypted &pDecrypted);
    static bool processDecryptedBody(const Gpg::Decrypted &pDecrypted, QVector<QString> &pInnerHead,
                                     QVector<QString> &pInnerBody);

  public:
    enum ReturnCode
    {
        ERROR,
        NOT_ENCRYPTED,
        DECRYPTED,
    };

    struct Statistic
    {
        const int mTotal;
        const int mDecrypted;
        const int mFailed;

        Statistic()
            : mTotal(0)
            , mDecrypted(0)
            , mFailed(0)
        {
        }

        Statistic(int pTotal, int pDecrypted, int pFailed)
            : mTotal(pTotal)
            , mDecrypted(pDecrypted)
            , mFailed(pFailed)
        {
        }
    };

    static ReturnCode stripEncryptionFromMail(const Mail &pMailInput, Mail &pMailOutput);
    static Statistic stripEncryptionFromFile(QString const &pPath);
};
}
