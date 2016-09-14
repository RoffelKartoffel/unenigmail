#pragma once

#include <QByteArray>

namespace unenigmail
{

class Gpg
{
  public:
    struct Decrypted
    {
        const int mSuccessFull;
        const QByteArray mStdOut;
        const QByteArray mStdErr;

        Decrypted(int pSuccessFull, QByteArray const &pStdOut, QByteArray const &pStdErr)
            : mSuccessFull(pSuccessFull)
            , mStdOut(pStdOut)
            , mStdErr(pStdErr)
        {
        }

        Decrypted()
            : mSuccessFull(false)
            , mStdOut(QByteArray())
            , mStdErr(QByteArray())
        {
        }

        bool isSuccessFull()
        {
            return mSuccessFull;
        }
    };

    static Decrypted decrypt(const QVector<QString> &pInput);
};
}
