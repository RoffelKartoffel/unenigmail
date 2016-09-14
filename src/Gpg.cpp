#include "Gpg.h"

#include <QDebug>
#include <QProcess>
#include <QString>
#include <QStringList>
#include <QVector>

using namespace unenigmail;

Gpg::Decrypted Gpg::decrypt(QVector<QString> const &pInput)
{
    QProcess process;
    process.start(QStringLiteral("gpg"), QStringList() << QStringLiteral("--decrypt"));
    process.waitForStarted(-1);

    if (process.state() != QProcess::Running)
    {
        qWarning() << "Failed to launch GPG!";
        return Decrypted();
    }

    for (QString const &line : pInput)
    {
        process.write((line + '\n').toLatin1());
    }
    process.closeWriteChannel();

    process.waitForFinished(-1);
    if (process.exitCode() != 0)
    {
        qWarning() << "GPG terminated with exit code" << process.exitCode();
        return Decrypted();
    }

    return Decrypted(process.exitCode() == 0, process.readAllStandardOutput(), process.readAllStandardError());
}
