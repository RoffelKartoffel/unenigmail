#include "Constants.h"
#include "MailConverter.h"

#include <QCommandLineParser>
#include <QCoreApplication>
#include <QDebug>
#include <QLoggingCategory>

using namespace unenigmail;


int main(int argc, char *argv[])
{
    QCoreApplication app(argc, argv);

    QCoreApplication::setApplicationName(APP_NAME);
    QCoreApplication::setApplicationVersion(VERSION);

    QCommandLineParser parser;
    parser.setApplicationDescription(APP_DESCRIOTION);
    parser.addHelpOption();

    parser.addPositionalArgument("file", "Mbox to strip encryption from.");

    QCommandLineOption verboseOption("v", "Show verbose program output.");
    parser.addOption(verboseOption);

    parser.process(app);

    if (!parser.isSet(verboseOption))
    {
        QLoggingCategory::setFilterRules("*.debug=false\n*.warning=false");
    }

    const QStringList args = parser.positionalArguments();
    if (args.size() != 1)
    {
        parser.showHelp(1);
        // exits automatically
    }

    const QString file = args.at(0);

    qInfo() << "Processing file:" << file;
    MailCoverter::Statistic stats = MailCoverter::stripEncryptionFromFile(file);
    qInfo() << "Files processed:" << stats.mTotal << "Files decrypted:" << stats.mDecrypted
            << "Decryption failed:" << stats.mFailed;

    return 0;
}
