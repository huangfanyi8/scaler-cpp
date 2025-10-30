#include"../../Qt-scaler/widgets/white_board.h"
#include<QApplication>

int main(int argv,char*argc[])
{
    QApplication app(argv,argc);

    scaler::qt_widgets::drawing_widget   w;
    w.show();
    return QApplication::exec();
}