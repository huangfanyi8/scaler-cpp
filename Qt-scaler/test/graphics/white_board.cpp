#include "../../Qt-scaler/graphics/white_board.hpp"

#include <QApplication>

int main(int argc,char **argv)
{
    QApplication app(argc,argv);

    scaler::main_view view;
    view.show();
    return QApplication::exec();;
}