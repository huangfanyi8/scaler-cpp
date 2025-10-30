#include<QPushButton>
#include<QPainter>
#include<QPaintEvent>
#include<tuple>
#include<QPropertyAnimation>
#include<QEnterEvent>
#include <QObject>
#include <QApplication>
#include <QPushButton>
#include <QPoint>
#include <QTimer>
#include <QPropertyAnimation>
#include <QMouseEvent>
#include <QPainter>
#include <QDebug>
#include <QDateTime>
#include <QList>
#include <QBitmap>
#include <QtMath>


namespace  CustomWidgets
{
    class switch_button
            :public QPushButton
    {
        Q_OBJECT
        Q_PROPERTY(qreal _center_pos WRITE _set_center_pos  READ  _get_center_pos)
    public:
        explicit switch_button(QWidget*parent = nullptr)
            :QPushButton{parent},_animation{new QPropertyAnimation{this,"_center_pos"}}
        {
              this->setFixedSize(_button_width,_button_height);
              this->setCheckable(true);

              QObject::connect(this,&QPushButton::toggled,this,[this](bool _value)
              {
                  this->_animation->setDuration(100);
                  this->_animation->setStartValue(this->_center_pos);
                  this->_animation->setEndValue(_value?_button_width-_button_height/2:_button_height/2);
                  this->_animation->start();
              });
        }

    private:
        static constexpr int _button_width  = 120;
        static constexpr int _button_height = 50;
        static constexpr int _handle_radius  = 20;//这是手柄半径
        static constexpr int _radius = 25;//这是光圈的半径
        bool _enter{false};
        qreal _center_pos{25};
        QPropertyAnimation*_animation;

    protected:
        [[nodiscard]]qreal _get_center_pos()const
        {return this->_center_pos;}

        void _set_center_pos(qreal pos)
        {
            this->_center_pos = pos;
            this->update();
        }

        void paintEvent(QPaintEvent*e)override
        {
            QPushButton::paintEvent(e);
            QPainter painter{this};
            painter.setRenderHint(QPainter::Antialiasing, true);
            painter.setPen(Qt::NoPen);
            constexpr auto _draw_rect = QRect{_radius-_handle_radius/2,_radius-_handle_radius/2,
                                              _button_width-2*(_radius-_handle_radius/2),_handle_radius};
            painter.setBrush(Qt::blue);
            painter.drawRoundedRect(_draw_rect,static_cast<double>(_handle_radius)/2,static_cast<double>(_handle_radius)/2);
           if(this->_enter)
           {
               painter.setBrush(Qt::white);
               painter.drawEllipse(QPointF{this->_center_pos,_radius},_radius,_radius);
           }

            painter.setBrush(Qt::blue);
            painter.drawEllipse(QPointF{this->_center_pos,_radius},_handle_radius,_handle_radius);
        }

    protected:
        void enterEvent(QEnterEvent*)override
        {
            this->_enter = true;
            this->update();
        }

        void leaveEvent(QEvent*)override
        {
            this->_enter = false;
            this->update();
        }
    };
}