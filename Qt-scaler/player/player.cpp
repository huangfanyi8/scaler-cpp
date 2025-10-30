#include"player.hpp"
#include<QHBoxLayout>
#include<QPainter>

namespace dong_dong_player
{
    //解决折叠表达式分配内存失效的情况
    template<class  Parent,class...Widgets>
    constexpr bool _allocate_widgets(Parent*&parent,Widgets*&...widgets)
    {
        size_t  _size = 0;
        ((widgets = new Widgets{parent},++_size),...);
        return _size== sizeof...(Widgets);
    }

    title_bar::title_bar(QWidget *parent)
            :QWidget{parent}
    {
        this->setObjectName("title_bar");
        this->_set_ui();
    }

    void title_bar::_set_ui()
    {
       // auto _ret = _allocate_widgets(this,this->_min_button,this->_max_button,this->_close_button);
        this->_min_button = new QPushButton{this};
        this->_close_button = new QPushButton{this};
        this->_max_button = new QPushButton{this};
        this->_setting_button = new QPushButton{this};
        this->_min_button->setObjectName("min_button");
        this->_max_button->setObjectName("max_button");
        this->_close_button->setObjectName("close_button");
        this->_setting_button->setObjectName("setting_button");
        this->_min_button->setFixedSize(50,30);
        this->_close_button->setFixedSize(50,30);
        this->_max_button->setFixedSize(50,30);
        this->_setting_button->setFixedSize(50,30);

        this->_set_qss();
        QObject::connect(this->_min_button,&QPushButton::clicked,this->parentWidget(),[this]{
            this->parentWidget()->showMinimized();
       });

        QObject::connect(this->_max_button,&QPushButton::clicked,this->parentWidget(),[this]{
            if(!this->parentWidget()->isMaximized())
                this->parentWidget()->showMaximized();
            else if(this->parentWidget()->isMaximized())
                this->parentWidget()->showNormal();
            this->update();
        });

        QObject::connect(this->_close_button,&QPushButton::clicked,this->parentWidget(),[this]{
            this->parentWidget()->close();
        });

        auto _title_bar_layout = new QHBoxLayout{this};
        _title_bar_layout->addWidget(this->_setting_button);;
        _title_bar_layout->addStretch();
        _title_bar_layout->setSpacing(5);
        _title_bar_layout->addWidget(this->_min_button);
        _title_bar_layout->addWidget(this->_max_button);
        _title_bar_layout->addWidget(this->_close_button);
        _title_bar_layout->setContentsMargins(2,2,2,2);
    }

    void  title_bar::_set_qss()
    {
        this->setStyleSheet(R"(
        QPushButton::hover
        {
                background-color:rgb(56,58,61);
        }
        #min_button{
    border:none;
    image: url(D:/c++/Qt/demo/res/min.svg);
    min-width: 30px;
    max-width: 30px;
    background-repeat:no-repeat;
    background-position:bottom;
    padding:5px;
}

        #max_button{
    border:none;
    image: url(D:/c++/Qt/demo/res/max.svg);
    min-width: 30px;
    max-width: 30px;
    background-repeat:no-repeat;
    background-position:bottom;
    padding:5px;
}

        #close_button{
    border:none;
    image: url(D:/c++/Qt/demo/res/close.svg);
    min-width: 30px;
    max-width: 30px;
    background-repeat:no-repeat;
    background-position:bottom;
    border-top-right-radius:15px;
    padding:5px;
}

        #setting_button{
    border:none;
    image: url(D:/c++/Qt/demo/res/setting.svg);
    background-repeat:no-repeat;
    background-position:bottom;
    border-top-left-radius:15px;
    padding:7px;
}
        #close_button::hover
        {
                background-color:rgb(232,17,35);
        }

    )");;
    }

    player::player(QWidget*parent)
        :QWidget{parent}
    {
        //设置无边框
        this->setWindowFlags(Qt::FramelessWindowHint);
        this->setAttribute(Qt::WA_TranslucentBackground);

        this->setMouseTracking(true);
        this->resize(600,400);
        QVBoxLayout*_main_layout =  new QVBoxLayout{this};
        _main_layout->addWidget(this->_bar);
        _main_layout->addStretch();
        _main_layout->setContentsMargins(0,0,0,0);

    }

    void  player::paintEvent(QPaintEvent*)
    {
        QPainter painter{this};
        painter.setRenderHint(QPainter::Antialiasing,true);
        painter.setBrush(QColor{40,42,54});
        painter.setPen(Qt::NoPen);
        painter.drawRoundedRect(this->rect(),20,20);
        this->setStyleSheet(R"(#title_bar{background-color:rgb(43,45,38);})");
    }

    void player::mouseMoveEvent(QMouseEvent *event)
    {
        if(event->buttons() ==  Qt::LeftButton)
        {
            auto _diff = event->globalPosition()-this->_global_pos;
            this->move(this->_current_pos+_diff.toPoint());
        }
        else if(event->button() == Qt::LeftButton)
        {
            const auto _r = this->_get_region(event->position());
            qDebug()<<static_cast<int>(_r);
            switch(_r)
            {
                case region::left:
                case region::right:
                    this->setCursor(Qt::SizeHorCursor);
                    break;
                case region::top:
                case region::bottom:
                    this->setCursor(Qt::SizeVerCursor);
                    break;
                case region::top_left:
                case region::bottom_right:
                    this->setCursor(Qt::SizeFDiagCursor);
                    break;
                case region::top_right:
                case region::bottom_left:
                    this->setCursor(Qt::SizeBDiagCursor);
                    break;
                default:
                    this->setCursor(Qt::ArrowCursor);
                    break;
            }
        }
    }

    void player::mouseReleaseEvent(QMouseEvent *)
    {
    }

    void player::mousePressEvent(QMouseEvent *event)
    {
        if(event->button() == Qt::LeftButton)
        {
            this->_global_pos = event->globalPosition();
            this->_current_pos = this->pos();
            this->setCursor(Qt::PointingHandCursor);
        }

    }
    rotate_button::rotate_button(QWidget*parent)
        :QPushButton{parent}
    {
        this->setFixedSize(40,30);
        this->setCheckable(true);
        this->setChecked(true);//初始值是未按下
        this->_button_animation = new QPropertyAnimation{this,"_angle"};

        QObject::connect(this,&QPushButton::toggled,this,[this](bool _value){
            this->_button_animation->setStartValue(this->_angle);
            this->_button_animation->setEndValue(_value?0:180);
            this->_button_animation->setDuration(1000);
            this->_button_animation->setEasingCurve(QEasingCurve::InOutQuint);
            this->_button_animation->start();
        });

    }

    void rotate_button::_set_angle(double angle)
    {
        this->_angle = angle;
        this->update();
    }

    void rotate_button::paintEvent(QPaintEvent*paint)
    {
        QPainter painter{this};
        //画一个六边形
        painter.setPen(Qt::NoPen);
        painter.setBrush(Qt::darkCyan);
        const auto h = static_cast<qreal>(this->height());
        const auto w = static_cast<qreal>(this->width());
        painter.translate(w/2,h/2);
        painter.rotate(this->_angle);
        const QPolygonF  hexagon = {QPointF{-w/4,h/2},
                              QPointF{w/4,h/2},
                                QPointF{w/2,0},
                                QPointF{w/4,-h/2},
                                QPointF{-w/4,-h/2},
                                QPointF{-w/2,0}};
        painter.drawPolygon(hexagon);
    }


    void rotate_button::_draw()
    {
        QPolygonF  polygon;
        polygon.resize(100);

    };
}


