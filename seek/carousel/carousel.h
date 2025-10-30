#ifndef SEEK_CAROUSEL_H
#define SEEK_CAROUSEL_H

#include <qboxlayout.h>
#include <QGraphicsPixmapItem>
#include <QLabel>


#include <QPropertyAnimation>
#include<QPushButton>

namespace seek
{
    template<class>struct _carousel_card;

    template<>
    struct _carousel_card<QGraphicsPixmapItem>
        :QGraphicsPixmapItem
    {
        explicit _carousel_card(int _z,QGraphicsItem *parent = nullptr)
            :QGraphicsPixmapItem{parent}
        {
            this->setZValue(_z);
        }

        QPropertyAnimation*_animation;
    };

    template<>
    struct  _carousel_card<QLabel>
        : QLabel
    {
        explicit _carousel_card(QWidget* parent = nullptr)
            :QLabel{ parent },_animation(new QPropertyAnimation{this,"geometry",this})
        {}

        QPixmap _pixmap;
        QPropertyAnimation*_animation;
        int _zvalue{};
    protected:
        void resizeEvent(QResizeEvent* event)override
        {
            if (!this->_pixmap.isNull())
                this->_pixmap=this->_pixmap.scaled(this->rect().width(), this->rect().height(),
                    Qt::KeepAspectRatio, Qt::SmoothTransformation);
        }
    };

    template<int _counts>
    class _slide_button_group
        :public QLabel
    {
    public:
        explicit _slide_button_group(QWidget* parent = nullptr)
            :QLabel{parent}
        {
            this->setObjectName("slide_button_group");
            this->_buttons.reserve(_counts);
            this->setFixedSize((_counts+1)*_h_padding+_counts*_button_size.width(),_button_size.height()+_v_padding*2);

            for (int index=0;index<_counts;++index)
            {
                _buttons[index] = new QPushButton{this};
                _buttons[index]->setObjectName(QString("button%1").arg(index));
                _buttons[index]->setFixedSize(_button_size);
            }
            this->setStyleSheet(QString{
            R"(QPushButton{border:none;border-radius:%1px;background-color:rgb(172,128,58);}
                        QPushButton::hover{background-color:rgb(254,254,6);})"
            }.arg(_h_padding));
            QHBoxLayout*_buttons_layout = new QHBoxLayout{this};
            _buttons_layout->setContentsMargins(_h_padding,_v_padding,_h_padding,_v_padding);
            _buttons_layout->setSpacing(_h_padding);
            _buttons_layout->addStretch();
            for (int index=0;index<_counts;++index)
                _buttons_layout->addWidget(_buttons[index]);

            _buttons_layout->addStretch();

            for (int index = 0;index<_counts;++index)
                QObject::connect(_buttons[index], &QPushButton::clicked, this, [&]{this->_value = index;});
        }
    private:
        std::vector<QPushButton*>_buttons;
        int _value;
        static constexpr auto _button_size = QSize{10,10};
        static constexpr int _h_padding = 5;
        static constexpr int _v_padding = 2;
    };

    template<class>class carousel_widget;

    template<>
    class carousel_widget<QLabel>
        :public QWidget
    {
    public:
        explicit  carousel_widget(QWidget* parent = nullptr)
            :QWidget(parent)
        {
            this->resize(_content_size+QSize(_around_button_width,_around_button_width));

        }

        ~carousel_widget()override
        {
            delete _cards;
        }
    private:
        void _setup_ui()
        {
            QVBoxLayout* _main_layout = new QVBoxLayout(this);

            this->_show_label = new QLabel{ this };
            this->_show_label->setObjectName("show_label");
            this->_show_label->resize(_content_size );
            this->_show_label->setStyleSheet("#show_label{border-width: 1px; border-style: solid; border-color: rgb(255, 170, 0);}");

            this->_left_button = new QPushButton{ this };
            this->_right_button = new QPushButton{ this };
            this->_left_button->setObjectName("left_button");
            this->_right_button->setObjectName("right_button");
            constexpr auto _radius = _around_button_width/2;
            this->setStyleSheet(QString(R"(#left_button{border:none;border-radius:%1px;background-color:rgba(0,0,0,50);}
                                                                        #right_button{border:none;border-radius:%1px;background-color:rgba(0,0,0,50);}
                                                                        #left_button:hover{border:none;border-radius:%1px;background-color:rgba(255,0,0,255);}
                                                                        #right_button:hover{border:none;border-radius:%1px;background-color:rgba(255,0,0,255);})").arg(_radius));
            _left_button->setFixedSize(_around_button_width,_around_button_width);
            _right_button->setFixedSize(_around_button_width,_around_button_width);

            auto _buttons = new _slide_button_group<_pixmap_counts>{ this };

            _main_layout->setContentsMargins(_around_button_width/2, _around_button_width/2,
                _around_button_width/2, _around_button_width/2);

            _main_layout->addWidget(_show_label);
            _main_layout->addWidget(_buttons);

            this->_geometry_list = new std::vector{ this->_get_all_geometry() };
            const QString _res_prefix = "D:/c++/Qt/seek/%1.jpg";

            for (int index = 0; index < _pixmap_counts; ++index)
            {
                _get_cards()[index]->_zvalue = index;
                _get_cards()[index]->setPixmap(_res_prefix.arg(index));
                _get_cards()[index]->setParent(this->_show_label);
                _get_cards()[index]->setGeometry((*_geometry_list)[_get_cards()[index]->_zvalue]);
                _get_cards()[index]->setAlignment(Qt::AlignCenter);
            }

            for (int index = 0; index < _pixmap_counts; ++index)
            {
                if (_get_cards()[index]->_zvalue == 1)
                    _get_cards()[index]->raise();
            }
        }

        ///获取所有的geometry
        std::vector<QRect> _get_all_geometry()
        {
            const auto _middle_size = this->_show_label->size() / 2;
            const auto _around_size = _middle_size / 2;
            const auto _middle_pos = QPoint{ this->_show_label->width() / 4, this->_show_label->height() / 4 };
            const auto _left_pos = _middle_pos - QPoint(_middle_size.width() / 4, -_middle_size.height() / 4);
            const auto _right_pos = _left_pos + QPoint(_middle_size.width(), 0);
            return { {_left_pos,_around_size},{_middle_pos,_middle_size},{_right_pos,_around_size} };
        }

        auto _get_cards() const {return *this->_cards;}
    protected:
        void resizeEvent(QResizeEvent* event) override
        {
            this->_left_button->move(_around_button_width/2,this->rect().height()/2-_around_button_width/2);
            this->_right_button->move(_around_button_width/2,this->rect().height()/2-_around_button_width/2);
        }
    private:
        std::vector<_carousel_card<QLabel>*>*_cards;
        std::vector<QRect>*_geometry_list;
        QPushButton* _left_button{};
        QPushButton* _right_button{};
        QLabel* _show_label{};
        static constexpr int _pixmap_counts = 3;//一共有多少张图片
        static constexpr auto _around_button_width = 60;
        static constexpr auto _content_size = QSize{ 600,400 };

    };
}
#endif //SEEK_CAROUSEL_H
