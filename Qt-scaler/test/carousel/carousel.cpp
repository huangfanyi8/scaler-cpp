#include <QApplication>
#include <QLabel>
#include <QPushButton>
#include <QPainter>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include<QPropertyAnimation>
#include<QParallelAnimationGroup>
#include <QResizeEvent>
#include <filesystem>

//QPoint QSize QPointF QSizeF互相转换
struct qt_pair
{
    double first;
    double second;
};

template<class Q>
constexpr auto _convert(const Q&obj)
{
    using T = std::decay_t<Q>;
    if constexpr(std::is_same_v<T,QSize>)
    { }
}
//subcontoller 用于显示轮播图的图片
struct  _carousel_label
    :public QLabel
{
    explicit _carousel_label(QWidget*parent = nullptr)
        :QLabel{parent}
    { }

    QPixmap _pixmap;
    int _zvalue ;//堆叠顺序,2是在顶层，1是在左边，3是在右边
protected:
    void resizeEvent(QResizeEvent*event)override
    {
        if(!this->_pixmap.isNull())
            this->_pixmap.scaled(this->rect().width(),this->rect().height());
        QLabel::resizeEvent(event);
    }
};

class Widget
    :public  QWidget
{
public:
    explicit  Widget(QWidget* parent = nullptr)
    {
        this->resize(_init_size);//初始化控件大小
        this->_setup_ui();

        QObject::connect(this->_left_button, &QPushButton::clicked, this, &Widget::_zvalue_change<false>);
        QObject::connect(this->_right_button, &QPushButton::clicked, this, &Widget::_zvalue_change<true>);
    }

    ~Widget()override
    {
        delete[] _pixmap_list;
        delete _geometry_list;
    }
private:
    void _setup_ui()
    {
        //创建属性动画
        this->_label1_animation = new QPropertyAnimation{ &_pixmap_list[0],"geometry",this };
        this->_label2_animation = new QPropertyAnimation{ &_pixmap_list[1],"geometry" ,this };
        this->_label3_animation = new QPropertyAnimation{ &_pixmap_list[2],"geometry" ,this };
        //布局管理器
        QHBoxLayout* _button_layout = new QHBoxLayout;
        QVBoxLayout* _main_layout = new QVBoxLayout(this);
        //设置显示图片的区域
        this->_show_label = new QLabel{ this };
        this->_show_label->setObjectName("show_label");
        this->_show_label->resize(_init_size - QSize{ 100,100 });
        this->_show_label->setStyleSheet("#show_label{border-width: 1px; border-style: solid; border-color: rgb(255, 170, 0);}");

        //设置轮播按钮
        this->_left_button = new QPushButton{ this };
        this->_right_button = new QPushButton{ this };
        _left_button->setFixedSize(_button_size);
        _right_button->setFixedSize(_button_size);

        //开始布局
        _button_layout->addWidget(_left_button);
        _button_layout->addStretch();
        _button_layout->addWidget(_right_button);
        _main_layout->setContentsMargins(50,50,50,50);
        _main_layout->addWidget(_show_label);
        _main_layout->addLayout(_button_layout);

        //设置所有轮播图片的位置与大小
        this->_geometry_list = new std::vector<QRect>{ this->_get_pixmaps() };
        const QString _res_prefix = "D:/c++/vs/QtWidgetsApplication1/res/%1.webp";
        //初始化所有的轮播图片
        for (int index = 0; index < _counts; ++index)
        {
            this->_pixmap_list[index]._zvalue = index;
            this->_pixmap_list[index].setPixmap(_res_prefix.arg(index));
            this->_pixmap_list[index].setParent(this->_show_label);
            this->_pixmap_list[index].setGeometry((*_geometry_list)[this->_pixmap_list[index]._zvalue]);
            this->_pixmap_list[index].setAlignment(Qt::AlignCenter);
        }

        for (int index = 0; index < _counts; ++index)
        {
            if (this->_pixmap_list[index]._zvalue == 1)
                this->_pixmap_list[index].raise();
        }
    }

    template<bool _forward>
    void _zvalue_change( )
    {
        if  constexpr (_forward)
        {
            //std::rotate(this->_geometry_list->rbegin(), this->_geometry_list->rbegin() + 1, this->_geometry_list->rend());
            for (int index = 0; index < _counts; ++index)
                this->_pixmap_list[index]._zvalue = (this->_pixmap_list[index]._zvalue + 1) % _counts;
        }

        else
        {
            //::rotate(this->_geometry_list->begin(), this->_geometry_list->begin() + 1, this->_geometry_list->end());
            for (int index = 0; index < _counts; ++index)
                this->_pixmap_list[index]._zvalue = this->_pixmap_list[index]._zvalue == 0 ? _counts - 1 : this->_pixmap_list[index]._zvalue - 1;
        }
        qDebug() << "curent geometry :";
        for (int index = 0;index < _counts; ++index)
        {
            qDebug() << _geometry_list[index];
        }
        this->_label1_animation->setStartValue(_pixmap_list[0].geometry());
        this->_label1_animation->setEndValue((*_geometry_list)[this->_pixmap_list[0]._zvalue]);
        this->_label2_animation->setStartValue(_pixmap_list[1].geometry());
        this->_label2_animation->setEndValue((*_geometry_list)[this->_pixmap_list[1]._zvalue]);
        this->_label3_animation->setStartValue(_pixmap_list[2].geometry());
        this->_label3_animation->setEndValue((*_geometry_list)[this->_pixmap_list[2]._zvalue]);


        this->_label1_animation->start();
        this->_label2_animation->start();
        this->_label3_animation->start();

        for (int index = 0; index < _counts; ++index)
        {
            if (this->_pixmap_list[index]._zvalue == 1)
                this->_pixmap_list[index].raise();
        }
    }

   std::vector<QRect> _get_pixmaps()
   {
       const auto _middle_size = this->_show_label->size() / 2;
       const auto _around_size = _middle_size / 2;
       const auto _middle_pos = QPoint{ this->_show_label->width() / 4, this->_show_label->height() / 4 };
       const auto _left_pos = _middle_pos - QPoint(_middle_size.width() / 4, -_middle_size.height() / 4);
       const auto _right_pos = _left_pos + QPoint( _middle_size.width(),0 );
        return { {_left_pos,_around_size},{_middle_pos,_middle_size},{_right_pos,_around_size} };
   }
private:
    QParallelAnimationGroup* _group_animation = new QParallelAnimationGroup{this};
    QLabel* _show_label{};//显示图片的区域
    _carousel_label* _pixmap_list = new _carousel_label[_counts];
    QPushButton* _left_button{};
    QPushButton* _right_button{};
    QPropertyAnimation* _label1_animation{};
    QPropertyAnimation* _label2_animation{};
    QPropertyAnimation* _label3_animation{};

    static constexpr auto _init_size = QSize{ 800,600 };//初始化轮播图控件大小
    static constexpr auto _button_size = QSize{80,40};//控制按钮的大小，为固定大小
    static constexpr int _counts = 3;
    std::vector<QRect>* _geometry_list;
};


int main(int argc, char* argv[])
{
    QApplication app(argc, argv);

    Widget window;
    window.show();

    return app.exec();
}