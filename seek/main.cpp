#include <QApplication>
#include <QLabel>
#include <QPushButton>
#include <QVBoxLayout>
#include <QPropertyAnimation>
#include <QResizeEvent>
#include<QButtonGroup>

namespace seek
{
    template<int _number>
    struct  _cycle_number
    {
        constexpr _cycle_number&operator++(int)
        {
            if (_base+1 == _number)
                return *this;
        }
        int _base;
    };

    struct _carousel_base
    {
        static constexpr int _around_switch_size = 40;//左右切换按钮大小
        static constexpr int _skip_switch_size = 40;//跳跃切换按钮大小
    };

    struct  _carousel_card
        : QLabel
    {
        explicit _carousel_card(QWidget* parent = nullptr)
            :QLabel{ parent },_animation{new QPropertyAnimation{this,"geometry",this}}
        {}

        QPixmap _pixmap;
        QPropertyAnimation* _animation;
        int _z{};

        void resizeEvent(QResizeEvent* event)override
        {
            if (!this->_pixmap.isNull())
                this->_pixmap=this->_pixmap.scaled(this->rect().width(), this->rect().height(),
                    Qt::KeepAspectRatio, Qt::SmoothTransformation);
        }
    };

    //轮播图图片切换按钮
    template<bool _around>
    struct _carousel_button
        :QWidget
    {
        using _base = _carousel_base;
        explicit _carousel_button(QWidget* parent = nullptr)
            :QWidget{parent}
        {
            this->setProperty("class",_around?"around":"normal");
            this->setFixedHeight(_around?_base::_around_switch_size:_base::_skip_switch_size);//设置切换按钮的大小
            const QString _qss = QString{R"(
                border:none;
                border-radius:%1px;)"};
        }
        static constexpr bool around = _around;//是不是左右切换按钮
    };

    struct  _slide_button_group
        : QFrame
    {
        explicit _slide_button_group(QWidget* parent = nullptr)
            :QFrame{parent}
        {
            this->setObjectName("slide_button_group");
            this->_buttons.reserve(_counts);
            this->setFixedSize((_counts+1)*_h_padding+_counts*_button_size.width(),_button_size.height()+_v_padding*2);

            for (int index=0;index<_counts;++index)
            {
                _buttons[index] = new QPushButton{this};
                _buttons[index]->setObjectName(QString("button%1").arg(index));
                _buttons[index]->setFixedSize(_button_size);
                this->_button_group->addButton(_buttons[index],index);
            }

            this->setStyleSheet(QString{
            R"(QPushButton{border:none;border-radius:%1px;background-color:rgb(172,128,58);}
                        QPushButton::hover{background-color:rgb(254,254,6);})"
            }.arg(_h_padding));

            auto*_buttons_layout = new QHBoxLayout{this};
            _buttons_layout->addStretch();
            _buttons_layout->setSpacing(_h_padding);
            for (int index=0;index<_counts;++index)
                _buttons_layout->addWidget(_buttons[index]);
            _buttons_layout->addStretch();
            _buttons_layout->setContentsMargins(_h_padding,_v_padding,_h_padding,_v_padding);
        }

        std::vector<QPushButton*>_buttons{};
        QButtonGroup* _button_group = new QButtonGroup{this};
        static constexpr auto _button_size = QSize{10,10};//按钮的大小
        static constexpr int _h_padding = 5;//按钮左右之间的外边距
        static constexpr int _v_padding = 2;//按钮与布局管理器的上下外边距
        static constexpr int _counts =5;//按钮的数量，与图片数量相关
};

    class carousel_widget
        :public  QWidget
{
public:
    explicit  carousel_widget(QWidget* parent = nullptr)
        :QWidget{parent}
    {
        this->resize(_bounding_rect_size+QSize{_around_button_size.width(),_around_button_size.height()});
        this->_cards.reserve(_pixmap_counts);
        this->_setup_ui();

        QObject::connect(this->_left_button, &QPushButton::clicked, this, &carousel_widget::_zvalue_change<false>);
        QObject::connect(this->_right_button, &QPushButton::clicked, this, &carousel_widget::_zvalue_change<true>);
        _set_button_group();
    }

private:
    void _setup_ui()
    {
        //主布局
        auto* _main_layout = new QVBoxLayout(this);
        //设置内容外接矩形
        this->_bounding_rect = new QLabel{ this };
        this->_bounding_rect->setObjectName("bounding_rect");
        this->_bounding_rect->resize(_bounding_rect_size );
        this->_bounding_rect->setStyleSheet("#bounding_rect{border-width: 1px; border-style: solid; border-color: rgb(255, 170, 0);}");
        //设置左右切换按钮
        this->_left_button = new QPushButton{ this };
        this->_right_button = new QPushButton{ this };
        _left_button->setFixedSize(_around_button_size);
        _right_button->setFixedSize(_around_button_size);
        _left_button->move(0,this->height()/2-_around_button_size.width()/2);
        _right_button->move(_bounding_rect->width(),this->height()/2-_around_button_size.width()/2);
        this->_right_button->setObjectName("right_button");
        this->_left_button->setObjectName("left_button");
        constexpr auto _radius = _around_button_size.width()/2;
        this->setStyleSheet(QString(R"(
            #left_button{border:none;border-radius:%1px;background-color:rgba(0,0,0,50);}
            #right_button{border:none;border-radius:%1px;background-color:rgba(0,0,0,50);}
            #left_button:hover{border:none;border-radius:%1px;background-color:rgba(255,0,0,255);}
            #right_button:hover{border:none;border-radius:%1px;background-color:rgba(255,0,0,255);})").arg(_radius));

        this->_button_group = new _slide_button_group{ this->_bounding_rect  };

        _main_layout->setContentsMargins(_around_button_size.width()/2, _around_button_size.width()/2,
            _around_button_size.width()/2, _around_button_size.width()/2);

        _main_layout->addWidget(_bounding_rect);

         this->_create_all_geometry();
        const QString _res_prefix = "D:/c++/Qt/seek/%1.jpg";

        for (int index = 0; index < _pixmap_counts; ++index)
        {
            this->_cards[index] = new _carousel_card{this};
            this->_cards[index]->_z = index;
            this->_cards[index]->setPixmap(_res_prefix.arg(index));
            this->_cards[index]->setParent(this->_bounding_rect);
            this->_cards[index]->setGeometry((_geometry_list)[this->_cards[index]->_z]);
            this->_cards[index]->setAlignment(Qt::AlignCenter);
        }

        for (int index = 0; index <_pixmap_counts; ++index)
        {
            if (this->_cards[index]->_z == 1)
                this->_cards[index]->raise();
        }

        this->_button_group->move(_bounding_rect->width()/2-this->_button_group->width()/2,_bounding_rect->height()/4*3);
    }

    void _set_button_group()
    {
        QObject::connect(this->_button_group->_button_group,
            &QButtonGroup::idClicked,this,
            [this](const int _id)
            {
                this->_clicked_button_index = _id;
                for (int index=0;index<_pixmap_counts;++index)
                    if (index==_id)
                        this->_button_group->_button_group->buttons()[_id]->setStyleSheet("background-color:rgb(254,254,6);");
                    else
                        this->_button_group->_button_group->buttons()[index]->setStyleSheet("background-color:rgb(172,128,58)");
                if (this->_current_button_index<this->_clicked_button_index)
                    _zvalue_change<false>();
                else if (this->_current_button_index>this->_clicked_button_index)
                    _zvalue_change<true>();
                this->_current_button_index = _clicked_button_index;
            });
    }

        //按下右边按钮，所有的卡片向右移动一个单位，当前按钮变色，下一个按钮变色
    template<bool _right>
    void _setup_around_button()
    {
        auto _z_change = [this]<bool _v>(const int _step)
        {
            if constexpr(_v)
            {
                for (int index = 0; index <_pixmap_counts; ++index)
                    this->_cards[index]->_z = (this->_cards[index]->_z + _step) %_pixmap_counts;
            }
            else
            {
                for (int index = 0; index < _pixmap_counts; ++index)
                    this->_cards[index]->_z = this->_cards[index]->_z == 0
                    ? _pixmap_counts - _step
                    : this->_cards[index]->_z -_step;
            }
        };
        //按下右边按钮
        if constexpr(_right)
        {
            _z_change<_right>(1);
        }
        else
        {

        }
    }

    template<bool _forward>
    void _zvalue_change() const
    {
        if  constexpr (_forward)
            for (int index = 0; index <_pixmap_counts; ++index)
                this->_cards[index]->_z = (this->_cards[index]->_z + 1) %_pixmap_counts;
        else
            for (int index = 0; index < _pixmap_counts; ++index)
                this->_cards[index]->_z = this->_cards[index]->_z == 0 ? _pixmap_counts - 1 : this->_cards[index]->_z - 1;

        for (int index = 0; index < _pixmap_counts; ++index)
        {
            this->_cards[index]->_animation->setStartValue(_cards[index]->geometry());
            this->_cards[index]->_animation->setEndValue(_geometry_list[this->_cards[index]->_z]);
        }

        for (int index = 0; index < _pixmap_counts; ++index)
            this->_cards[index]->_animation->start();

        for (int index = 0; index < _pixmap_counts; ++index)
            if (this->_cards[index]->_z == 1)
                this->_cards[index]->raise();
    }

        //设置geometry
    void _create_all_geometry()
    {
        const auto _middle_size = this->_bounding_rect->size() / 2;
        const auto _around_size = _middle_size / 2;
        const auto _middle_pos = QPoint{ this->_bounding_rect->width() / 4, this->_bounding_rect->height() / 4 };
        const auto _left_pos = _middle_pos - QPoint(_middle_size.width() / 4, -_middle_size.height() / 4);
        const auto _right_pos = _left_pos + QPoint(_middle_size.width(), 0);
        this->_geometry_list.emplace_back(_left_pos,_around_size);
        this->_geometry_list.emplace_back(_middle_pos,_middle_size);
        this->_geometry_list.emplace_back(_right_pos,_around_size);
        for (int index = 0;index<_pixmap_counts-_cards_counts;++index)
            this->_geometry_list.push_back(_zero_geometry);
    }

private:
    QLabel* _bounding_rect{};//内容(轮播图)外接矩形
    QPushButton* _left_button{};//图片切换按钮
    QPushButton* _right_button{};//图片切换按钮
    _slide_button_group*_button_group = nullptr;
    int _current_button_index = 0 ;//当前高亮按钮的下标
    int _clicked_button_index;//鼠标点击按钮的下标
    std::vector<_carousel_card*> _cards;
    std::vector<QRect> _geometry_list;

    static constexpr auto _bounding_rect_size = QSize{ 600,400 };//外接矩形的大小
    static constexpr auto _around_button_size = QSize{ 40,40 };//左右切换按钮的大小
    static constexpr auto _zero_geometry = QRect{};
    static constexpr int _cards_counts = 3;
    static constexpr int _pixmap_counts = 5;
};
}

#include <QApplication>
#include <QMainWindow>
#include <QGraphicsView>
#include <QGraphicsScene>
#include <QGraphicsPixmapItem>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QLabel>
#include <QPropertyAnimation>
#include <QParallelAnimationGroup>
#include <QGraphicsDropShadowEffect>

// 自定义图形项，继承自QObject和QGraphicsPixmapItem
class AnimatedPixmapItem : public QObject, public QGraphicsPixmapItem
{
    Q_OBJECT
    Q_PROPERTY(QPointF pos READ pos WRITE setPos)
    Q_PROPERTY(qreal scale READ scale WRITE setScale)
    Q_PROPERTY(qreal opacity READ opacity WRITE setOpacity)

public:
    AnimatedPixmapItem(const QPixmap &pixmap, QGraphicsItem *parent = nullptr)
        : QGraphicsPixmapItem(pixmap, parent)
    {
    }
};

class CardCarousel : public QGraphicsView
{
    Q_OBJECT

public:
    CardCarousel(QWidget *parent = nullptr)
        : QGraphicsView(parent), currentIndex(0)
    {
        // 设置场景
        scene = new QGraphicsScene(this);
        setScene(scene);
        setRenderHint(QPainter::Antialiasing);
        setFrameStyle(QFrame::NoFrame);
        setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

        // 创建卡牌
        createCards();

        // 初始布局卡牌
        layoutCards();
    }

    void createCards()
    {
        // 创建5张示例卡牌
        for (int i = 0; i < 5; ++i) {
            // 创建卡牌图像
            QPixmap pixmap(200, 300);
            QColor color = QColor::fromHsv(i * 72, 255, 200);
            pixmap.fill(color);

            // 在卡牌上添加编号和装饰
            QPainter painter(&pixmap);
            painter.setPen(Qt::white);
            painter.setFont(QFont("Arial", 24, QFont::Bold));
            painter.drawText(pixmap.rect(), Qt::AlignCenter, QString::number(i + 1));

            // 添加边框
            painter.setPen(QPen(Qt::black, 2));
            painter.drawRect(pixmap.rect().adjusted(1, 1, -1, -1));

            AnimatedPixmapItem *card = new AnimatedPixmapItem(pixmap);

            // 添加阴影效果
            QGraphicsDropShadowEffect *shadow = new QGraphicsDropShadowEffect;
            shadow->setBlurRadius(15);
            shadow->setOffset(5, 5);
            shadow->setColor(QColor(0, 0, 0, 160));
            card->setGraphicsEffect(shadow);

            // 设置卡牌中心点
            card->setTransformOriginPoint(card->boundingRect().center());

            cards.append(card);
            scene->addItem(card);
        }
    }

    void layoutCards()
    {
        const int cardSpacing = 50; // 卡牌之间的间距
        const int totalCards = cards.size();

        // 计算卡牌位置
        for (int i = 0; i < totalCards; ++i) {
            AnimatedPixmapItem *card = cards[i];

            // 计算卡牌在轮播中的位置
            int relativePos = (i - currentIndex + totalCards) % totalCards;

            // 设置卡牌位置、缩放和透明度
            double x = 0;
            double scale = 1.0;
            double opacity = 1.0;
            int zValue = 0;

            // 根据相对位置确定卡牌状态
            if (relativePos == 0) {
                // 当前卡牌 - 居中
                x = 0;
                scale = 1.0;
                opacity = 1.0;
                zValue = 100;
            } else if (relativePos == 1) {
                // 右侧第一张卡牌
                x = 200 + cardSpacing;
                scale = 0.85;
                opacity = 0.8;
                zValue = 90;
            } else if (relativePos == 2) {
                // 右侧第二张卡牌
                x = 400 + cardSpacing * 2;
                scale = 0.7;
                opacity = 0.6;
                zValue = 80;
            } else if (relativePos == totalCards - 1) {
                // 左侧第一张卡牌
                x = -200 - cardSpacing;
                scale = 0.85;
                opacity = 0.8;
                zValue = 90;
            } else if (relativePos == totalCards - 2) {
                // 左侧第二张卡牌
                x = -400 - cardSpacing * 2;
                scale = 0.7;
                opacity = 0.6;
                zValue = 80;
            } else {
                // 其他卡牌（隐藏）
                x = (relativePos > totalCards / 2) ? -600 : 600;
                scale = 0.5;
                opacity = 0.3;
                zValue = 70;
            }

            // 设置卡牌属性
            card->setPos(x, 0);
            card->setScale(scale);
            card->setOpacity(opacity);
            card->setZValue(zValue);
        }
    }

public slots:
    void rotateClockwise()
    {
        currentIndex = (currentIndex + 1) % cards.size();
        animateRotation();
    }

    void rotateCounterClockwise()
    {
        currentIndex = (currentIndex - 1 + cards.size()) % cards.size();
        animateRotation();
    }

private:
    void animateRotation()
    {
        const int cardSpacing = 50;
        const int totalCards = cards.size();

        // 创建动画组
        QParallelAnimationGroup *animationGroup = new QParallelAnimationGroup(this);

        // 为每个卡牌创建动画
        for (int i = 0; i < totalCards; ++i) {
            AnimatedPixmapItem *card = cards[i];
            int relativePos = (i - currentIndex + totalCards) % totalCards;

            // 计算目标位置、缩放和透明度
            double targetX = 0;
            double targetScale = 1.0;
            double targetOpacity = 1.0;
            int targetZValue = 0;

            // 根据相对位置确定目标状态
            if (relativePos == 0) {
                // 当前卡牌 - 居中
                targetX = 0;
                targetScale = 1.0;
                targetOpacity = 1.0;
                targetZValue = 100;
            } else if (relativePos == 1) {
                // 右侧第一张卡牌
                targetX = 200 + cardSpacing;
                targetScale = 0.85;
                targetOpacity = 0.8;
                targetZValue = 90;
            } else if (relativePos == 2) {
                // 右侧第二张卡牌
                targetX = 400 + cardSpacing * 2;
                targetScale = 0.7;
                targetOpacity = 0.6;
                targetZValue = 80;
            } else if (relativePos == totalCards - 1) {
                // 左侧第一张卡牌
                targetX = -200 - cardSpacing;
                targetScale = 0.85;
                targetOpacity = 0.8;
                targetZValue = 90;
            } else if (relativePos == totalCards - 2) {
                // 左侧第二张卡牌
                targetX = -400 - cardSpacing * 2;
                targetScale = 0.7;
                targetOpacity = 0.6;
                targetZValue = 80;
            } else {
                // 其他卡牌（隐藏）
                targetX = (relativePos > totalCards / 2) ? -600 : 600;
                targetScale = 0.5;
                targetOpacity = 0.3;
                targetZValue = 70;
            }

            // 位置动画
            QPropertyAnimation *posAnimation = new QPropertyAnimation(card, "pos");
            posAnimation->setDuration(400);
            posAnimation->setStartValue(card->pos());
            posAnimation->setEndValue(QPointF(targetX, 0));
            posAnimation->setEasingCurve(QEasingCurve::OutCubic);
            animationGroup->addAnimation(posAnimation);

            // 缩放动画
            QPropertyAnimation *scaleAnimation = new QPropertyAnimation(card, "scale");
            scaleAnimation->setDuration(400);
            scaleAnimation->setStartValue(card->scale());
            scaleAnimation->setEndValue(targetScale);
            scaleAnimation->setEasingCurve(QEasingCurve::OutCubic);
            animationGroup->addAnimation(scaleAnimation);

            // 透明度动画
            QPropertyAnimation *opacityAnimation = new QPropertyAnimation(card, "opacity");
            opacityAnimation->setDuration(400);
            opacityAnimation->setStartValue(card->opacity());
            opacityAnimation->setEndValue(targetOpacity);
            opacityAnimation->setEasingCurve(QEasingCurve::OutCubic);
            animationGroup->addAnimation(opacityAnimation);

            // 更新Z值
            card->setZValue(targetZValue);
        }

        // 启动动画
        animationGroup->start(QAbstractAnimation::DeleteWhenStopped);
    }

private:
    QGraphicsScene *scene;
    QList<AnimatedPixmapItem*> cards;
    int currentIndex;
};

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr) : QMainWindow(parent)
    {
        // 创建中央部件
        QWidget *centralWidget = new QWidget(this);
        setCentralWidget(centralWidget);

        // 创建主布局
        QVBoxLayout *mainLayout = new QVBoxLayout(centralWidget);
        mainLayout->setContentsMargins(20, 20, 20, 20);

        // 创建标题
        QLabel *titleLabel = new QLabel("Card Stack Carousel", this);
        titleLabel->setAlignment(Qt::AlignCenter);
        titleLabel->setStyleSheet("font-size: 24px; font-weight: bold; margin: 10px; color: #333;");

        // 创建轮播图
        CardCarousel *carousel = new CardCarousel(this);
        carousel->setFixedSize(900, 400);
        carousel->setStyleSheet("background: #f5f5f5; border-radius: 10px; border: 1px solid #ddd;");

        // 创建控制按钮
        QHBoxLayout *buttonLayout = new QHBoxLayout();
        QPushButton *leftButton = new QPushButton("? Counter Clockwise", this);
        QPushButton *rightButton = new QPushButton("Clockwise ?", this);

        // 设置按钮样式
        QString buttonStyle =
            "QPushButton {"
            "   font-size: 16px;"
            "   padding: 10px 20px;"
            "   background: #4CAF50;"
            "   color: white;"
            "   border: none;"
            "   border-radius: 5px;"
            "   min-width: 180px;"
            "}"
            "QPushButton:hover {"
            "   background: #45a049;"
            "}"
            "QPushButton:pressed {"
            "   background: #3d8b40;"
            "}";

        leftButton->setStyleSheet(buttonStyle);
        rightButton->setStyleSheet(buttonStyle);

        buttonLayout->addWidget(leftButton);
        buttonLayout->addStretch();
        buttonLayout->addWidget(rightButton);

        // 添加说明文本
        QLabel *infoLabel = new QLabel("Click buttons to rotate the card stack clockwise or counter-clockwise", this);
        infoLabel->setAlignment(Qt::AlignCenter);
        infoLabel->setStyleSheet("color: #666; margin: 10px;");

        // 添加到主布局
        mainLayout->addWidget(titleLabel);
        mainLayout->addSpacing(20);
        mainLayout->addWidget(carousel, 1, Qt::AlignCenter);
        mainLayout->addSpacing(10);
        mainLayout->addWidget(infoLabel);
        mainLayout->addSpacing(10);
        mainLayout->addLayout(buttonLayout);

        // 连接信号槽
        connect(leftButton, &QPushButton::clicked, carousel, &CardCarousel::rotateCounterClockwise);
        connect(rightButton, &QPushButton::clicked, carousel, &CardCarousel::rotateClockwise);

        // 设置窗口属性
        setWindowTitle("Qt6 Card Stack Carousel");
        setMinimumSize(1000, 650);

        // 居中显示
        centerWindow();
    }

private:
    void centerWindow()
    {
        QRect screenGeometry = QApplication::primaryScreen()->availableGeometry();
        int x = (screenGeometry.width() - width()) / 2;
        int y = (screenGeometry.height() - height()) / 2;
        move(x, y);
    }
};

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    // 设置应用程序信息
    app.setApplicationName("Card Carousel");
    app.setApplicationVersion("1.0");

    MainWindow window;
    window.show();

    return app.exec();
}

#include "main.moc"
