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
        static constexpr int _around_switch_size = 40;//�����л���ť��С
        static constexpr int _skip_switch_size = 40;//��Ծ�л���ť��С
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

    //�ֲ�ͼͼƬ�л���ť
    template<bool _around>
    struct _carousel_button
        :QWidget
    {
        using _base = _carousel_base;
        explicit _carousel_button(QWidget* parent = nullptr)
            :QWidget{parent}
        {
            this->setProperty("class",_around?"around":"normal");
            this->setFixedHeight(_around?_base::_around_switch_size:_base::_skip_switch_size);//�����л���ť�Ĵ�С
            const QString _qss = QString{R"(
                border:none;
                border-radius:%1px;)"};
        }
        static constexpr bool around = _around;//�ǲ��������л���ť
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
        static constexpr auto _button_size = QSize{10,10};//��ť�Ĵ�С
        static constexpr int _h_padding = 5;//��ť����֮�����߾�
        static constexpr int _v_padding = 2;//��ť�벼�ֹ�������������߾�
        static constexpr int _counts =5;//��ť����������ͼƬ�������
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
        //������
        auto* _main_layout = new QVBoxLayout(this);
        //����������Ӿ���
        this->_bounding_rect = new QLabel{ this };
        this->_bounding_rect->setObjectName("bounding_rect");
        this->_bounding_rect->resize(_bounding_rect_size );
        this->_bounding_rect->setStyleSheet("#bounding_rect{border-width: 1px; border-style: solid; border-color: rgb(255, 170, 0);}");
        //���������л���ť
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

        //�����ұ߰�ť�����еĿ�Ƭ�����ƶ�һ����λ����ǰ��ť��ɫ����һ����ť��ɫ
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
        //�����ұ߰�ť
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

        //����geometry
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
    QLabel* _bounding_rect{};//����(�ֲ�ͼ)��Ӿ���
    QPushButton* _left_button{};//ͼƬ�л���ť
    QPushButton* _right_button{};//ͼƬ�л���ť
    _slide_button_group*_button_group = nullptr;
    int _current_button_index = 0 ;//��ǰ������ť���±�
    int _clicked_button_index;//�������ť���±�
    std::vector<_carousel_card*> _cards;
    std::vector<QRect> _geometry_list;

    static constexpr auto _bounding_rect_size = QSize{ 600,400 };//��Ӿ��εĴ�С
    static constexpr auto _around_button_size = QSize{ 40,40 };//�����л���ť�Ĵ�С
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

// �Զ���ͼ����̳���QObject��QGraphicsPixmapItem
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
        // ���ó���
        scene = new QGraphicsScene(this);
        setScene(scene);
        setRenderHint(QPainter::Antialiasing);
        setFrameStyle(QFrame::NoFrame);
        setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
        setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

        // ��������
        createCards();

        // ��ʼ���ֿ���
        layoutCards();
    }

    void createCards()
    {
        // ����5��ʾ������
        for (int i = 0; i < 5; ++i) {
            // ��������ͼ��
            QPixmap pixmap(200, 300);
            QColor color = QColor::fromHsv(i * 72, 255, 200);
            pixmap.fill(color);

            // �ڿ�������ӱ�ź�װ��
            QPainter painter(&pixmap);
            painter.setPen(Qt::white);
            painter.setFont(QFont("Arial", 24, QFont::Bold));
            painter.drawText(pixmap.rect(), Qt::AlignCenter, QString::number(i + 1));

            // ��ӱ߿�
            painter.setPen(QPen(Qt::black, 2));
            painter.drawRect(pixmap.rect().adjusted(1, 1, -1, -1));

            AnimatedPixmapItem *card = new AnimatedPixmapItem(pixmap);

            // �����ӰЧ��
            QGraphicsDropShadowEffect *shadow = new QGraphicsDropShadowEffect;
            shadow->setBlurRadius(15);
            shadow->setOffset(5, 5);
            shadow->setColor(QColor(0, 0, 0, 160));
            card->setGraphicsEffect(shadow);

            // ���ÿ������ĵ�
            card->setTransformOriginPoint(card->boundingRect().center());

            cards.append(card);
            scene->addItem(card);
        }
    }

    void layoutCards()
    {
        const int cardSpacing = 50; // ����֮��ļ��
        const int totalCards = cards.size();

        // ���㿨��λ��
        for (int i = 0; i < totalCards; ++i) {
            AnimatedPixmapItem *card = cards[i];

            // ���㿨�����ֲ��е�λ��
            int relativePos = (i - currentIndex + totalCards) % totalCards;

            // ���ÿ���λ�á����ź�͸����
            double x = 0;
            double scale = 1.0;
            double opacity = 1.0;
            int zValue = 0;

            // �������λ��ȷ������״̬
            if (relativePos == 0) {
                // ��ǰ���� - ����
                x = 0;
                scale = 1.0;
                opacity = 1.0;
                zValue = 100;
            } else if (relativePos == 1) {
                // �Ҳ��һ�ſ���
                x = 200 + cardSpacing;
                scale = 0.85;
                opacity = 0.8;
                zValue = 90;
            } else if (relativePos == 2) {
                // �Ҳ�ڶ��ſ���
                x = 400 + cardSpacing * 2;
                scale = 0.7;
                opacity = 0.6;
                zValue = 80;
            } else if (relativePos == totalCards - 1) {
                // ����һ�ſ���
                x = -200 - cardSpacing;
                scale = 0.85;
                opacity = 0.8;
                zValue = 90;
            } else if (relativePos == totalCards - 2) {
                // ���ڶ��ſ���
                x = -400 - cardSpacing * 2;
                scale = 0.7;
                opacity = 0.6;
                zValue = 80;
            } else {
                // �������ƣ����أ�
                x = (relativePos > totalCards / 2) ? -600 : 600;
                scale = 0.5;
                opacity = 0.3;
                zValue = 70;
            }

            // ���ÿ�������
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

        // ����������
        QParallelAnimationGroup *animationGroup = new QParallelAnimationGroup(this);

        // Ϊÿ�����ƴ�������
        for (int i = 0; i < totalCards; ++i) {
            AnimatedPixmapItem *card = cards[i];
            int relativePos = (i - currentIndex + totalCards) % totalCards;

            // ����Ŀ��λ�á����ź�͸����
            double targetX = 0;
            double targetScale = 1.0;
            double targetOpacity = 1.0;
            int targetZValue = 0;

            // �������λ��ȷ��Ŀ��״̬
            if (relativePos == 0) {
                // ��ǰ���� - ����
                targetX = 0;
                targetScale = 1.0;
                targetOpacity = 1.0;
                targetZValue = 100;
            } else if (relativePos == 1) {
                // �Ҳ��һ�ſ���
                targetX = 200 + cardSpacing;
                targetScale = 0.85;
                targetOpacity = 0.8;
                targetZValue = 90;
            } else if (relativePos == 2) {
                // �Ҳ�ڶ��ſ���
                targetX = 400 + cardSpacing * 2;
                targetScale = 0.7;
                targetOpacity = 0.6;
                targetZValue = 80;
            } else if (relativePos == totalCards - 1) {
                // ����һ�ſ���
                targetX = -200 - cardSpacing;
                targetScale = 0.85;
                targetOpacity = 0.8;
                targetZValue = 90;
            } else if (relativePos == totalCards - 2) {
                // ���ڶ��ſ���
                targetX = -400 - cardSpacing * 2;
                targetScale = 0.7;
                targetOpacity = 0.6;
                targetZValue = 80;
            } else {
                // �������ƣ����أ�
                targetX = (relativePos > totalCards / 2) ? -600 : 600;
                targetScale = 0.5;
                targetOpacity = 0.3;
                targetZValue = 70;
            }

            // λ�ö���
            QPropertyAnimation *posAnimation = new QPropertyAnimation(card, "pos");
            posAnimation->setDuration(400);
            posAnimation->setStartValue(card->pos());
            posAnimation->setEndValue(QPointF(targetX, 0));
            posAnimation->setEasingCurve(QEasingCurve::OutCubic);
            animationGroup->addAnimation(posAnimation);

            // ���Ŷ���
            QPropertyAnimation *scaleAnimation = new QPropertyAnimation(card, "scale");
            scaleAnimation->setDuration(400);
            scaleAnimation->setStartValue(card->scale());
            scaleAnimation->setEndValue(targetScale);
            scaleAnimation->setEasingCurve(QEasingCurve::OutCubic);
            animationGroup->addAnimation(scaleAnimation);

            // ͸���ȶ���
            QPropertyAnimation *opacityAnimation = new QPropertyAnimation(card, "opacity");
            opacityAnimation->setDuration(400);
            opacityAnimation->setStartValue(card->opacity());
            opacityAnimation->setEndValue(targetOpacity);
            opacityAnimation->setEasingCurve(QEasingCurve::OutCubic);
            animationGroup->addAnimation(opacityAnimation);

            // ����Zֵ
            card->setZValue(targetZValue);
        }

        // ��������
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
        // �������벿��
        QWidget *centralWidget = new QWidget(this);
        setCentralWidget(centralWidget);

        // ����������
        QVBoxLayout *mainLayout = new QVBoxLayout(centralWidget);
        mainLayout->setContentsMargins(20, 20, 20, 20);

        // ��������
        QLabel *titleLabel = new QLabel("Card Stack Carousel", this);
        titleLabel->setAlignment(Qt::AlignCenter);
        titleLabel->setStyleSheet("font-size: 24px; font-weight: bold; margin: 10px; color: #333;");

        // �����ֲ�ͼ
        CardCarousel *carousel = new CardCarousel(this);
        carousel->setFixedSize(900, 400);
        carousel->setStyleSheet("background: #f5f5f5; border-radius: 10px; border: 1px solid #ddd;");

        // �������ư�ť
        QHBoxLayout *buttonLayout = new QHBoxLayout();
        QPushButton *leftButton = new QPushButton("? Counter Clockwise", this);
        QPushButton *rightButton = new QPushButton("Clockwise ?", this);

        // ���ð�ť��ʽ
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

        // ���˵���ı�
        QLabel *infoLabel = new QLabel("Click buttons to rotate the card stack clockwise or counter-clockwise", this);
        infoLabel->setAlignment(Qt::AlignCenter);
        infoLabel->setStyleSheet("color: #666; margin: 10px;");

        // ��ӵ�������
        mainLayout->addWidget(titleLabel);
        mainLayout->addSpacing(20);
        mainLayout->addWidget(carousel, 1, Qt::AlignCenter);
        mainLayout->addSpacing(10);
        mainLayout->addWidget(infoLabel);
        mainLayout->addSpacing(10);
        mainLayout->addLayout(buttonLayout);

        // �����źŲ�
        connect(leftButton, &QPushButton::clicked, carousel, &CardCarousel::rotateCounterClockwise);
        connect(rightButton, &QPushButton::clicked, carousel, &CardCarousel::rotateClockwise);

        // ���ô�������
        setWindowTitle("Qt6 Card Stack Carousel");
        setMinimumSize(1000, 650);

        // ������ʾ
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

    // ����Ӧ�ó�����Ϣ
    app.setApplicationName("Card Carousel");
    app.setApplicationVersion("1.0");

    MainWindow window;
    window.show();

    return app.exec();
}

#include "main.moc"
