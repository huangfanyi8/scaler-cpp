
#ifndef SCALER_GRAPHICS_VIEW_HPP
#define SCALER_GRAPHICS_VIEW_HPP

#include <QGraphicsView>
#include <QGraphicsScene>
#include <QGraphicsPolygonItem>
#include <QHBoxLayout>
#include <QPushButton>
#include <QLabel>
#include <QGraphicsPixmapItem>
#include <QGraphicsSceneMouseEvent>
#include <QColorDialog>
#include <QPen>
#include <QBrush>
#include<QMouseEvent>
#include<deque>

namespace scaler
{
    class main_scene
        :public QGraphicsScene
    {
        main_scene()
        {

        }

        void mouseMoveEvent(QGraphicsSceneMouseEvent* event) override;
        void mousePressEvent(QGraphicsSceneMouseEvent* event) override;
        void mouseReleaseEvent(QGraphicsSceneMouseEvent* event) override;
    private:
        QGraphicsView * _view=new QGraphicsView(this);
    };
    class scaler_graphics_item
        :public QGraphicsItem
    {
    public:
        explicit scaler_graphics_item()
            :QGraphicsItem{}
        {
            this->setFlags(ItemIsMovable|ItemIsSelectable|ItemSendsGeometryChanges);
        }

    protected:
        void mousePressEvent(QGraphicsSceneMouseEvent* event) override
        {
            if (event->button() == Qt::LeftButton)
                this->_begin = event->pos();
            else
                    QGraphicsItem::mousePressEvent(event);
        }

        void mouseMoveEvent(QGraphicsSceneMouseEvent* event) override
        {
            if (event->buttons() & Qt::LeftButton)
            {
                this->_end = event->pos();
                this->_rect.setBottomRight(_end);
                this->prepareGeometryChange();
                qDebug()<<_rect;
                this->update();
            }
            else
                QGraphicsItem::mouseMoveEvent(event);
        }

        void mouseReleaseEvent(QGraphicsSceneMouseEvent* event) override
        {
            this->_rect.setRect(0,0,0,0);
            QGraphicsItem::mouseReleaseEvent(event);
        }

        void paint(QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget) override
        {
            painter->setPen(Qt::black);
            painter->setBrush(QColor{0,0,255,50});
            painter->drawRect(this->_rect);
        }
    protected:
        QRectF boundingRect()const override
        {return this->_rect;}
    private:
        QRectF _rect{0,0,0,0};
        QPointF _begin;
        QPointF _end;
        QRectF _big_rect = {0,0,400,400};
    };

    template<class GraphicsItem>
    class interactive_item
        :public GraphicsItem
    {
        void ok()
        {

        }
    };

    template<>
    class interactive_item<QGraphicsPolygonItem>
        :public QGraphicsPolygonItem
    {
    public:
        template<class PolygonF,
            class = std::enable_if_t<std::is_constructible_v<PolygonF,QPolygonF>>>
        explicit interactive_item(PolygonF&&initialist,QGraphicsItem*parent =nullptr)
            :QGraphicsPolygonItem{static_cast<PolygonF&&>(initialist),parent}
        {
            QPolygonF polygon;
            polygon << QPointF(0, -50) << QPointF(-30, 30) << QPointF(30, 30);
            setPolygon(polygon);
            setBrush(Qt::yellow);
            setFlag(QGraphicsItem::ItemIsSelectable, true);

            this->setTransformOriginPoint(this->boundingRect().center());
        }

    protected:

        void mousePressEvent(QGraphicsSceneMouseEvent *event)override
        {
            if (event->button() == Qt::LeftButton)
            {
                this->_center= this->sceneBoundingRect().center();
                this->_last = this->scenePos();
                event->accept();
            }
            else
                QGraphicsPolygonItem::mousePressEvent(event);;
        }

        void mouseMoveEvent(QGraphicsSceneMouseEvent *event)override
        {
            if (event->buttons()==Qt::LeftButton)
            {
                QPointF center = this->mapToScene(this->boundingRect().center());
                QPointF current_pos = event->scenePos();

                QPointF v1 = _last - center;
                QPointF v2 = current_pos - center;

                qreal angle1 = std::atan2(v1.y(), v1.x());
                qreal angle2 = std::atan2 (v2.y(), v2.x());
                qreal angle_delta = (angle2 - angle1) * 180.0 / M_PI;

                setRotation(_rotation + angle_delta);
                _last = current_pos;
                _rotation = rotation();
                event->accept();
            }
            else
                QGraphicsPolygonItem::mouseMoveEvent(event);
        }
    private:
        QPolygonF _polygon_list;
        QPointF _center;
        QPointF _last;//
        qreal _rotation;//鼠标控制旋转的角度
    };

    template<>
    class interactive_item<QGraphicsRectItem>
        :public QGraphicsRectItem
    {
    public:
        explicit interactive_item(QGraphicsRectItem *parent = nullptr)
        {this->setFlags(ItemIsMovable|ItemIsSelectable|ItemIsSelectable|ItemSendsGeometryChanges);}

        explicit interactive_item(const QRectF&rect,QGraphicsRectItem *parent = nullptr)
            :_rect{rect},QGraphicsRectItem(parent),_dotted_rect(rect.adjusted(-_padding,-_padding,_padding,_padding))
        {
            this->setFlags(ItemIsMovable|ItemIsSelectable|ItemIsSelectable);
            auto bounding_rect = (rect);
            /*
            *dx1: 左边界的调整量（正数向右移动，负数向左移动）
            dy1: 上边界的调整量（正数向下移动，负数向上移动）
            dx2: 右边界的调整量（正数向右移动，负数向左移动）
            dy2: 下边界的调整量（正数向下移动，负数向上移动）
             *
             */
            bounding_rect.adjust(-(_padding+_handle_radius),
                -(_padding+_handle_radius),
                _padding+_handle_radius,
                _padding+_handle_radius);
            this->setRect(bounding_rect);
        }
    protected:
        void mousePressEvent(QGraphicsSceneMouseEvent* event) override
        {
            if (event->button() == Qt::LeftButton)
            {
                this->update();
            }
            QGraphicsItem::mousePressEvent(event);;
        }

        void mouseMoveEvent(QGraphicsSceneMouseEvent *event) override
        {
            if (event->buttons() == Qt::LeftButton)
            {
                this->_rect.setTopLeft(this->rect().topLeft()+QPointF{_handle_radius+_padding,_padding+_handle_radius});
                this->_dotted_rect.setTopLeft(this->rect().topLeft()+QPointF{_handle_radius,_handle_radius});
                this->update();
                QGraphicsRectItem::mouseMoveEvent(event);
            }
        }

        void mouseReleaseEvent(QGraphicsSceneMouseEvent *event) override
        {
                QGraphicsRectItem::mouseReleaseEvent(event);
        }

        void mouseDoubleClickEvent(QGraphicsSceneMouseEvent* event) override
        {
            if (event->button() == Qt::LeftButton)
            {
                QColor color = QColorDialog::getColor(Qt::white, nullptr, "选择填充颜色");
                if(color.isValid())
                {

                }
            }
        }
        // 重写绘制，增加控制点
        void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) override
        {
            //初始化画笔
            painter->setRenderHint(QPainter::Antialiasing);
            QPen pen;
            //绘制中间内容
            painter->setBrush(this->_brush);
            painter->setPen(Qt::NoPen);
            painter->drawRect(this->rect().adjusted((_padding+_handle_radius),
            (_padding+_handle_radius),
            -(_padding+_handle_radius),
            -(_padding+_handle_radius)));
            if (this->isSelected())
            {
                //绘制虚线方框
                pen.setStyle(Qt::DotLine);
                pen.setColor(_handle_color);
                painter->setBrush(Qt::NoBrush);
                painter->setPen(pen);
                painter->drawRect(this->rect().adjusted(_handle_radius,_handle_radius,-_handle_radius,-_handle_radius));

                //绘制手柄
                painter->setBrush(_handle_color);
                painter->setPen(Qt::NoPen);
                painter->drawEllipse(this->_dotted_rect.topLeft(),_handle_radius,_handle_radius);
                painter->drawEllipse(this->_dotted_rect.topRight(),_handle_radius,_handle_radius);
                painter->drawEllipse(this->_dotted_rect.bottomLeft(),_handle_radius,_handle_radius);
                painter->drawEllipse(this->_dotted_rect.bottomRight(),_handle_radius,_handle_radius);
            }
        }

        static constexpr  qreal _handle_radius = 10;
        static constexpr QColor _handle_color = QColor{200,100,100};
        static constexpr qreal  _padding = 20;
        QBrush _brush = QColor{0,0,255,50};
        QPen _pen = _handle_color;
        QRectF _rect;//保存矩形的大小
        QRectF _dotted_rect;
    };

}

namespace scaler
{
    class main_view
            :public QWidget
    {
    public:
        explicit main_view(QWidget*parent = nullptr)noexcept
            :QWidget{parent}
        {
            this->move(100,100);
            this->_scene = new QGraphicsScene{this};
            this->_view  = new QGraphicsView{this->_scene};
            this->resize(1000,600);
            //设置在中心
            this->_scene->setSceneRect(100,100,800,500);
            this->_scene->setBackgroundBrush(QBrush(QColor(240, 240, 240)));
            this->_view->setRenderHints(QPainter::Antialiasing|QPainter::TextAntialiasing|QPainter::SmoothPixmapTransform|QPainter::LosslessImageRendering);
            _view->setSceneRect(_scene->sceneRect());
            _view->setDragMode(QGraphicsView::RubberBandDrag);
            this->_set_ui();
        }
    private:
        template<size_t _num,class ... String>
        void _set_button(std::index_sequence<_num>,String...string)
        {
            this->_button_list = new QPushButton[_num];
            size_t _index=0;
            (_button_list[_index++].setText(std::forward<String>(string)),...);
        }

        void _set_ui()
        {
            auto _main_layout = new QVBoxLayout{this};
            auto _button_layout = new QHBoxLayout;

            auto _add_rect_button = new QPushButton("添加矩形");
            auto _add_circle_button = new QPushButton("添加圆形");
            auto _add_triangle_button = new QPushButton("添加三角形");
            auto _add_extra_button = new QPushButton("添加测试项");
            auto _clear_button = new QPushButton("清空场景");
            auto _grid_button = new QPushButton("隐藏网格");
            auto _fill_button = new QPushButton{"填充"};
            auto _status_label = new QLabel("使用鼠标选择和移动图元，双击更改颜色");

            QObject::connect(_add_rect_button, &QPushButton::clicked, this, [=, this]{
                auto *item = new   interactive_item<QGraphicsRectItem>(QRectF{100,100,200,100});
                this->_scene->addItem(item);
            });

            QObject::connect(_add_extra_button, &QPushButton::clicked, this, [=, this]{
                auto *item = new  scaler_graphics_item;
                this->_scene->addItem(item);
            });
            QObject::connect(_add_triangle_button,
                &QPushButton::clicked,
                this,
                [=, this]{
                auto *item = new  interactive_item<QGraphicsPolygonItem>{QPolygonF{QPointF{400,400},QPointF{450,500},QPointF{250,500}}};
                                item->setPos(400,400);
                this->_scene->addItem(item);
            });

            QObject::connect(_fill_button,&QPushButton::clicked,this,[this]{
                QColor color = QColorDialog::getColor(Qt::white, nullptr, "选择填充颜色");
                if(color.isValid())
                {

                }
            });
            _button_layout->addWidget(_add_rect_button);
            _button_layout->addWidget(_add_circle_button);
            _button_layout->addWidget(_add_triangle_button);
            _button_layout->addWidget(_add_extra_button);
            _button_layout->addWidget(_clear_button);
            _button_layout->addWidget(_grid_button);
            _button_layout->addWidget(_fill_button);
            _button_layout->addStretch();

            // 将按钮布局、视图和状态标签添加到主布局
            _main_layout->addLayout(_button_layout);
            _main_layout->addWidget(_view);
            _main_layout->addWidget(_status_label);
            this->setLayout(_main_layout);
        }
    private:
        QGraphicsView *_view = nullptr;
        QGraphicsScene *_scene = nullptr;
        QPushButton*_button_list = nullptr;
        template<size_t _num>
        static constexpr auto _index_Sequence = std::index_sequence<_num>{};
    };


}



#endif //
