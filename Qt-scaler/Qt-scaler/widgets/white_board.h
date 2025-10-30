//*基于QWidgets的画图板（白板）*//

#ifndef SCALER_WIDGETS_WHITE_BOARD_H
#define SCALER_WIDGETS_WHITE_BOARD_H

#include <QRectF>
#include <QMouseEvent>
#include <QLabel>
#include <QPushButton>
#include <QPainter>
#include <deque>
#include <QColorDialog>
#include <QHBoxLayout>
#include <QVBoxLayout>

namespace scaler::qt_widgets
{
    enum drawing_shape:unsigned int
    {
        scaler_no_shape,
        scaler_rect,
        scaler_triangle,
        scaler_hexagon,
        scaler_ellipse,
        scaler_line
    };

    enum drawing_operation
    {
        scaler_rotate,
        scaler_move,
        scaler_no_operation
    };

    inline constexpr int quadrant(const QPointF&first,const QPointF&end)
    {
        const bool _x_comp = end.x()>first.x();
        const bool _y_comp = end.y()>first.y();
        if (_x_comp&&_y_comp)
            return 4;
        else if (_x_comp&&!_y_comp)
            return 1;
        else if (!_x_comp&&_y_comp)
            return 3;
        else return 2;
    }

    constexpr QRectF bounding_rect(const QPointF&first,const QPointF&end)
    {
        const auto _diff_h = end.y()-first.y();
        const auto _diff_w = end.x()-first.x();
        switch(quadrant(first,end))
        {
            case 1:
                return {first + QPointF{0, _diff_h},QPointF{_diff_w, 0} + first};
            case 4:
                return {first, end};
            case 2:
                return {end, first};
            case 3:
                return {QPointF{_diff_w, 0} + first, first + QPointF{0, _diff_h}};
        default:throw;
        }
        return {};
    }

    class drawing_widget
            :public QWidget
    {
    public:
        drawing_widget()
        {
            this->_current_shape = new _drawing_shape;
            this->_offset = new QPointF;
            this->resize(500,500);
            this->_view = new QLabel{this};
            this->_view->resize(500,400);
            this->_view->setStyleSheet(R"(QLabel{background-color:QColor(240, 240, 240);})");
            _set_ui();
        }

        ~drawing_widget()noexcept override
        {
            delete _current_shape;
            delete _offset;
        }
    protected:
        void mousePressEvent(QMouseEvent*event)override
        {
            if (event->button()==Qt::LeftButton)
            {
                const auto current = event->pos();
                //绘图模式
                if (this->_current_shape->_shape!=scaler_no_shape)
                    _current_shape->_rect.setTopLeft(current);
                //拖拽模式
                else if ((this->_current_shape->_operation!=scaler_no_operation))
                {
                    for (auto&shape:_shapes)
                        shape._operation = scaler_no_operation;
                    for (auto&shape:_shapes)
                    {
                        if (shape._contains(current))
                        {
                            shape._operation = scaler_move;
                            *this->_offset = current-shape._rect.topLeft();
                            break;
                        }
                    }
                }
            }

            QWidget::mousePressEvent(event);
        }

        void mouseMoveEvent(QMouseEvent*event)override
        {
            if (event->buttons() ==Qt::LeftButton)
            {
                //绘图模式
                if (this->_current_shape->_shape!=scaler_no_shape)
                    _current_shape->_rect.setBottomRight(event->pos());
                //拖拽模式
                else if ((this->_current_shape->_operation!=scaler_no_operation))
                {
                    for (_drawing_shape&shape:_shapes)
                    {
                        if (shape._operation == scaler_move)
                            shape._rect.moveTo(event->pos().toPointF()-*_offset);
                    }
                }
            }

            this->update(this->_view->rect());
        }

        void mouseReleaseEvent(QMouseEvent*event)override
        {

            this->_shapes.emplace_back(*this->_current_shape);
        }

        void paintEvent(QPaintEvent*event)override
        {
            QPainter painter{this};
            painter.setPen(this->_pen);
            painter.setRenderHint(QPainter::Antialiasing,true);
            for(const auto& shape:this->_shapes)
                shape._draw(painter);
            _current_shape->_draw(painter);
        }
    private:

        void _set_ui()
        {
            this->_main_layout = new QVBoxLayout{this};
            this->_button_layout = new QHBoxLayout;

            auto _add_rect_button = new QPushButton("矩形");
            auto _add_ellipse_button = new QPushButton("圆形");
            auto _add_triangle_button = new QPushButton("三角形");
            auto _add_hexagon_button = new QPushButton("六边形");
            auto _clear_button = new QPushButton("清空");
            auto _undo_button = new QPushButton{"撤销"};
            auto _redo_button = new QPushButton{"重做"};
            auto _move_button = new QPushButton{"移动"};
            auto _rotate_button = new QPushButton("旋转");
            auto _status_label = new QLabel("已添加图元");

            _status_label->setFixedHeight(50);
            _button_layout->addWidget(_add_rect_button);
            _button_layout->addWidget(_add_ellipse_button);
            _button_layout->addWidget(_add_triangle_button);
            _button_layout->addWidget(_add_hexagon_button);
            _button_layout->addWidget(_clear_button);
            _button_layout->addWidget(_undo_button);
            _button_layout->addWidget(_redo_button);
            _button_layout->addWidget(_rotate_button);
            _button_layout->addWidget(_move_button);
            _button_layout->addStretch();

            // 将按钮布局、视图和状态标签添加到主布局
            _main_layout->addLayout(_button_layout);
            _main_layout->addWidget(_view);
            _main_layout->addWidget(_status_label);
            this->setLayout(_main_layout);

            QObject::connect(_clear_button,&QPushButton::clicked,
                this->_view,[this]
                {
                    this->_shapes.clear();
                    this->_current_shape->_rect.setRect(0,0,0,0);
                    this->update(this->_view->rect());
                });

            QObject::connect(_add_rect_button,&QPushButton::clicked,
                this->_view,[this]
                {
                    this->_current_shape->_shape = scaler_rect;
                    this->_current_shape->_operation = scaler_no_operation;
                    this->_pen.setColor(Qt::black);
                    this->_pen.setStyle(Qt::DashLine);
                    this->_pen.setWidth(2);
                });

            QObject::connect(_add_hexagon_button,&QPushButton::clicked,
                this->_view,[this]
                {
                    this->_current_shape->_shape = scaler_hexagon;
                    this->_current_shape->_operation = scaler_no_operation;
                    this->_pen.setColor(Qt::black);
                    this->_pen.setStyle(Qt::DashLine);
                    this->_pen.setWidth(2);
                });

            QObject::connect(_add_triangle_button,&QPushButton::clicked,
                this->_view,[this]
                {
                    this->_current_shape->_shape = scaler_triangle;
                    this->_current_shape->_operation = scaler_no_operation;
                    this->_pen.setColor(Qt::black);
                    this->_pen.setStyle(Qt::DashLine);
                    this->_pen.setWidth(2);
                });

            QObject::connect(_add_ellipse_button,&QPushButton::clicked,
                this->_view,[this]
                {
                    this->_pen.setColor(Qt::black);
                    this->_pen.setStyle(Qt::DashLine);
                    this->_pen.setWidth(2);
                    this->_current_shape->_shape = scaler_ellipse;
                    this->_current_shape->_operation = scaler_no_operation;
                });

            QObject::connect(_rotate_button,&QPushButton::clicked,
                this->_view,[this]
                {
                    this->_current_shape->_operation = scaler_rotate;
                    this->_current_shape->_shape = scaler_no_shape;
                    this->update(this->_view->rect());
                });

            QObject::connect(_move_button,&QPushButton::clicked,
            this->_view,[this]
            {
                this->_current_shape->_operation = scaler_move;
                this->_current_shape->_shape = scaler_no_shape;
                this->update(this->_view->rect());
            });
        }

        struct _drawing_shape
        {
            _drawing_shape() = default;

            void _draw(QPainter& painter)const
            {
                const auto width = _rect.width();
                const auto height = _rect.height();
                if (_shape  == scaler_hexagon)
                {
                    const auto _half_width = width/2;
                    const auto _half_height = height/2;
                    painter.drawPolygon({_rect.topLeft()+QPointF{_half_width,0},
                            _rect.topLeft()+QPointF{width,_half_height/2},
                            _rect.topLeft()+QPointF{width,_half_height/2*3},
                            _rect.topLeft()+QPointF{_half_width,height},
                            _rect.topLeft()+QPointF{0,_half_height/2*3},
                            _rect.topLeft()+QPointF{0,_half_height/2}});
                }
                if(_shape == scaler_triangle)
                    painter.drawPolygon({_rect.topLeft()+QPointF{width/2,0},
                        _rect.topLeft()+QPointF{width,height},
                        _rect.topLeft()+QPointF{0,height}});
                else if (_shape ==scaler_rect)
                    painter.drawRect(_rect);
                else if (_shape ==scaler_ellipse)
                    painter.drawEllipse(this->_rect);
            }

            [[nodiscard]] bool _contains(const QPointF& point) const
            {
                switch (_shape)
                {
                case scaler_rect:
                    return _rect.contains(point);
                case scaler_ellipse:
                {
                    // 简化的椭圆碰撞检测
                    const QPointF center = _rect.center();
                    const auto rx = _rect.width() / 2.0;
                    const auto ry = _rect.height() / 2.0;
                    const auto dx = (point.x() - center.x()) / rx;
                    const auto dy = (point.y() - center.y()) / ry;
                    return (dx * dx + dy * dy) <= 1.0;
                }
                case scaler_line:
                {
                    const auto p1 = _rect.topLeft();
                    const auto p2 = _rect.bottomRight();
                    const auto distance = std::abs((p2.y() - p1.y()) * point.x() -
                                             (p2.x() - p1.x()) * point.y() +
                                             p2.x() * p1.y() - p2.y() * p1.x()) /
                                    std::sqrt(std::pow(p2.y() - p1.y(), 2) +
                                             std::pow(p2.x() - p1.x(), 2));
                    return distance < 5.0 &&
                           point.x() >= std::min(p1.x(), p2.x()) - 5 &&
                           point.x() <= std::max(p1.x(), p2.x()) + 5 &&
                           point.y() >= std::min(p1.y(), p2.y()) - 5 &&
                           point.y() <= std::max(p1.y(), p2.y()) + 5;
                }
                }
                return false;
            }

            QRectF _rect{0,0,0,0};///bounding_rect
            drawing_operation _operation =  scaler_no_operation;
            drawing_shape _shape = scaler_no_shape;
        };
    private:
        QPen _pen{Qt::black};
        std::deque<_drawing_shape> _shapes;//存储所有的图形信息
        std::deque<_drawing_shape> _cache_list;//缓存
        QLabel*_view;//绘图区域
        _drawing_shape *_current_shape = nullptr;//存储当前的图形或者鼠标信息
        QVBoxLayout* _main_layout{} ;
        QHBoxLayout* _button_layout{};
        QPointF *_offset;
    };
}
#endif //SCALER_WIDGETS_WHITE_BOARD_H