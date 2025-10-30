#ifndef CAROUSEL_H
#define CAROUSEL_H

#include <QLabel>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QButtonGroup>
#include <QMouseEvent>
#include <QPainter>
#include <QRect>

namespace scaler
{
  enum scaler_operator_t:unsigned int
  {
      scaler_rectangle =0,
      scaler_ellipse =1,
      scaler_line =2,
      scaler_hexagon =3,
      scaler_triangle =4,
      scaler_rotate =5,
      scaler_move =6,
      scaler_select =7,
      scaler_null = 8
  };

    struct _scaler_shape
    {
      _scaler_shape()noexcept = default;

      template<class ... Rects,class = std::enable_if_t<std::is_constructible_v<QRectF,Rects...>>>
      explicit constexpr _scaler_shape(Rects&&...rects)
        : _bounding_rect{static_cast<Rects&&>(rects)...}
      {}

      static void _draw(const scaler_operator_t shape,QPainter &painter,const QPointF&begin,const QPointF&end)
      {
        if (shape <= scaler_triangle)
        {
          const auto width = end.x()-begin.x();
          const auto height = end.y()- begin.y();
          if (shape  ==scaler_hexagon)
          {
            const auto _half_width = width/2;
            const auto _half_height = height/2;
            painter.drawPolygon({begin+QPointF{_half_width,0},
                    begin+QPointF{width,_half_height/2},
                    begin+QPointF{width,_half_height/2*3},
                    begin+QPointF{_half_width,height},
                    begin+QPointF{0,_half_height/2*3},
                    begin+QPointF{0,_half_height/2}});
          }
          if(shape == scaler_triangle)
            painter.drawPolygon({begin+QPointF{width/2,0},begin+QPointF{width,height},begin+QPointF{0,height}});
          else if (shape ==scaler_rectangle)
            painter.drawRect(QRectF{begin,end});
          else if (shape ==scaler_ellipse)
            painter.drawEllipse(QRectF{begin,end});
        }
        else if (shape< scaler_null)
        {
          painter.setPen(Qt::DotLine);
          painter.drawRect({begin,end});
        }
      }

      void _draw(QPainter& painter)const
      {_draw(this->_mode,painter,_bounding_rect.topLeft(),_bounding_rect.bottomRight());}

      QRectF _bounding_rect{0,0,0,0};
      QColor _color{Qt::white};
      scaler_operator_t  _mode = scaler_null;
    };


  class main_window;

  class scaler_drawing_widget
    : public QWidget
{
  Q_OBJECT
public:

  explicit scaler_drawing_widget(QWidget* parent = nullptr)
    : QWidget(parent)
  {
    this->setMinimumSize(800, 600);
  }

  void clear()
  {
    _shapes.clear();
    update();
  }

protected:
  void paintEvent(QPaintEvent* event) override
  {
    Q_UNUSED(event);

    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);

    // 绘制以前所有图形
    for (const auto & shape : _shapes)
      shape._draw(painter);

    // 绘制当前正在绘制的图形
      painter.setPen(QPen(Qt::black, 2, Qt::DashLine));

    _scaler_shape::_draw(this->_mode,painter,_begin,_current);
  }

  void mousePressEvent(QMouseEvent* event) override
  {
    if (event->button() == Qt::LeftButton)
    {
      if (this->_mode == scaler_null)
        event->ignore();
      else if (this->_mode>=scaler_rotate)
      {
        // 拖拽模式
        this->_begin = event->pos();
        for (auto&shape : _shapes)
        {
          if (shape._bounding_rect.contains(event->pos()))
          {
            this->_drag_shapes = &shape;
            shape._mode = this->_mode;
            this->_offset = event->pos()-_drag_shapes->_bounding_rect.topLeft();
            break;
          }
        }
      }
      else
      {
        // 绘图模式：开始绘制新图形
        _begin = _current = event->pos();
        this->_shapes.emplace_back(this->_begin,this->_current);
      }
    }
    this->update();
  }

  void mouseMoveEvent(QMouseEvent* event) override
  {
    _current = event->pos();

    if (event->buttons() & Qt::LeftButton)
    {
      // mode>=scaler_rotate,开启拖拽选择旋转模式
      if (this->_mode>=scaler_rotate)
      {
        const auto  new_pos = event->pos() - this->_offset;
        switch (_mode)
        {
        case scaler_select:
        {

        }
        case scaler_move:
        {
            _drag_shapes->_bounding_rect.moveTo(new_pos);
            break;
        }

        }//switch

        this->update();
      }
    }
    //mode< scaler_rotate,直接开启绘图
    else
    {
      this->_current =  event->pos();
      this->update();
    }
  }

  void mouseReleaseEvent(QMouseEvent* event) override
  {

  }

private:

    friend class main_window;
    //当前所处的模式，由主界面的操作按钮控制传入
  scaler_operator_t _mode = scaler_null;
  _scaler_shape*_drag_shapes = nullptr;
  QPointF _begin;//鼠标按下的坐标起始坐标
  QPointF _current;//鼠标移动时候的坐标
  QPointF _offset;//鼠标当前按下的位置与当前外接矩形的距离
  std::vector<_scaler_shape> _shapes;//存储所有的形状
};

  class main_window
    :public QWidget
  {
  public:
    explicit main_window(QWidget*parent = nullptr)
      :QWidget(parent)
    {
      this->resize(1000,700);
      this->setMinimumSize(800,600);
      this->_drawing_widget = new scaler_drawing_widget(this);
      this->_drawing_widget->resize(800,600);
      this->_main_layout = new QVBoxLayout(this);
      this->_button_layout = new QHBoxLayout;
      this->_button_list = new QPushButton[_button_count];
      this->_message_status = new QLabel{this};
      this->_message_status->resize(800,50);
      this->_message_status->setText("message");
      this->_set_button("rectangle","ellipse","line","hexagon","triangle","rotate","move","select");
    }

    template<class ... Strings>
    unsigned short  _set_button(Strings&&... strings)
    {
      static_assert(_button_count<sizeof...(Strings)+1);
      unsigned short _count= 0;
      ((this->_button_list[_count].setText(std::forward<Strings>(strings)),
        this->_button_layout->addWidget(&_button_list[_count]),
        _button_list[_count].setCheckable(true),
        connect(&_button_list[_count],&QPushButton::clicked,_button_list,[this,_count]
        {
          for (int index= 0;index<_button_count;index++)
            _button_list[index].setChecked(false),_button_list[_count].setChecked(true);
          this->_drawing_widget->_mode = static_cast<scaler_operator_t>(_count);
        }),
        ++_count),...);
      this->_main_layout->addLayout(_button_layout);
      this->_main_layout->addWidget(this->_drawing_widget);
      this->_main_layout->addWidget(this->_message_status);
      this->_main_layout->addStretch();
      return _count;
    }

    ~main_window()override
    {delete[] _button_list;}

  private:
    friend class scaler_drawing_widget;
    QPushButton * _button_list = nullptr;
    scaler_drawing_widget*_drawing_widget = nullptr;
    QLabel*_message_status;
    QVBoxLayout* _main_layout ;
    QHBoxLayout* _button_layout ;
    scaler_operator_t _mode;
    static constexpr unsigned short  _button_count= 8;
    template<size_t..._stretch>
    using _stretch_sequence = std::index_sequence<_stretch...>;
  };

}




#endif
