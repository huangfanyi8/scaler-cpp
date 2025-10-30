#ifndef FRAMELESS_WINDOW_PLAYER_HPP
#define FRAMELESS_WINDOW_PLAYER_HPP

#include<QPushButton>
#include<QMouseEvent>
#include<QEnterEvent>
#include<QPropertyAnimation>

namespace dong_dong_player
{
    class title_bar
            :public QWidget
    {
        Q_OBJECT
    public:
        explicit title_bar(QWidget*parent = nullptr);
    protected:

    private:
        void _set_ui();
        void _set_qss();
    private:
        QPushButton*_close_button = nullptr;
        QPushButton*_max_button = nullptr;
        QPushButton*_min_button = nullptr;
        QPushButton*_setting_button = nullptr;
    };

    class rotate_button
            :public  QPushButton
    {
            Q_OBJECT
            Q_PROPERTY(double  _angle  WRITE _set_angle READ _get_angle)
    public:
        explicit rotate_button(QWidget*);

    private:
        void _draw();
        [[nodiscard]] double _get_angle()const{return this->_angle;}
        void _set_angle(double );

    protected:
        void paintEvent(QPaintEvent*)override;
    private:
        double _angle = 0;
        QPropertyAnimation*_button_animation = nullptr;
    };


    class player
            :public QWidget
    {
    public:
        enum struct region
        {
            bottom,//0
            bottom_left,
            top_left,
            top,//3
            left,//4
            right,//5
            bottom_right,
            top_right,
            center
        };
        explicit player(QWidget*parent = nullptr);

    protected:
        void paintEvent(QPaintEvent*)override;
        void mouseMoveEvent(QMouseEvent*)override;
        void mousePressEvent(QMouseEvent*)override;
        void mouseReleaseEvent(QMouseEvent*)override;
        [[nodiscard]] region _get_region(const QPointF&point)const
        {
            const auto x =  static_cast<int>(point.x());
            const auto y = static_cast<int>( point.y());
            //遵循左闭右开的规则
            if(x<_padding_size)
            {
                if(y<_padding_size)
                    return region::top_left;
                else if(y< this->height()-_padding_size)
                    return  region::left;
                else
                    return region::bottom_left;
            }
            else if(x<this->width()-_padding_size)
            {
                if(y<_padding_size)
                    return region::top;
                else if(y< this->height()-_padding_size)
                    return  region::center;
                else
                    return region::bottom;
            }
            else
            {
                if(y<_padding_size)
                    return region::top_right;
                else if(y< this->height()-_padding_size)
                    return  region::right;
                else
                    return region::bottom_right;
            }
        }
        void enterEvent(QEnterEvent*event)override
        {

        }
    private:
        title_bar*_bar = new title_bar{this};//自定义标题栏
        QPoint _current_pos;

        QPointF _global_pos;
        static constexpr size_t _padding_size = 5;///缩放的边框范围
    };
}


#endif //FRAMELESS_WINDOW_PLAYER_HPP
