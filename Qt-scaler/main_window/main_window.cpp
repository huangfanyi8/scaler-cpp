#include"main_window.hpp"
#include <QPainter>
#include <QDebug>
#include <cmath>

Bouncing_Ball_Loading::Bouncing_Ball_Loading(QWidget *parent)
        : QWidget(parent)
{
    this->updateBallPositions(m_ballCount);

    m_timer = new QTimer(this);
    m_updateTimer = new QTimer(this);
    connect(m_updateTimer, &QTimer::timeout, this, [=] {
        update();
    });
    m_updateTimer->setTimerType(Qt::PreciseTimer);

    m_updateTimer->start(15);

    connect(m_timer, &QTimer::timeout, this, [=] {
        if (m_balls.size() - 1 < m_timerCount)
        {
            m_timer->stop();
            m_timerCount = 0;
            return;
        }

        qDebug() << "当前球号：" << m_timerCount;

        m_balls[m_timerCount]->startAnimation();
        m_timerCount++;
    });
    m_timer->setTimerType(Qt::PreciseTimer);

    m_timer->start(200);
}

void Bouncing_Ball_Loading::paintEvent(QPaintEvent *event)
{
    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);
    painter.setBrush(QColor(239, 239, 239, 255));
    painter.setPen(Qt::NoPen);
    painter.drawRect(rect());

    for (auto sphere : m_balls)
    {
        painter.setBrush(QColor(117,117,117,106));

        qreal endshadowY = sphere->fallEndPosition().ry();
        qreal positionY = sphere->position().ry();
        qreal shadowWidth = (positionY / endshadowY) * 30;
        painter.drawEllipse(sphere->fallEndPosition() + QPointF(0.0,25.0), 2 + shadowWidth, 5);

        painter.setBrush(sphere->ballColor());
        painter.setPen(Qt::NoPen);
        painter.drawEllipse(sphere->position(), sphere->widthRadius(), sphere->heightRadius());
    }
}

void Bouncing_Ball_Loading::updateBallPositions(int ballCount)
{
    if (!m_balls.empty())
    {
        for (Sphere *sphere : m_balls)
        {
            delete sphere;
        }
        m_balls.clear();
    }
    if (ballCount <= 0)
    {
        qDebug() << "球的数量小于等于0";
        return;
    }

    int _x = width() / (ballCount + 3);
    int _y = height() / 5;

    for (int i = 1; i < ballCount + 1; ++i)
    {
        Sphere *sphere1 = new Sphere(this);
        sphere1->setStartAndEndPosition(QPointF(_x * (i * 2 -1) , _y), QPointF(_x * (i * 2 -1), height() - _y));
        sphere1->configureAnimation();

        m_balls.push_back(sphere1);
    };
}

void Bouncing_Ball_Loading::setSize(QSize size)
{
    this->resize(size);
    this->updateBallPositions(m_ballCount);
    m_timer->start(200);
}

Sphere::Sphere(QObject *parent) : QObject(parent)
{

}

void Sphere::startAnimation()
{
    m_animationFinished = false;
    animation->start();
}

void Sphere::configureAnimation()
{
    QPointF L_position = m_position;
    QPointF L_fallEndPosition = m_fallEndPosition;

    animation = new QPropertyAnimation(this, "position");
    animation->setDuration(400);
    animation->setStartValue(L_position);
    animation->setEndValue(L_fallEndPosition);
    animation->setEasingCurve(QEasingCurve::Linear);

    connect(animation, &QPropertyAnimation::finished, this, [=] {
        animation2 = new QPropertyAnimation(this, "widthRadius");
        animation2->setDuration(100);
        animation2->setKeyValueAt(0.0,30);
        animation2->setKeyValueAt(0.4,40);
        animation2->setKeyValueAt(0.6,40);
        animation2->setKeyValueAt(1.0,30);
        animation2->setEasingCurve(QEasingCurve::Linear);

        animation3 = new QPropertyAnimation(this, "heightRadius");
        animation3->setDuration(100);
        animation3->setKeyValueAt(0.0,30);
        animation3->setKeyValueAt(0.4,10);
        animation3->setKeyValueAt(0.6,10);
        animation3->setKeyValueAt(1.0,30);
        animation3->setEasingCurve(QEasingCurve::Linear);

        animation2->start();
        animation3->start();

        connect(animation2, &QPropertyAnimation::finished, this, [=] {
            animation4 = new QPropertyAnimation(this, "position");
            animation4->setDuration(400);
            animation4->setStartValue(L_fallEndPosition);
            animation4->setEndValue(L_position);
            animation4->setEasingCurve(QEasingCurve::Linear);
            animation4->start();

            connect(animation4, &QPropertyAnimation::finished, this, [=] {
                startAnimation();
                m_animationFinished = true;
            });
        });
    });
}

void Sphere::setStartAndEndPosition(QPointF startPosition, QPointF endPosition)
{
    if (m_position == startPosition && m_fallEndPosition == endPosition)
        return;
    m_position = startPosition;
    m_fallEndPosition = endPosition;
    emit positionChanged();
    emit fallEndPositionChanged();
}

QPointF Sphere::position() const
{
    return m_position;
}

void Sphere::setPosition(QPointF newPosition)
{
    if (m_position == newPosition)
        return;
    m_position = newPosition;
    emit positionChanged();
}

int Sphere::widthRadius() const
{
    return m_widthRadius;
}

void Sphere::setWidthRadius(int newWidthRadius)
{
    if (m_widthRadius == newWidthRadius)
        return;
    m_widthRadius = newWidthRadius;
    emit widthRadiusChanged();
}

int Sphere::heightRadius() const
{
    return m_heightRadius;
}

void Sphere::setHeightRadius(int newHeightRadius)
{
    if (m_heightRadius == newHeightRadius)
        return;
    m_heightRadius = newHeightRadius;
    emit heightRadiusChanged();
}

QPointF Sphere::fallEndPosition() const
{
    return m_fallEndPosition;
}

void Sphere::setFallEndPosition(QPointF newFallEndPosition)
{
    if (m_fallEndPosition == newFallEndPosition)
        return;
    m_fallEndPosition = newFallEndPosition;
    emit fallEndPositionChanged();
}
#include <QDebug>
#include <QFont>
#include <QFontMetrics>
#include <QPainterPath>

Rectangle_Loading::Rectangle_Loading(QWidget *parent)
        : QWidget(parent),
          m_rotationAngle(0),
          m_borderWidth(5),
          m_rotationAnimation(nullptr),
          m_borderAnimation(nullptr),
          m_dotTimer(nullptr),
          m_currentDotCount(0),
          m_drawColor(QColor(255,64,91,255))
{
    setupAnimations();

    m_dotTimer = new QTimer(this);
    connect(m_dotTimer, &QTimer::timeout, this, &Rectangle_Loading::updateLoadingDots);
    m_dotTimer->start(500);
}

Rectangle_Loading::~Rectangle_Loading()
{
    if (m_rotationAnimation) {
        m_rotationAnimation->stop();
    }
    if (m_borderAnimation) {
        m_borderAnimation->stop();
    }
    if (m_dotTimer) {
        m_dotTimer->stop();
    }
}

void Rectangle_Loading::setRotationAngle(qreal angle) {
    if (m_rotationAngle != angle) {
        m_rotationAngle = angle;
        update();
    }
}

void Rectangle_Loading::setBorderWidth(int width) {
    if (m_borderWidth != width) {
        m_borderWidth = width;
        update();
    }
}

void Rectangle_Loading::setupAnimations()
{
    m_rotationAnimation = new QPropertyAnimation(this, "rotationAngle");
    m_rotationAnimation->setStartValue(0);
    m_rotationAnimation->setEndValue(360);
    m_rotationAnimation->setDuration(3000);
    m_rotationAnimation->setLoopCount(-1);
    m_rotationAnimation->setEasingCurve(QEasingCurve::Linear);
    m_rotationAnimation->start();

    m_borderAnimation = new QPropertyAnimation(this, "borderWidth");
    m_borderAnimation->setStartValue(5);
    m_borderAnimation->setEndValue(20);
    m_borderAnimation->setDuration(1500);
    m_borderAnimation->setLoopCount(-1);
    m_borderAnimation->setDirection(QAbstractAnimation::Forward);
    m_borderAnimation->setEasingCurve(QEasingCurve::OutCubic);
    m_borderAnimation->setKeyValueAt(0, 5);
    m_borderAnimation->setKeyValueAt(0.5, 20);
    m_borderAnimation->setKeyValueAt(1, 5);
    m_borderAnimation->start();
}

void Rectangle_Loading::paintEvent(QPaintEvent *event)
{
    Q_UNUSED(event);

    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);
    painter.setBrush(QColor(239, 239, 239, 255));
    painter.setPen(Qt::NoPen);
    painter.drawRect(rect());
    int minDimension = qMin(width(), height());

    int squareSide = minDimension / 2;

    int minBorderWidth = qMax(2, minDimension / 40);
    int maxBorderWidth = qMax(minBorderWidth + 5, minDimension / 10);

    if (m_borderAnimation->state() == QAbstractAnimation::Running) {
        m_borderAnimation->setKeyValueAt(0, minBorderWidth);
        m_borderAnimation->setKeyValueAt(0.5, maxBorderWidth);
        m_borderAnimation->setKeyValueAt(1, minBorderWidth);
    }
    m_borderWidth = qBound(minBorderWidth, m_borderWidth, maxBorderWidth);

    if (squareSide <= m_borderWidth * 2) {
        squareSide = m_borderWidth * 2 + 1;
    }

    QPen pen;
    pen.setColor(m_drawColor);
    pen.setWidth(m_borderWidth);
    pen.setJoinStyle(Qt::MiterJoin);
    painter.setPen(pen);

    painter.save();
    painter.translate(width() / 2.0, height() / 2.0);
    painter.rotate(m_rotationAngle);
    painter.drawRect(-squareSide / 2.0, -squareSide / 2.0, squareSide, squareSide);
    painter.restore();

    painter.setPen(m_drawColor);
    QFont font;
    font.setPointSize(qMax(10, minDimension / 22));
    font.setBold(true);
    painter.setFont(font);

    QString baseText = "Loading";
    QString dots = "";
    for (int i = 0; i < m_currentDotCount; ++i) {
        dots += ".";
    }
    QString displayText = baseText + dots;

    QFontMetrics fm(font);
    int textWidth = fm.horizontalAdvance(displayText);
    int textHeight = fm.height();

    int x = (width() - textWidth) / 2;
    int y = (height() - textHeight) / 2 + fm.ascent();
    painter.drawText(x, y, displayText);
}

void Rectangle_Loading::resizeEvent(QResizeEvent *event)
{
    Q_UNUSED(event);
    update();
}

void Rectangle_Loading::updateLoadingDots()
{
    m_currentDotCount = (m_currentDotCount + 1) % 4;
    update();
}
Square_Pseudo_Bounce_Loading::Square_Pseudo_Bounce_Loading(QWidget *parent)
        : QWidget{parent},
          m_squareSize(0),
          m_offsetY(0),
          m_currentRotation(0),
          m_offsetAnimation1(nullptr),
          m_offsetAnimation2(nullptr),
          m_bounceGroup(nullptr),
          m_rotationAnimation(nullptr),
          m_mainAnimation(nullptr)
{
    setAttribute(Qt::WA_TranslucentBackground);
    QSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    setupAnimations();
}

Square_Pseudo_Bounce_Loading::~Square_Pseudo_Bounce_Loading()
{
    if (m_mainAnimation) {
        m_mainAnimation->stop();
        delete m_mainAnimation;
    }
}

void Square_Pseudo_Bounce_Loading::setOffsetY(qreal offset)
{
    if (m_offsetY == offset)
        return;
    m_offsetY = offset;
    update();
}

void Square_Pseudo_Bounce_Loading::setCurrentRotation(qreal rotation)
{
    if (m_currentRotation == rotation)
        return;
    m_currentRotation = rotation;
    update();
}

void Square_Pseudo_Bounce_Loading::setupAnimations()
{
    m_offsetAnimation1 = new QPropertyAnimation(this, "offsetY");
    m_offsetAnimation1->setStartValue(0.0);
    m_offsetAnimation1->setEndValue(-bounceHeight);
    m_offsetAnimation1->setDuration(350);
    m_offsetAnimation1->setEasingCurve(QEasingCurve::OutQuad);
    QObject::connect(this->m_offsetAnimation1,&QPropertyAnimation::finished,this,[this]
    {
        this->m_offsetAnimation1->setDirection(this->m_offsetAnimation1->direction() == QAbstractAnimation::Backward?QAbstractAnimation::Forward:QAbstractAnimation::Backward);
        this->m_offsetAnimation1->setEasingCurve(this->m_offsetAnimation1->direction() == QAbstractAnimation::Backward?QEasingCurve::InQuad:QEasingCurve::OutQuad);
    });

    m_rotationAnimation = new QPropertyAnimation(this, "currentRotation");
    m_rotationAnimation->setStartValue(0.0);
    m_rotationAnimation->setEndValue(360.0);
    m_rotationAnimation->setDuration(2800);
    m_rotationAnimation->setEasingCurve(QEasingCurve::Linear);

    m_mainAnimation = new QParallelAnimationGroup(this);
    m_mainAnimation->addAnimation(m_offsetAnimation1);
    m_mainAnimation->addAnimation(m_rotationAnimation);
    m_mainAnimation->setLoopCount(-1);

    m_mainAnimation->start();
}

void Square_Pseudo_Bounce_Loading::updateSquareGeometry()
{
    qreal minDim = qMin(width(), height());
    m_squareSize = minDim / 7.0;

    update();
}

void Square_Pseudo_Bounce_Loading::paintEvent(QPaintEvent *event)
{
    Q_UNUSED(event);

    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing);
    painter.setBrush(QColor(96, 151, 226, 255));
    painter.setPen(Qt::NoPen);
    painter.drawRect(rect());

    //painter.drawImage(QRect(0, 0, width(), height()),blur_image1);
    painter.save();

    painter.setPen(Qt::NoPen);
    painter.setBrush(Qt::white);

    //QPainterPath path;
    //path.addEllipse(width() / 2.0 - height() / 4.3,
    //                0 + m_squareSize / 4.0 + -m_offsetY * 0.8,
    //                height() / 2.0,
    //                height() / 2.0);
//
    //painter.setClipPath(path);
    //
    painter.translate(width() / 2.0, height() / 2.0 + m_offsetY);

    painter.rotate(m_initialRotation + m_currentRotation);

    painter.drawRoundedRect(QRectF(-m_squareSize / 2.0, -m_squareSize / 2.0, m_squareSize, m_squareSize),m_squareSize / 10,m_squareSize / 10);
    painter.restore();
}

void Square_Pseudo_Bounce_Loading::resizeEvent(QResizeEvent *event)
{
    Q_UNUSED(event);
    updateSquareGeometry();
}