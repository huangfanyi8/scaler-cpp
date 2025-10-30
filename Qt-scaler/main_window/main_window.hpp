#ifndef BOUNCING_BALL_LOADING_H
#define BOUNCING_BALL_LOADING_H

#include <QWidget>
#include <QTimer>
#include <QPropertyAnimation>
#include <QParallelAnimationGroup>
#include <QSequentialAnimationGroup>
#include <QPainter>

#include <QObject>
#include <vector>


class Sphere : public QObject
{
Q_OBJECT
    Q_PROPERTY(QPointF position READ position WRITE setPosition NOTIFY positionChanged)
    Q_PROPERTY(int widthRadius READ widthRadius WRITE setWidthRadius NOTIFY widthRadiusChanged)
    Q_PROPERTY(int heightRadius READ heightRadius WRITE setHeightRadius NOTIFY heightRadiusChanged)

    Q_PROPERTY(QPointF fallEndPosition READ fallEndPosition WRITE setFallEndPosition NOTIFY fallEndPositionChanged)

public:
    explicit Sphere(QObject *parent = nullptr);

    void startAnimation();
    void configureAnimation();

    void setStartAndEndPosition(QPointF startPosition, QPointF endPosition);

public:
    QPointF position() const;
    void setPosition(QPointF newPosition);
    int widthRadius() const;
    void setWidthRadius(int newWidthRadius);

    int heightRadius() const;
    void setHeightRadius(int newHeightRadius);

    QPointF fallEndPosition() const;
    void setFallEndPosition(QPointF newFallEndPosition);

    QColor ballColor() const { return m_ballColor; }
    void setBallColor(const QColor &color) { m_ballColor = color; }

    bool animationFinished() const { return m_animationFinished; }
    void setAnimationFinished(bool finished) { m_animationFinished = finished; }
signals:
    void positionChanged();
    void widthRadiusChanged();

    void heightRadiusChanged();

    void fallEndPositionChanged();

private:
    QPointF m_position;
    int m_widthRadius{30};
    int m_heightRadius{30};
    QPointF m_fallEndPosition;

    QColor m_ballColor = QColor(255,64,91,255);

    QPropertyAnimation *animation;
    QPropertyAnimation *animation2;
    QPropertyAnimation *animation3;
    QPropertyAnimation *animation4;

    bool m_animationFinished = true;
};

class Bouncing_Ball_Loading : public QWidget
{
Q_OBJECT
public:
    explicit Bouncing_Ball_Loading(QWidget *parent = nullptr);

    void updateBallPositions(int ballCount);

    void setSize(QSize size);

protected:
    void paintEvent(QPaintEvent *event) override;

private slots:

private:
    int m_ballCount = 3;
    std::vector<Sphere*> m_balls;

    QTimer *m_timer;
    int m_timerCount = 0;
    int m_currentTime = 240;

    QTimer *m_updateTimer;
};
class Rectangle_Loading : public QWidget
{
Q_OBJECT
    Q_PROPERTY(qreal rotationAngle READ rotationAngle WRITE setRotationAngle)
    Q_PROPERTY(int borderWidth READ borderWidth WRITE setBorderWidth)

public:
    explicit Rectangle_Loading(QWidget *parent = nullptr);
    ~Rectangle_Loading();

    qreal rotationAngle() const { return m_rotationAngle; }
    void setRotationAngle(qreal angle);

    int borderWidth() const { return m_borderWidth; }
    void setBorderWidth(int width);

protected:
    void paintEvent(QPaintEvent *event) override;
    void resizeEvent(QResizeEvent *event) override;

private slots:
    void updateLoadingDots();

private:
    qreal m_rotationAngle;
    int m_borderWidth;
    QPropertyAnimation *m_rotationAnimation;
    QPropertyAnimation *m_borderAnimation;

    QTimer *m_dotTimer;
    int m_currentDotCount;

    QColor m_drawColor;

    void setupAnimations();
};
class Square_Pseudo_Bounce_Loading : public QWidget
{
Q_OBJECT

    Q_PROPERTY(qreal offsetY READ offsetY WRITE setOffsetY)
    Q_PROPERTY(qreal currentRotation READ currentRotation WRITE setCurrentRotation)

public:
    explicit Square_Pseudo_Bounce_Loading(QWidget *parent = nullptr);
    ~Square_Pseudo_Bounce_Loading();

    qreal offsetY() const { return m_offsetY; }
    qreal currentRotation() const { return m_currentRotation; }

    void setOffsetY(qreal offset);
    void setCurrentRotation(qreal rotation);

protected:
    void paintEvent(QPaintEvent *event) override;
    void resizeEvent(QResizeEvent *event) override;

private:
    qreal m_squareSize;//矩形的边长
    static constexpr qreal m_initialRotation =45.0;
    qreal m_offsetY;
    qreal m_currentRotation;
    static constexpr qreal bounceHeight{20};
    QPropertyAnimation *m_offsetAnimation1;
    QPropertyAnimation *m_offsetAnimation2;
    QSequentialAnimationGroup *m_bounceGroup;
    QPropertyAnimation *m_rotationAnimation;
    QParallelAnimationGroup *m_mainAnimation;

    void setupAnimations();
    void updateSquareGeometry();
};
#endif // BOUNCING_BALL_LOADING_H
