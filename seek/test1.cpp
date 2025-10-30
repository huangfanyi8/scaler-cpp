#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QWidget>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGridLayout>
#include <QPushButton>
#include <QLabel>
#include <QTextEdit>
#include <QLineEdit>
#include <QListWidget>
#include <QTabWidget>
#include <QMenuBar>
#include <QStatusBar>
#include <QToolBar>
#include <QSplitter>
#include <QFrame>
#include <QScrollArea>
#include <QGroupBox>

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private slots:
    void onMenuAction();
    void onButtonClicked();

private:
    void setupUI();
    void setupMenuBar();
    void setupToolBar();
    void setupStatusBar();
    void setupCentralWidget();
    void applyPyDraculaTheme();

    // UI Components
    QWidget *centralWidget;
    QVBoxLayout *mainLayout;
    QHBoxLayout *topLayout;
    QHBoxLayout *bottomLayout;
    QSplitter *mainSplitter;
    QSplitter *leftSplitter;
    QSplitter *rightSplitter;

    // Left Panel
    QWidget *leftPanel;
    QVBoxLayout *leftLayout;
    QLabel *logoLabel;
    QListWidget *navigationList;
    QGroupBox *userGroup;
    QLabel *userAvatar;
    QLabel *userName;
    QLabel *userStatus;

    // Center Panel
    QWidget *centerPanel;
    QVBoxLayout *centerLayout;
    QTabWidget *mainTabs;
    QWidget *homeTab;
    QWidget *projectsTab;
    QWidget *settingsTab;

    // Home Tab Content
    QVBoxLayout *homeLayout;
    QLabel *welcomeLabel;
    QTextEdit *recentActivity;
    QGroupBox *quickActions;
    QGridLayout *quickActionsLayout;
    QPushButton *newProjectBtn;
    QPushButton *openProjectBtn;
    QPushButton *settingsBtn;
    QPushButton *helpBtn;

    // Projects Tab Content
    QVBoxLayout *projectsLayout;
    QHBoxLayout *projectsHeader;
    QLineEdit *searchBox;
    QPushButton *filterBtn;
    QPushButton *sortBtn;
    QListWidget *projectsList;

    // Settings Tab Content
    QVBoxLayout *settingsLayout;
    QGroupBox *appearanceGroup;
    QGroupBox *generalGroup;
    QGroupBox *advancedGroup;

    // Right Panel
    QWidget *rightPanel;
    QVBoxLayout *rightLayout;
    QGroupBox *activityGroup;
    QTextEdit *activityLog;
    QGroupBox *notificationsGroup;
    QListWidget *notificationsList;

    // Menu and Toolbar
    QMenuBar *menuBar;
    QToolBar *toolBar;
    QStatusBar *statusBar;
};

#include <QApplication>

#include <QMargins>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
{
    setupUI();
    applyPyDraculaTheme();
    setWindowTitle("PyDracula - Qt6 Application");
    setMinimumSize(1200, 800);
    resize(1400, 900);
}

MainWindow::~MainWindow()
{
}

void MainWindow::setupUI()
{
    setupMenuBar();
    setupToolBar();
    setupCentralWidget();
    setupStatusBar();
}

void MainWindow::setupMenuBar()
{
    menuBar = new QMenuBar(this);
    setMenuBar(menuBar);

    // File Menu
    QMenu *fileMenu = menuBar->addMenu("&File");
    fileMenu->addAction("&New Project", this, &MainWindow::onMenuAction);
    fileMenu->addAction("&Open Project", this, &MainWindow::onMenuAction);
    fileMenu->addAction("&Save", this, &MainWindow::onMenuAction);
    fileMenu->addSeparator();
    fileMenu->addAction("E&xit", this, &QWidget::close);

    // Edit Menu
    QMenu *editMenu = menuBar->addMenu("&Edit");
    editMenu->addAction("&Undo", this, &MainWindow::onMenuAction);
    editMenu->addAction("&Redo", this, &MainWindow::onMenuAction);
    editMenu->addSeparator();
    editMenu->addAction("&Cut", this, &MainWindow::onMenuAction);
    editMenu->addAction("&Copy", this, &MainWindow::onMenuAction);
    editMenu->addAction("&Paste", this, &MainWindow::onMenuAction);

    // View Menu
    QMenu *viewMenu = menuBar->addMenu("&View");
    viewMenu->addAction("&Full Screen", this, &MainWindow::onMenuAction);
    viewMenu->addAction("&Zoom In", this, &MainWindow::onMenuAction);
    viewMenu->addAction("&Zoom Out", this, &MainWindow::onMenuAction);

    // Help Menu
    QMenu *helpMenu = menuBar->addMenu("&Help");
    helpMenu->addAction("&About", this, &MainWindow::onMenuAction);
    helpMenu->addAction("&Documentation", this, &MainWindow::onMenuAction);
}

void MainWindow::setupToolBar()
{
    toolBar = addToolBar("Main Toolbar");
    toolBar->addAction("New", this, &MainWindow::onButtonClicked);
    toolBar->addAction("Open", this, &MainWindow::onButtonClicked);
    toolBar->addAction("Save", this, &MainWindow::onButtonClicked);
    toolBar->addSeparator();
    toolBar->addAction("Undo", this, &MainWindow::onButtonClicked);
    toolBar->addAction("Redo", this, &MainWindow::onButtonClicked);
    toolBar->addSeparator();
    toolBar->addAction("Settings", this, &MainWindow::onButtonClicked);
}

void MainWindow::setupCentralWidget()
{
    centralWidget = new QWidget(this);
    setCentralWidget(centralWidget);

    mainLayout = new QVBoxLayout(centralWidget);
    mainLayout->setContentsMargins(5, 5, 5, 5);
    mainLayout->setSpacing(5);

    // Create main splitter
    mainSplitter = new QSplitter(Qt::Horizontal, this);
    mainLayout->addWidget(mainSplitter);

    // Left Panel
    leftPanel = new QWidget();
    leftPanel->setMaximumWidth(250);
    leftPanel->setMinimumWidth(200);
    leftLayout = new QVBoxLayout(leftPanel);
    leftLayout->setContentsMargins(10, 10, 10, 10);
    leftLayout->setSpacing(10);

    // Logo
    logoLabel = new QLabel("PyDracula");
    logoLabel->setAlignment(Qt::AlignCenter);
    logoLabel->setStyleSheet("font-size: 24px; font-weight: bold; color: #ff79c6;");
    leftLayout->addWidget(logoLabel);

    // Navigation
    navigationList = new QListWidget();
    navigationList->addItem("🏠 Home");
    navigationList->addItem("📁 Projects");
    navigationList->addItem("⚙️ Settings");
    navigationList->addItem("📊 Analytics");
    navigationList->addItem("👥 Team");
    navigationList->addItem("💬 Messages");
    leftLayout->addWidget(navigationList);

    // User Info
    userGroup = new QGroupBox("User Profile");
    userAvatar = new QLabel("👤");
    userAvatar->setAlignment(Qt::AlignCenter);
    userAvatar->setStyleSheet("font-size: 48px;");
    userName = new QLabel("John Doe");
    userName->setAlignment(Qt::AlignCenter);
    userName->setStyleSheet("font-weight: bold; color: #f8f8f2;");
    userStatus = new QLabel("Online");
    userStatus->setAlignment(Qt::AlignCenter);
    userStatus->setStyleSheet("color: #50fa7b;");

    QVBoxLayout *userLayout = new QVBoxLayout(userGroup);
    userLayout->addWidget(userAvatar);
    userLayout->addWidget(userName);
    userLayout->addWidget(userStatus);
    leftLayout->addWidget(userGroup);

    leftLayout->addStretch();
    mainSplitter->addWidget(leftPanel);

    // Center Panel
    centerPanel = new QWidget();
    centerLayout = new QVBoxLayout(centerPanel);
    centerLayout->setContentsMargins(10, 10, 10, 10);
    centerLayout->setSpacing(10);

    // Main Tabs
    mainTabs = new QTabWidget();

    // Home Tab
    homeTab = new QWidget();
    homeLayout = new QVBoxLayout(homeTab);
    homeLayout->setSpacing(15);

    welcomeLabel = new QLabel("Welcome to PyDracula");
    welcomeLabel->setStyleSheet("font-size: 28px; font-weight: bold; color: #f8f8f2; margin: 10px;");
    homeLayout->addWidget(welcomeLabel);

    // Recent Activity
    QGroupBox *activityGroup = new QGroupBox("Recent Activity");
    recentActivity = new QTextEdit();
    recentActivity->setMaximumHeight(200);
    recentActivity->setPlainText("• Project 'WebApp' updated\n• New file 'main.py' created\n• Build completed successfully\n• Team member joined project");
    QVBoxLayout *activityLayout = new QVBoxLayout(activityGroup);
    activityLayout->addWidget(recentActivity);
    homeLayout->addWidget(activityGroup);

    // Quick Actions
    quickActions = new QGroupBox("Quick Actions");
    quickActionsLayout = new QGridLayout(quickActions);

    newProjectBtn = new QPushButton("New Project");
    openProjectBtn = new QPushButton("Open Project");
    settingsBtn = new QPushButton("Settings");
    helpBtn = new QPushButton("Help");

    quickActionsLayout->addWidget(newProjectBtn, 0, 0);
    quickActionsLayout->addWidget(openProjectBtn, 0, 1);
    quickActionsLayout->addWidget(settingsBtn, 1, 0);
    quickActionsLayout->addWidget(helpBtn, 1, 1);

    homeLayout->addWidget(quickActions);
    homeLayout->addStretch();

    mainTabs->addTab(homeTab, "🏠 Home");

    // Projects Tab
    projectsTab = new QWidget();
    projectsLayout = new QVBoxLayout(projectsTab);
    projectsLayout->setSpacing(10);

    projectsHeader = new QHBoxLayout();
    searchBox = new QLineEdit();
    searchBox->setPlaceholderText("Search projects...");
    filterBtn = new QPushButton("Filter");
    sortBtn = new QPushButton("Sort");

    projectsHeader->addWidget(searchBox);
    projectsHeader->addWidget(filterBtn);
    projectsHeader->addWidget(sortBtn);
    projectsHeader->addStretch();

    projectsList = new QListWidget();
    projectsList->addItem("📱 Mobile App Project");
    projectsList->addItem("🌐 Web Application");
    projectsList->addItem("🤖 AI/ML Project");
    projectsList->addItem("📊 Data Analysis");
    projectsList->addItem("🎮 Game Development");

    projectsLayout->addLayout(projectsHeader);
    projectsLayout->addWidget(projectsList);

    mainTabs->addTab(projectsTab, "📁 Projects");

    // Settings Tab
    settingsTab = new QWidget();
    settingsLayout = new QVBoxLayout(settingsTab);
    settingsLayout->setSpacing(15);

    appearanceGroup = new QGroupBox("Appearance");
    QVBoxLayout *appearanceLayout = new QVBoxLayout(appearanceGroup);
    appearanceLayout->addWidget(new QLabel("Theme: PyDracula"));
    appearanceLayout->addWidget(new QLabel("Font Size: Medium"));
    appearanceLayout->addWidget(new QLabel("Color Scheme: Dark"));

    generalGroup = new QGroupBox("General");
    QVBoxLayout *generalLayout = new QVBoxLayout(generalGroup);
    generalLayout->addWidget(new QLabel("Language: English"));
    generalLayout->addWidget(new QLabel("Auto-save: Enabled"));
    generalLayout->addWidget(new QLabel("Notifications: On"));

    advancedGroup = new QGroupBox("Advanced");
    QVBoxLayout *advancedLayout = new QVBoxLayout(advancedGroup);
    advancedLayout->addWidget(new QLabel("Debug Mode: Off"));
    advancedLayout->addWidget(new QLabel("Log Level: Info"));
    advancedLayout->addWidget(new QLabel("Cache Size: 256MB"));

    settingsLayout->addWidget(appearanceGroup);
    settingsLayout->addWidget(generalGroup);
    settingsLayout->addWidget(advancedGroup);
    settingsLayout->addStretch();

    mainTabs->addTab(settingsTab, "⚙️ Settings");

    centerLayout->addWidget(mainTabs);
    mainSplitter->addWidget(centerPanel);

    // Right Panel
    rightPanel = new QWidget();
    rightPanel->setMaximumWidth(300);
    rightPanel->setMinimumWidth(250);
    rightLayout = new QVBoxLayout(rightPanel);
    rightLayout->setContentsMargins(10, 10, 10, 10);
    rightLayout->setSpacing(10);

    // Activity Log
    activityGroup = new QGroupBox("Activity Log");
    activityLog = new QTextEdit();
    activityLog->setMaximumHeight(200);
    activityLog->setPlainText("14:30 - User logged in\n14:25 - Project saved\n14:20 - File modified\n14:15 - Build started");
    QVBoxLayout *activityLogLayout = new QVBoxLayout(activityGroup);
    activityLogLayout->addWidget(activityLog);
    rightLayout->addWidget(activityGroup);

    // Notifications
    notificationsGroup = new QGroupBox("Notifications");
    notificationsList = new QListWidget();
    notificationsList->addItem("🔔 New message from team");
    notificationsList->addItem("📁 Project update available");
    notificationsList->addItem("⚡ System update ready");
    notificationsList->addItem("🎉 Welcome to PyDracula!");
    QVBoxLayout *notificationsLayout = new QVBoxLayout(notificationsGroup);
    notificationsLayout->addWidget(notificationsList);
    rightLayout->addWidget(notificationsGroup);

    rightLayout->addStretch();
    mainSplitter->addWidget(rightPanel);

    // Set splitter proportions
    mainSplitter->setSizes({250, 800, 300});
}

void MainWindow::setupStatusBar()
{
    statusBar = new QStatusBar(this);
    setStatusBar(statusBar);
    statusBar->showMessage("Ready");
    statusBar->addPermanentWidget(new QLabel("PyDracula v1.0"));
}

void MainWindow::applyPyDraculaTheme()
{
    // PyDracula color scheme
    QString styleSheet = R"(
        QMainWindow {
            background-color: #282a36;
            color: #f8f8f2;
        }

        QWidget {
            background-color: #282a36;
            color: #f8f8f2;
        }

        QMenuBar {
            background-color: #44475a;
            color: #f8f8f2;
            border-bottom: 1px solid #6272a4;
        }

        QMenuBar::item {
            background-color: transparent;
            padding: 4px 8px;
        }

        QMenuBar::item:selected {
            background-color: #6272a4;
        }

        QMenu {
            background-color: #44475a;
            color: #f8f8f2;
            border: 1px solid #6272a4;
        }

        QMenu::item:selected {
            background-color: #6272a4;
        }

        QToolBar {
            background-color: #44475a;
            border: none;
            spacing: 3px;
        }

        QToolBar QToolButton {
            background-color: transparent;
            border: 1px solid transparent;
            padding: 4px;
            margin: 2px;
        }

        QToolBar QToolButton:hover {
            background-color: #6272a4;
            border: 1px solid #8be9fd;
        }

        QStatusBar {
            background-color: #44475a;
            color: #f8f8f2;
            border-top: 1px solid #6272a4;
        }

        QTabWidget::pane {
            border: 1px solid #6272a4;
            background-color: #282a36;
        }

        QTabBar::tab {
            background-color: #44475a;
            color: #f8f8f2;
            padding: 8px 16px;
            margin-right: 2px;
        }

        QTabBar::tab:selected {
            background-color: #282a36;
            border-bottom: 2px solid #ff79c6;
        }

        QTabBar::tab:hover {
            background-color: #6272a4;
        }

        QGroupBox {
            font-weight: bold;
            border: 2px solid #6272a4;
            border-radius: 5px;
            margin-top: 10px;
            padding-top: 10px;
        }

        QGroupBox::title {
            subcontrol-origin: margin;
            left: 10px;
            padding: 0 5px 0 5px;
            color: #ff79c6;
        }

        QPushButton {
            background-color: #44475a;
            color: #f8f8f2;
            border: 1px solid #6272a4;
            border-radius: 4px;
            padding: 8px 16px;
            font-weight: bold;
        }

        QPushButton:hover {
            background-color: #6272a4;
            border: 1px solid #8be9fd;
        }

        QPushButton:pressed {
            background-color: #ff79c6;
            color: #282a36;
        }

        QLineEdit {
            background-color: #44475a;
            color: #f8f8f2;
            border: 1px solid #6272a4;
            border-radius: 4px;
            padding: 6px;
        }

        QLineEdit:focus {
            border: 2px solid #8be9fd;
        }

        QTextEdit {
            background-color: #44475a;
            color: #f8f8f2;
            border: 1px solid #6272a4;
            border-radius: 4px;
            selection-background-color: #6272a4;
        }

        QListWidget {
            background-color: #44475a;
            color: #f8f8f2;
            border: 1px solid #6272a4;
            border-radius: 4px;
            selection-background-color: #6272a4;
        }

        QListWidget::item {
            padding: 6px;
            border-bottom: 1px solid #6272a4;
        }

        QListWidget::item:selected {
            background-color: #6272a4;
            color: #f8f8f2;
        }

        QListWidget::item:hover {
            background-color: #6272a4;
        }

        QSplitter::handle {
            background-color: #6272a4;
        }

        QSplitter::handle:horizontal {
            width: 3px;
        }

        QSplitter::handle:vertical {
            height: 3px;
        }
    )";

    setStyleSheet(styleSheet);
}

void MainWindow::onMenuAction()
{
    // Handle menu actions
    statusBar->showMessage("Menu action triggered", 2000);
}

void MainWindow::onButtonClicked()
{
    // Handle button clicks
    statusBar->showMessage("Button clicked", 2000);
}

// main.cpp
int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    MainWindow demo;
    demo.show();

    return app.exec();
}
#include "test1.moc"
#endif // MAINWINDOW_H

