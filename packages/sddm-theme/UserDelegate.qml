import QtQuick 2.0

Item {

  width: 500
  height: 250

  property string name: model.name
  property string realName: (model.realName === "") ? model.name : model.realName

  //User's Name
  Text {
    id: usersName

    color: "white"
    font {
      family: "monospace"
      pointSize: 20
    }
    text: realName
    anchors.horizontalCenter: parent.horizontalCenter
  }

  //User's Profile Pic
  Image {
    id: usersPic

    width: 128
    height: 128
    anchors {
      top: usersName.bottom
      topMargin: 50
      horizontalCenter: parent.horizontalCenter
    }
    source: "assets/fallen_angel.png"
  }

  MouseArea {
    anchors.fill: parent
    onClicked: {
      listView.currentIndex = index;
      page.state = "login";
      loginFrame.name = name
      loginFrame.realName = realName
      loginFrame.icon = "assets/fallen_angel.png"
      listView.focus = false
      if (config.autoFocusPassword == "true")
      focusDelay.start();
      else
      loginFrame.focus = true;
    }
  }

}
