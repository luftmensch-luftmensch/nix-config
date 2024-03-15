import QtQuick 2.15
import QtQuick.Controls 2.2
import SddmComponents 2.0
import QtGraphicalEffects 1.12
import QtQuick.Window 2.15

Rectangle {
	id: page

	width: Screen.width
	height: Screen.height
	Text {
		font.family: "Monospace";
	}

	focus: true

	// Background Fill: Just in case video/background image fails to load.
	Rectangle {
		anchors.fill: parent
		color: "black"
	}

	// property variant images: [
	// 	"./backgrounds/Atlante.jpg",
	// 	"./backgrounds/Greek Gods constellations.jpg",
	// 	"./backgrounds/Greek Mythology.jpg",
	// 	"./backgrounds/Heracle.jpg",
	// 	"./backgrounds/Icarus - Herbert James Draper.jpg",
	// 	"./backgrounds/lucifero.jpg",
	// 	"./backgrounds/Prometheus.png",
	// 	"./backgrounds/The\ nature\ of\ fear\ -\ L\'occhio\ occidentale.jpg",
	// 	"./backgrounds/The\ nature\ of\ fear.jpg",
	// ]

	Image{
		id: pic
		anchors.fill: parent
		width: Screen.width
		height: Screen.height

		fillMode: Image.PreserveAspectCrop
		// source: images[(Math.floor(Math.random() * images.length))]
		source: "assets/background.jpg"
	}

	ShaderEffectSource {
		id: blurMask

		sourceItem: page
		height: parent.height
		sourceRect: Qt.rect(x,y,width,height)
		visible: config.FullBlur == "true" || config.PartialBlur == "true" ? true : false
	}

	GaussianBlur {
		id: blur

		height: parent.height
		width: parent.width
		source: config.FullBlur == "true" ? pic : blurMask
		radius: config.BlurRadius
		samples: config.BlurRadius * 2 + 1
		cached: true
		anchors.centerIn: parent
		visible: config.FullBlur == "true" || config.PartialBlur == "true" ? true : false
	}

	//Time and Date
	Clock {
		id: clock
		y: parent.height * config.relativePositionY - clock.height / 2
		x: parent.width * config.relativePositionX - clock.width / 2
		color: "white"
		timeFont.pointSize: 35
		dateFont.pointSize: 12
		timeFont.family: "Monospace"
		dateFont.family: "Monospace"
	}


	Login {
		id: loginFrame
		visible: false
		opacity: 0
	}

	PowerFrame {
		id: powerFrame
	}

	ListView {
		id: sessionSelect
		width: currentItem.width
		height: count * currentItem.height
		model: sessionModel
		currentIndex: sessionModel.lastIndex
		visible: false
		opacity: 0
		flickableDirection: Flickable.AutoFlickIfNeeded
		anchors {
			bottom: powerFrame.top
			right: page.right
			rightMargin: 35
		}
		delegate: Item {
			width: 100
			height: 50
			Text {
				width: parent.width
				height: parent.height
				text: name
				color: "white"
				opacity: (delegateArea.containsMouse || sessionSelect.currentIndex == index) ? 1 : 0.3
				font {
					pointSize: (config.enableHDPI == "true") ? 6 : 12
					family: "monospace"
				}
				wrapMode: Text.Wrap
				horizontalAlignment: Text.AlignHCenter
				verticalAlignment: Text.AlignVCenter

				Behavior on opacity {
					NumberAnimation { duration: 250; easing.type: Easing.InOutQuad}
				}
			}

			MouseArea {
				id: delegateArea
				anchors.fill: parent
				hoverEnabled: true
				onClicked: {
					sessionSelect.currentIndex = index
					sessionSelect.state = ""
				}
			}
		}

		states: State {
			name: "show"
			PropertyChanges {
				target: sessionSelect
				visible: true
				opacity: 1
			}
		}

		transitions: [
			Transition {
				from: ""
				to: "show"
				SequentialAnimation {
					PropertyAnimation {
						target: sessionSelect
						properties: "visible"
						duration: 0
					}
					PropertyAnimation {
						target: sessionSelect
						properties: "opacity"
						duration: 500
					}
				}
			},
			Transition {
				from: "show"
				to: ""
				SequentialAnimation {
					PropertyAnimation {
						target: sessionSelect
						properties: "opacity"
						duration: 500
					}
					PropertyAnimation {
						target: sessionSelect
						properties: "visible"
						duration: 0
					}
				}
			}
		]

	}

	ChooseUser {
		id: listView
		visible: true
		opacity: 1
	}

	states: State {
		name: "login"
		PropertyChanges {
			target: listView
			visible: false
			opacity: 0
		}

		PropertyChanges {
			target: loginFrame
			visible: true
			opacity: 1
		}
	}

	transitions: [
		Transition {
			from: ""
			to: "login"
			reversible: false

			SequentialAnimation {
				PropertyAnimation {
					target: listView
					properties: "opacity"
					duration: 500
				}
				PropertyAnimation {
					target: listView
					properties: "visible"
					duration: 0
				}
				PropertyAnimation {
					target: loginFrame
					properties: "visible"
					duration: 0
				}
				PropertyAnimation {
					target: loginFrame
					properties: "opacity"
					duration: 500
				}
			}
		},
		Transition {
			from: "login"
			to: ""
			reversible: false

			SequentialAnimation {
				PropertyAnimation {
					target: loginFrame
					properties: "opacity"
					duration: 500
				}
				PropertyAnimation {
					target: loginFrame
					properties: "visible"
					duration: 0
				}
				PropertyAnimation {
					target: listView
					properties: "visible"
					duration: 0
				}
				PropertyAnimation {
					target: listView
					properties: "opacity"
					duration: 500
				}
			}
		}]
	}
