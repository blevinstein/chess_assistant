var boardStyle = {
  "border": "5px solid black",
  "height": 850,
  "width": 850
};

var messageStyle = {
  "color": "red"
};

var selectedStyle = {
  "fill": "yellow",
  "opacity": 0.5,
  "pointerEvents": "none"
};

var textStyle = {
  "alignmentBaseline": "central",
  "fontFamily": "sans-serif",
  "pointerEvents": "none",
  "textAnchor": "middle"
};

function getCharacter(color, piece) {
  if      (color == 'white' && piece == 'K') return "\u2654";
  else if (color == 'white' && piece == 'Q') return "\u2655";
  else if (color == 'white' && piece == 'R') return "\u2656";
  else if (color == 'white' && piece == 'B') return "\u2657";
  else if (color == 'white' && piece == 'N') return "\u2658";
  else if (color == 'white' && piece == 'P') return "\u2659";
  else if (color == 'black' && piece == 'K') return "\u265a";
  else if (color == 'black' && piece == 'Q') return "\u265b";
  else if (color == 'black' && piece == 'R') return "\u265c";
  else if (color == 'black' && piece == 'B') return "\u265d";
  else if (color == 'black' && piece == 'N') return "\u265e";
  else if (color == 'black' && piece == 'P') return "\u265f";
  else return "";
}

Number.prototype.upto = function(t) {
  var list = [];
  for (var i = this; i < t; i++) list.push(i);
  return list;
};

Number.prototype.downto = function(t) {
  var list = [];
  for (var i = this; i > t; i++) list.push(i);
  return list;
};

/* Given [color] "white"/"black", returns CSS color for rendering */
function getCssColor(color) {
  if (color == "white") return "#ddd";
  if (color == "black") return "#666";
  else return "purple";
}

function allLocations() {
  return (0).upto(8).map(rank =>
      (0).upto(8).map(file =>
          fileStr(file) + rankStr(rank)));
}

function fileStr(fileNum) { return String.fromCharCode(97 + fileNum); }

function rankStr(rankNum) { return String.fromCharCode(49 + rankNum); }

function fileOf(loc) { return loc.charCodeAt(0) - 97; }

function rankOf(loc) { return loc.charCodeAt(1) - 49; }

function getPos(loc) { return [fileOf(loc) * 100, (7 - rankOf(loc)) * 100]; }

window.ChessSquare = React.createClass({
  handleClick(event) {
    this.props.onClick(event);
  },

  render() {
    return (
      <g onClick={this.handleClick}>
        <rect width="100" height="100" fill={getCssColor(this.props.background)}></rect>
        <text style={textStyle} x="50" y="50" fontSize="50">
          {this.props.piece ? getCharacter(this.props.color, this.props.piece) : ""}
        </text>
      </g>
    );
  }
});

window.ShowMove = React.createClass({
  render() {
    return (
      <line x1={getPos(this.props.source)[0] + 50}
          y1={getPos(this.props.source)[1] + 50}
          x2={getPos(this.props.dest)[0] + 50}
          y2={getPos(this.props.dest)[1] + 50}
          stroke={this.props.color} strokeWidth="5" strokeLinecap="round" />
    );
  }
});

window.ChessBoard = React.createClass({
  getInitialState() { return {}; },

  getBackground(loc) {
    return (fileOf(loc) + rankOf(loc)) % 2 ? "white" : "black";
  },

  componentDidMount() {
    var self = this;
    $.get("/new-board", function(position) {
      self.setState(position);
    });
  },

  handleClick(loc) {
    var self = this;
    return function (event) {
      if (!self.state.selected) {
        // First click: select a source square
        var request = {
          "position": {
            "map": self.state.map,
            "toMove": self.state.toMove,
            "history": self.state.history
          },
          "source": loc
        };
        self.setState({"selected": loc, "errorMessage": null});
        $.post("/get-moves", JSON.stringify(request), function (moves) {
          self.setState({"selectedMoves": []});
          // Augment each move with additional information, and add to state
          moves.map(move => {
            var isLegalRequest = {
              "position": {
                "map": self.state.map,
                "toMove": self.state.toMove,
                "history": self.state.history
              },
              "source": move.source,
              "dest": move.dest
            };
            $.post("/is-legal", JSON.stringify(isLegalRequest), function(isLegal) {
              // TODO: Refactor into getMoveDetails(move, callback)
              var augmentedMove = {
                "source": move.source,
                "dest": move.dest,
                "isLegal": isLegal.success,
                "invalidReason": isLegal.reason
              };
              // DEBUG
              console.log(augmentedMove);
              self.setState({"selectedMoves": self.state.selectedMoves.concat([augmentedMove])});
            });
          });
        });
      } else {
        // Second click: select a dest square
        var request = {
          "position": {
            "map": self.state.map,
            "toMove": self.state.toMove,
            "history": self.state.history
          },
          "source": self.state.selected,
          "dest": loc
          /* TODO: promote */
        };
        self.setState({"selected": null, "selectedMoves": null});
        if (self.state.selectedMoves.map(move => move.dest).indexOf(loc) != -1) {
          $.post("/is-legal", JSON.stringify(request), function (isLegal) {
            if (isLegal.success) {
              $.post("/make-move", JSON.stringify(request), function (newPosition) {
                self.setState(newPosition);
              });
            } else {
              self.setState({"errorMessage": isLegal.reason});
            }
          });
        }
      }
    };
  },

  getTranslation(loc) {
    var pos = getPos(loc)
    return "translate(" + pos[0] + "," + pos[1] + ")";
  },

  render() {
    var self = this;
    return (
      <div>
        <div style={messageStyle}>
          {self.state.errorMessage ? self.state.errorMessage : ""}
        </div>
        <svg style={boardStyle}>
          {(0).upto(8).map((file, i) =>
              <text x={file * 100 + 100} y="25" style={textStyle} key={"fileLabel" + i}>
                {fileStr(file)}
              </text>)}
          {(0).upto(8).map((rank, i) =>
              <text x="25" y={700 - rank * 100 + 100} style={textStyle} key={"rankLabel" + i}>
                {rankStr(rank)}
              </text>)}
          <g transform="translate(50, 50)">
            {allLocations().map(function (row, i) {
              return (
                <g key={"rank" + i}>
                  {row.map(function (loc) {
                    if (self.state.map && self.state.map[loc]) {
                      return (
                        <g transform={self.getTranslation(loc)}
                            key={loc}>
                          <ChessSquare
                              background={self.getBackground(loc)}
                              color={self.state.map[loc][0]}
                              piece={self.state.map[loc][1]}
                              onClick={self.handleClick(loc)} />
                        </g>
                      );
                    } else {
                      return (
                        <g transform={self.getTranslation(loc)}
                            key={loc}>
                          <ChessSquare
                              background={self.getBackground(loc)}
                              key={loc}
                              onClick={self.handleClick(loc)}
                              x={getPos(loc)[0]}
                              y={getPos(loc)[1]}/>
                        </g>
                      );
                    }
                  })}
                </g>
              )
            })}
          {self.state.selected
              ? <g transform={self.getTranslation(self.state.selected)}>
                <rect width="100" height="100" style={selectedStyle} />
              </g>
              : <g></g>}
          {self.state.selectedMoves
              ? self.state.selectedMoves.map((move, i) => (
                <ShowMove key={"move" + i}
                    color={move.isLegal ? "green" : "orange"}
                    dest={move.dest}
                    source={move.source} />
              ))
              : <g></g>}
          </g>
        </svg>
      </div>
    );
  }
});
