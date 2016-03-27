"use strict";

var LEFT_ARROW = 37;
var SIZE = 80;

var boardStyle = {
  "border": "5px solid black",
  "height": 8.5 * SIZE,
  "width": 8.5 * SIZE
};

var lineStyle = {
  "pointerEvents": "none"
};

var messageStyle = {
  "color": "red"
};

var selectedStyle = {
  "fillOpacity": 0,
  "stroke": "yellow",
  "strokeWidth": 5,
  "pointerEvents": "none"
};

var textStyle = {
  "alignmentBaseline": "central",
  "fontFamily": "sans-serif",
  "pointerEvents": "none",
  "textAnchor": "middle",
  "userSelect": "none",
  "WebkitUserSelect": "none"
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

/* Given [color] "white"/"black", returns CSS style for rendering */
function getCssStyle(color) {
  if      (color == "white") return {"fill": "#ddd"};
  else if (color == "black") return {"fill": "#666"};
  else if (color == "none") return {"opacity": 0};
  else return "purple";
}

function allLocations() {
  return (0).upto(8).map((rank) =>
      (0).upto(8).map((file) =>
          fileStr(file) + rankStr(rank)));
}

function fileStr(fileNum) { return String.fromCharCode(97 + fileNum); }

function rankStr(rankNum) { return String.fromCharCode(49 + rankNum); }

function fileOf(location) { return location.charCodeAt(0) - 97; }

function rankOf(location) { return location.charCodeAt(1) - 49; }

function getPos(location) {
  return [fileOf(location) * SIZE, (7 - rankOf(location)) * SIZE];
}

window.ChessSquare = React.createClass({
  handleClick(event) {
    this.props.onClick(event);
  },

  render() {
    return (
      <g onClick={this.handleClick}>
        <rect style={getCssStyle(this.props.background)}
            height={SIZE}
            width={SIZE}>
        </rect>
        <text style={textStyle} x={SIZE/2} y={SIZE/2} fontSize={SIZE/2} >
          {this.props.piece
              ? getCharacter(this.props.color, this.props.piece)
              : ""}
        </text>
      </g>
    );
  }
});

window.ShowMove = React.createClass({
  render() {
    var source = getPos(this.props.source);
    var dest = getPos(this.props.dest);
    var strokeLength = Math.sqrt(
        Math.pow(source[0] - dest[0], 2) + Math.pow(source[1] - dest[1], 2));
    return (
      <line x1={getPos(this.props.source)[0] + SIZE/2}
          y1={getPos(this.props.source)[1] + SIZE/2}
          x2={getPos(this.props.dest)[0] + SIZE/2}
          y2={getPos(this.props.dest)[1] + SIZE/2}
          stroke={this.props.color}
          strokeLinecap="round"
          strokeOpacity={this.props.opacity}
          strokeWidth={3 * SIZE / Math.pow(strokeLength, 0.5)}
          style={lineStyle} />
    );
  }
});

window.ChessBoard = React.createClass({
  getInitialState() { return { "selectedMoves": [] }; },

  getBackground(location) {
    return (fileOf(location) + rankOf(location)) % 2 ? "white" : "black";
  },

  handleHashChange() {
    $.post("/get-board-after",
        JSON.stringify(document.location.hash.slice(1).split("/")),
        (position) => {
          this.setState(position);
          this.setState({"errorMessage": null, "selected": null, "selectedMoves": []});
        }.bind(this));
  },

  componentDidMount() {
    $(document.body).on('keydown', this.handleKeyDown);
    window.onhashchange = this.handleHashChange;
    if (document.location.hash) {
      this.handleHashChange();
    } else {
      $.get("/new-board", (position) => this.setState(position));
    }
  },

  componentWillUnmount: function() {
    $(document.body).off('keydown', this.handleKeyDown);
  },

  rewind() {
    if (this.state && this.state.history && this.state.history.length > 0) {
      this.setState({
        "history": this.state.history.slice(1),
        "map": this.state.history[0].map,
        "toMove": this.state.history[0].toMove
      });
      var moves = document.location.hash.split("/");
      document.location.hash = moves.slice(0, moves.length - 1).join("/");
    }
  },

  handleKeyDown(event) {
    if (event.keyCode == LEFT_ARROW) {
      this.rewind();
      return false;
    } else {
      console.log(event);
    }
  },

  // Given move: { source, dest }
  // Calls callback( augmentedMove: { source, dest, isLegal, [invalidReason] } )
  // TODO: Think about refactoring to take [position] as an arg. This is very
  // limiting.
  getMoveDetails(move, callback) {
    var position = this.getPosition();
    var excludeReasons = ["Wrong color to move", "Can't capture same color"];
    var isLegalRequest = {
      "position": position,
      "source": move.source,
      "dest": move.dest
    };
    $.post("/is-legal", JSON.stringify(isLegalRequest), function(isLegal) {
      var destData = position.map[move.dest];
      var sourceData = position.map[move.source];
      var augmentedMove = {
        "dest": move.dest,
        "destColor": destData ? destData[0] : null,
        "destPiece": destData ? destData[1] : null,
        "invalidReason": isLegal.reason,
        "isAttack": destData ? destData[0] != sourceData[0] : false,
        "isDefense": destData ? destData[0] == sourceData[0] : false,
        "isLegal": isLegal.success || excludeReasons.indexOf(isLegal.reason) >= 0,
        "isOpen": !destData,
        "isStrictlyLegal": isLegal.success,
        "source": move.source,
        "sourceColor": sourceData[0],
        "sourcePiece": sourceData[1]
      };
      callback(augmentedMove);
    });
  },

  // Given source: Location
  // Calls callback( moves: List[Move] )
  getMovesFrom(source, callback) {
    var request = {
      "position": this.getPosition(),
      "source": source
    };
    $.post("/get-moves", JSON.stringify(request), (moves) => callback(moves));
  },

  // Given dest: Location
  // Calls callback( moves: List[Move] )
  getMovesTo(dest, callback) {
    var request = {
      "position": this.getPosition(),
      "dest": dest
    };
    $.post("/get-moves", JSON.stringify(request), (moves) => callback(moves));
  },

  // Returns a Position representing the current board state.
  getPosition() {
    return {
      "map": this.state.map,
      "toMove": this.state.toMove,
      "history": this.state.history
    };
  },

  // Given move: { source, dest, [promote] }
  // Calls success( newPosition: Position ) or failure( reason: InvalidReason )
  // TODO: Add NeedsPromotion extends InvalidReason
  makeMove(move, success, failure) {
    var request = {
      "dest": move.dest,
      "position": this.getPosition(),
      "promote": move.promote,
      "source": move.source,
    };
    this.getMoveDetails(request, (augmentedMove) =>
      augmentedMove.isStrictlyLegal
          ?  $.post("/make-move", JSON.stringify(request), (newPosition) =>
              success(newPosition))
          : failure(augmentedMove.invalidReason)
    )
  },

  handleClick(location) {
    return function (event) {
      if (!this.state.selected) {
        // First click: select a source square
        this.setState({"errorMessage": null, "selected": location});
        // Show moves
        this.getMovesTo(location, moves =>
          moves.map((move) => {
            this.getMoveDetails(move, (augmentedMove) => {
              this.setState(
                {"selectedMoves":
                    this.state.selectedMoves.concat([augmentedMove])})
            }.bind(this))
          }.bind(this))
        );
        this.getMovesFrom(location, moves =>
          moves.map((move) => {
            this.getMoveDetails(move, (augmentedMove) => {
              this.setState(
                {"selectedMoves":
                    this.state.selectedMoves.concat([augmentedMove])})
            }.bind(this))
          }.bind(this))
        );
      } else {
        // Second click: select a dest square
        var move = { "source": this.state.selected, "dest": location };
        this.getMoveDetails(move, (augmentedMove) => {
          this.makeMove(move,
            (newPosition) => {
              var piece = newPosition.map[move.dest][1];
              document.location.hash += "/" + (piece != 'P' ? piece : '') + move.source + move.dest;
              this.setState(newPosition);
            }.bind(this),
            (invalidReason) => {
              this.setState({"errorMessage": invalidReason})
            }.bind(this))
        }.bind(this));
        this.setState({"selected": null, "selectedMoves": []});
      }
    }.bind(this);
  },

  // TODO: make board resizable
  getTranslation(location) {
    var pos = getPos(location)
    return "translate(" + pos[0] + "," + pos[1] + ")";
  },

  getMoveColor(move) {
    if      (move.isOpen) return "orange";
    else if (move.isAttack && move.sourceColor == "white") return "red";
    else if (move.isAttack && move.sourceColor == "black") return "darkRed";
    else if (move.isDefense && move.sourceColor == "white") return "green";
    else if (move.isDefense && move.sourceColor == "black") return "darkGreen";
    else throw ["Uncolorable move", move];
  },

  render() {
    return (
      <div>
        <div style={messageStyle}>
          {this.state.errorMessage ? this.state.errorMessage : ""}
        </div>
        <svg style={boardStyle}>
          {(0).upto(8).map((file, i) =>
              <text x={file * SIZE + SIZE} y="25" style={textStyle} key={"fileLabel" + i}>
                {fileStr(file)}
              </text>)}
          {(0).upto(8).map((rank, i) =>
              <text x="25" y={7 * SIZE - rank * SIZE + SIZE} style={textStyle} key={"rankLabel" + i}>
                {rankStr(rank)}
              </text>)}
          <g transform={"translate(" + SIZE/2 + "," + SIZE/2 + ")"}>
            {allLocations().map((row, i) => {
              return <g key={"rankBackground" + i}>
                {row.map((location) => {
                  return <g key={"background" + location}
                      transform={this.getTranslation(location)}>
                    <ChessSquare key={"background" + location}
                        background={this.getBackground(location)}
                        onClick={this.handleClick(location)} />
                  </g>
                }.bind(this))}
              </g>
            }.bind(this))}
            {this.state.selected
                ? <g transform={this.getTranslation(this.state.selected)}>
                  <rect width={SIZE} height={SIZE} style={selectedStyle} />
                </g>
                : <g></g>}
            {this.state.selectedMoves
                ? this.state.selectedMoves.map((move, i) => {
                  return <ShowMove key={"move" + i}
                      color={this.getMoveColor(move)}
                      dest={move.dest}
                      source={move.source}
                      opacity={move.isLegal ? 0.7 : 0.2} />
                }.bind(this))
                : <g></g>}
            {allLocations().map((row, i) => {
              return (
                <g key={"rank" + i}>
                  {row.map((location) => {
                    if (this.state.map && this.state.map[location]) {
                      return (
                        <g key={location}
                            transform={this.getTranslation(location)}>
                          <ChessSquare
                              background="none"
                              color={this.state.map[location][0]}
                              piece={this.state.map[location][1]}
                              onClick={this.handleClick(location)} />
                        </g>
                      );
                    } else {
                      return (
                        <g transform={this.getTranslation(location)}
                            key={location}>
                          <ChessSquare
                              background="none"
                              key={location}
                              onClick={this.handleClick(location)}
                              x={getPos(location)[0]}
                              y={getPos(location)[1]}/>
                        </g>
                      );
                    }
                  }.bind(this))}
                </g>
              )
            }.bind(this))}
          </g>
        </svg>
      </div>
    );
  }
});
