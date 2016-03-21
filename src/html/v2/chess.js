var boardStyle = {
  "border": "5px solid black",
  "height": 850,
  "width": 850
};

var lineStyle = {
  "pointerEvents": "none"
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

/* Given [color] "white"/"black", returns CSS color for rendering */
function getCssColor(color) {
  if (color == "white") return "#ddd";
  if (color == "black") return "#666";
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
  return [fileOf(location) * 100, (7 - rankOf(location)) * 100];
}

window.ChessSquare = React.createClass({
  handleClick(event) {
    this.props.onClick(event);
  },

  render() {
    return (
      <g onClick={this.handleClick}>
        <rect fill={getCssColor(this.props.background)}
            height="100"
            width="100">
        </rect>
        <text style={textStyle} x="50" y="50" fontSize="50">
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
      <line x1={getPos(this.props.source)[0] + 50}
          y1={getPos(this.props.source)[1] + 50}
          x2={getPos(this.props.dest)[0] + 50}
          y2={getPos(this.props.dest)[1] + 50}
          stroke={this.props.color}
          strokeLinecap="round"
          strokeOpacity="0.5"
          strokeWidth={1000 / Math.pow(strokeLength, 1)}
          style={lineStyle} />
    );
  }
});

window.ChessBoard = React.createClass({
  getInitialState() { return { "selectedMoves": [] }; },

  getBackground(location) {
    return (fileOf(location) + rankOf(location)) % 2 ? "white" : "black";
  },

  componentDidMount() {
    var self = this;
    $.get("/new-board", function(position) {
      self.setState(position);
    });
  },

  // Given move: { source, dest }
  // Calls callback( augmentedMove: { source, dest, isLegal, [invalidReason] } )
  // TODO: Think about refactoring to take [position] as an arg. This is very
  // limiting.
  getMoveDetails(move, callback) {
    var self = this;
    var isLegalRequest = {
      "position": self.getPosition(),
      "source": move.source,
      "dest": move.dest
    };
    $.post("/is-legal", JSON.stringify(isLegalRequest), function(isLegal) {
      var augmentedMove = {
        "source": move.source,
        "dest": move.dest,
        "isLegal": isLegal.success,
        "invalidReason": isLegal.reason
      };
      callback(augmentedMove);
    });
  },

  // Given source: Location
  // Calls callback( moves: List[Move] )
  getMovesFrom(source, callback) {
    var self = this;
    var request = {
      "position": self.getPosition(),
      "source": source
    };
    $.post("/get-moves", JSON.stringify(request), (moves) => callback(moves));
  },

  // Given dest: Location
  // Calls callback( moves: List[Move] )
  getMovesTo(dest, callback) {
    var self = this;
    var request = {
      "position": self.getPosition(),
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
      "position": this.getPosition(),
      "source": move.source,
      "dest": move.dest,
      "promote": move.promote
    };
    this.getMoveDetails(request, (augmentedMove) =>
      augmentedMove.isLegal
          ?  $.post("/make-move", JSON.stringify(request), (newPosition) =>
              success(newPosition))
          : failure(augmentedMove.invalidReason)
    )
  },

  handleClick(location) {
    var self = this;
    return function (event) {
      if (!self.state.selected) {
        // First click: select a source square
        self.setState({"errorMessage": null, "selected": location});
        // Show moves
        self.getMovesFrom(location, moves =>
          moves.map((move) => self.getMoveDetails(move, (augmentedMove) =>
            self.setState(
              {"selectedMoves":
                  self.state.selectedMoves.concat([augmentedMove])})
          ))
        );
        self.getMovesTo(location, moves =>
          moves.map((move) => self.getMoveDetails(move, (augmentedMove) =>
            self.setState(
              {"selectedMoves":
                  self.state.selectedMoves.concat([augmentedMove])})
          ))
        );
      } else {
        // Second click: select a dest square
        // TODO: Here we assume that [selectedMoves] contains moves from [source]. Fix
        if (self.state.selectedMoves.map(move => move.dest).indexOf(location) != -1) {
          self.makeMove({"source": self.state.selected, "dest": location},
              (newPosition) => self.setState(newPosition),
              (invalidReason) => self.setState({"errorMessage": invalidReason}));
        }
        self.setState({"selected": null, "selectedMoves": []});
      }
    };
  },

  getTranslation(location) {
    var pos = getPos(location)
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
                  {row.map(function (location) {
                    if (self.state.map && self.state.map[location]) {
                      return (
                        <g transform={self.getTranslation(location)}
                            key={location}>
                          <ChessSquare
                              background={self.getBackground(location)}
                              color={self.state.map[location][0]}
                              piece={self.state.map[location][1]}
                              onClick={self.handleClick(location)} />
                        </g>
                      );
                    } else {
                      return (
                        <g transform={self.getTranslation(location)}
                            key={location}>
                          <ChessSquare
                              background={self.getBackground(location)}
                              key={location}
                              onClick={self.handleClick(location)}
                              x={getPos(location)[0]}
                              y={getPos(location)[1]}/>
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
