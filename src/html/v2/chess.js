var boardStyle = {
  "border": "5px solid black",
  "height": 800,
  "width": 800
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
  return (0).upto(8).map(function(rank) {
    var r = String.fromCharCode(49 + rank);
    return (0).upto(8).map(function(file) {
      var f = String.fromCharCode(97 + file);
      return f + r;
    });
  }).reverse();
}

function fileOf(loc) { return loc.charCodeAt(0) - 97; }

function rankOf(loc) { return loc.charCodeAt(1) - 49; }

function getBackground(loc) {
  return (fileOf(loc) + rankOf(loc)) % 2 ? "white" : "black";
}

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

window.ChessBoard = React.createClass({
  componentDidMount() {
    var self = this;
    $.get("/new-board", function(data) {
      self.setState(JSON.parse(data));
    });
  },

  handleClick(loc) {
    var self = this;
    return function (event) {
      var request = {
        "position": self.state,
        "source": loc
      };
      $.post("/get-moves", JSON.stringify(request), function (data, success) {
        console.log(data);
      });
    };
  },

  render() {
    var self = this;
    return (
      <svg style={boardStyle}>
        {allLocations().map(function (row, i) {
          return (
            <g key={"rank" + i}>
              {row.map(function (loc) {
                if (self.state && self.state.map && self.state.map[loc]) {
                  return (
                    <g transform={"translate(" + fileOf(loc) * 100 + "," + rankOf(loc) * 100 + ")"}
                        key={loc}>
                      <ChessSquare
                          background={getBackground(loc)}
                          color={self.state.map[loc][0]}
                          piece={self.state.map[loc][1]}
                          onClick={self.handleClick(loc)} />
                    </g>
                  );
                } else {
                  return (
                    <g transform={"translate(" + fileOf(loc) * 100 + "," + rankOf(loc) * 100 + ")"}
                        key={loc}>
                      <ChessSquare
                          background={getBackground(loc)}
                          key={loc}
                          onClick={self.handleClick(loc)}
                          x={fileOf(loc) * 100}
                          y={rankOf(loc) * 100}/>
                    </g>
                  );
                }
              })}
            </g>
          )
        })}
      </svg>
    );
  }
});
