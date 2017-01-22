var WebSocketServer = require('uws').Server;

function _wrapConnection (uwsConnection) {
  var connection = {
    uwsConnection: uwsConnection,
    messageQueue: [],
    onMessage: null
  };

  uwsConnection.on('message', function (message) {
    if (connection.onMessage !== null) {
      connection.messageQueue.push(message);
      connection.onMessage(connection.messageQueue.shift())();
      connection.onMessage = null;
    } else {
      connection.messageQueue.push(message);
    }
  });

  return connection;
}

exports.create = function (port) {
  return function () {
    var wss = new WebSocketServer({ port: port });

    var server = {
      server: wss,
      connectionQueue: [],
      onConnection: null
    };

    wss.on('connection', function (uwsConnection) {
      var connection = _wrapConnection(uwsConnection);
      if (server.onConnection !== null) {
        server.connectionQueue.push(connection);
        server.onConnection(server.connectionQueue.shift())();
        server.onConnection = null;
      } else {
        server.connectionQueue.push(connection);
      }
    });

    return server;
  }
};

exports.acceptConnection = function (server) {
  return function (done) {
    return function () {
      if (server.connectionQueue.length > 0) {
        done(server.connectionQueue.shift())();
      } else {
        server.onConnection = done;
      }
    }
  };
};

exports.logConnection = function (connection) {
  console.log(connection);
};

exports.logMessage = function (connection) {
  console.log(connection);
};

exports.receiveMessage = function (connection) {
  return function (done) {
    return function () {
      if (connection.messageQueue.length > 0) {
        done(connection.messageQueue.shift())();
      } else {
        connection.onMessage = done;
      }
    }
  };
};

exports.sendMessage = function (connection) {
  return function (message) {
    connection.send(message);
  };
};
