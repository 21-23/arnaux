var WebSocketServer = require('uws').Server;

function _wrapConnection (uwsConnection) {
  var connection = {
    uwsConnection: uwsConnection,
    messageQueue: [],
    onMessage: null,
    onClose: null
  };

  uwsConnection.on('message', function (message) {
    if (connection.onMessage !== null) {
      connection.messageQueue.push(message);
      var messageHandler = connection.onMessage;
      connection.onMessage = null;
      messageHandler(connection.messageQueue.shift())();
    } else {
      connection.messageQueue.push(message);
    }
  });

  uwsConnection.on('close', function () {
    if (connection.onClose !== null) {
      connection.onClose(connection)();
      connection.onClose = null;
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
        var connectionHandler = server.onConnection;
        server.onConnection = null;
        connectionHandler(server.connectionQueue.shift())();
      } else {
        server.connectionQueue.push(connection);
      }
    });

    return server;
  };
};

exports.acceptConnection = function (server) {
  return function (done) {
    return function () {
      if (server.connectionQueue.length > 0) {
        done(server.connectionQueue.shift())();
      } else {
        server.onConnection = done;
      }
    };
  };
};

exports.receiveMessage = function (connection) {
  return function (done) {
    return function () {
      if (connection.messageQueue.length > 0) {
        done(connection.messageQueue.shift())();
      } else {
        connection.onMessage = done;
      }
    };
  };
};

exports.closeConnection = function (connection) {
  return function (done) {
    return function () {
      connection.onClose = done;
    };
  };
};

exports.sendMessage = function (connection) {
  return function (message) {
    return function () {
      connection.uwsConnection.send(message);
    };
  };
};

exports.connectionEq = function (connectionA) {
  return function (connectionB) {
    return connectionA === connectionB;
  };
};
