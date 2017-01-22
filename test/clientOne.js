var uws = require('uws');

var connection = new uws('ws://localhost:3000');

connection.on('error', () => { console.log('error!'); });

connection.on('open', () => {
  connection.send('{"payload":"{\\"name\\":\\"Jenny\\"}","type":"checkin"}');
});

connection.on('message', console.log);
