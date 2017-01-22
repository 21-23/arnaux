var uws = require('uws');

var connection = new uws('ws://localhost:3000');

connection.on('error', () => { console.log('error!'); });

connection.on('open', () => {
  connection.send('{"payload":"{\\"name\\":\\"Joey\\"}","type":"checkin"}');
  setTimeout(() => {
    connection.send('{"payload":"{\\"door\\":\\"cellar\\"}","type":"message","destination":"Jenny"}');
    connection.send('{"payload":"{\\"cellar\\":\\"door\\"}","type":"message","destination":"Erewhon"}');
  }, 5000);
});

connection.on('message', console.log);
