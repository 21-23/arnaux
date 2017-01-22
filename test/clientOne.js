var uws = require('uws');

var connection = new uws('ws://localhost:3000');

connection.on('error', () => { console.log('error!'); });

connection.on('open', () => {
  connection.send('{"payload":"{\\"identity\\":\\"Jenny\\"}","type":"checkin"}');
  setTimeout(() => {
    connection.send('{"payload":"{\\"cellar\\":\\"door\\"}","type":"message","destination":"Joey"}');
    connection.send('{"payload":"{\\"cellar\\":\\"door\\"}","type":"message","destination":"Erewhon"}');
  }, 5000);
});

connection.on('message', console.log);
