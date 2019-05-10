import Bugout from 'bugout';
import { Elm } from './Client.elm';

const app = Elm.Client.init();
let client;

app.ports.connectToServer.subscribe(function(serverAddress) {
  client = new Bugout(serverAddress);
  client.on('server', function() {
    console.log('Connected to the server');
    app.ports.connectedToServer.send(null);
    app.ports.sendMessage.subscribe(function(args) {
      client.rpc('message', args, function() {});
    });
    app.ports.sendAuth.subscribe(function(args) {
      console.log('Authenticate', args);
      client.rpc('authenticate', args, function() {});
    });
  });
  client.on('message', function(address, message, packet) {
    console.log('Message', address, message, packet);
    app.ports.serverMessage.send(message);
  });
  client.on('timeout', function(address) {
    console.log('Server timeout', address);
  });
});
