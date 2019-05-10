import Bugout from 'bugout';
import { Elm } from './Server.elm';

const app = Elm.Server.init();
const server = new Bugout({
  seed: localStorage['wolfadex__de-slack__server-seed'],
});

localStorage['wolfadex__de-slack__server-seed'] = server.seed;
app.ports.setAddress.send(server.address());
app.ports.sendMessageToAddress.subscribe(function({ address, message }) {
  server.send(address, message);
});

server.on('connections', function(count) {
  console.log('Clients connected', count);
  app.ports.connectionCountChange.send(count);
});

server.on('seen', app.ports.clientSeen.send);

server.on('left', function(address) {
  console.log('Client left', address);
});

server.register('message', function(address, message, callback) {
  callback && callback({ placeholder: 'workAround' });
  console.log('message', message);
  app.ports.clientMessage.send({ address, message });
});
server.register('authenticate', function(address, body, callback) {
  callback && callback({});
  console.log('Authenticate', body);
  app.ports.clientAuthenticate.send({ address, body });
});
