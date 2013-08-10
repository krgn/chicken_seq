var zmq = require('zmq'),
    sock = zmq.socket('pull');

sock.connect('tcp://127.0.0.1:3000');

sock.on('message', function(msg){
  console.log('trig %s', msg.toString());
});
