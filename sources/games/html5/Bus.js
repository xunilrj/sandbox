export default function Bus()
{
  this.subscribes = [];
  this.newSubscribe = function(f, callback) {
    this.subscribes.push({
      predicate: f,
      publish: function(x){
        callback(x);
      }
    });
  };
  this.newTopic = function(f) {
    var topicSubscribers = [];
    const topic = {
      then: function(x){
        topicSubscribers.push({
          callback: x
        });
      }
    };
    this.subscribes.push({
      predicate: f,
      publish: function(x){
        for(var i=0;i<topicSubscribers.length;++i)
        {
          topicSubscribers[i].callback(x);
        }
      }
    });
    return topic;
  };
  this.publish = function(msg) {
    const immutableMsg = new Proxy(msg, {
      get: function(target, prop) {
        return target[prop];
      }
    });
    for(var i=0;i<this.subscribes.length;++i){
      let x = this.subscribes[i];
      if(x.predicate){
        try{
          if(x.predicate(msg)) {
            x.publish(immutableMsg);
          }
        }
        catch(e){}
      }
    }
  }
}