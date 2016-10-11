require! {
  'wait' : {repeat}
}


class MessageCache

  (cleanup-interval = 60_000) ~>
    @cleanup-interval = cleanup-interval
    @cache = {}
    repeat @cleanup-interval, ->
      now = Date.now!
      for id, timestamp of @cache when (timestamp - now) >= @cleanup-interval
        @remove id


  push: (id, timestamp) ~>
    @cache[id] = timestamp


  remove: (id) ~>
    delete @cache[id]


  get-original-timestamp: (id) ~>
    @cache[id]


module.exports = MessageCache
