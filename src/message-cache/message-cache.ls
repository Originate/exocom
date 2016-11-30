require! {
  'wait' : {repeat}
  'nanoseconds'
}


class MessageCache

  (cleanup-interval = 60_000) ~>
    @cleanup-interval = cleanup-interval
    @cache = {}
    repeat @cleanup-interval, ->
      now = nanoseconds process.hrtime!
      for id, timestamp of @cache when (timestamp - now) >= @cleanup-interval * 1e9
        @remove id


  push: (id, timestamp) ~>
    @cache[id] = timestamp


  remove: (id) ~>
    delete @cache[id]


  get-original-timestamp: (id) ~>
    @cache[id]


module.exports = MessageCache
