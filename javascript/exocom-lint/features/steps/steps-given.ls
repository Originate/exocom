require! {
  'async'
  'cucumber': {Given}
  'fs'
  'path'
}


Given 'I am in the directory of an application with the services:', (table) ->
  @app-dir := path.join process.cwd!, 'tmp', 'app'
  @create-empty-app 'app'
  @update-app-yml (app-config) ->
    for row in table.hashes!
      app-config.services.public[row.NAME] = {location: row.LOCATION}


Given 'my services are configured with:', (table) ->
  @write-services table, @app-dir


Given 'my {string} service is configured with the message translation:', (serviceName, dataTable) ->
  @update-app-yml (app-config) ->
    app-config.services.public[serviceName].message-translation = dataTable.hashes().map (x) ->
      {'public': x.PUBLIC, 'internal': x.INTERNAL}
