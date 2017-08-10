require! {
  'fs-extra' : fs
  'path'
  'js-yaml' : yaml
}



World = !->

  @create-empty-app = (app-name) ->
    @app-dir = path.join process.cwd!, 'tmp', app-name
    fs.empty-dir-sync @app-dir
    fs.write-file-sync path.join(@app-dir, 'application.yml'), """
      name: #{app-name}
      description: Empty test application
      version: 1.0.0

      dependencies:
        - type: exocom
          version: 0.24.0

      services:
        public: {}
        private: {}
      """


  @update-app-yml = (fn) ->
    app-yml-path = path.join @app-dir, 'application.yml'
    app-config = yaml.safe-load fs.read-file-sync(app-yml-path), 'utf8'
    fn app-config
    fs.write-file-sync app-yml-path, yaml.safe-dump(app-config)


  @write-services = (table, @app-dir) ->
    for row in table.hashes!
      content = "messages:"
      if row.SENDS
        content += "\n sends: "
        for message in row.SENDS.split(', ')
          content += "\n    - #{message}"
      if row.RECEIVES
        content += "\n receives: "
        for message in row.RECEIVES.split(', ')
          content += "\n    - #{message}"
      fs.mkdir-sync path.join(@app-dir, row.LOCATION)
      fs.write-file-sync path.join(@app-dir, row.LOCATION, 'service.yml'), content



module.exports = World
