lazy val jsForLua = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Full)
  .in(file("js-for-lua"))
  .settings(
    commonSettings,
    name := "js-for-lua"
  )
  .jsConfigure(_.dependsOn(utils.js))
  .jsSettings(
    scalaJSLinkerConfig ~= {
      // Enable ECMAScript module output
      _.withModuleKind(ModuleKind.ESModule)
        // Use .mjs extension
        .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
    }
  ) 