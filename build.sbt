addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.14.0",
  "com.lihaoyi" %% "ammonite-ops" % "1.5.0",
  "com.lihaoyi" % "ammonite" % "1.5.0" cross CrossVersion.full,
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scalaz" %% "scalaz-core" % "7.2.27"
  )

import scalariform.formatter.preferences._

scalariformPreferences := scalariformPreferences.value
      .setPreference(AlignParameters, true)
      .setPreference(AlignArguments, true)
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
      .setPreference(SpaceInsideParentheses, true)
      .setPreference(SpacesWithinPatternBinders, true)
      .setPreference(SpacesAroundMultiImports, true)
      .setPreference(DanglingCloseParenthesis, Preserve)
      .setPreference(DoubleIndentConstructorArguments, true)

