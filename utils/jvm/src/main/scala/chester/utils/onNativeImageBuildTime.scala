package chester.utils

import org.graalvm.nativeimage.ImageInfo

inline def onNativeImageBuildTime(f: => Unit): Unit =
  if (ImageInfo.inImageBuildtimeCode) f else ()

inline def onNativeImageRunTime(f: => Unit): Unit =
  if (ImageInfo.inImageRuntimeCode) f else ()

inline def onNativeImage(f: => Unit): Unit =
  if (ImageInfo.inImageRuntimeCode || ImageInfo.inImageBuildtimeCode) f else ()

inline def ifNativeImageRunTime[A](f: => A)(g: => A): A =
  if (ImageInfo.inImageRuntimeCode) f else g
