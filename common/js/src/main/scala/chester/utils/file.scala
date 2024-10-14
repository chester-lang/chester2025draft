package chester.utils

import typings.node.fsMod
import typings.node.bufferMod.global.BufferEncoding

def readFileFrom(path: String): String =
    fsMod.readFileSync(path, BufferEncoding.utf8)
def fileExists(path: String): Boolean = fsMod.existsSync(path)
