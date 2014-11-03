package NewHDL.Simulation

import NewHDL.Core.HDLClass
import NewHDL.Core.Base
import NewHDL.Core.HDLBase._

class Waiter(block: HDLBlock) {
  def next = block.exps
}

class SyncWaiter(block: HDLSyncBlock) extends Waiter(block) {
  val reg = block.reg
  val when = block.when
}

class AsyncWaiter(block: HDLAsyncBlock) extends Waiter(block) {
  val senslist = block.senslist
}

class DelayWaiter(block: HDLDelayBlock, time: Int) extends Waiter(block) {
  val duration = block.duration
}
