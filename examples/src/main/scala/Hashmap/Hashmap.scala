package  NewHDLExample.Hashmap

import NewHDL.Core.HDLBase._

class Hashmap(reset:HDL[Boolean],clock:HDL[Boolean],set:HDL[Boolean],
               key:HDL[Unsigned],datain:HDL[Unsigned],operation:HDL[Unsigned],
               dataout:HDL[Unsigned],error:HDL[Unsigned],ready:HDL[Boolean],
               size:Int,width:Int,length:Int) extends HDLClass{

  def dualPortRam(clock:HDL[Boolean],write_enable:HDL[Boolean],
                   read_addr:HDL[Unsigned],write_addr:HDL[Unsigned],
                   datain:HDL[Unsigned],dataout:HDL[Unsigned],size:Int,width:Int):HDLBlock = {
    val tmp = HDLlize((1 to size).map(Unsigned(_, width)))

    sync(clock,0){
      when(write_enable is 1){
        tmp(write_addr) := datain
      }
      dataout := tmp(read_addr)
    }
  }

  def hashmap = module {
    val state = HDLlize(Unsigned(0,4))
    val op_cached = HDLlize(Unsigned(0,2))
    val key_cached = HDLlize(Unsigned(0,width))
    val di_cached = HDLlize(Unsigned(0,width))
    val key_di = HDLlize(Unsigned(0,width))
    val key_do = HDLlize(Unsigned(0,width))
    val value_di = HDLlize(Unsigned(0,width))
    val value_do = HDLlize(Unsigned(0,width))
    val compare = HDLlize(Unsigned(0,2))
    val num = HDLlize(Unsigned(0,length))
    val read_addr = HDLlize(Unsigned(0,length))
    val write_addr = HDLlize(Unsigned(0,length))
    val write_enable = HDLlize(b0)


    dualPortRam(clock,write_enable,read_addr,write_addr,key_di,key_do,size,width)
    dualPortRam(clock,write_enable,read_addr,write_addr,value_di,value_do,size,width)
    async{
      when (key_cached < key_do){
        compare := 1
      }.elsewhen(key_cached is key_do){
        compare := 2
      }.otherwise{
        compare := 3
      }
    }

    sync(clock,1){
      when(reset){
        num := 0
        state := 0
        ready := 1
        error := 0
      }.elsewhen(set){
        ready := 0
        error := 0
        write_addr := 0
        write_enable := 0
        op_cached := operation
        key_cached := key
        di_cached := datain
        read_addr := 1
        when(operation is 0){
          state := 1
        }.elsewhen(operation is 1){
          state := 3
        }.elsewhen(operation is 2){
          state := 5
        }.otherwise{
          state := 9
        }
      }.elsewhen(ready isnot 1){
        when (state is 1){
          when ((read_addr > num) | (compare is 1)){
            error := 1
            ready := 1
          }.elsewhen(compare is 2){
            state := 2
          }.otherwise{
            write_enable := 0
            read_addr := read_addr + 1
          }
        }.elsewhen (state is 2){
          dataout := value_do
          ready := 1
          write_enable := 0
        }.elsewhen(state is 3){
          when((compare is 1) | (read_addr > num)){
            error := 1
            ready := 1
          }.elsewhen(compare is 2){
            state := 4
            write_enable := 1
            write_addr := read_addr
            key_di := key_cached
            value_di := di_cached
          }.otherwise{
            write_enable := 0
            read_addr := read_addr + 1
          }
        }.elsewhen(state is 4){
          dataout := value_do
          ready := 1
          write_enable := 0
        }.elsewhen(state is 5){
          when(read_addr > num){
            state := 8
            write_enable := 1
            write_addr := read_addr
            key_di := key_cached
            value_di := di_cached
          }.elsewhen(compare is 1){
            state := 6
            write_enable := 1
            write_addr := read_addr
            key_di := key_cached
            value_di := di_cached
          }.elsewhen(compare is 2){
            state := 7
            write_enable := 1
            write_addr := read_addr
            key_di := key_cached
            value_di := di_cached
          }.otherwise{
            write_enable := 0
            read_addr := read_addr + 1
          }
        }.elsewhen(state is 6){
          when (read_addr is num) {
            state := 8
            write_enable := 1
            write_addr := read_addr + 1
            key_di := key_do
            value_di := value_do
          }.otherwise {
            write_enable := 1
            write_addr := read_addr + 1
            key_di := key_do
            value_di := value_do
            read_addr := read_addr + 1
          }
        }.elsewhen(state is 7){
          dataout := value_do
          ready := 1
          write_enable := 0
        }.elsewhen(state is 8){
          dataout := value_do
          num := num + 1
          ready := 1
          write_enable := 0
        }.elsewhen(state is 9){
          when ((compare is 1) |(read_addr > num)) {
            error := 1
            ready := 1
          }.elsewhen (compare is 2) {
            state := 1
            write_enable := 0
            read_addr := read_addr + 1
          }.otherwise{
            read_addr := read_addr + 1
            write_enable := 0
          }
        }.elsewhen(state is 10){
          when (read_addr > num) {
            state := 11
            write_enable := 0
          }.otherwise {
            write_enable := 1
            write_addr := read_addr - 1
            key_di := key_do
            value_di := value_do
            read_addr := read_addr + 1
          }
        }.otherwise{
          num := num - 1
          ready := 1
          write_enable := 0
        }
      }.otherwise{}
    }
  }
  override val toCompile = List(hashmap)
}


object Main{
  def main(args: Array[String]): Unit ={
    new Hashmap(b0, b0, b0, Unsigned(0,16),Unsigned(0,16),Unsigned(0,2),Unsigned(0,16),
    Unsigned(0,4),b0,256,16,8).compile.toConsole
  }
}