/**
 * jpf-swag: Sometimes Working Assertion Generator
 */
package edu.ucsb.cs.jpf.swag

import gov.nasa.jpf.{PropertyListenerAdapter, Config, JPF}
import gov.nasa.jpf.report._
import gov.nasa.jpf.vm._
import scala.collection.mutable.{Map => MMap, Set => MSet}
import gov.nasa.jpf.jvm.bytecode.{IfInstruction, GOTO, JVMReturnInstruction}

case class Method(name: String, cfg: MMap[Int, Set[Int]]) {
    def addEdge(from: Int, to: Int): Method = {
        copy(cfg=cfg + (from -> (cfg.getOrElse(from, Set[Int]()) + to)))
        val s = cfg.getOrElse(from, Set())
        cfg(from) = s + to
        this
    }

    def addEdge(from: Instruction, to: Instruction): Method = {
        addEdge(from.getPosition, to.getPosition)
    }
}

class CFGListener(config: Config, jpf: JPF) extends PropertyListenerAdapter with PublisherExtension {

    val methods: MMap[String, Method] = MMap()
    jpf.addPublisherExtension(classOf[ConsolePublisher], this)

    def getMethod(insn: Instruction): Method = {
        val name = insn.getMethodInfo.getLongName
        methods.get(name) match {
            case Some(m) => m
            case None =>
                val m = Method(name, MMap())
                methods(name) = m
                m
        }
    }

    override def executeInstruction(vm: VM, thread: ThreadInfo, insn: Instruction): Unit = insn match {
        case ifinsn:IfInstruction =>
            getMethod(ifinsn).addEdge(ifinsn, ifinsn.getTarget)       // then branch
                             .addEdge(ifinsn, ifinsn.getNext(thread)) // else branch
        case goto:GOTO =>
            getMethod(goto).addEdge(goto, goto.getTarget)
        case _ if insn.getNext != null =>
            getMethod(insn).addEdge(insn, insn.getNext)
        case _ => ()
    }

    override def publishFinished(publisher: Publisher): Unit = {
        val pw = publisher.getOut

        publisher.publishTopicStart("Method summaries")

        for (m <- methods.values) {
            if (m.name.contains("foo")) {
                pw.println(s"CFG for ${m.name}")
                pw.println(s"digraph '${m.name}' {")
                for (from <- m.cfg.keys; to <- m.cfg(from)) {
                    pw.println(s"$from -> $to")
                }
                pw.println("}")
            }
        }
    }
}