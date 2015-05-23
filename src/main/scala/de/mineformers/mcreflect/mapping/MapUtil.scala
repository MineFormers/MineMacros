package de.mineformers.mcreflect.mapping

import net.minecraft.launchwrapper.Launch

object MapUtil {
  def srgRequired: Boolean = {
    if (Launch.blackboard != null && Launch.blackboard.containsKey("fml.deobfuscatedEnvironment"))
      Launch.blackboard.get("fml.deobfuscatedEnvironment").asInstanceOf[java.lang.Boolean].booleanValue
    else
      true
  }
}