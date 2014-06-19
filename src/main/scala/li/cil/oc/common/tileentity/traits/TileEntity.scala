package li.cil.oc.common.tileentity.traits

import cpw.mods.fml.common.FMLCommonHandler
import cpw.mods.fml.relauncher.{Side, SideOnly}
import li.cil.oc.OpenComputers
import li.cil.oc.client.Sound
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.network.NetworkManager
import net.minecraft.network.play.server.S35PacketUpdateTileEntity
import org.apache.logging.log4j.Level

trait TileEntity extends net.minecraft.tileentity.TileEntity {
  def world = getWorldObj

  def x = xCoord

  def y = yCoord

  def z = zCoord

  def block = getBlockType

  val isClient = FMLCommonHandler.instance.getEffectiveSide.isClient

  val isServer = FMLCommonHandler.instance.getEffectiveSide.isServer

  // ----------------------------------------------------------------------- //

  override def updateEntity() {
    super.updateEntity()
    if (world.getWorldTime % 40 == 0 && block.getLightValue(world, x, y, z) > 0) {
      world.markBlockForUpdate(x, y, z)
    }
  }

  override def validate() {
    super.validate()
    initialize()
  }

  override def invalidate() {
    super.invalidate()
    dispose()
  }

  override def onChunkUnload() {
    super.onChunkUnload()
    dispose()
  }

  protected def initialize() {}

  protected def dispose() {
    if (isClient) {
      // Note: chunk unload is handled by sound via event handler.
      Sound.stopLoop(this)
    }
  }

  // ----------------------------------------------------------------------- //

  @SideOnly(Side.CLIENT)
  def readFromNBTForClient(nbt: NBTTagCompound) {}

  def writeToNBTForClient(nbt: NBTTagCompound) {}

  // ----------------------------------------------------------------------- //

  override def getDescriptionPacket = {
    val nbt = new NBTTagCompound()
    try writeToNBTForClient(nbt) catch {
      case e: Throwable => OpenComputers.log.log(Level.WARN, "There was a problem writing a TileEntity description packet. Please report this if you see it!", e)
    }
    if (nbt.hasNoTags) null else new S35PacketUpdateTileEntity(x, y, z, -1, nbt)
  }

  override def onDataPacket(manager: NetworkManager, packet: S35PacketUpdateTileEntity) {
    try readFromNBTForClient(packet.func_148857_g()) catch {
      case e: Throwable => OpenComputers.log.log(Level.WARN, "There was a problem reading a TileEntity description packet. Please report this if you see it!", e)
    }
  }
}
