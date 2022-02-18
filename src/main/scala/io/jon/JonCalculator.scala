package io.jon

import tyrian.Html.*
import tyrian.*

import scala.scalajs.js.annotation.*

@JSExportTopLevel("TyrianApp")
object JonCalculator extends TyrianApp[Msg, Model]:

  def init(flags: Map[String, String]): (Model, Cmd[Msg]) =
    (Model.empty, Cmd.Empty)

  def update(msg: Msg, model: Model): (Model, Cmd[Msg]) = msg match
    case Msg.UpdateAbv(newAbv)     => (model.updateAbv(newAbv), Cmd.Empty)
    case Msg.UpdatePrice(newPrice) => (model.updatePrice(newPrice), Cmd.Empty)
    case Msg.UpdateVolume(newVolume) =>
      (model.updateVolume(newVolume), Cmd.Empty)
    case Msg.Calculate => (model.calculateJons, Cmd.Empty)
    case Msg.Clear     => (Model.empty, Cmd.Empty) // TODO also clear view

  def view(model: Model): Html[Msg] =
    div(
      // TODO line breaks without divs?
      div(
        "A jon is a unit of efficiency in drink purchasing."
      ),
      div(
        "1 jon is defined as 1 ml pure alcohol per Euro"
      ),
      div(
        input(
          placeholder := "ABV in %",
          onInput(s => Msg.UpdateAbv(s.toDouble))
        )
      ),
      div(
        input(
          placeholder := "Volume in ml",
          onInput(s => Msg.UpdateVolume(s.toDouble))
        )
      ),
      div(
        input(
          placeholder := "Price in Euro",
          onInput(s => Msg.UpdatePrice(s.toDouble))
        )
      ),
      // TODO truncate
      // TODO only show if non-optional
      div(s"Your drink has ${model.jons.toString} jon!"),
      button(onClick(Msg.Calculate))("Calculate!"),
      button(onClick(Msg.Clear))("Clear!")
    )

  def subscriptions(model: Model): Sub[Msg] =
    Sub.Empty

//TODO play around with refinement types
// TODO make units dropdown
// TODO make custom types
case class Model(
    abv: Double,
    price: Double,
    volume: Double,
    jons: Double
): // Make output part of model to only update on click. TODO smart?
  def updateAbv(newAbv: Double)       = this.copy(abv = newAbv)
  def updatePrice(newPrice: Double)   = this.copy(price = newPrice)
  def updateVolume(newVolume: Double) = this.copy(volume = newVolume)
  def calculateJons =
    this.copy(jons = (volume * abv * 0.01) / price) // TODO this sucks

object Model:
  // TODO make empty model use optional
  val empty: Model = Model(0.0, 0.0, 0.0, 0.0)

// TODO Enum for case classes?
// TODO errors
enum Msg:
  case UpdateAbv(newAbv: Double)
  case UpdatePrice(newPrice: Double)
  case UpdateVolume(newVolume: Double)
  case Calculate
  case Clear
