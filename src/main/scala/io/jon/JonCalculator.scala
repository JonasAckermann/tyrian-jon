package io.jon

import tyrian.Html.*
import tyrian.*

import scala.scalajs.js.annotation.*
import scala.util.Try

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
    val message = model.jons match
      case Left(Error.ParseError(msg)) =>
        div(`class` := "failresult")(s"Could not parse input: $msg")
      case Left(Error.CalculationError(msg)) =>
        div(`class` := "failresult")(s"Could not calculate jon: $msg")
      case Right(jon) =>
        div(`class` := "result")(s"Your drink has $jon jon!") // TODO truncate

    def valueOf(in: Either[Error, Double]): String = in match
      case Left(_)      => ""
      case Right(0.0)   => ""
      case Right(value) => value.toString

    div(`class` := "container")(
      div(`class` := "largetext")(
        text("🍻")
      ),
      div(
        text("A jon is a unit of efficiency in drink purchasing."),
        br,
        text("1 jon is defined as 1 ml pure alcohol per Euro.")
      ),
      div(
        input(
          placeholder := "ABV in %",
          // TODO figure out error here
          // value       := valueOf(model.abv),
          onInput(s => Msg.UpdateAbv(s))
        ),
        br,
        input(
          placeholder := "Volume in ml",
          onInput(s => Msg.UpdateVolume(s))
        ),
        br,
        input(
          placeholder := "Price in Euro",
          onInput(s => Msg.UpdatePrice(s))
        )
      ),
      message,
      div(
        button(onClick(Msg.Calculate))("Calculate!"),
        button(onClick(Msg.Clear))("Clear!")
      )
    )

  def subscriptions(model: Model): Sub[Msg] =
    Sub.Empty

enum Error(msg: String):
  case ParseError(msg: String)       extends Error(msg)
  case CalculationError(msg: String) extends Error(msg)

//TODO play around with refinement types
// TODO make units dropdown
// TODO make custom types
case class Model(
    abv: Either[Error, Double],
    price: Either[Error, Double],
    volume: Either[Error, Double],
    jons: Either[Error, Double]
): // Make output part of model to only update on click.

  private def parseToEither(in: String, label: String): Either[Error, Double] =
    Try(in.toDouble).toEither.left.map(_ =>
      Error.ParseError(s"Could not parse $label: $in")
    )

  def updateAbv(newAbv: String) =
    this.copy(abv = parseToEither(newAbv, "ABV"))

  def updatePrice(newPrice: String) =
    this.copy(price = parseToEither(newPrice, "price"))

  def updateVolume(newVolume: String) =
    this.copy(volume = parseToEither(newVolume, "volume"))

  def calculateJons =
    val jons: Either[Error, Double] = for {
      v <- volume
      a <- abv
      p <- price
      _ = println(s"$v, $a, $p")
      // Weirdly division by 0 does not throw. Can't think of any other failure, so handle this one manually
      j <-
        if (p == 0) Left(Error.CalculationError("Price can not be 0.0"))
        else Right((v * a * 0.01) / p)
    } yield j
    this.copy(jons = jons)

object Model:
  val empty: Model =
    Model(
      Right(0.0),
      Right(0.0),
      Right(0.0),
      Right(0.0)
    ).calculateJons // Need to initialize

enum Msg:
  case UpdateAbv(newAbv: String)
  case UpdatePrice(newPrice: String)
  case UpdateVolume(newVolume: String)
  case Calculate
  case Clear
