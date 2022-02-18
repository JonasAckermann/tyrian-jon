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
// TODO can I make more specific errors?
case class Model(
    abv: Either[Error, Double],
    price: Either[Error, Double],
    volume: Either[Error, Double],
    jons: Either[Error, Double]
): // Make output part of model to only update on click.

  def updateAbv(newAbv: String) =
    // TODO read up on try and either
    val maybeParsed = Try(newAbv.toDouble)
    val updatedAbv =
      if (maybeParsed.isFailure)
        Left(Error.ParseError(s"Could not parse ABV: $newAbv"))
      else Right(maybeParsed.get) // is this even scala
    this.copy(abv = updatedAbv)

  def updatePrice(newPrice: String) =
    val maybeParsed = Try(newPrice.toDouble)
    val updatedPrice =
      if (maybeParsed.isFailure)
        Left(Error.ParseError(s"Could not parse Price: $newPrice"))
      else Right(maybeParsed.get) // is this even scala
    this.copy(price = updatedPrice)

  def updateVolume(newVolume: String) =
    val maybeParsed = Try(newVolume.toDouble)
    val updatedVolume =
      if (maybeParsed.isFailure)
        Left(Error.ParseError(s"Could not parse Volume: $newVolume"))
      else Right(maybeParsed.get) // is this even scala
    this.copy(volume = updatedVolume)

  def calculateJons =
    val jons: Either[Error, Double] = for {
      v <- volume
      a <- abv
      p <- price
      _ = println(s"$v, $a, $p")
      // Weirdly division by 0 does not throw
      j <-
        if (p == 0) Left(Error.CalculationError("Price can not be 0.0"))
        else
          Try((v * a * 0.01) / p).toEither.left.map(err =>
            Error.CalculationError(err.getMessage)
          )
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
