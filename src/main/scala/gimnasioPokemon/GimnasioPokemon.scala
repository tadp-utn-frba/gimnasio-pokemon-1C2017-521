package gimnasioPokemon

import scala.util.Try
import scala.util.Success
import scala.util.Failure

object GimnasioPokemon {

  case class Caracteristicas(energiaMaxima: Int, fuerza: Int, velocidad: Int) {
    def ganarVelocidad(unaVelocidad: Int) = copy(velocidad = velocidad + unaVelocidad)

    def +(c: Caracteristicas) = Caracteristicas(
      energiaMaxima + c.energiaMaxima,
      fuerza + c.fuerza,
      velocidad + c.velocidad)

    def *(n: Int) = copy(energiaMaxima * n, fuerza * n, velocidad * n)
  }

  object Caracteristicas {
    def cero = Caracteristicas(0, 0, 0)
  }

  trait Estado {
    def pokemon: Pokemon
    def realizarActividad(actividad: Actividad) = {
      actividad(this)
    }
    
    def map(f: (Pokemon => Pokemon)): Estado = ???
    def flatMap(f: (Pokemon => Estado)): Estado = ???
  }

  case class Normal(pokemon: Pokemon) extends Estado
  case class KO(pokemon: Pokemon) extends Estado {
    override def realizarActividad(actividad: Actividad) = {
      NoPuedeRealizar(pokemon)
    }
  }
  case class Paralizado(pokemon: Pokemon) extends Estado
  case class Dormido(pokemon: Pokemon, turnosRestantes:Int = 3) extends Estado {
    override def realizarActividad(actividad: Actividad) = {
      if(turnosRestantes > 1)
        Dormido(pokemon, turnosRestantes -1)
      else
        Normal(pokemon)
    }
  }
  case class NoPuedeRealizar(pokemon: Pokemon) extends Estado {
    override def realizarActividad(actividad: Actividad) = {
      NoPuedeRealizar(pokemon)
    }
  }

  implicit def pokemonAEstado(p: Pokemon) = Normal(p)
  
  type Actividad = (Estado => Estado)
  
  object Actividades {
    def levantar(peso: Int)(estado: Estado): Estado = {
      estado match {
        case Paralizado(pokemon) => KO(pokemon)
        case _ => estado.pokemon.doLevantar(peso)
      }
    }
    
    def nadar(minutos: Int)(estado: Estado): Estado = {
      estado.pokemon.doNadar(minutos)
    }
    
    def descansar : Actividad = { estado: Estado =>
      estado match {
        case Normal(p) => p.doDescansar()
        case o => o 
      }
    }
  }

  case class Pokemon(
      experiencia: Int,
      energia: Int,
      especie: Especie,
      caracteristicasEntrenadas: Caracteristicas = Caracteristicas.cero) {
    def doLevantar(peso: Int): Estado = this.especie.levantar(peso, this)
    def doDescansar(): Estado = 
      if(this.energiaBaja)
        Dormido(this.copy(energia = energiaMaxima))
      else
        this.copy(energia = energiaMaxima)
    
    def doNadar(minutos: Int): Estado = especie.nadar(minutos, this)

    def ganarExperiencia(exp: Int) = this.copy(experiencia = experiencia + exp)

    def perderEnergia(unaEnergia: Int) =
      this.copy(energia = energia - unaEnergia)

    def modificarCaracteristicasEntrenadas(modificador: Caracteristicas => Caracteristicas) =
      copy(caracteristicasEntrenadas = modificador(caracteristicasEntrenadas))

    def ganarVelocidad(unaVelocidad: Int) =
      modificarCaracteristicasEntrenadas(_.ganarVelocidad(unaVelocidad))

    def energiaMaxima = caracteristicas.energiaMaxima
    def fuerza = caracteristicas.fuerza
    def velocidad = caracteristicas.velocidad
    
    def energiaBaja = energia <= energiaMaxima * 0.5

    def caracteristicas =
      especie.caracteristicasParaNivel(nivel) + caracteristicasEntrenadas

    lazy val nivel = especie.nivelPara(experiencia)

  }

  trait TipoPokemon {
    def esDebilContra(t: TipoPokemon) = debilidades.contains(t)
    def debilidades: List[TipoPokemon] = List()
  }
  case object Fuego extends TipoPokemon {
    override def debilidades = List(Agua)
  }
  case object Volador extends TipoPokemon
  case object Pelea extends TipoPokemon
  case object Fantasma extends TipoPokemon
  case object Agua extends TipoPokemon

  case object Tierra extends TipoPokemon {
    override def debilidades = List(Agua)
  }

  case object Roca extends TipoPokemon {
    override def debilidades = List(Agua)
  }

  case class AlgunTipoEs(tipo: TipoPokemon) {
    def unapply(tipos: (TipoPokemon, Option[TipoPokemon])) =
      tipos._1 == tipo || tipos._2 == Some(tipo)
  }

  class NoPuedeRealizarActividadException extends RuntimeException

  //Ejemplo: val charizard = Especie(Fuego, Some(Volador))
  case class Especie(
      tipoPrincipal: TipoPokemon,
      tipoSecundario: Option[TipoPokemon] = None,
      resistenciaEvolutiva: Int,
      caracteristicasBase: Caracteristicas,
      incrementoCaracteristicas: Caracteristicas) {
    def tipos = (tipoPrincipal, tipoSecundario)

    val algunoEsFantasma = AlgunTipoEs(Fantasma)
    val algunoEsPelea = AlgunTipoEs(Pelea)
    val algunoEsAgua = AlgunTipoEs(Agua)

    def algunTipoEs(tipo: TipoPokemon) = {
      tipoPrincipal == tipo || tipoSecundario.contains(tipo)
    }

    def levantar(peso: Int, pokemon: Pokemon): Estado = tipos match {
      case algunoEsFantasma() =>
        NoPuedeRealizar(pokemon)
      case _ if peso > pokemon.fuerza * 10 => Paralizado(pokemon.perderEnergia(10))
      case algunoEsPelea() =>
        pokemon.ganarExperiencia(peso * 2)
      case _ => pokemon.ganarExperiencia(peso)
    }

    def nadar(minutos: Int, pokemon: Pokemon): Estado = {
      val nuevoPokemon = pokemon.perderEnergia(minutos).ganarExperiencia(200 * minutos)

      tipos match {
        case _ if esDebilContra(Agua) => KO(pokemon)
        case algunoEsAgua()           => nuevoPokemon.ganarVelocidad(minutos / 60)
        case _                        => nuevoPokemon
      }
    }

    def esDebilContra(t: TipoPokemon) = {
      tipoPrincipal.esDebilContra(t) || tipoSecundario.exists(_.esDebilContra(t))
    }

    def caracteristicasParaNivel(nivel: Int) = {
      caracteristicasBase +
        incrementoCaracteristicas * (nivel - 1)
    }

    def nivelPara(experiencia: Int) = (experiencia / resistenciaEvolutiva).max(1).min(100)
  }

}