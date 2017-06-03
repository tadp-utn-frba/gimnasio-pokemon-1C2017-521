package gimnasioPokemno

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import gimnasioPokemon.GimnasioPokemon._
class GimnasioPokemonSuite extends FlatSpec with Matchers {

  def exampleEspecie(tipo: TipoPokemon = Fuego,
                     tipoSecundario: Option[TipoPokemon] = None,
                     resistenciaEvolutiva: Int = 350,
                     caracteristicasBase: Caracteristicas = Caracteristicas(200, 100, 30),
                     incrementosCaracteristicas: Caracteristicas = Caracteristicas(10, 5, 2)) =
    Especie(tipo, tipoSecundario, resistenciaEvolutiva, caracteristicasBase, incrementosCaracteristicas)

  def examplePokemon(
    nivel: Int = 1,
    experiencia: Int = 10,
    energia: Int = 100, energiaMaxima: Int = 200,
    fuerza: Int = 100, velocidad: Int = 30,
    especie: Especie = exampleEspecie()) = {
    Pokemon(experiencia, energia, especie)
  }
  
  import Actividades._
  
  implicit def tipoAEspecie(unTipo: TipoPokemon) = exampleEspecie(tipo = unTipo)
  implicit def tipoAEspecie(tipos: (TipoPokemon, TipoPokemon)) =
    exampleEspecie(tipo = tipos._1, tipoSecundario = Some(tipos._2))

  val unCharizard = examplePokemon(especie = (Fuego, Volador))
  val unMachop = examplePokemon(especie = Pelea)
  val unGengar = examplePokemon(especie = Fantasma)
  val unMagikarp = examplePokemon(especie = Agua)

  "cuando un pokemon descansa" should
    "su energía debería ser igual a la máxima" in {
      //Hacer descansar al charizard
      val otroCharizard = unCharizard.doDescansar().pokemon
      assert(otroCharizard.energia === 200)
    }

  "cuando un pokemon levanta pesas correctamente" should
    "gana un punto de experiencia por cada kg levantado" in {

      val otroCharizard = unCharizard.doLevantar(10).pokemon
      assert(otroCharizard.experiencia === 20)
    }

  "cuando un pokemon de pelea levanta pesas correctamente" should
    "gana dos puntos de experiencia por cada kg levantado" in {

      val resultado = Normal(unMachop)
      .realizarActividad(Actividades.levantar(10))
      assert(resultado === Normal(unMachop.copy(experiencia = 30)))
    }

  "un pokemon fantasma" should
    "no poder levantar pesas" in {

      val resultado = unGengar.doLevantar(10)
      assert(resultado == NoPuedeRealizar(unGengar))
    }

  "cuando un pokemon levanta más de lo que puede" should
    "pierde 10 de energia y no gana experiencia" in {

      val resultado = 
        Normal(unMachop).realizarActividad(levantar(3000))
      assert(resultado === Paralizado(unMachop.copy(energia = 90)))
    }

  "cuando un pokemon nada 1 minuto" should
    "pierde un punto de energia y gana 200 de experiencia" in {
      val otroMachop = unMachop.doNadar(1).pokemon

      assert(otroMachop.experiencia === 210)
      assert(otroMachop.energia === 99)
    }

  "cuando un pokemon de tipo agua nada 1 hora" should
    """pierde 60 puntos de energia y
       gana 12000 de experiencia y gana un punto de velocidad""" in {

      val otroMagikarp = unMagikarp.doNadar(60).pokemon

      assert(otroMagikarp.experiencia === 12010)
      assert(otroMagikarp.energia === 40)
      assert(otroMagikarp.velocidad === 97)
      assert(otroMagikarp.nivel === 34)
 
    }
  
  "cuando que pierde contra el agua nada" should 
    "queda en KO y no gana experiencia" in {
     
    
    val resultado = Normal(unCharizard).realizarActividad(Actividades.nadar(1))

    assert(resultado == KO(unCharizard))
  }
  
  "cuando un pokemon paralizado levanta pesas" should
    "no gana experiencia y queda KO" in {

      val resultado = Paralizado(unMachop)
      .realizarActividad(Actividades.levantar(10))
      assert(resultado === KO(unMachop))
    }
  
  "cuando un pokemon descansa, está normal y tiene menos del 50% de energia" should
    "recupera su energía y queda Dormido" in {

      val resultado = Normal(unMachop.copy(energia = 1))
        .realizarActividad(descansar)
      assert(resultado === Dormido(unMachop.copy(energia = unMachop.energiaMaxima)))
    }
  
  "un pokemon dormido" should
    "se despierta después de tres actividades" in {

      val resultado = Dormido(unMachop).realizarActividad(levantar(10))
      assert(resultado === Dormido(unMachop, 2))
      
      val resultado2 = resultado.realizarActividad(levantar(10)) 
      assert(resultado2 === Dormido(unMachop, 1))
      
      assert(resultado2.realizarActividad(levantar(10)) === Normal(unMachop))
    }
    
  
}




