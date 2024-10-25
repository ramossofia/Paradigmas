import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import static org.junit.jupiter.api.Assertions.*;

public class ExplorerTests {

    @Test public void test01NuevoExplorerConPosicionInicial() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        assertEquals(0, explorer.getX());
        assertEquals(0, explorer.getY());
        assertEquals("N", explorer.getDireccion());
    }

    @Test public void test02MoverAdelanteConDireccionNorte() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        explorer.moverAdelante();
        assertEquals(0, explorer.getX());
        assertEquals(1, explorer.getY());
    }

    @Test public void test03MoverAtrasConDireccionSur() {
        Explorer explorer = new Explorer(0, 1, new Sur());
        explorer.moverAtras();
        assertEquals(0, explorer.getX());
        assertEquals(2, explorer.getY());
    }

    @Test public void test04RotarIzquierdaDesdeNorte() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        explorer.rotarIzquierda();
        assertEquals("O", explorer.getDireccion());
    }

    @Test public void test05RotarDerechaDesdeNorte() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        explorer.rotarDerecha();
        assertEquals("E", explorer.getDireccion());
    }

    @Test public void test06AbrirEscotillaSuperiorExitosamente() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        explorer.abrirEscotillaSuperior();
        assertTrue(explorer.isEscotillaSuperiorAbierta());
    }

    @Test public void test07NoSePuedeAbrirEscotillaSuperiorSiInferiorEstaAbierta() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        explorer.abrirEscotillaInferior();
        assertThrowsLike("No se puede abrir escotilla superior con la inferior abierta",
                () -> explorer.abrirEscotillaSuperior());
    }

    @Test public void test08CerrarEscotillasExitosamente() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        explorer.abrirEscotillaSuperior();
        explorer.cerrarEscotillas();
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
    }

    @Test public void test09NoSePuedeCerrarSiNoHayEscotillasAbiertas() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        assertThrowsLike("No hay escotillas abiertas para cerrar",
                () -> explorer.cerrarEscotillas());
    }

    @Test public void test10ProcesarComandosMoverAdelanteYRotarIzquierda() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("fl", explorer);
        assertEquals(0, explorer.getX());
        assertEquals(1, explorer.getY());
        assertEquals("O", explorer.getDireccion());
    }

    @Test public void test11ComandoInvalidoLanzaExcepcion() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        assertThrowsLike("Comando no válido: x",
                () -> procesador.procesar("x", explorer));
    }

    @Test public void test12ComandoDesconocidoLanzaExcepcion() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        assertThrowsLike("Comando no válido: x",
                () -> procesador.procesar("x", explorer));
    }

    @Test public void test13RotarCompletaDerecha() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        explorer.rotarDerecha();
        explorer.rotarDerecha();
        explorer.rotarDerecha();
        explorer.rotarDerecha();
        assertEquals("N", explorer.getDireccion());
    }

    @Test public void test14RotarCompletaIzquierda() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        explorer.rotarIzquierda();
        explorer.rotarIzquierda();
        explorer.rotarIzquierda();
        explorer.rotarIzquierda();
        assertEquals("N", explorer.getDireccion());
    }

    @Test public void test15NoSePuedeAbrirEscotillaInferiorSiSuperiorEstaAbierta() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        explorer.abrirEscotillaSuperior();
        assertThrowsLike("No se puede abrir escotilla inferior con la superior abierta",
                () -> explorer.abrirEscotillaInferior());
    }

    @Test public void test16ProcesarComandoParaAbrirEscotillaInferior() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("o", explorer);
        assertTrue(explorer.isEscotillaInferiorAbierta());
    }

    @Test public void test17RotarDerechaYAvanzar() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("rf", explorer);  // Rotar derecha y avanzar
        assertEquals(1, explorer.getX());
        assertEquals(0, explorer.getY());
        assertEquals("E", explorer.getDireccion());
    }

    @Test public void test18RotarIzquierdaYAvanzar() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("lf", explorer);  // Rotar izquierda y avanzar
        assertEquals(-1, explorer.getX());
        assertEquals(0, explorer.getY());
        assertEquals("O", explorer.getDireccion());
    }

    @Test public void test19ProcesarComandoParaCerrarEscotillas() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        explorer.abrirEscotillaSuperior();
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("c", explorer);  // Cerrar escotillas
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
    }

    @Test public void test20ProcesarComandoComplejo() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("flrfc", explorer);  // Avanzar, rotar izquierda, avanzar, rotar derecha, cerrar escotillas
        assertEquals(0, explorer.getX());
        assertEquals(1, explorer.getY());
        assertEquals("E", explorer.getDireccion());
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
    }


    private static void assertThrowsLike(String msg, Executable executable) {
        Exception exception = assertThrows(Exception.class, executable);
        assertEquals(msg, exception.getMessage());
    }
}
