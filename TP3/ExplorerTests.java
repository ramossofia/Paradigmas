package Explorer;

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

    @Test public void test06AbrirEscotillaSuperiorExitosamente() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        try {
            explorer.abrirEscotillaSuperior();
        } catch (Exception e) {
            fail("Exception should not be thrown");
        }
        assertTrue(explorer.isEscotillaSuperiorAbierta());
    }

    @Test public void test07NoSePuedeAbrirEscotillaSuperiorSiInferiorEstaAbierta() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        try {
            explorer.abrirEscotillaInferior();
        } catch (Exception e) {
            fail("Exception should not be thrown");
        }
        assertThrowsLike("No se puede abrir escotilla superior con la inferior abierta",
                () -> explorer.abrirEscotillaSuperior());
    }

    @Test public void test15NoSePuedeAbrirEscotillaInferiorSiSuperiorEstaAbierta() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        try {
            explorer.abrirEscotillaSuperior();
        } catch (Exception e) {
            fail("Exception should not be thrown");
        }
        assertThrowsLike("No se puede abrir escotilla inferior con la superior abierta",
                () -> explorer.abrirEscotillaInferior());
    }

    @Test public void test08CerrarEscotillasExitosamente() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        try {
            explorer.abrirEscotillaSuperior();
            explorer.cerrarEscotillas();
        } catch (Exception e) {
            fail("Exception should not be thrown");
        }
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
    }

    @Test public void test09NoSePuedeCerrarSiNoHayEscotillasAbiertas() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        assertThrowsLike("No hay escotillas abiertas para cerrar",
                () -> explorer.cerrarEscotillas());
    }

    @Test public void test10ProcesarComandosMoverAdelanteYRotarIzquierda() throws Exception {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("fl", explorer);
        assertEquals(0, explorer.getX());
        assertEquals(1, explorer.getY());
        assertEquals("O", explorer.getDireccion());
    }

    @Test public void test16ProcesarComandoParaAbrirEscotillaInferior() throws Exception {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("o", explorer);
        assertTrue(explorer.isEscotillaInferiorAbierta());
    }

    @Test public void test19ProcesarComandoParaCerrarEscotillas() throws Exception {
        Explorer explorer = new Explorer(0, 0, new Norte());
        try {
            explorer.abrirEscotillaSuperior();
        } catch (Exception e) {
            fail("Exception should not be thrown");
        }
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("c", explorer);  // Cerrar escotillas
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
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

    @Test public void test17RotarDerechaYAvanzar() throws Exception {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("rf", explorer);
        assertEquals(1, explorer.getX());
        assertEquals(0, explorer.getY());
        assertEquals("E", explorer.getDireccion());
    }

    @Test public void test18RotarIzquierdaYAvanzar() throws Exception {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("lf", explorer);
        assertEquals(-1, explorer.getX());
        assertEquals(0, explorer.getY());
        assertEquals("O", explorer.getDireccion());
    }

    @Test public void test20ProcesarComandoComplejo() throws Exception {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        procesador.procesar("flrfc", explorer);
        assertEquals(0, explorer.getX());
        assertEquals(2, explorer.getY());
        assertEquals("E", explorer.getDireccion());
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
    }

    @Test public void test21NoSePuedeAspirarSiEscotillaSuperiorEstaCerrada() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        assertThrowsLike("No se puede aspirar sin abrir la escotilla superior",
                () -> explorer.aspirar());
    }

    @Test public void test22NoSePuedeRecogerMuestraSiEscotillaInferiorEstaCerrada() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        assertThrowsLike("No se puede recoger muestra sin abrir la escotilla inferior",
                () -> explorer.recogerMuestra());
    }


    @Test public void test25ComandosConsecutivosInvalidosDetienenProcesamiento() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        assertThrowsLike("Comando no válido: x",
                () -> procesador.procesar("xxf", explorer));
        assertEquals(0, explorer.getX());
        assertEquals(0, explorer.getY());
    }

    @Test public void test27DireccionesActualizadasCorrectamenteDespuesDeMultiplesRotaciones() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        explorer.rotarDerecha();
        explorer.rotarDerecha();
        explorer.rotarIzquierda();
        assertEquals("E", explorer.getDireccion());
    }

    @Test public void test28ExplorerNoSeMueveConComandosErroneosAntesDeUnoValido() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        assertThrowsLike("Comando no válido: x",
                () -> procesador.procesar("xfl", explorer));
        assertEquals(0, explorer.getX());
        assertEquals(0, explorer.getY());
    }

    @Test public void test29IntentarCerrarEscotillasCuandoAmbasEstanCerradas() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        assertThrowsLike("No hay escotillas abiertas para cerrar",
                () -> explorer.cerrarEscotillas());
    }

    @Test public void test30EstadoNoCambiaConComandosNoReconocidos() {
        Explorer explorer = new Explorer(0, 0, new Norte());
        ProcesadorComandos procesador = new ProcesadorComandos();
        assertThrowsLike("Comando no válido: x",
                () -> procesador.procesar("x", explorer));
        assertEquals(0, explorer.getX());
        assertEquals(0, explorer.getY());
        assertEquals("N", explorer.getDireccion());
    }

    private static void assertThrowsLike(String msg, Executable executable) {
        Exception exception = assertThrows(Exception.class, executable);
        assertEquals(msg, exception.getMessage());
    }
}
