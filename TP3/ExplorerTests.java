package Explorer;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.util.Map;
import java.util.stream.IntStream;

import static Explorer.EstadoEscotilla.*;
import static org.junit.jupiter.api.Assertions.*;

public class ExplorerTests {

    @Test
    public void test01NuevoExplorerConPosicionInicial() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        assertPositionAndDirection(explorer, 0, 0, "N");
    }

    @Test
    public void test02MoverAdelanteConDireccionNorte() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        explorer.moverAdelante();
        assertPosition(explorer, 0, 1);
    }

    @Test
    public void test03MoverAtrasConDireccionSur() {
        Explorer explorer = createExplorer(0, 1, new Sur());
        explorer.moverAtras();
        assertPosition(explorer, 0, 2);
    }

    @Test
    public void test04RotarIzquierdaDesdeNorte() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        explorer.rotarIzquierda();
        assertEquals("O", explorer.getDireccion());
    }

    @Test
    public void test05RotarDerechaDesdeNorte() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        explorer.rotarDerecha();
        assertEquals("E", explorer.getDireccion());
    }

    @Test
    public void test06AbrirEscotillaSuperiorExitosamente() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        explorer.abrirEscotillaSuperior();
        assertTrue(explorer.isEscotillaSuperiorAbierta());
    }

    @Test
    public void test07NoSePuedeAbrirEscotillaSuperiorSiInferiorEstaAbierta() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        abrirEscotillaInferior(explorer);
        assertThrowsLike(NO_SE_PUEDE_ABRIR_ESCOTILLA_SUPERIOR_CON_LA_INFERIOR_ABIERTA,
                () -> explorer.abrirEscotillaSuperior());
    }

    @Test
    public void test08CerrarEscotillasExitosamente() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        abrirEscotillaSuperior(explorer);
        explorer.cerrarEscotillas();
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
    }

    @Test
    public void test09NoSePuedeCerrarSiNoHayEscotillasAbiertas() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        assertThrowsLike(NO_HAY_ESCOTILLAS_ABIERTAS_PARA_CERRAR,
                () -> explorer.cerrarEscotillas());
    }

    @Test
    public void test10ProcesarComandosMoverAdelanteYRotarIzquierda() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        procesarComandos(explorer, "fl");
        assertPositionAndDirection(explorer, 0, 1, "O");
    }

    @Test
    public void test11ComandosInvalidosLanzanExcepcion() {
        assertComandoInvalido("z", createExplorer(0, 0, new Norte()), 0, 0, "N");
        assertComandoInvalido("x", createExplorer(0, 0, new Norte()), 0, 0, "N");
    }

    @Test
    public void test12RotarCompletaDerecha() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        rotateMultipleTimes(explorer, "D", 4);
        assertEquals("N", explorer.getDireccion());
    }

    @Test
    public void test13RotarCompletaIzquierda() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        rotateMultipleTimes(explorer, "I", 4);
        assertEquals("N", explorer.getDireccion());
    }

    @Test
    public void test14NoSePuedeAbrirEscotillaInferiorSiSuperiorEstaAbierta() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        abrirEscotillaSuperior(explorer);
        assertThrowsLike(NO_SE_PUEDE_ABRIR_ESCOTILLA_INFERIOR_CON_LA_SUPERIOR_ABIERTA,
                () -> explorer.abrirEscotillaInferior());
    }

    @Test
    public void test15ProcesarComandoParaAbrirEscotillaInferior() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        procesarComandos(explorer, "o");
        assertTrue(explorer.isEscotillaInferiorAbierta());
    }

    @Test
    public void test16RotarDerechaYAvanzar() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        procesarComandos(explorer, "rf");
        assertPositionAndDirection(explorer, 1, 0, "E");
    }

    @Test
    public void test17RotarIzquierdaYAvanzar() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        procesarComandos(explorer, "lf");
        assertPositionAndDirection(explorer, -1, 0, "O");
    }

    @Test
    public void test18ProcesarComandoParaCerrarEscotillas() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        abrirEscotillaSuperior(explorer);
        procesarComandos(explorer, "c");
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
    }

    @Test
    public void test19ProcesarComandoComplejo() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        procesarComandos(explorer, "flrfr");
        assertPositionAndDirection(explorer, 0, 2, "E");
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
    }

    @Test
    public void test20NoSePuedeAspirarSiEscotillaSuperiorEstaCerrada() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        assertThrowsLike(NO_SE_PUEDE_ASPIRAR_SIN_ABRIR_LA_ESCOTILLA_SUPERIOR,
                () -> explorer.aspirar());
    }

    @Test
    public void test21NoSePuedeRecogerMuestraSiEscotillaInferiorEstaCerrada() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        assertThrowsLike(NO_SE_PUEDE_RECOGER_MUESTRA_SIN_ABRIR_LA_ESCOTILLA_INFERIOR,
                () -> explorer.recogerMuestra());
    }

    @Test
    public void test22ComandosConsecutivosInvalidosDetienenProcesamiento() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        assertComandoInvalido("xxf", explorer, 0, 0, "N");
    }

    @Test
    public void test23DireccionesActualizadasCorrectamenteDespuesDeMultiplesRotaciones() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        rotateMultipleTimes(explorer, "D", 2);
        rotateMultipleTimes(explorer, "I", 1);
        assertEquals("E", explorer.getDireccion());
    }

    @Test
    public void test24ExplorerNoSeMueveConComandosErroneosAntesDeUnoValido() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        assertComandoInvalido("xfl", explorer, 0, 0, "N");
    }

    @Test
    public void test25IntentarCerrarEscotillasCuandoAmbasEstanCerradas() {
        Explorer explorer = createExplorer(0, 0, new Norte());
        assertThrowsLike(NO_HAY_ESCOTILLAS_ABIERTAS_PARA_CERRAR,
                () -> explorer.cerrarEscotillas());
    }

    private static void assertThrowsLike(String msg, Executable executable) {
        Exception exception = assertThrows(Exception.class, executable);
        assertEquals(msg, exception.getMessage());
    }

    private static Explorer createExplorer(int x, int y, Direccion direccion) {
        return new Explorer(x, y, direccion);
    }

    private static void assertPosition(Explorer explorer, int expectedX, int expectedY) {
        assertEquals(expectedX, explorer.getX());
        assertEquals(expectedY, explorer.getY());
    }

    private static void assertPositionAndDirection(Explorer explorer, int expectedX, int expectedY, String expectedDirection) {
        assertPosition(explorer, expectedX, expectedY);
        assertEquals(expectedDirection, explorer.getDireccion());
    }

    private static void rotateMultipleTimes(Explorer explorer, String direction, int times) {
        Map<String, Runnable> actions = Map.of(
                "D", explorer::rotarDerecha,
                "I", explorer::rotarIzquierda
        );
        IntStream.range(0, times).forEach(i -> actions.get(direction).run());
    }

    private void assertComandoInvalido(String comandos, Explorer explorer, int expectedX, int expectedY, String expectedDirection) {
        assertThrowsLike("Comando no válido: " + comandos.charAt(0),
                () -> new ProcesadorComandos().procesar(comandos, explorer));
        assertPositionAndDirection(explorer, expectedX, expectedY, expectedDirection);
    }

    private static void abrirEscotillaSuperior(Explorer explorer) {
        explorer.abrirEscotillaSuperior();
    }

    private static void abrirEscotillaInferior(Explorer explorer) {
        explorer.abrirEscotillaInferior();
    }

    private static void procesarComandos(Explorer explorer, String comandos) {
        new ProcesadorComandos().procesar(comandos, explorer);
    }
}
