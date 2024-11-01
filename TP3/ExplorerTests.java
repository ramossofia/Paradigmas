package Explorer;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;

public class ExplorerTests {

    @Test
    public void test01NuevoExplorerConPosicionInicial() {
        Explorer explorer = createExplorer();
        assertPositionAndDirection(explorer, 0, 0, "N");
    }

    @Test
    public void test02MoverAdelanteConDireccionNorte() {
        Explorer explorer = createExplorer();
        explorer.moverAdelante();
        assertPosition(explorer, 0, 1);
    }

    @Test
    public void test03MoverAtrasConDireccionSur() {
        Explorer explorer = createExplorerWithDirection(new Sur());
        explorer.moverAtras();
        assertPosition(explorer, 0, 2);
    }

    @Test
    public void test04RotarIzquierdaDesdeNorte() {
        Explorer explorer = createExplorer();
        explorer.rotarIzquierda();
        assertEquals("O", explorer.getDireccion());
    }

    @Test
    public void test05RotarDerechaDesdeNorte() {
        Explorer explorer = createExplorer();
        explorer.rotarDerecha();
        assertEquals("E", explorer.getDireccion());
    }

    @Test
    public void test06AbrirEscotillaSuperiorExitosamente() {
        Explorer explorer = createExplorer();
        explorer.abrirEscotillaSuperior();
        assertTrue(explorer.isEscotillaSuperiorAbierta());
    }

    @Test
    public void test07NoSePuedeAbrirEscotillaSuperiorSiInferiorEstaAbierta() {
        Explorer explorer = createExplorer();
        abrirEscotillaInferior(explorer);
        assertThrowsLike("No se puede abrir escotilla superior con la inferior abierta",
                () -> explorer.abrirEscotillaSuperior());
    }

    @Test
    public void test08CerrarEscotillasExitosamente() {
        Explorer explorer = createExplorer();
        abrirEscotillaSuperior(explorer);
        explorer.cerrarEscotillas();
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
    }

    @Test
    public void test09NoSePuedeCerrarSiNoHayEscotillasAbiertas() {
        Explorer explorer = createExplorer();
        assertThrowsLike("No hay escotillas abiertas para cerrar",
                () -> explorer.cerrarEscotillas());
    }

    @Test
    public void test10ProcesarComandosMoverAdelanteYRotarIzquierda() {
        Explorer explorer = createExplorer();
        procesarComandos(explorer, "fl");
        assertPositionAndDirection(explorer, 0, 1, "O");
    }

    @Test
    public void test11ComandoInvalidoLanzaExcepcion() {
        assertComandoInvalido("x", createExplorer());
    }

    @Test
    public void test12ComandoDesconocidoLanzaExcepcion() {
        assertComandoInvalido("x", createExplorer());
    }

    @Test
    public void test13RotarCompletaDerecha() {
        Explorer explorer = createExplorer();
        rotateMultipleTimes(explorer, "D", 4);
        assertEquals("N", explorer.getDireccion());
    }

    @Test
    public void test14RotarCompletaIzquierda() {
        Explorer explorer = createExplorer();
        rotateMultipleTimes(explorer, "I", 4);
        assertEquals("N", explorer.getDireccion());
    }

    @Test
    public void test15NoSePuedeAbrirEscotillaInferiorSiSuperiorEstaAbierta() {
        Explorer explorer = createExplorer();
        abrirEscotillaSuperior(explorer);
        assertThrowsLike("No se puede abrir escotilla inferior con la superior abierta",
                () -> explorer.abrirEscotillaInferior());
    }

    @Test
    public void test16ProcesarComandoParaAbrirEscotillaInferior() {
        Explorer explorer = createExplorer();
        procesarComandos(explorer, "o");
        assertTrue(explorer.isEscotillaInferiorAbierta());
    }

    @Test
    public void test17RotarDerechaYAvanzar() {
        Explorer explorer = createExplorer();
        procesarComandos(explorer, "rf");
        assertPositionAndDirection(explorer, 1, 0, "E");
    }

    @Test
    public void test18RotarIzquierdaYAvanzar() {
        Explorer explorer = createExplorer();
        procesarComandos(explorer, "lf");
        assertPositionAndDirection(explorer, -1, 0, "O");
    }

    @Test
    public void test19ProcesarComandoParaCerrarEscotillas() {
        Explorer explorer = createExplorer();
        abrirEscotillaSuperior(explorer);
        procesarComandos(explorer, "c");
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
    }

    @Test
    public void test20ProcesarComandoComplejo() {
        Explorer explorer = createExplorer();
        procesarComandos(explorer, "flrfr");
        assertPositionAndDirection(explorer, 0, 2, "E");
        assertFalse(explorer.isEscotillaSuperiorAbierta());
        assertFalse(explorer.isEscotillaInferiorAbierta());
    }

    @Test
    public void test21NoSePuedeAspirarSiEscotillaSuperiorEstaCerrada() {
        Explorer explorer = createExplorer();
        assertThrowsLike("No se puede aspirar sin abrir la escotilla superior",
                () -> explorer.aspirar());
    }

    @Test
    public void test22NoSePuedeRecogerMuestraSiEscotillaInferiorEstaCerrada() {
        Explorer explorer = createExplorer();
        assertThrowsLike("No se puede recoger muestra sin abrir la escotilla inferior",
                () -> explorer.recogerMuestra());
    }

    @Test
    public void test23ComandosConsecutivosInvalidosDetienenProcesamiento() {
        Explorer explorer = createExplorer();
        assertThrowsLike("Comando no válido: x",
                () -> procesarComandos(explorer, "xxf"));
        assertPosition(explorer, 0, 0);
    }

    @Test
    public void test24DireccionesActualizadasCorrectamenteDespuesDeMultiplesRotaciones() {
        Explorer explorer = createExplorer();
        rotateMultipleTimes(explorer, "D", 2);
        rotateMultipleTimes(explorer, "I", 1);
        assertEquals("E", explorer.getDireccion());
    }

    @Test
    public void test25ExplorerNoSeMueveConComandosErroneosAntesDeUnoValido() {
        Explorer explorer = createExplorer();
        assertThrowsLike("Comando no válido: x",
                () -> procesarComandos(explorer, "xfl"));
        assertPositionAndDirection(explorer, 0, 0, "N");
    }

    @Test
    public void test26IntentarCerrarEscotillasCuandoAmbasEstanCerradas() {
        Explorer explorer = createExplorer();
        assertThrowsLike("No hay escotillas abiertas para cerrar",
                () -> explorer.cerrarEscotillas());
    }

    @Test
    public void test27EstadoNoCambiaConComandosNoReconocidos() {
        Explorer explorer = createExplorer();
        assertComandoInvalido("x", explorer);
        assertPositionAndDirection(explorer, 0, 0, "N");
    }

    private static void assertThrowsLike(String msg, Executable executable) {
        Exception exception = assertThrows(Exception.class, executable);
        assertEquals(msg, exception.getMessage());
    }

    private static Explorer createExplorer() {
        return new Explorer(0, 0, new Norte());
    }

    private static Explorer createExplorerWithDirection(Direccion direccion) {
        return new Explorer(0, 1, direccion);
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
        Runnable rotationAction = direction.equals("D")
                ? explorer::rotarDerecha
                : explorer::rotarIzquierda;

        IntStream.range(0, times).forEach(i -> rotationAction.run());
    }

    private static void assertComandoInvalido(String comando, Explorer explorer) {
        assertThrowsLike("Comando no válido: " + comando,
                () -> new ProcesadorComandos().procesar(comando, explorer));
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
