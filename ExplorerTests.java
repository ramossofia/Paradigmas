package Explorer;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class ExplorerTests {

    // 1. Estado inicial
    @Test
    public void test01ElExplorerEmpiezaEnPosicionYOrientacionInicial() {
        Explorer explorer = new Explorer(0, 0, "N");
        assertEquals(0, explorer.getX());
        assertEquals(0, explorer.getY());
        assertEquals("N", explorer.getDireccion());
    }

    // 2. Comandos de movimiento
    @Test
    public void test02ExplorerSeMueveAdelanteApuntandoAlNorte() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("f");
        assertEquals(0, explorer.getX());
        assertEquals(1, explorer.getY());
    }

    @Test
    public void test03ExplorerSeMueveAtrasApuntandoAlNorte() {
        Explorer explorer = new Explorer(0, 1, "N");
        explorer.ejecutarComando("b");
        assertEquals(0, explorer.getX());
        assertEquals(0, explorer.getY());
    }

    @Test
    public void test04ExplorerSeMueveAdelanteApuntandoAlEste() {
        Explorer explorer = new Explorer(0, 0, "E");
        explorer.ejecutarComando("f");
        assertEquals(1, explorer.getX());
        assertEquals(0, explorer.getY());
    }

    // 3. Comandos de rotación
    @Test
    public void test05ExplorerGiraALaIzquierdaDeNorteAHaciaOeste() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("l");
        assertEquals("O", explorer.getDireccion());
    }

    @Test
    public void test06ExplorerGiraALaDerechaDeNorteAHaciaEste() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("r");
        assertEquals("E", explorer.getDireccion());
    }

    // 4. Operaciones de escotilla
    @Test
    public void test07AbrirEscotillaSuperior() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("O");
        assertTrue(explorer.escotillaSuperiorAbierta());
    }

    @Test
    public void test08NoSePuedeAbrirEscotillaInferiorSiLaSuperiorEstaAbierta() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("O");
        assertThrows(ComandoInvalidoException.class, () -> {
            explorer.ejecutarComando("o");
        });
    }

    @Test
    public void test09CerrarEscotillaSuperior() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("O");
        explorer.ejecutarComando("c");
        assertFalse(explorer.escotillaSuperiorAbierta());
    }

    @Test
    public void test10NoSePuedeAspirarSiEscotillaSuperiorCerrada() {
        Explorer explorer = new Explorer(0, 0, "N");
        assertThrows(OperacionInvalidaException.class, () -> {
            explorer.ejecutarComando("a");
        });
    }

    @Test
    public void test11NoSePuedeRecogerMuestraSiEscotillaInferiorCerrada() {
        Explorer explorer = new Explorer(0, 0, "N");
        assertThrows(OperacionInvalidaException.class, () -> {
            explorer.ejecutarComando("i");
        });
    }

    // 5. Comandos inválidos
    @Test
    public void test12ComandoInvalidoDetieneElProcesamiento() {
        Explorer explorer = new Explorer(0, 0, "N");
        assertThrows(ComandoInvalidoException.class, () -> {
            explorer.ejecutarComando("x");
        });
    }

    // 6. Casos de borde y secuencia múltiple
    @Test
    public void test13VariosComandosValidosSeProcesanCorrectamente() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("ffrflf");
        assertEquals(1, explorer.getX());
        assertEquals(3, explorer.getY());
        assertEquals("O", explorer.getDireccion());
    }

    @Test
    public void test14ExplorerNoSeMueveFueraDelLimite() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.setLimiteGrid(0, 0, 10, 10);  // Definir límites de la cuadrícula
        explorer.ejecutarComando("f");
        explorer.ejecutarComando("b");
        assertEquals(0, explorer.getY());  // No debe ir más allá del límite negativo
    }

    @Test
    public void test15SecuenciaDeComandoDetenidaPorComandoInvalido() {
        Explorer explorer = new Explorer(0, 0, "N");
        assertThrows(ComandoInvalidoException.class, () -> {
            explorer.ejecutarComando("ffxl");
        });
        assertEquals(0, explorer.getX());
        assertEquals(2, explorer.getY());  // Solo los primeros dos 'f' se ejecutan
    }

    @Test
    public void test16MovimientoYComandosDeEscotillaEnSecuencia() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("fO");
        assertEquals(1, explorer.getY());  // Se mueve hacia adelante
        assertTrue(explorer.escotillaSuperiorAbierta());  // Escotilla superior abierta
    }

    @Test
    public void test17ExplorerManejaLargaSecuenciaDeComandos() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("ffffffffffffffffffffffffffffffffffffffffffffffff");
        assertEquals(40, explorer.getY());  // Mueve hacia adelante 40 veces
    }

    @Test
    public void test18CerrarEscotillaYaCerradaLanzaExcepcion() {
        Explorer explorer = new Explorer(0, 0, "N");
        assertThrows(OperacionInvalidaException.class, () -> {
            explorer.ejecutarComando("c");
        });
    }

    @Test
    public void test19VariosComandosAdelante() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("fff");
        assertEquals(3, explorer.getY());
    }

    @Test
    public void test20ExplorerGiraEnUnCírculoCompleto() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("rrrr");  // Gira 360 grados
        assertEquals("N", explorer.getDireccion());
    }

    @Test
    public void test21MovimientoBloqueadoSiEscotillaSuperiorAbierta() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("O");
        assertThrows(OperacionInvalidaException.class, () -> {
            explorer.ejecutarComando("f");
        });
    }

    @Test
    public void test22DeshacerUltimoComando() {
        Explorer explorer = new Explorer(0, 0, "N");
        explorer.ejecutarComando("f");
        explorer.deshacerUltimoComando();
        assertEquals(0, explorer.getY());  // Vuelve a la posición original
    }

    @Test
    public void test23MensajeDeErrorCorrectoParaComandoInvalido() {
        Explorer explorer = new Explorer(0, 0, "N");
        Exception exception = assertThrows(ComandoInvalidoException.class, () -> {
            explorer.ejecutarComando("x");
        });
        assertEquals("Comando 'x' inválido.", exception.getMessage());
    }
}

