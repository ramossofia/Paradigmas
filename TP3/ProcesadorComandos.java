package Explorer;

public class ProcesadorComandos {

    public void procesar(String comandosStr, Explorer explorer) {
        for (char comando : comandosStr.toCharArray()) {
            Comando accion = crearComando(comando);
            accion.ejecutar(explorer);
        }
    }

    private Comando crearComando(char comando) {
        if (comando == 'f') {
            return new ComandoMoverAdelante();
        } else if (comando == 'b') {
            return new ComandoMoverAtras();
        } else if (comando == 'l') {
            return new ComandoRotarIzquierda();
        } else if (comando == 'r') {
            return new ComandoRotarDerecha();
        } else if (comando == 's') {
            return new ComandoAbrirEscotillaSuperior();
        } else if (comando == 'i') {
            return new ComandoAbrirEscotillaInferior();
        } else if (comando == 'c') {
            return new ComandoCerrarEscotillas();
        } else {
            throw new IllegalArgumentException("Comando no reconocido: " + comando);
        }
    }
}