// ProcesadorComandos.java
package Explorer;

public class ProcesadorComandos {

    public void procesar(String comandosStr, Explorer explorer) {
        for (char comando : comandosStr.toCharArray()) {
            Comando accion = crearComando(comando);
            if (accion instanceof ComandoCerrarEscotillas) {
                if (explorer.isEscotillaSuperiorAbierta() || explorer.isEscotillaInferiorAbierta()) {
                    accion.ejecutar(explorer);
                } else {
                    // Skip the command if no escotillas are open
                    continue;
                }
            } else {
                accion.ejecutar(explorer);
            }
        }
    }

    private Comando crearComando(char comando) {
        switch (comando) {
            case 'f':
                return new ComandoMoverAdelante();
            case 'b':
                return new ComandoMoverAtras();
            case 'l':
                return new ComandoRotarIzquierda();
            case 'r':
                return new ComandoRotarDerecha();
            case 's':
                return new ComandoAbrirEscotillaSuperior();
            case 'i':
                return new ComandoAbrirEscotillaInferior();
            case 'c':
                return new ComandoCerrarEscotillas();
            case 'o':
                return new ComandoAbrirEscotillaInferior();
            default:
                throw new IllegalArgumentException("Comando no válido: " + comando);
        }
    }
}
