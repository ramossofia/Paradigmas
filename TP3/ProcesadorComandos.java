package Explorer;

import java.util.HashMap;
import java.util.Map;

public class ProcesadorComandos {

    private static final Map<Character, Comando> COMANDOS;

    // Static block to initialize commands
    static {
        COMANDOS = new HashMap<>(Map.of(
                'f', new ComandoMoverAdelante(),
                'b', new ComandoMoverAtras(),
                'l', new ComandoRotarIzquierda(),
                'r', new ComandoRotarDerecha(),
                'O', new ComandoAbrirEscotillaSuperior(),
                'o', new ComandoAbrirEscotillaInferior(),
                'c', new ComandoCerrarEscotillas()
        ));

        for (char c = 0; c < 128; c++) {
            COMANDOS.putIfAbsent(c, new ComandoNoValido(c));
        }
    }

    public void procesar(String comandosStr, Explorer explorer) throws Exception {
        for (char comando : comandosStr.toCharArray()) {
            COMANDOS.get(comando).ejecutar(explorer);
        }
    }
}
