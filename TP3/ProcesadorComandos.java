package Explorer;

import java.util.HashMap;
import java.util.Map;

public class ProcesadorComandos {

    private static final Map<Character, Comando> COMANDOS = new HashMap<>();

    // Static block to initialize commands
    static {
        // Define comandos válidos
        COMANDOS.put('f', new ComandoMoverAdelante());
        COMANDOS.put('b', new ComandoMoverAtras());
        COMANDOS.put('l', new ComandoRotarIzquierda());
        COMANDOS.put('r', new ComandoRotarDerecha());
        COMANDOS.put('O', new ComandoAbrirEscotillaSuperior());
        COMANDOS.put('o', new ComandoAbrirEscotillaInferior());
        COMANDOS.put('c', new ComandoCerrarEscotillas());

        // Define `ComandoNoValido` para caracteres no mapeados
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
