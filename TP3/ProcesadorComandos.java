package Explorer;

import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

public class ProcesadorComandos {

    private final Map<Character, Comando> comandosMap = Map.of(
            'f', new ComandoMoverAdelante(),
            'b', new ComandoMoverAtras(),
            'l', new ComandoRotarIzquierda(),
            'r', new ComandoRotarDerecha(),
            'O', new ComandoAbrirEscotillaSuperior(),
            'o', new ComandoAbrirEscotillaInferior(),
            'c', new ComandoCerrarEscotillas()
    );

    public void procesar(String comandosStr, Explorer explorer) {
        Stream.of(comandosStr.split(""))
                .map(s -> s.charAt(0))
                .forEach(comando -> Optional.ofNullable(comandosMap.get(comando))
                        .orElse(new ComandoNoValido(comando))
                        .ejecutar(explorer));
    }
}
