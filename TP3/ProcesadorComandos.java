package Explorer;

import java.util.List;
import java.util.stream.Stream;

public class ProcesadorComandos {

    private final List<Comando> comandosList = List.of(
            new MoverAtras(),
            new RotarIzquierda(),
            new RotarDerecha(),
            new AbrirEscotillaSuperior(),
            new AbrirEscotillaInferior(),
            new MoverAdelante(),
            new CerrarEscotillas()
    );

    public void procesar(String comandosStr, Explorer explorer) {
        Stream.of(comandosStr.split(""))
                .map(s -> s.charAt(0))
                .forEach(comando -> comandosList.stream()
                        .filter(cmd -> cmd.canHandle(comando))
                        .findFirst()
                        .orElseThrow(() -> new RuntimeException("Comando no válido: " + comando))
                        .ejecutar(explorer));
    }
}
