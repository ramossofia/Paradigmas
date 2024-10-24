public class ProcesadorComandos {

    public void procesar(String comandos, Explorer explorer) {
        for (char comando : comandos.toCharArray()) {
            switch (comando) {
                case 'f':  // Avanzar
                    explorer.moverAdelante();
                    break;
                case 'b':  // Retroceder
                    explorer.moverAtras();
                    break;
                case 'l':  // Rotar a la izquierda
                    explorer.rotarIzquierda();
                    break;
                case 'r':  // Rotar a la derecha
                    explorer.rotarDerecha();
                    break;
                case 's':  // Abrir escotilla superior
                    try {
                        explorer.abrirEscotillaSuperior();
                    } catch (IllegalStateException e) {
                        System.out.println("Error: " + e.getMessage());
                    }
                    break;
                case 'i':  // Abrir escotilla inferior
                    try {
                        explorer.abrirEscotillaInferior();
                    } catch (IllegalStateException e) {
                        System.out.println("Error: " + e.getMessage());
                    }
                    break;
                case 'c':  // Cerrar escotillas
                    explorer.cerrarEscotillas();
                    break;
                default:
                    throw new IllegalArgumentException("Comando no reconocido: " + comando);
            }
        }
    }
}
