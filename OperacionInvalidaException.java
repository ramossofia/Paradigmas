package Explorer;

public class OperacionInvalidaException extends RuntimeException {
    public OperacionInvalidaException(String message) {
        super(message);
    }

    public class Sur extends Direccion {
        @Override
        public void moverAdelante(Explorer explorer) {
            if (explorer.getY() > explorer.getGridLimites()[2]) {
                explorer.setY(explorer.getY() - 1);
            }
        }

        @Override
        public void moverAtras(Explorer explorer) {
            if (explorer.getY() < explorer.getGridLimites()[3]) {
                explorer.setY(explorer.getY() + 1);
            }
        }

        @Override
        public Direccion girarIzquierda() {
            return new Este();
        }

        @Override
        public Direccion girarDerecha() {
            return new Oeste();
        }

        @Override
        public String getNombre() {
            return "SUR";
        }
    }
}
