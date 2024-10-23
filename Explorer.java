package Explorer;

public class Explorer {
    private int x;
    private int y;
    private String direccion;
    private boolean escotillaAbierta;
    private int[] gridLimites;
    private String ultimoComando;

    public Explorer(int i, int i1, String n) {
        this.x = 0;
        this.y = 0;
        this.direccion = "NORTE";
        this.escotillaAbierta = false;
        this.gridLimites = new int[]{0, 0, 0, 0};
        this.ultimoComando = "";
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    public String getDireccion() {
        return direccion;
    }

    public void ejecutarComando(String comando) throws ComandoInvalidoException, OperacionInvalidaException {
        this.ultimoComando = comando;

        if (comando.equals("f")) {
            moverAdelante();
        } else if (comando.equals("b")) {
            moverAtras();
        } else if (comando.equals("l")) {
            girarIzquierda();
        } else if (comando.equals("r")) {
            girarDerecha();
        } else if (comando.equals("O")) {
            if (escotillaAbierta) {
                throw new OperacionInvalidaException("La escotilla ya está abierta.");
            }
            this.escotillaAbierta = true;
        } else if (comando.equals("C")) {
            if (!escotillaAbierta) {
                throw new OperacionInvalidaException("La escotilla ya está cerrada.");
            }
            this.escotillaAbierta = false;
        } else {
            throw new ComandoInvalidoException("Comando '" + comando + "' inválido.");
        }
    }


    public boolean escotillaSuperiorAbierta() {
        return escotillaAbierta;
    }

    public void setLimiteGrid(int xMin, int xMax, int yMin, int yMax) {
        this.gridLimites = new int[]{xMin, xMax, yMin, yMax};
    }

    public void deshacerUltimoComando() throws ComandoInvalidoException, OperacionInvalidaException {
        if (ultimoComando.isEmpty()) {
            return;
        }
        if (ultimoComando.equals("f")) {
            moverAtras();
        } else if (ultimoComando.equals("b")) {
            moverAdelante();
        } else if (ultimoComando.equals("l")) {
            girarDerecha();
        } else if (ultimoComando.equals("r")) {
            girarIzquierda();
        } else if (ultimoComando.equals("O")) {
            this.escotillaAbierta = false;
        } else if (ultimoComando.equals("C")) {
            this.escotillaAbierta = true;
        } else {
            throw new ComandoInvalidoException("Comando no reconocido: " + ultimoComando);
        }
        this.ultimoComando = "";
    }


    private void moverAdelante() {
        if (escotillaAbierta) {
            throw new OperacionInvalidaException("No se puede mover con la escotilla abierta.");
        }
        if (direccion.equals("NORTE")) {
            if (y < gridLimites[3]) y++;
        } else if (direccion.equals("SUR")) {
            if (y > gridLimites[2]) y--;
        } else if (direccion.equals("ESTE")) {
            if (x < gridLimites[1]) x++;
        } else if (direccion.equals("OESTE")) {
            if (x > gridLimites[0]) x--;
        }
    }


    private void moverAtras() {
        if (escotillaAbierta) {
            throw new OperacionInvalidaException("No se puede mover con la escotilla abierta.");
        }
        if (direccion.equals("NORTE")) {
            if (y > gridLimites[2]) y--;
        } else if (direccion.equals("SUR")) {
            if (y < gridLimites[3]) y++;
        } else if (direccion.equals("ESTE")) {
            if (x > gridLimites[0]) x--;
        } else if (direccion.equals("OESTE")) {
            if (x < gridLimites[1]) x++;
        }
    }


    private void girarIzquierda() {
        if (direccion.equals("NORTE")) {
            direccion = "OESTE";
        } else if (direccion.equals("SUR")) {
            direccion = "ESTE";
        } else if (direccion.equals("ESTE")) {
            direccion = "NORTE";
        } else if (direccion.equals("OESTE")) {
            direccion = "SUR";
        }
    }


    private void girarDerecha() {
        if (direccion.equals("NORTE")) {
            direccion = "ESTE";
        } else if (direccion.equals("SUR")) {
            direccion = "OESTE";
        } else if (direccion.equals("ESTE")) {
            direccion = "SUR";
        } else if (direccion.equals("OESTE")) {
            direccion = "NORTE";
        }
    }

    public int[] getGridLimites() {
        return gridLimites;
    }

    public void setX(int i) {
        this.x = x;
    }

    public void setY(int y) {
        this.y = y;
    }
}