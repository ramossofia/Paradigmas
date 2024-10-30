package Explorer;

public class Oeste extends Direccion {
    private boolean escotillaSuperiorAbierta = false;
    private boolean escotillaInferiorAbierta = false;

    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() - 1, explorer.getY());
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX() + 1, explorer.getY());
    }

    @Override
    public Direccion rotarIzquierda() {
        return new Sur();
    }

    @Override
    public Direccion rotarDerecha() {
        return new Norte();
    }

    @Override
    public String getNombre() {
        return "O";
    }

    @Override
    public void abrirEscotillaSuperior(Explorer explorer) throws EscotillaException {
        validarAbrirEscotillaSuperior();
        escotillaSuperiorAbierta = true;
    }

    @Override
    public void abrirEscotillaInferior(Explorer explorer) throws EscotillaException {
        validarAbrirEscotillaInferior();
        escotillaInferiorAbierta = true;
    }

    @Override
    public void cerrarEscotillas(Explorer explorer) throws EscotillaException {
        validarCerrarEscotillas();
        escotillaSuperiorAbierta = false;
        escotillaInferiorAbierta = false;
    }

    @Override
    public void aspirar(Explorer explorer) throws EscotillaException {
        validarAspirar();
    }

    @Override
    public void recogerMuestra(Explorer explorer) throws EscotillaException {
        validarRecogerMuestra();
    }

    @Override
    public boolean isEscotillaSuperiorAbierta() {
        return escotillaSuperiorAbierta;
    }

    @Override
    public boolean isEscotillaInferiorAbierta() {
        return escotillaInferiorAbierta;
    }

    @Override
    protected void validarAbrirEscotillaSuperior() throws EscotillaException {
    }

    @Override
    protected void validarAbrirEscotillaInferior() throws EscotillaException {
    }

    @Override
    protected void validarCerrarEscotillas() throws EscotillaException {
    }

    @Override
    protected void validarAspirar() throws EscotillaException {
    }

    @Override
    protected void validarRecogerMuestra() throws EscotillaException {
    }
}
