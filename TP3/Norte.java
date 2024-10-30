package Explorer;

public class Norte extends Direccion {
    private EstadoEscotilla estado;

    public Norte() {
        this.estado = new EscotillasCerradas(this);
    }

    @Override
    public void moverAdelante(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() + 1);
    }

    @Override
    public void moverAtras(Explorer explorer) {
        explorer.actualizarPosicion(explorer.getX(), explorer.getY() - 1);
    }

    @Override
    public Direccion rotarIzquierda() {
        return new Oeste();
    }

    @Override
    public Direccion rotarDerecha() {
        return new Este();
    }

    @Override
    public String getNombre() {
        return "N";
    }

    @Override
    public void abrirEscotillaSuperior(Explorer explorer) throws EscotillaException {
        estado.abrirEscotillaSuperior();
    }

    @Override
    public void abrirEscotillaInferior(Explorer explorer) throws EscotillaException {
        estado.abrirEscotillaInferior();
    }

    @Override
    public void cerrarEscotillas(Explorer explorer) throws EscotillaException {
        estado.cerrarEscotillas();
    }

    @Override
    public void aspirar(Explorer explorer) throws EscotillaException {
        estado.aspirar();
    }

    @Override
    public void recogerMuestra(Explorer explorer) throws EscotillaException {
        estado.recogerMuestra();
    }

    public void setEstado(EstadoEscotilla estado) {
        this.estado = estado;
    }

    @Override
    public boolean isEscotillaSuperiorAbierta() {
        return estado instanceof EscotillaSuperiorAbierta;
    }

    @Override
    public boolean isEscotillaInferiorAbierta() {
        return estado instanceof EscotillaInferiorAbierta;
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
