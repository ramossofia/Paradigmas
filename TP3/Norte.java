package Explorer;

public class Norte extends Direccion {
    private EstadoEscotilla estado;

    public Norte() {
        this.estado = new EscotillasCerradas(this);
    }

    public void setEstado(EstadoEscotilla estado) {
        this.estado = estado;
    }

    public EstadoEscotilla getEstado() {
        return estado;
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
    public void abrirEscotillaInferior(Explorer explorer) {
        estado.abrirEscotillaInferior();
    }

    @Override
    public void abrirEscotillaSuperior(Explorer explorer) {
        estado.abrirEscotillaSuperior();
    }

    @Override
    public void cerrarEscotillas(Explorer explorer) {
        estado.cerrarEscotillas();
    }

    @Override
    public void aspirar(Explorer explorer) {
        estado.aspirar();
    }

    @Override
    public void recogerMuestra(Explorer explorer) {
        estado.recogerMuestra();
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
    protected void validarAbrirEscotillaSuperior() {
        // Validation logic for opening the superior hatch
    }

    @Override
    protected void validarAbrirEscotillaInferior() {
        // Validation logic for opening the inferior hatch
    }

    @Override
    protected void validarCerrarEscotillas() {
        // Validation logic for closing the hatches
    }

    @Override
    protected void validarAspirar() {
        // Validation logic for aspirating
    }

    @Override
    protected void validarRecogerMuestra() {
        // Validation logic for collecting samples
    }
}
