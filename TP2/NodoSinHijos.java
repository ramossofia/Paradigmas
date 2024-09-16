package tree;

import java.util.LinkedList;
import java.util.Queue;

public class NodoSinHijos extends Nodo {

    public NodoSinHijos(Object carga) {
        super(carga);
    }

    @Override
    public Nodo atLeft(Nodo left) {
        return new NodoConHijoIzquierdo(carga, left);
    }

    @Override
    public Nodo atRight(Nodo right) {
        return new NodoConHijoDerecho(carga, right);
    }

    @Override
    public Nodo left() {
        throw new UnsupportedOperationException("Nada a la siniestra!");
    }

    @Override
    public Nodo right() {
        throw new UnsupportedOperationException("Nada a la diestra!");
    }

    @Override
    public void dfs(LinkedList<Object> result) {
        result.add(carga);
    }

    @Override
    public void addChildrenToQueue(Queue<Nodo> queue) {
    }
}
