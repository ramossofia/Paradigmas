package tree;

import java.util.LinkedList;
import java.util.Queue;

public class NodoSinHijos extends Nodo {

    public NodoSinHijos(Object carga) {
        super(carga);
    }

    public Nodo atLeft(Nodo left) {
        return new NodoConHijoIzquierdo(carga, left);
    }

    public Nodo atRight(Nodo right) {
        return new NodoConHijoDerecho(carga, right);
    }

    public Nodo left() {
        throw new UnsupportedOperationException("Nada a la siniestra!");
    }


    public Nodo right() {
        throw new UnsupportedOperationException("Nada a la diestra!");
    }


    public void dfs(LinkedList<Object> result) {
        result.add(carga);
    }


    public void addChildrenToQueue(Queue<Nodo> queue) {
    }
}
