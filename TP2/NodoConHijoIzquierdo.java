package tree;

import java.util.LinkedList;
import java.util.Queue;

public class NodoConHijoIzquierdo extends Nodo {
    private Nodo left;

    public NodoConHijoIzquierdo(Object carga, Nodo left) {
        super(carga);
        this.left = left;
    }


    public Nodo atLeft(Nodo left) {
        return new NodoConHijos(carga, left, null);
    }


    public Nodo atRight(Nodo right) {
        return new NodoConHijos(carga, this.left, right);
    }


    public Nodo left() {
        return left;
    }


    public Nodo right() {
        throw new UnsupportedOperationException("Nada a la diestra!");
    }


    public void dfs(LinkedList<Object> result) {
        result.add(carga);
        left.dfs(result);
    }


    public void addChildrenToQueue(Queue<Nodo> queue) {
        queue.add(left);
    }
}
