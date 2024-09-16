package tree;

import java.util.LinkedList;
import java.util.Queue;

public class NodoConHijoIzquierdo extends Nodo {
    private Nodo left;

    public NodoConHijoIzquierdo(Object carga, Nodo left) {
        super(carga);
        this.left = left;
    }

    @Override
    public Nodo atLeft(Nodo left) {
        return new NodoConHijos(carga, left, null);  // Convert to NodoConHijos
    }

    @Override
    public Nodo atRight(Nodo right) {
        return new NodoConHijos(carga, this.left, right);  // Convert to NodoConHijos
    }

    @Override
    public Nodo left() {
        return left;  // Polymorphism ensures this is handled correctly
    }

    @Override
    public Nodo right() {
        throw new UnsupportedOperationException("Nada a la diestra!");  // Polymorphic control
    }

    @Override
    public void dfs(LinkedList<Object> result) {
        result.add(carga);
        left.dfs(result);  // Delegate to left child
    }

    @Override
    public void addChildrenToQueue(Queue<Nodo> queue) {
        queue.add(left);  // Only left child to add
    }
}
