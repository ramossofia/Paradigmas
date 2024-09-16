package tree;

import java.util.LinkedList;
import java.util.Queue;

public class TreeVacio {

    public TreeVacio() {
    }

    public Object getCarga() {
        throw new UnsupportedOperationException("El árbol vacío no tiene carga.");
    }

    public void dfs(LinkedList<Object> result) {
    }

    public void bfs(Queue<Nodo> queue) {
    }

    public void addChildrenToQueue(Queue<Nodo> queue) {
    }

    public TreeVacio atLeft(TreeVacio left) {
        throw new UnsupportedOperationException("No se puede asignar hijos a un árbol vacío.");
    }

    public TreeVacio atRight(TreeVacio right) {
        throw new UnsupportedOperationException("No se puede asignar hijos a un árbol vacío.");
    }

    public TreeVacio left() {
        throw new UnsupportedOperationException("El árbol vacío no tiene hijo izquierdo.");
    }

    public TreeVacio right() {
        throw new UnsupportedOperationException("El árbol vacío no tiene hijo derecho.");
    }
}
