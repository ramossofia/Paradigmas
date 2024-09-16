package tree;

import java.util.LinkedList;
import java.util.Queue;

public class TreeVacio {

    public TreeVacio() {
        // Represents an empty tree
    }

    public Object getCarga() {
        throw new UnsupportedOperationException("El árbol vacío no tiene carga.");
    }

    public void dfs(LinkedList<Object> result) {
        // Do nothing, as this tree is empty
    }

    public void bfs(Queue<Nodo> queue) {
        // Do nothing, as this tree is empty
    }

    public void addChildrenToQueue(Queue<Nodo> queue) {
        // Do nothing, as there are no children in an empty tree
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
