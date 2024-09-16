package tree;

import java.util.LinkedList;
import java.util.Queue;

public class Tree {
    private Nodo root;

    public Tree(Object carga) {
        this.root = new NodoSinHijos(carga);
    }

    public LinkedList<Object> dfs() {
        LinkedList<Object> result = new LinkedList<>();
        root.dfs(result);
        return result;
    }

    public LinkedList<Object> bfs() {
        LinkedList<Object> result = new LinkedList<>();
        Queue<Nodo> queue = new LinkedList<>();
        queue.add(root);

        while (!queue.isEmpty()) {
            Nodo current = queue.poll();
            result.add(current.getCarga());
            current.addChildrenToQueue(queue);
        }

        return result;
    }

    public Tree atLeft(Tree leftTree) {
        root = root.atLeft(leftTree.root);
        return this;
    }

    public Tree atRight(Tree rightTree) {
        root = root.atRight(rightTree.root);
        return this;
    }

    public Tree left() {
        return new Tree(root.left().getCarga());
    }

    public Tree right() {
        return new Tree(root.right().getCarga());
    }

    public Object carga() {
        return root.getCarga();
    }
}
