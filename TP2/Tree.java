package tree;

import java.util.LinkedList;
import java.util.Queue;

public class Tree {
    private Arista root;

    public Tree(Object carga) {
        this.root = new AristaExistente(carga);
    }

    public LinkedList<Object> dfs() {
        LinkedList<Object> result = new LinkedList<>();
        root.dfs(result);
        return result;
    }

    public LinkedList<Object> bfs() {
        LinkedList<Object> result = new LinkedList<>();
        Queue<Arista> queue = new LinkedList<>();
        queue.add(root);

        while (!queue.isEmpty()) {
            Arista current = queue.poll();
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
        Arista leftChild = root.left();
        return new Tree(leftChild.getCarga());
    }

    public Tree right() {
        Arista rightChild = root.right();
        return new Tree(rightChild.getCarga());
    }

    public Object carga() {
        return root.getCarga();
    }
}
