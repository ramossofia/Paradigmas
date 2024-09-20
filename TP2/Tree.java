package tree;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

public class Tree {
    private Leaf root;

    public Tree(Object carga) {
        this.root = new LeafWithSubTree(carga);
    }

    public ArrayList<Object> dfs() {
        ArrayList<Object> result = new ArrayList<>();
        root.dfs(result);
        return result;
    }

    public ArrayList<Object> bfs() {
        ArrayList<Object> result = new ArrayList<>();
        Queue<Leaf> queue = new LinkedList<>();
        queue.add(root);

        while (!queue.isEmpty()) {
            Leaf current = queue.poll();
            result.add(current.getCarga());
            current.addChildrenToQueue(queue);
        }

        return result;
    }

    public Tree atLeft(Tree leftTree) {
        root = root.addChildAt(0, leftTree.root);
        return this;
    }

    public Tree atRight(Tree rightTree) {
        root = root.addChildAt(1, rightTree.root);
        return this;
    }

    public Tree left() {
        Leaf leftChild = root.getChildAt(0);
        return new Tree(leftChild.getCarga());
    }

    public Tree right() {
        Leaf rightChild = root.getChildAt(1);
        return new Tree(rightChild.getCarga());
    }

    public Object carga() {
        return root.getCarga();
    }
}
