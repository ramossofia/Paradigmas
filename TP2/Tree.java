package tree;

import java.util.LinkedList;
import java.util.Queue;

public class Tree {
    private Nodo root;

    public Tree(Object carga) {
        this.root = new NodoSinHijos(carga);  // Start with a node without children
    }

    // Depth-First Search (DFS)
    public LinkedList<Object> dfs() {
        LinkedList<Object> result = new LinkedList<>();
        root.dfs(result);  // Let the node classes handle the DFS traversal
        return result;
    }

    // Breadth-First Search (BFS)
    public LinkedList<Object> bfs() {
        LinkedList<Object> result = new LinkedList<>();
        Queue<Nodo> queue = new LinkedList<>();
        queue.add(root);  // Start with the root node

        while (!queue.isEmpty()) {
            Nodo current = queue.poll();
            result.add(current.getCarga());
            current.addChildrenToQueue(queue);  // Delegate to the node class to add children to the queue
        }

        return result;
    }

    // Attach a left subtree
    public Tree atLeft(Tree leftTree) {
        root = root.atLeft(leftTree.root);  // Let the node classes handle attaching the left subtree
        return this;
    }

    // Attach a right subtree
    public Tree atRight(Tree rightTree) {
        root = root.atRight(rightTree.root);  // Let the node classes handle attaching the right subtree
        return this;
    }

    // Get the left subtree
    public Tree left() {
        return new Tree(root.left().getCarga());  // Polymorphic call, will work according to the node type
    }

    // Get the right subtree
    public Tree right() {
        return new Tree(root.right().getCarga());  // Polymorphic call, will work according to the node type
    }

    // Get the root node's carga
    public Object carga() {
        return root.getCarga();
    }
}
