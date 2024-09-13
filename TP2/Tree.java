package tree;

import org.w3c.dom.Node;

import javax.swing.tree.TreeNode;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class Tree {
    private TreeNode root; //nose si esta bien, si no me daba error

    //constructor
    public Tree( Object a ) {
        this.root = new TreeNode(a,null,null );
    }

    //clase nodo
    private class TreeNode {
        private Object value;
        private TreeNode left;
        private TreeNode right;

        //constructor
        public TreeNode( Object value, TreeNode left, TreeNode right ) {
            this.value = value;
            this.left = left;
            this.right = right;
        }
    }


    public List<Object> dfs() {
        List<Object> list = new ArrayList<>();
        dfsAlgorithm(root, list);
        return null;
    }

    private void dfsAlgorithm(TreeNode node, List<Object> list) {
        if (node==null) return;
        list.add(node.value);
        dfsAlgorithm(node.right, list);
        dfsAlgorithm(node.left, list);
    }

    public List bfs() {
        List<Object> list = new ArrayList<>();

        return null;
    }

    private void bfsAlgorithm(TreeNode node, List<Object> list) {
        if (node==null) return;
        Queue<Tree.TreeNode> queue = new LinkedList<>();
        queue.add(node);

        while (!queue.isEmpty()) {
            TreeNode current = queue.poll(); //poll elimina y devuelve el primer nodo de la cola
            list.add(current.value);

            if (current.left != null) { //se van agregando a la cola los hijos y asi hasta que la cola quede vacia
                queue.add(current.left);
            }
            if (current.right != null) {
                queue.add(current.right);
            }
        }
    }


    public Tree atLeft(Tree OtherTree) {
        TreeNode currentLeft = this.root.left; //preguntar que hago con este subarbol, donde lo inserto cuando agrego el otro

        this.root.left = OtherTree.root;

        if (currentLeft != null) {
            OtherTree.root.left = currentLeft;
        }

        return this;
    }

    public Tree atRight( Tree OtherTree ) {
        TreeNode currentRight = this.root.right; //como agarro todo el subarbol

        return this;
    }

    public Tree right() {
        if (this.root.right == null) return null;

        return new Tree(this.root.right); //como devuelvo los hijos y el subarbol
    }

    public Tree left() {
        return null;
    }

    public Object carga() {
        return (this.root != null) ?  this.root.value : null;
    }
}
