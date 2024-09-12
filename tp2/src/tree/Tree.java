package tree;

import java.util.ArrayList;
import java.util.List;

public class Tree {
    private TreeNode root;

    public Tree() {
        this.root= null;
    }

    public Tree (Object rootValue){
        this.root= new TreeNode(rootValue);
    }

    public TreeNode getRoot(){
        return this.root;
    }

    public List dfs() {
        return null;
    }
        List result = new ArrayList();
        dfsRecursive(root,result);
        return result;
        }

    private void dfsRecursive(TreeNode node, List result) {
        if (node == null) {
            return;
        }
        result.add(node.getValue());
        for (TreeNode child : node.getChildren()) {
            dfsRecursive(child, result);
        }
    }
    }



    public List bfs() {
        return null;
    }

    public Tree atLeft( Tree b ) {
        return null;
    }

    public Tree atRight( Tree b ) {
        return null;
    }

    public Tree right() {
        return null;
    }

    public Tree left() {
        return null;
    }

    public Object carga() {
        return null;
    }
}

