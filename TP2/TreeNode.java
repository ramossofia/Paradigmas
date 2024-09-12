package tree;

import java.util.ArrayList;
import java.util.List;

public class TreeNode {
    //atributos
    Object value;
    List<TreeNode> children;

    //constructor
    public TreeNode(Object value) {
        this.value= value;
        this.children = new ArrayList<>();
    }

    //metodo para agregar nodo hijo
    public void addChild (TreeNode child){
        this.children.add(child);
    }

    //metodo para obtener hijo
    public List<TreeNode> getChildren() {
        return this.children;
    }

    //metodo para obtener valor
    public Object getValue(){
        return this.value;
    }
}
