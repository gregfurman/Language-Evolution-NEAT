public class Cell {

    int x;
    int y;

    Cell(){

    }

    public String toString(){
        return "[ ]";
    }

    Cell(int x, int y){
        this.x = x;
        this.y = y;
    }

    public int[] move(){
        return new int[]{x,y};
    }

}
