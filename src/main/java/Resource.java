import java.util.Random;

public class Resource extends Cell {

    Random random;
    float reward;
    char type;
    boolean active;

    Resource(char type){

        active = true;
        random = new Random();
        this.type = type;
        reward = random.nextFloat();

    }

    void consume(){
        active = false;
        type = ' ';
    }

    boolean checkStatus(){
        return active;
    }

    public String toString() {
        return "["+type+"]";
    }
}
