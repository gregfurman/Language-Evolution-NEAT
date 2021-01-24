import java.util.Random;

public class Resource extends Cell {

    Random random;
    double reward;
    char type;
    boolean active;

    Resource(char type){

        active = true;
        random = new Random();
        this.type = type;
        reward = random.nextInt(8)+1;

    }

    Resource(char type, boolean gaussian){

        active = true;
        random = new Random();
        this.type = type;

        if (gaussian)
            reward = Math.abs(random.nextGaussian()*2+4);
        else
            reward = random.nextInt(8)+1;

    }

    Resource(Resource resource){

        active = resource.active;
        type = resource.type;
        reward = resource.reward;

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
