import java.util.concurrent.atomic.AtomicInteger;

public class Generation {

    AtomicInteger generation;

    Generation(int generation){
        this.generation = new AtomicInteger(generation);
    }


    void next(){
        generation.incrementAndGet();
    }

    int get(){
        return generation.get();
    }

    void set(int generation){

        this.generation.getAndSet(generation);
        System.out.println(generation);

    }


}
