import java.util.Arrays;

public class Application {


    public static void main(String[] args){

        //Make config?

        int[] config = Arrays.asList(args).stream().mapToInt(Integer::parseInt).toArray();

        Environment environment = new Environment(config[0],config[1],config[2]);

        Neuroevolution ne = new Neuroevolution(environment,config[3]);

        ne.begin();




//        Thread thread = new Thread(ne);
//
//
//        try {
//
//            long startTime = System.nanoTime();
//
//            thread.start();
//            thread.join();
//
//            long endTime = System.nanoTime();
//
//            long duration = (endTime - startTime);
//            System.out.println("Total Duration: " + duration / 1000000);
//        } catch (InterruptedException e){
//            e.printStackTrace();
//        }




    }

}
