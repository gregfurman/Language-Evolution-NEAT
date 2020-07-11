import java.util.Arrays;

public class Application {


    public static void main(String[] args){

//        Config config = new Config(Arrays.asList(args).stream().mapToInt(Integer::parseInt).toArray());

        Config config = new Config().load("config");

        Environment environment = new Environment(config);

        Neuroevolution ne = new Neuroevolution(environment,config);

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
