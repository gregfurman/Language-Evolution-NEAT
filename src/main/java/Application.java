public class Application {


    public static void main(String[] args){

        Environment environment = new Environment(50,50);

        Neuroevolution ne = new Neuroevolution(environment);

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
