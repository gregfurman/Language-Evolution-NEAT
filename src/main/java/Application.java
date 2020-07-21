import java.util.Arrays;

public class Application {


    public static void main(String[] args){



        double[] resources =Arrays.stream(Arrays.copyOfRange(args, 1, args.length-1))
                .mapToDouble(Double::parseDouble)
                .toArray();



        Config config = new Config().load("config");
        config.setAgent_no(Integer.parseInt(args[0]));
        config.setId(Integer.parseInt(args[0]));
        config.loadPopulation(Boolean.parseBoolean(args[args.length-1]));
        config.loadStatsRecorders();


        Config[] configs = new Config[resources.length];
        Thread[] threads = new Thread[resources.length];


        for (int index=0; index< resources.length; index++){

            configs[index]= new Config(config);
            configs[index].setResource_no(resources[index]);

            Environment environment = new Environment(configs[index]);

            threads[index]=new Thread(new Neuroevolution(environment,configs[index]));
            threads[index].start();

        }


        for (Thread thread : threads) {


            try{
                thread.join();
            } catch (InterruptedException e){
                e.printStackTrace();
            }

        }

        System.out.println("Finished Evolution");

        config.closeStatsRecorders();


    }

}
