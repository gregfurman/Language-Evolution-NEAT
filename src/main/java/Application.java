import org.encog.neural.neat.NEATNetwork;
import org.encog.neural.neat.NEATPopulation;

import java.util.ArrayList;
import java.util.List;


public class Application {


    public static void main(String[] args) {



        Neuroevolution neuroevolution;
        Environment environment;
        NEATPopulation population;

        Config config = new Config(new Config().load("config"));

//        config.loadStatsRecorders();

//        neuroevolution = new Neuroevolution(config);
//
//        NEATPopulation population = neuroevolution.loadPopulation();
//
//        environment = new Environment(config, population);
//        environment.loadGrid();
//
//        environment.begin();


        if (config.isTrain()) {

            config.loadStatsRecorders();
            environment = new Environment(config);

            neuroevolution = new Neuroevolution(environment, config);
            neuroevolution.begin();


        } else {


            final int ENVIRONMENT_AMOUNT = 5;

            Environment environment1, environment2;

            int[] resources = {500, 1000, 1500, 2000, 2500};
            int[] agents = {25, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 1000};
            double[] splits = {0};
            int[] dimensions = {50, 75, 100, 150};


            for (int dim : dimensions)
                for (double split : splits) {
                    for (int agent : agents) {


                        for (int resource : resources) {

                            config.setResources(resource);
                            config.setAgent_no(agent);
                            config.setSplit(split);
                            config.setDims(dim, dim);

                            config.loadStatsRecorders();

                            neuroevolution = new Neuroevolution(config);

                            population = neuroevolution.loadPopulation();

                            List<Thread> environments = new ArrayList<>();

                            for (int env = 0; env < ENVIRONMENT_AMOUNT; env++) {

                                environment = new Environment(config, population);
                                environment.loadGrid();


                                for (int trial = 0; trial <= config.getTrials(); trial++) {

                                    environment.setID(env, trial);

//                                    environment2 = new Environment(environment);
//                                    environment1 = new Environment(environment);


//                                    environment1.setNetwork(null);

                                    NEATNetwork n = null;

                                    environments.add(new Thread( new Environment(environment)));
                                    environments.add(new Thread( new Environment(environment,n)));


                                }


                            }

                            for (Thread e : environments) {
                                e.start();
                            }

                            for (Thread e :environments) {
                                try {
                                    e.join();
                                } catch (InterruptedException error){
                                    error.printStackTrace();
                                }
                            }


                            config.closeStatsRecorders();

                        }
                    }
                }


        }

    }


}
