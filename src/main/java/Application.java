import org.encog.neural.neat.NEATPopulation;

import java.util.Arrays;

public class Application {


    public static void main(String[] args){


        Config config = new Config().load("config");



        if (config.isTrain()) {

        double[] resources =Arrays.stream(Arrays.copyOfRange(args, 1, args.length))
                .mapToDouble(Double::parseDouble)
                .toArray();

        config.setAgent_no(Integer.parseInt(args[0]));
        config.setId(Integer.parseInt(args[0]));

            Config[] configs = new Config[resources.length];
            Thread[] threads = new Thread[resources.length];

            config.loadStatsRecorders();

            for (int index = 0; index < resources.length; index++) {

                configs[index] = new Config(config);
                configs[index].setResource_no(resources[index]);


                Environment environment = new Environment(configs[index]);

                threads[index] = new Thread(new Neuroevolution(environment, configs[index]));

                threads[index].start();

            }


            for (Thread thread : threads) {


                try {
                    thread.join();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }

            }

        } else {


            Environment environment, environment1, environment2;
            Thread env1, env2;

            int[] resources = {500,1000,1500,2000,2500};
            int[] agents  = {25,50,100,150,200,250,300,350,400,450,500,1000};
            double[] splits = {0.1, 0.2, 0.3, 0.4, 0.5};
            int[] dimensions = {50,75,100,150};


            for (int dim : dimensions)
            for (double split : splits) {
                for (int agent : agents) {


                    for (int resource : resources) {

                        config.setResources(resource);
                        config.setAgent_no(agent);
                        config.setSplit(split);
                        config.setDims(dim,dim);

                        config.loadStatsRecorders();

                        Neuroevolution neuroevolution = new Neuroevolution(config);

                        NEATPopulation population = neuroevolution.loadPopulation();
                        neuroevolution.savePopulation(population);
                        population = neuroevolution.loadPopulation();


                        for (int env = 0; env < 10; env++) {

                            environment = new Environment(config, population);
                            environment.loadGrid();


                            for (int trial = 0; trial <= config.getTrials(); trial++) {


                                environment2 = new Environment(environment);
                                environment1 = new Environment(environment);

                                env2 = new Thread(environment2);
                                env1 = new Thread(environment1);

                                environment1.setNetwork(null);

                                env1.start();
                                env2.start();

                                try {
                                    env1.join();
                                    env2.join();


                                    StatsRecorder wordlist = config.getWordList();

                                    for (String word : environment1.getWordList(trial, env)) {

                                        wordlist.write(word);

                                    }

                                    for (String word : environment2.getWordList(trial, env)) {

                                        wordlist.write(word);

                                    }

                                    System.out.println(String.format("Trial=%d Environment=%d Resources=%d Population=%d", trial, env, resource,agent));

                                } catch (InterruptedException e) {
                                    e.printStackTrace();
                                }

                            }


                        }

                        config.closeStatsRecorders();


                    }

                }
            }
        }



    }


}
