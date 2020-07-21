import org.encog.ml.ea.genome.Genome;
import org.encog.ml.ea.population.Population;
import org.encog.ml.ea.train.basic.TrainEA;
import org.encog.neural.neat.NEATPopulation;
import org.encog.neural.neat.NEATUtil;
import org.encog.neural.neat.PersistNEATPopulation;

import java.io.*;


public class Neuroevolution implements Runnable {

    private int POPULATION_SIZE;

    int iteration;
    Environment environment;
    Generation generation;

    Config config;

    public Neuroevolution(Environment environment,int iteration){

        generation = new Generation(1);
        this.environment = new Environment(environment,generation);
        this.iteration = iteration;

    }

    public Neuroevolution(Environment environment,Config config){

        generation = new Generation(1);
        this.environment = new Environment(environment,generation);

        this.config = config;
        this.iteration = config.getGenerations();
        this.POPULATION_SIZE = config.getPopulation_size();

    }



    public void begin(){

        ScoreCalculate scoreCalculator = new ScoreCalculate(environment, config);

        NEATPopulation population = loadPopulation();

        TrainEA evolution = NEATUtil.constructNEATTrainer(population,scoreCalculator);

        System.out.println("***************************\nRunning experiment with following parameters\n" + config.experimentDetails() + "\n***************************\n");

            for (int i = evolution.getIteration(); i < iteration; i++) {

                System.out.println("Running generation " + (i+1) + " of iteration " + iteration);

                evolution.iteration();

                savePopulation(evolution.getPopulation());

                double best = evolution.getBestGenome().getScore();

                config.recordFitness(i,best);

                System.out.println("Best Score: " +best + "\n");
                generation.next();

            }

            evolution.finishTraining();

    }



    public void savePopulation(Population population){
        PersistNEATPopulation persistNEATPopulation = new PersistNEATPopulation();


        String filename = String.format("population_%d_%d.eg",config.getAgent_no(),config.getResource_no());

        try {
            persistNEATPopulation.save(new FileOutputStream(filename),population);
        } catch (IOException e){
            e.printStackTrace();
        }
    }

    public NEATPopulation loadPopulation(){

        if (config.isLoadPopulation()) {

            PersistNEATPopulation persistNEATPopulation = new PersistNEATPopulation();

            String filename = String.format("population_%d_%d.eg", config.getAgent_no(), config.getResource_no());


            File file = new File(filename);

            if (file.exists() && file.isFile()) {

                try {

                    FileInputStream stream = new FileInputStream(filename);

                    if (stream.getChannel().size() > 0)
                        return (NEATPopulation) persistNEATPopulation.read(new FileInputStream(filename));
                } catch (IOException e) {
                    e.printStackTrace();
                }

            }

        }

        NEATPopulation population = new NEATPopulation(9,1,POPULATION_SIZE);
        population.setInitialConnectionDensity(1.0);
        population.reset();

        return population;

    }


    @Override
    public void run(){

        begin();

    }




}