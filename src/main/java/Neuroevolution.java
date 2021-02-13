import static org.encog.persist.EncogDirectoryPersistence.*;

import org.encog.ml.ea.population.Population;
import org.encog.ml.ea.train.basic.TrainEA;
import org.encog.neural.neat.NEATPopulation;
import org.encog.neural.neat.NEATUtil;
import org.encog.neural.neat.PersistNEATPopulation;
import org.encog.persist.PersistError;

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


    public Neuroevolution(Config config){
        this.config = config;
    }

    public void begin(){


        ScoreCalculate scoreCalculator = new ScoreCalculate(environment, config);


        NEATPopulation population = loadPopulation();

        TrainEA evolution = NEATUtil.constructNEATTrainer(population, scoreCalculator);

        if (evolution.getIteration() < config.getGenerations()) {

            generation.set(evolution.getIteration()+1);

            System.out.println("***************************\nRunning experiment with following parameters\n" + config.experimentDetails() + "\n***************************\n");

            for (int i = evolution.getIteration(); i < iteration; i++) {

                System.out.println("Running generation " + (i + 1) + " of iteration " + iteration);

                evolution.iteration();

                savePopulation(evolution.getPopulation());

                double best = evolution.getBestGenome().getScore();

                config.recordFitness(i, best);

                System.out.println("Best Score: " + best + "\n");
                generation.next();

            }

        }

        evolution.finishTraining();

    }





    public void savePopulation(Population population){
        PersistNEATPopulation persistNEATPopulation = new PersistNEATPopulation();


        int agents = config.getAgent_no();
        int resources = config.getResource_no();


        String filename = String.format("./savedNetworks/population_%d_%d.eg",agents,resources);


        try {
            saveObject(new File(filename), population);
            return;
        } catch (PersistError error){
            System.out.println("Error saving EG file.");
        }

        try {
            persistNEATPopulation.save(new FileOutputStream(filename),population);
        } catch (IOException e){
            e.printStackTrace();
        }


    }

    public NEATPopulation loadPopulation(){

        if (config.isLoadPopulation()) {

            final int DEFAULT_RESOURCES = 2000;

            PersistNEATPopulation persistNEATPopulation = new PersistNEATPopulation();

            int agents = config.getAgent_no();
            int resources =   config.getResource_no() <= DEFAULT_RESOURCES ? config.getResource_no(): DEFAULT_RESOURCES;


            String filename = String.format("./savedNetworks/population_%d_%d.eg", agents, resources);

            File file = new File(filename);

            try {
                return (NEATPopulation) loadObject(file);
            } catch (PersistError error) {
                System.out.println(filename + ": " + error.getMessage());
            }


            if (file.exists() && file.isFile()) {

                try {

                    FileInputStream stream = new FileInputStream(filename);

                    if (stream.getChannel().size() > 0)
                        return (NEATPopulation) persistNEATPopulation.read(new FileInputStream(filename));

                } catch (IOException e) {
                    System.out.println(filename + ": " + "Not valid persist object file.");
                }

            }

        }

        NEATPopulation population = new NEATPopulation(9,1,config.getPopulation_size());
        population.setInitialConnectionDensity(1.0);
        population.reset();


        return population;

    }


    @Override
    public void run(){

        begin();

    }




}