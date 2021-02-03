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

    boolean control = false;

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

        this.control = config.isControl();

    }


    public Neuroevolution(Config config){
        this.config = config;
    }

    public void begin(){


        ScoreCalculate scoreCalculator = new ScoreCalculate(environment, config);

        if (control) {

            System.out.println("***************************\nRunning experiment with following parameters\n" + config.experimentDetails() + "\n***************************\n");

            for (int i = 1; i < iteration; i++) {

                System.out.println("Running generation " + (i + 1) + " of iteration " + iteration);

                scoreCalculator.calculateControlScore();

                double best = config.getCalculator().max();

                config.recordFitness(i, best);

                System.out.println("Best Score: " + best + "\n");
                generation.next();

            }


        } else {

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

    }



    public void savePopulation(Population population){
        PersistNEATPopulation persistNEATPopulation = new PersistNEATPopulation();


        int agents = config.getAgent_no();
        int resources = config.getResource_no();

        // CHANGE THIS LINE!!!!
        if (resources > 2000) resources=2000;


        String filename = String.format("population_%d_%d.eg",agents,resources);


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

            PersistNEATPopulation persistNEATPopulation = new PersistNEATPopulation();

            int agents = config.getAgent_no();
            int resources = config.getResource_no();

            // CHANGE THIS LINE!!!!
            if (agents>500) agents=500;
            if (resources > 2000) resources=2000;


            String filename = String.format("population_%d_%d.eg", agents, resources);

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