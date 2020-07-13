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
        NEATPopulation population =  new NEATPopulation(9,1,POPULATION_SIZE);
        population.setInitialConnectionDensity(1.0);
        population.reset();

        TrainEA evolution = NEATUtil.constructNEATTrainer(population,scoreCalculator);

        System.out.println("Experiment: " +config.getId() +"\nStarting Evolution with "+ POPULATION_SIZE + " networks\n***************************\n");


            for (int i = evolution.getIteration(); i < iteration; i++) {

                System.out.println("Running generation " + (i+1) + " of iteration " + iteration);

                evolution.iteration();


                double best = evolution.getBestGenome().getScore();

                config.getFitnessStats().write((i+1)+"," + config.getCalculator().summaryStatistics() + "," + config.getTrialCalculator().sum(true)+","+ best + ","+config.getResource_no() + ","+config.getAgent_no());

                System.out.println("Best Score: " +best + "\n");
                generation.next();


            }


            evolution.finishTraining();

    }



    public void savePopulation(NEATPopulation population){
        PersistNEATPopulation persistNEATPopulation = new PersistNEATPopulation();

        try {
            persistNEATPopulation.save(new FileOutputStream("population.eg"),population);
        } catch (IOException e){
            e.printStackTrace();
        }
    }

    public void loadPopulation(NEATPopulation population){
        PersistNEATPopulation persistNEATPopulation = new PersistNEATPopulation();

        try {
            population=(NEATPopulation) persistNEATPopulation.read(new FileInputStream("population.eg"));
        } catch (IOException e){
            e.printStackTrace();
        }
    }




    public String summaryStatistics(Population population){

        double currentAverage = population.flatten().stream().mapToDouble(Genome::getScore).average().getAsDouble();
        double variance = population.flatten().stream()
                .map(i -> i.getScore() - currentAverage)
                .map(i -> i*i)
                .mapToDouble(i -> i).sum()/(population.size()-1);


        System.out.println("Average: " + currentAverage + "\nVariance: " + variance);

        return currentAverage+","+variance;

    }

    @Override
    public void run(){

        begin();

    }




}