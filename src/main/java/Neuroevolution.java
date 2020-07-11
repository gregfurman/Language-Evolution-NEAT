import org.encog.ml.MLMethod;
import org.encog.ml.ea.genome.Genome;
import org.encog.ml.ea.population.Population;
import org.encog.ml.ea.train.basic.TrainEA;
import org.encog.neural.neat.NEATNetwork;
import org.encog.neural.neat.NEATPopulation;
import org.encog.neural.neat.NEATUtil;
import org.encog.neural.neat.PersistNEATPopulation;
import org.encog.util.obj.SerializeObject;
import org.encog.util.simple.EncogUtility;

import static org.encog.util.simple.EncogUtility.*;
import static org.encog.persist.EncogDirectoryPersistence.*;

import java.io.*;


public class Neuroevolution implements Runnable {

    private int POPULATION_SIZE;

    int experimentIndex;
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

        StatsRecorder wordlist =new StatsRecorder("wordList.json");
        ScoreCalculate scoreCalculator = new ScoreCalculate(environment,wordlist, config);
        NEATPopulation population =  new NEATPopulation(9,1,POPULATION_SIZE);
        population.setInitialConnectionDensity(1.0);
        population.reset();

        TrainEA evolution = NEATUtil.constructNEATTrainer(population,scoreCalculator);

        System.out.println("Experiment: " +experimentIndex+ " of iteration " + iteration+"\nStarting Evolution with "+ POPULATION_SIZE + " networks\n***************************\n");

        StatsRecorder fitnessStats = new StatsRecorder("fitness.csv","Generation,average,variance,best");


            for (int i = evolution.getIteration(); i < iteration; i++) {

                System.out.println("Running generation " + (i+1) + " of iteration " + iteration);
                evolution.iteration();

                double best = evolution.getBestGenome().getScore();

                fitnessStats.write((i+1)+"," + summaryStatistics(evolution.getPopulation()) + "," + best);


                System.out.println("Best Score: " +best + "\n");
                wordlist.flush();
                generation.next();

            }

            wordlist.close();
            fitnessStats.close();

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