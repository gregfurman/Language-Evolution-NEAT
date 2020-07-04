import org.encog.ml.CalculateScore;
import org.encog.ml.MLMethod;
import org.encog.neural.neat.NEATNetwork;

import java.io.*;
import java.util.concurrent.atomic.AtomicInteger;

public class ScoreCalculate implements CalculateScore {


    volatile int generation, experimentNumber;
    Environment environment;

    AtomicInteger populationSize;

    final static int TRIALS = 10;

    public ScoreCalculate(int generation,int experimentNumber, Environment environment){

        this.generation = generation;
        this.experimentNumber = experimentNumber;
        this.environment = environment;

    }


    public ScoreCalculate(Environment environment){

        this.environment = environment;

    }

    public ScoreCalculate(Environment environment, int populationSize){

        this.environment = environment;
        this.populationSize = new AtomicInteger(populationSize);

    }

    @Override
    public double calculateScore(MLMethod mlMethod) {

        System.out.println(mlMethod.getClass());

        NEATNetwork network = (NEATNetwork) mlMethod;


//        Thread[] tests = new Thread[TRIALS];
        SimulationDriver[] drivers = new SimulationDriver[TRIALS];


        float score = 0;

        for (int trial = 0; trial < TRIALS; trial++) {
            Environment env = new Environment(environment);
            env.loadGrid(1500, 500, network);
            drivers[trial] = new SimulationDriver(env, 5000);
            drivers[trial].begin();
            score += drivers[trial].getFitness();
            drivers[trial] = null;

        }

        return (double) score / TRIALS;


    }


    @Override
    public boolean shouldMinimize() {
        return false;
    }

    @Override
    public boolean requireSingleThreaded() {
        return false;
    }



}