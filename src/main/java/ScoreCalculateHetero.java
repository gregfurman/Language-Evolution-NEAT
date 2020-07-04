import org.encog.ml.CalculateScore;
import org.encog.ml.MLMethod;
import org.encog.neural.neat.FactorNEATGenome;
import org.encog.neural.neat.NEATGenomeFactory;
import org.encog.neural.neat.NEATNetwork;

import java.io.*;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class ScoreCalculateHetero implements CalculateScore {


    volatile int generation, experimentNumber;
    Environment environment;

    List<Agent> agents;
    Agent agent;

    AtomicInteger populationSize;

    final static int TRIALS = 10;


    public ScoreCalculateHetero(Environment environment){

        this.environment = environment;

    }

    public ScoreCalculateHetero(Environment environment, int populationSize){

        this.environment = environment;
        this.populationSize = new AtomicInteger(populationSize);

    }


    public ScoreCalculateHetero(Agent agent){
        this.agent = agent;
    }

    public ScoreCalculateHetero(List<Agent> agents, int populationSize){

        this.agents = agents;
        this.populationSize = new AtomicInteger(populationSize);

    }

    @Override
    public double calculateScore(MLMethod mlMethod) {

        NEATNetwork network = (NEATNetwork) mlMethod;

        Agent agent = environment.getAgent(network.hashCode());
        System.out.println(network.hashCode());

        while(environment.running.get()){}

        return agent.getFitness();
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