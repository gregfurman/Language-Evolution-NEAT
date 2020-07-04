import org.encog.ml.ea.population.Population;
import org.encog.ml.ea.species.Species;
import org.encog.ml.ea.train.basic.BasicEA;
import org.encog.ml.ea.train.basic.TrainEA;
import org.encog.neural.neat.NEATNetwork;
import org.encog.neural.neat.NEATPopulation;
import org.encog.neural.neat.NEATUtil;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.OptionalDouble;

public class Neuroevolution implements Runnable {

    final static private int POPULATION_SIZE = 3;
    int experimentIndex;
    int iteration;
    Environment environment;


    public Neuroevolution(int iteration,int experimentIndex){

        this.experimentIndex = experimentIndex;
        this.iteration = iteration;

    }

    public Neuroevolution(int experimentIndex){

        this.experimentIndex = experimentIndex;

    }

    public Neuroevolution(Environment environment){

        this.environment = new Environment(environment);

    }

    public void begin(){

        //        long startTime = System.nanoTime();

        ScoreCalculate scoreCalculator = new ScoreCalculate(environment);
        NEATPopulation population =  new NEATPopulation(9,1,POPULATION_SIZE);
        population.setInitialConnectionDensity(1.0);
        population.reset();

        TrainEA evolution = NEATUtil.constructNEATTrainer(population,scoreCalculator);

        iteration = 1000;

        System.out.println("Experiment: " +experimentIndex+ " of iteration " + iteration+"\nStarting Evolution with "+ POPULATION_SIZE + " networks\n***************************\n");

        try {
            File file = new File("./test.csv");

            boolean newFile = file.createNewFile();


            BufferedWriter writer = new BufferedWriter(new FileWriter(file, true));

            if (!newFile){
                writer.write("Generation,averageFitness,maxFitness\n");
            }


            for (int i = evolution.getIteration(); i < iteration; i++) {

                System.out.println("Running generation " + i + " of iteration " + iteration);
                evolution.iteration();


                double best = evolution.getBestGenome().getScore();
                OptionalDouble average = averageScore(evolution.getPopulation());

                writer.write((i+1)+"," + average.getAsDouble() + "," + best + "\n");


                System.out.println("Best Score: " +best + "\n");

            }

            writer.close();

            evolution.finishTraining();

        } catch (IOException e){
            e.printStackTrace();
        }

//        System.out.println("Time: " + (System.nanoTime()- startTime)/1000000/1000);


//        System.out.println("finished thread " + iteration);




//        NEATNetwork bestPerformingNetwork = (NEATNetwork) evolution.getCODEC().decode(evolution.getBestGenome());

//        try {
//            SerializeObject.save(new File("bestnetworks_" + experimentIndex + ".txt"), bestPerformingNetwork);
//        }catch (IOException e){
//            System.out.println("Could not save network");
//        }
//
//        EncogUtility.saveEGB( "bestnetworks_"+experimentIndex+".txt" , data);
//
//
//        EncogDirectoryPersistence.saveObject(new File("bestnetworks_"+experimentIndex+".txt"), bestPerformingNetwork);

    }


    public void beginHetero(){

        //        long startTime = System.nanoTime();

        ScoreCalculateHetero scoreCalculator = new ScoreCalculateHetero(environment,POPULATION_SIZE);
        NEATPopulation population =  new NEATPopulation(9,1,POPULATION_SIZE);
        population.setInitialConnectionDensity(1.0);
        population.reset();
        TrainEA evolution = NEATUtil.constructNEATTrainer(population,scoreCalculator);


        Environment env;

        env = new Environment(environment);
        env.loadGrid(1500, POPULATION_SIZE);
        env.assignNetworks(evolution.getPopulation().flatten());

        iteration = 1000;

        System.out.println("Experiment: " +experimentIndex+ " of iteration " + iteration+"\nStarting Evolution with "+ POPULATION_SIZE + " networks\n***************************\n");

        try {
            File file = new File("./NEAT_FITNESS_HETERO.csv");

            boolean newFile = file.createNewFile();


            BufferedWriter writer = new BufferedWriter(new FileWriter(file, true));

            if (!newFile){
                writer.write("Generation,averageFitness,maxFitness\n");
            }



            for (int i = evolution.getIteration(); i < iteration; i++) {



                System.out.println("Running generation " + i + " of iteration " + iteration);

                env.begin();

                evolution.iteration();


                env = new Environment(environment);
                env.loadGrid(1500, POPULATION_SIZE);
                env.assignNetworks(evolution.getPopulation().flatten());



                double best = evolution.getBestGenome().getScore();
                OptionalDouble average = averageScore(evolution.getPopulation());

                writer.write((i+1)+"," + average + "," + best + "\n");




                System.out.println("Best Score: " +best + "\n");

            }

            writer.close();

            evolution.finishTraining();

        } catch (IOException e){
            e.printStackTrace();
        }

//        System.out.println("Time: " + (System.nanoTime()- startTime)/1000000/1000);


//        System.out.println("finished thread " + iteration);




//        NEATNetwork bestPerformingNetwork = (NEATNetwork) evolution.getCODEC().decode(evolution.getBestGenome());

//        try {
//            SerializeObject.save(new File("bestnetworks_" + experimentIndex + ".txt"), bestPerformingNetwork);
//        }catch (IOException e){
//            System.out.println("Could not save network");
//        }
//
//        EncogUtility.saveEGB( "bestnetworks_"+experimentIndex+".txt" , data);
//
//
//        EncogDirectoryPersistence.saveObject(new File("bestnetworks_"+experimentIndex+".txt"), bestPerformingNetwork);

    }


    public OptionalDouble averageScore(Population population){

        OptionalDouble currentAverage = population.getSpecies().stream().mapToDouble(Species::getBestScore).average();
        System.out.println("Average species score: " + currentAverage.orElse(-1));

        return currentAverage;

    }

    @Override
    public void run(){

        begin();

    }




}