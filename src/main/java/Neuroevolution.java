import org.encog.ml.ea.genome.Genome;
import org.encog.ml.ea.population.Population;
import org.encog.ml.ea.species.Species;
import org.encog.ml.ea.train.basic.BasicEA;
import org.encog.ml.ea.train.basic.TrainEA;
import org.encog.neural.neat.NEATPopulation;
import org.encog.neural.neat.NEATUtil;

public class Neuroevolution implements Runnable {

    final static private int POPULATION_SIZE = 150;
    int experimentIndex;
    int iteration;
    Environment environment;
    Generation generation;


    public Neuroevolution(int iteration,int experimentIndex){

        this.experimentIndex = experimentIndex;
        this.iteration = iteration;

    }

    public Neuroevolution(int experimentIndex){

        this.experimentIndex = experimentIndex;

    }

    public Neuroevolution(Environment environment){

        generation = new Generation(1);
        this.environment = new Environment(environment,generation);

    }

    public void begin(){

        StatsRecorder wordlist =new StatsRecorder("wordList.json");
        ScoreCalculate scoreCalculator = new ScoreCalculate(environment,wordlist);
        NEATPopulation population =  new NEATPopulation(9,1,POPULATION_SIZE);
        population.setInitialConnectionDensity(1.0);
        population.reset();

        TrainEA evolution = NEATUtil.constructNEATTrainer(population,scoreCalculator);

        iteration = 100;

        System.out.println("Experiment: " +experimentIndex+ " of iteration " + iteration+"\nStarting Evolution with "+ POPULATION_SIZE + " networks\n***************************\n");

        StatsRecorder fitnessStats = new StatsRecorder("fitness.csv","Generation,average,variance,best");


            for (int i = evolution.getIteration(); i < iteration; i++) {

                System.out.println("Running generation " + i + " of iteration " + iteration);
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


    public String summaryStatistics(Population population){

//        OptionalDouble currentAverage = population.getSpecies().stream().mapToDouble(Species::getBestScore).average();

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