import org.encog.ml.CalculateScore;
import org.encog.ml.MLMethod;
import org.encog.neural.neat.NEATNetwork;

public class ScoreCalculate implements CalculateScore {


    Environment environment;
    StatsRecorder statsRecorder;

    final static int TRIALS = 5;
    final static int AGENTS = 200;
    final static int RESOURCES = 2300;
    final static int MAX_ITERATIONS = 20000;




    public ScoreCalculate(Environment environment, StatsRecorder statsRecorder){

        this.environment = environment;
        this.statsRecorder = statsRecorder;
    }


    @Override
    public double calculateScore(MLMethod mlMethod) {

        NEATNetwork network = (NEATNetwork) mlMethod;


        SimulationDriver[] drivers = new SimulationDriver[TRIALS];


        float score = 0;

        for (int trial = 0; trial < TRIALS; trial++) {
            Environment env = new Environment(environment);
            env.loadGrid(RESOURCES, AGENTS, network);
            drivers[trial] = new SimulationDriver(env, MAX_ITERATIONS);


//            recordWords(drivers[trial].wordlist(true,trial));


            drivers[trial].begin();
            score += drivers[trial].getFitness();

            if (env.generation.get() == 100)
            recordWords(drivers[trial].wordlist(false,trial));
            drivers[trial] = null;


        }

        return (double) score / TRIALS;


    }

    void recordWords(String[] wordlist){
        boolean success=true;
        for(String words : wordlist ){
            success = success && statsRecorder.write(words);
            if (!success)
                break;
        }

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