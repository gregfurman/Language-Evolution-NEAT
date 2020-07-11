import org.encog.ml.CalculateScore;
import org.encog.ml.MLMethod;
import org.encog.neural.neat.NEATNetwork;

public class ScoreCalculate implements CalculateScore {


    Environment environment;
    StatsRecorder statsRecorder;

    private int TRIALS, GENERATIONS;
    final static int MAX_ITERATIONS = 20000;

    Config config;


    public ScoreCalculate(Environment environment, StatsRecorder statsRecorder, int TRIALS){

        this.environment = environment;
        this.statsRecorder = statsRecorder;
        this.TRIALS = TRIALS;
    }

    public ScoreCalculate(Environment environment, StatsRecorder statsRecorder, Config config){

        this.environment = environment;
        this.statsRecorder = statsRecorder;
        this.config = config;
        this.GENERATIONS = config.getGenerations();
        this.TRIALS = config.getTrials();

    }


    @Override
    public double calculateScore(MLMethod mlMethod) {

        NEATNetwork network = (NEATNetwork) mlMethod;


        SimulationDriver[] drivers = new SimulationDriver[TRIALS];


        float score = 0;

        for (int trial = 0; trial < TRIALS; trial++) {


            Environment env = new Environment(environment,network);

            drivers[trial] = new SimulationDriver(env, MAX_ITERATIONS);
            drivers[trial].begin();
            score += drivers[trial].getFitness();

            if (env.generation.get() == config.getGenerations())
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