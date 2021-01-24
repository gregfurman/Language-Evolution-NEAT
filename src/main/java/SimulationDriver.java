
public class SimulationDriver implements Runnable{


    Environment environment;
    int iterations;
    final static int DEFAULT_ITERATIONS=5000;


    public SimulationDriver(Environment environment, int iterations){

        this.environment = environment;
        this.iterations = iterations;

    }

    public SimulationDriver(Environment environment){

        this.environment = environment;
        this.iterations = DEFAULT_ITERATIONS;

    }


    public double getFitness(){
        return environment.getFitness(false);
    }

    public String getSummaryStats(){
        return environment.getSummaryStatistics();

    }

    public String[] wordlist(boolean status,int trial){
        return environment.getWordList(status,trial);
    }

    public void begin(){

        environment.loadGrid();
        environment.begin(iterations);

    }

    public void run(){

        begin();

    }



}
