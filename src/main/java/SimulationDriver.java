import org.encog.neural.neat.NEATNetwork;

public class SimulationDriver implements Runnable{


    Environment environment;
    int resources,agents, iterations=0,Generation;

    public SimulationDriver(int generation,Environment environment, int resources, int agents){


        this.environment = environment;
        this.resources = resources;
        this.agents = agents;
        Generation = generation;

    }

    public SimulationDriver(int generation, Environment environment, int resources, int agents, int iterations){


        this.environment = environment;
        this.resources = resources;
        this.agents = agents;
        this.iterations = iterations;
        Generation = generation;

    }


    public SimulationDriver(int generation, Environment environment){

        this.environment = environment;
        Generation = generation;

    }

    public SimulationDriver(Environment environment, int iterations){

        this.environment = environment;
        this.iterations = iterations;

    }



    public float getFitness(){
        return environment.getFitness(false);
    }


    public void begin(){

//        environment.loadGrid(resources, agents);

        if (iterations > 0)
            environment.begin(iterations);
        else
            environment.begin();

    }

    public void run(){

        begin();

    }



}
