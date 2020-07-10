import com.google.gson.Gson;
import com.google.gson.JsonElement;
import org.encog.ml.MLRegression;
import org.encog.neural.neat.NEATNetwork;

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;


public class Environment implements Runnable{

    class CheckNeighbours  extends RecursiveAction {
        private final int THRESHOLD = 1000;
        final Cell[][] grid;
        Environment env;
        final int lo, hi;

        CheckNeighbours(Environment env) {
            this.grid = env.grid;
            this.env = env;
            this.lo = 0;
            this.hi = grid.length-1;
        }


        private CheckNeighbours(Cell[][] array, int lo, int hi) {
            this.grid = array;
            this.lo = lo;
            this.hi = hi;
        }

        protected void compute() {
            if (hi - lo < THRESHOLD) {
                for (int i = lo; i < hi; ++i)
                    for (int j = lo; j < hi; ++j){
                        if (grid[i][j].getClass() == Agent.class ) {
                            Agent agent = (Agent) grid[i][j];
                            Cell resource = null;
                            agent.neighbours = new ArrayList<>();

                            Class type = Resource.class;

                            int resource_x=-1, resource_y=-1;


                            outerloop:
                            for (int colNum = i - 1; colNum <= (i + 1); colNum += 1) {
                                for (int rowNum = j - 1; rowNum <= (j + 1); rowNum += 1) {
                                    if (!((colNum == i) && (rowNum == j))) {
                                        if (env.withinGrid(colNum, rowNum)) {
                                            if (grid[colNum][rowNum].getClass() == type) {

                                                if (type == Agent.class) {

                                                    agent.addNeighbour((Agent) grid[colNum][rowNum]);
                                                    grid[resource_x][resource_y] = new Cell();


                                                } else {
                                                    resource = (Resource) grid[colNum][rowNum];
                                                    resource_x = colNum;
                                                    resource_y = rowNum;

//                                                    System.out.println(colNum + " " + rowNum);
                                                    colNum = i;
                                                    rowNum = j;
                                                    type = Agent.class;
                                                }
                                            }
                                        }
                                    }
                                }
                            }


                            if (agent.neighbours.size() > 0) {

                                Game game = new Game(agent, (Resource) resource);
                                game.run();

//                                try {
//                                    Game game = new Game(agent, (Resource) resource);
//                                    Thread thread = new Thread(game);
//                                    thread.start();
//
//                                    thread.join();
//                                } catch (InterruptedException e) {
//                                    e.printStackTrace();
//                                }

                            }
                        }
                    }


            } else {
                int mid = (lo + hi) >>> 1;
                invokeAll(new CheckNeighbours(grid, lo, mid), new CheckNeighbours(grid, mid, hi));
            }
        }


        void serialCheck(){
            for (int i = lo; i < hi; ++i)
                for (int j = lo; j < hi; ++j){
                    if (grid[i][j].getClass() == Agent.class ) {
                        Agent agent = (Agent) grid[i][j];
                        Cell resource = null;
                        agent.neighbours = new ArrayList<>();

                        Class type = Resource.class;

                        int resource_x=-1, resource_y=-1;


                        outerloop:
                        for (int colNum = i - 1; colNum <= (i + 1); colNum += 1) {
                            for (int rowNum = j - 1; rowNum <= (j + 1); rowNum += 1) {
                                if (!((colNum == i) && (rowNum == j))) {
                                    if (env.withinGrid(colNum, rowNum)) {
                                        if (grid[colNum][rowNum].getClass() == type) {

                                            if (type == Agent.class) {

                                                agent.addNeighbour((Agent) grid[colNum][rowNum]);
                                                grid[resource_x][resource_y] = new Cell();


                                            } else {
                                                resource = (Resource) grid[colNum][rowNum];
                                                resource_x = colNum;
                                                resource_y = rowNum;

//                                                    System.out.println(colNum + " " + rowNum);
                                                colNum = i;
                                                rowNum = j;
                                                type = Agent.class;
                                            }
                                        }
                                    }
                                }
                            }
                        }


                        if (agent.neighbours.size() > 0) {

                            Game game = new Game(agent, (Resource) resource);
                            game.begin(false);

                        }
                    }
                }
        }


    }
    class MoveAgents  extends RecursiveAction {
        private final int THRESHOLD = 1000;
        final Cell[][] grid;
        final int lo, hi;
        Random random;

        MoveAgents(Environment env) {
            this.grid = env.grid;
            this.lo = 0;
            this.hi = grid.length-1;
        }


        private MoveAgents(Cell[][] array, int lo, int hi) {
            this.grid = array;
            this.lo = lo;
            this.hi = hi;
        }

        protected void compute() {
            if (hi - lo < THRESHOLD) {
                for (int i = lo; i < hi; ++i)
                    for (int j = lo; j < hi; ++j){
                        if (grid[i][j].getClass() == Agent.class ) {
                            int[] coord = coord(i,j);
                            if (grid[coord[0]][coord[1]].getClass() == Cell.class){
                                grid[coord[0]][coord[1]]= grid[i][j];
                                grid[i][j] = new Cell();
                            }

                        }


                    }


            } else {
                int mid = (lo + hi) >>> 1;
                invokeAll(new MoveAgents(grid, lo, mid), new MoveAgents(grid, mid, hi));
            }
        }

        void serialMove(){
            for (int i = lo; i < hi; ++i)
                for (int j = lo; j < hi; ++j){
                    if (grid[i][j].getClass() == Agent.class ) {
//                        if (((Agent) grid[i][j]).getFitness() >= 0.5){

                        if(true){
                            int[] coord = coord(i, j);
                            if (grid[coord[0]][coord[1]].getClass() == Cell.class) {
                                ((Agent) grid[i][j]).decrementFitness(0.05);
                                grid[coord[0]][coord[1]] = grid[i][j];
                                grid[i][j] = new Cell();

                            }

                        }
                    }
                }
        }


        int[] coord(int x, int y){

            random = new Random();
            int dir = random.nextInt(4);

            if (dir == 0) {
                if (grid[0].length-2 > x)
                    x++;
            } else if (dir == 1){
                if(grid.length-2 > y)
                    y++;

            } else if (dir == 2){
                if (0 < x)
                    x--;

            } else{
                if (0 < y) {
                    y--;
                }

            }
            return new int[]{x,y};

        }


    }
    class parallelSum  extends RecursiveTask<Float> {
        private final int THRESHOLD = 20;
        final Cell[][] grid;
        final int lo, hi;

        private parallelSum(Cell[][] array, int lo, int hi) {
            this.grid = array;
            this.lo = lo;
            this.hi = hi;
        }

        protected Float compute() {

            float sum = 0;
            int range = hi-lo;
            if (hi - lo < THRESHOLD) {
                for (int i = lo; i < hi; ++i)
                    for (int j = lo; j < hi; ++j) {
                        if (grid[i][j].getClass() == Agent.class) {
                            sum += ((Agent) grid[i][j]).getFitness();
                        }
                    }


            } else {
                int mid = lo + range/2;

                parallelSum left = new parallelSum(grid, lo, mid);
                parallelSum right = new parallelSum(grid, mid, hi);

                left.fork();
                float rightSum = right.compute();
                float leftSum = left.join();
                return (rightSum+leftSum);
            }




            return sum;
        }



        float serialSum(){

            float sum= 0;

            for (int i = lo; i < hi; ++i)
                for (int j = lo; j < hi; ++j) {
                    if (grid[i][j].getClass() == Agent.class) {
                        sum += ((Agent) grid[i][j]).getFitness();
                    }
                }

            return sum;

        }

    }


    private final int Movement_Limit = 100;
    Cell[][] grid;
    int MAX_X, MAX_Y;
    NEATNetwork network;
    ConcurrentHashMap networkIDs;
    AtomicBoolean running;
    List<Agent>agents;
    Generation generation;
    private int initial_fitness;




    public Environment(int x, int y){

        agents = Collections.synchronizedList(new ArrayList<>());
        networkIDs = new ConcurrentHashMap();

//        agents = new ConcurrentLinkedQueue<>();
//        agents = new ArrayList<>();

        grid = new Cell[x][y];

        MAX_X = x;
        MAX_Y = y;

        for (int i = 0; i < x; i++)
            for (int j = 0; j < y; j++)
                grid[i][j] =  new Cell();

    }

    public Environment(int x, int y, int initial_fitness){

        agents = Collections.synchronizedList(new ArrayList<>());
        networkIDs = new ConcurrentHashMap();
        this.initial_fitness = initial_fitness;

//        agents = new ConcurrentLinkedQueue<>();
//        agents = new ArrayList<>();

        grid = new Cell[x][y];

        MAX_X = x;
        MAX_Y = y;

        for (int i = 0; i < x; i++)
            for (int j = 0; j < y; j++)
                grid[i][j] =  new Cell();

    }

    public Environment(int x, int y, int totalResources,int totalAgents, NEATNetwork network){


        grid = new Cell[x][y];

        MAX_X = x;
        MAX_Y = y;

        for (int i = 0; i < x; i++)
            for (int j = 0; j < y; j++)
                grid[i][j] =  new Cell();


        this.network = network;
        loadGrid(totalResources,totalAgents);

    }


    public Environment(Environment environment, Generation generation){

        this.generation = generation;
        agents = Collections.synchronizedList(new ArrayList<>());
        networkIDs = new ConcurrentHashMap();
        this.initial_fitness = environment.initial_fitness;
        MAX_X = environment.MAX_X;
        MAX_Y = environment.MAX_Y;
        this.grid = java.util.Arrays.stream(environment.grid).map(el -> el.clone()).toArray($ -> environment.grid.clone());

        if (environment.network != null){
            network = environment.network;
        }

    }

    public Environment(Environment environment){

        generation = environment.generation;
        agents = Collections.synchronizedList(new ArrayList<>());
        networkIDs = new ConcurrentHashMap();
        this.initial_fitness = environment.initial_fitness;
        MAX_X = environment.MAX_X;
        MAX_Y = environment.MAX_Y;
        this.grid = java.util.Arrays.stream(environment.grid).map(el -> el.clone()).toArray($ -> environment.grid.clone());

        if (environment.network != null){
            network = environment.network;
        }

    }


    public void loadGrid(int totalResources,int totalAgents){
        loadResouces(totalResources);
        loadAgents(totalAgents);

    }

    public void loadGrid(int totalResources,int totalAgents, NEATNetwork network){
        loadResouces(totalResources);
        loadAgents(totalAgents, network);

    }

    void loadResouces(int resources){

        int split_1 = Math.floorDiv(resources,3);
        int split_2 = (2*resources)/3;

        Random random = new Random();

        int x = random.nextInt(MAX_X);
        int y = random.nextInt(MAX_Y);

        int counter = 0;
        char type = ' ';
        do{


            if (counter < split_1){
                type = 'A';
            } else if (counter < split_2){
                type = 'B';
            } else if (counter >= split_2){
                type= 'C';
            }

            if (grid[x][y].getClass() == Cell.class){
                counter++;
                grid[x][y] = new Resource(type,true);
            }

            x = random.nextInt(MAX_X);
            y = random.nextInt(MAX_Y);

        } while(counter < resources);

    }

    void loadAgents(int totalAgents){

        Random random = new Random();

        int x = random.nextInt(MAX_X);
        int y = random.nextInt(MAX_Y);
        int counter = 0;
        do{

            if (grid[x][y].getClass() == Cell.class){

                Agent agent = new Agent(counter);
                agent.configureWordMap();
                grid[x][y] = agent;
                agents.add(agent);
                counter++;
            }

            x = random.nextInt(MAX_X);
            y = random.nextInt(MAX_Y);

        } while(counter < totalAgents);

    }

    void loadAgents(int totalAgents, NEATNetwork network){

        Random random = new Random();

        int x = random.nextInt(MAX_X);
        int y = random.nextInt(MAX_Y);
        int counter = 0;
        do{

            if (grid[x][y].getClass() == Cell.class){
                Agent agent = new Agent(counter, network, initial_fitness);
                agent.configureWordMap();
                grid[x][y] = agent;
                agents.add(agent);
                counter++;
            }

            x = random.nextInt(MAX_X);
            y = random.nextInt(MAX_Y);

        } while(counter < totalAgents);

    }


    public MLRegression getNetwork(int hashcode){

        return (MLRegression) networkIDs.get(hashcode);

    }


    public Agent getAgent(int hashcode){

        return (Agent)networkIDs.get(hashcode);
    }



    public List<Agent> getAgents(){

        return agents;

    }

    public String[] getWordList(){

        String[] wordlist = new String[agents.size()];
        Gson gson = new Gson();
        for (Agent agent :  agents){
            JsonElement element = agent.getWordMap();
            element.getAsJsonObject().addProperty("Generation",generation.get());
            wordlist[agent.getId()] = gson.toJson(element);
        }

        return wordlist;

    }

    public String[] getWordList(boolean begin, int trial){

        String status;
        if (begin)
            status="start";
        else
            status="finished";

        String[] wordlist = new String[agents.size()];
        Gson gson = new Gson();
        for (Agent agent : agents) {
            JsonElement element = agent.getWordMap();
            element.getAsJsonObject().addProperty("Generation", generation.get());
            element.getAsJsonObject().addProperty("Status",status);
            element.getAsJsonObject().addProperty("trial",trial);
            wordlist[agent.getId()] = gson.toJson(element);
        }

    return wordlist;

    }


    public void displayGrid(){
        for (Cell[] temp : grid){
            System.out.println(Arrays.toString(temp));
        }

    }

    void displayNumber(){
        System.out.println("Resources: "+count(Resource.class) +"\nAgents: " + count(Agent.class) + "\n");
    }


    public void begin(int MAX){

        int iteration = 0;
        boolean multithreaded = false;


        running = new AtomicBoolean(true);

        while(count(Resource.class) > 0 && iteration < MAX){
            ++iteration;
            move(multithreaded);
            check(multithreaded);

        }


        running.getAndSet(false);

    }

    public void begin(){

        int iteration = 0;
        boolean multithreaded = false;


        running = new AtomicBoolean(true);

        while(count(Resource.class) > 0 && iteration < Movement_Limit){
            ++iteration;
            move(multithreaded);
            check(multithreaded);
            displayNumber();
        }


        running.getAndSet(false);

    }

    public void reset(){

        grid = new Cell[MAX_X][MAX_Y];

        for (int i = 0; i < MAX_X; i++)
            for (int j = 0; j < MAX_Y; j++)
                grid[i][j] =  new Cell();


    }

    public void move(boolean multithreaded){

        MoveAgents moveAgents = new MoveAgents(this);


        if (multithreaded){
            ForkJoinPool forkJoinPool = new ForkJoinPool();
            forkJoinPool.invoke(moveAgents);
        } else {

            moveAgents.serialMove();

        }
    }

    public void check(boolean multithreaded){
        CheckNeighbours checkAgents = new CheckNeighbours(this);


        if (multithreaded){
            ForkJoinPool forkJoinPool = new ForkJoinPool();
            forkJoinPool.invoke(checkAgents);
        } else{

            checkAgents.serialCheck();

        }

    }

    public void add(Agent agent){
        grid[agent.x][agent.y] = agent;

    }

    public int count(Class type) {
        int counter = 0;
        for(int x = 0; x < MAX_X; x++)
        {
            for(int y = 0; y < MAX_Y; y++)
            {
                if(type == grid[x][y].getClass())
                {
                    counter++;
                }
            }
        }

        return counter;
    }

    public int countAgents() {
        int counter = 0;
        for(int x = 0; x < MAX_X; x++)
        {
            for(int y = 0; y < MAX_Y; y++)
            {
                if(grid[x][y].getClass() == Agent.class && ((Agent)grid[x][y]).fitness < 1)
                {
                    counter++;
                }
            }
        }

        return counter;
    }

    public boolean withinGrid(int col, int row){

        if((col < 0) || (row <0) ) {
            return false;
        }

        if((col >= MAX_X) || (row >= MAX_Y)) {
            return false;
        }

        return true;

    }

    public double getFitness(boolean multithreaded){

        if (multithreaded) {
            ForkJoinPool pool = new ForkJoinPool();
            return pool.invoke(new parallelSum(grid, 0, grid.length));
        } else{
            return agents.stream().mapToDouble(Agent::getFitness).sum();


        }
    }


    public double getAverageFitness(){

        return agents.stream().mapToDouble(Agent::getFitness).average().getAsDouble();


    }
    double getVariance(double average){


        return agents.stream()
                .map(i -> i.getFitness() - average)
                .map(i -> i*i)
                .mapToDouble(i -> i).sum()/(agents.size()-1);


    }

    public String getSummaryStatistics(){

        double average = getAverageFitness();

        double variance = getVariance(average);

        return  average + "," + variance;
    }


    @Override
    public void run() {

        begin();

    }
}
