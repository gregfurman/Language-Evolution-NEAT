import com.google.gson.Gson;
import com.google.gson.JsonElement;
import org.encog.ml.MLRegression;
import org.encog.neural.neat.NEATNetwork;
import org.encog.neural.neat.NEATPopulation;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Stream;


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


        void check(Agent agent,Resource resource,int c, int r){

            int rowStart  = Math.max( r - 1, lo   );
            int rowFinish = Math.min( r + 1, hi );
            int colStart  = Math.max( c - 1, lo   );
            int colFinish = Math.min( c + 1, hi );


            agent.neighbours = new ArrayList<>();

            for ( int row = rowStart; row <= rowFinish; row++ ) {
                for ( int col = colStart; col <= colFinish; col++ ) {

                    if (grid[col][row].hasAgent() &&grid[col][row].getAgent().id != agent.id) {

                        agent.addNeighbour(grid[col][row].getAgent());

                    }

                }
            }


            if (agent.neighbours.size() > 0){
                Game game = new Game(agent,resource);
                game.begin(false);

                grid[c][r].setResource(null);
            }




        }

        void serialMove(){

            for (int i = lo; i < hi+1; ++i)
                for (int j = lo; j < hi+1; ++j){

                    if (grid[i][j].hasAgent()) {

                        Agent agent = new Agent(grid[i][j].getAgent());



                        if (grid[i][j].hasResource()){

                            check(agent,grid[i][j].getResource(),i,j);


                        }


                        int[] coord = coord(i, j);

                        if (!grid[coord[0]][coord[1]].hasAgent()) {

                            agent.decrementFitness(0.05);
                            grid[coord[0]][coord[1]].setAgent(agent);
                            grid[i][j].setAgent(null);


                        }

                        if (grid[coord[0]][coord[1]].hasResource() && grid[coord[0]][coord[1]].hasAgent() ){

                            check(agent,grid[coord[0]][coord[1]].getResource(),coord[0],coord[1]);


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


    private final int Movement_Limit = 1000;
    Cell[][] grid;

    NEATNetwork network;

    List<Agent>agents;
    Generation generation;

    int MAX_X, MAX_Y, AGENT_NO,RESOURCE_NO;
    private int initial_fitness, trialID,envID;



    Config config;




    public Environment(int x, int y){

        agents = Collections.synchronizedList(new ArrayList<>());

        grid = new Cell[x][y];

        MAX_X = x;
        MAX_Y = y;

        for (int i = 0; i < x; i++)
            for (int j = 0; j < y; j++)
                grid[i][j] =  new Cell();

    }

    public Environment(int x, int y, int initial_fitness){

        agents = new ArrayList<>();
        this.initial_fitness = initial_fitness;

        grid = new Cell[x][y];

        MAX_X = x;
        MAX_Y = y;

        for (int i = 0; i < x; i++)
            for (int j = 0; j < y; j++)
                grid[i][j] =  new Cell();

    }

    public Environment(Environment environment, Generation generation){

        this(environment);
        this.generation = generation;

    }

    public Environment(Environment environment, NEATNetwork network){

        this(environment);
        this.network = network;

    }

    public Environment(Environment environment) {

        generation = environment.generation;
        agents = new ArrayList<>();

        trialID = environment.trialID;
        envID = environment.envID;

        this.initial_fitness = environment.initial_fitness;
        MAX_X = environment.MAX_X;
        MAX_Y = environment.MAX_Y;

        grid = new Cell[environment.grid.length][environment.grid[0].length];

        for (int row = 0; row < environment.grid.length; row++)
            for (int col=0; col < environment.grid[0].length; col++) {

                grid[row][col] = new Cell();

                if (environment.grid[row][col].hasAgent()) {
                    Agent agent = new Agent(environment.grid[row][col].getAgent());
                    grid[row][col].setAgent(agent);
                }

                if (environment.grid[row][col].hasResource()) {
                    Resource resource = new Resource(environment.grid[row][col].getResource());
                    grid[row][col].setResource(resource);

                }

        }



        RESOURCE_NO = environment.RESOURCE_NO;
        AGENT_NO = environment.AGENT_NO;
        network = environment.network;
        this.config = environment.config;

    }


    public Environment(Config config){

        this(config.getDim_x(),config.getDim_y(),config.getInitial_fitness());

        this.config = config;

        AGENT_NO = config.getAgent_no();
        RESOURCE_NO = config.getResource_no();

    }


    public Environment(Config config, NEATPopulation population){

        this(config);
        this.network = (NEATNetwork)population.getCODEC().decode(population.getBestGenome());

    }


    public Environment deepcopy(){

        Environment environment = new Environment(this);
        environment.agents = new ArrayList<>();
        for (Agent agent : agents){
            environment.agents.add(new Agent(agent));
        }

        return environment;
    }


    public void loadGrid(){

        loadAgents();
        loadResources();

    }


    private int[] k_quantile(int N, int k){

        int[] quantiles = new int[k];

        for (int i = 0; i < k; i++){
            quantiles[i] = (N-1)*(i+1)/k;
        }

        return quantiles;


    }

    void shuffle(int[][] a) {
        Random random = new Random();

        for (int i = a.length - 1; i > 0; i--) {
            for (int j = a[i].length - 1; j > 0; j--) {
                int m = random.nextInt(i + 1);
                int n = random.nextInt(j + 1);

                int temp = a[i][j];
                a[i][j] = a[m][n];
                a[m][n] = temp;
            }
        }
    }

    void loadResources(int resources){


        if (resources>0) {
            int cells = count(Cell.class);

            resources = cells > resources ? resources : (cells * 80) / 100;

            char[] resource_types = {'A', 'B', 'C', 'D', 'E'};
            int[] quantiles = k_quantile(resources, config.getLanguageNumber());

            Random random = new Random();

            int x = random.nextInt(MAX_X);
            int y = random.nextInt(MAX_Y);

            int counter = 0;
            int index = 0;

            char type = ' ';

            do {


                if (counter <= quantiles[index]) {

                    type = resource_types[index];

                } else {
                    index++;
                }


                if (!grid[x][y].hasResource()) {
                    counter++;
                    grid[x][y].setResource(new Resource(type, true));
                }

                x = random.nextInt(MAX_X);
                y = random.nextInt(MAX_Y);


            } while (counter < resources);


        }
    }

    void loadResources(){

        loadResources(RESOURCE_NO);
    }




    void loadAgents(int totalAgents, NEATNetwork network){

        char[] types = {'A','B','C','D','E'};
        Random random = new Random();
        long seed = random.nextLong();

        int x = random.nextInt(MAX_X);
        int y = random.nextInt(MAX_Y);
        int counter = 0;
        do{

            if (!grid[x][y].hasAgent()){
                Agent agent = new Agent(counter, network, initial_fitness);
                agent.configureWordMap(Arrays.copyOfRange(types,0,config.getLanguageNumber()),seed);
                grid[x][y].setAgent(agent);
                agents.add(agent);
                counter++;
                if (config.getSplit() == 0 || counter == (int)(config.getSplit()*config.getAgent_no())){
                    seed = random.nextLong();
                }

            }

            x = random.nextInt(MAX_X);
            y = random.nextInt(MAX_Y);

        } while(counter < totalAgents);

    }

    void loadAgents(){

        loadAgents(AGENT_NO,network);

    }





    public List<Agent> getAgents(){

        return agents;

    }


    void displayWordList(){

        for (int i=0;i<config.getAgent_no();i++){


            System.out.println(String.format("%d: %s",(i+1),getWordList()[i]));
        }


    }

    public String[] getWordList(){

        String[] wordlist = new String[agents.size()];
        Gson gson = new Gson();
        for (Agent agent :  agents){
            JsonElement element = agent.getWordMap();
            element.getAsJsonObject().addProperty("resources",config.getResource_no());
            wordlist[agent.getId()] = gson.toJson(element);
        }

        return wordlist;

    }

    public String[] getWordList(boolean begin, int trial) {

        String status;
        if (begin)
            status = "start";
        else
            status = "finished";

        String[] wordlist = new String[agents.size()];
        Gson gson = new Gson();

        if (config.isTrain()) {


            for (Agent agent : agents) {
                JsonElement element = agent.getWordMap();
                element.getAsJsonObject().addProperty("Generation", generation.get());
                element.getAsJsonObject().addProperty("Status", status);
                element.getAsJsonObject().addProperty("trial", trial);
                element.getAsJsonObject().addProperty("resources", config.getResource_no());
                wordlist[agent.getId()] = gson.toJson(element);
            }
        } else {

            for (Agent agent : agents) {
                JsonElement element = agent.getWordMap();
                element.getAsJsonObject().addProperty("Status", status);
                element.getAsJsonObject().addProperty("trial", trial);
                element.getAsJsonObject().addProperty("resources", config.getResource_no());
                element.getAsJsonObject().addProperty("split", config.getSplit());
                element.getAsJsonObject().addProperty("dimension", config.getDim_x()*config.getDim_y());


                wordlist[agent.getId()] = gson.toJson(element);

            }

        }


        return wordlist;

    }

    public String[] getWordList(int trial, int env) {

        String[] wordlist = new String[agents.size()];
        Gson gson = new Gson();

        if (config.isTrain()) {

            for (Agent agent : agents) {
                JsonElement element = agent.getWordMap();
                element.getAsJsonObject().addProperty("Generation", generation.get());
                element.getAsJsonObject().addProperty("trial", trial);
                element.getAsJsonObject().addProperty("resources", config.getResource_no());
                element.getAsJsonObject().addProperty("environment", env);

                wordlist[agent.getId()] = gson.toJson(element);
            }
        } else {

            for (Agent agent : agents) {
                JsonElement element = agent.getWordMap();
                element.getAsJsonObject().addProperty("trial", trial);
                element.getAsJsonObject().addProperty("resources", config.getResource_no());
                element.getAsJsonObject().addProperty("environment", env);
                element.getAsJsonObject().addProperty("split", config.getSplit());
                element.getAsJsonObject().addProperty("world", config.getDim_x()*config.getDim_y());


                wordlist[agent.getId()] = gson.toJson(element);

            }

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

        int prev = count(Resource.class);

        boolean overflow = prev < config.getResource_no();  //config.getResource_no() + config.getAgent_no() > config.getDim_x()*config.getDim_y();

        while(count(Resource.class) > 0 && iteration < MAX){
            ++iteration;
            move(multithreaded);

        }


        if (overflow){

            int res_count = count(Resource.class);

            int remainder = config.getResource_no()-res_count;
            int difference = prev - res_count;


           for (int i =0;i<10;i++)
            {
//                System.out.println(difference +" " +count(Resource.class) + " " + remainder + " " + prev);

                remainder -= difference;
                loadResources(difference);


                while (count(Resource.class) > 0 && iteration < MAX) {
                    ++iteration;
                    move(multithreaded);
//                    check(multithreaded);
                }


                difference = prev - count(Resource.class);

                if (remainder == 0)
                    break;


            }



        }

        collectAgents();




    }


    void collectAgents(){


        for (int row = 0; row < grid.length; row++)
            for (int col=0; col < grid[0].length; col++) {

                if (grid[row][col].hasAgent()) {
                    agents.add(grid[row][col].getAgent());
                }


            }



    }


    public void begin(){

        begin(Movement_Limit);


        System.out.println(String.format("Trial = %d, Environment = %d, Population = %d, Resources = %d, Size = %d",trialID,envID,config.agent_no,config.resource_no,config.getDim_x()*config.getDim_y()));

        StatsRecorder wordlist = config.getWordList();

        for (String word : getWordList(trialID, envID)) {
            wordlist.write(word);
        }

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

        if (type == Agent.class)
            return (int) Arrays.stream(grid).flatMap(Arrays::stream).filter(Cell::hasAgent).count();
        else if (type == Resource.class)
            return (int) Arrays.stream(grid).flatMap(Arrays::stream).filter(Cell::hasResource).count();

        return (int) Arrays.stream(grid).flatMap(Arrays::stream).count();
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



    public void setNetwork(MLRegression method){
        for (Agent agent: agents
             ) {agent.setNetwork(method);

        }

    }

    void setID(int envID, int trialID){

        this.envID = envID;
        this.trialID = trialID;

    }


    @Override
    public void run() {

        begin();



    }
}
