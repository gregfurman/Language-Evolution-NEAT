import java.util.ArrayList;
import java.util.HashMap;

import static java.util.Comparator.comparing;


public class Game implements Runnable{

    Resource resource;
    ArrayList<Agent> agents;


    public Game(Agent agent, Resource resource){

        this.agents = agent.neighbours;
        this.agents.add(agent);

        this.resource = resource;

    }


    public void loadGame(ArrayList<Agent> agents,Resource resource){

        for(Agent agent : agents){
            agent.neighbours = new ArrayList<>(agents);
            agent.neighbours.remove(agent);
            agent.setResource(resource);
        }


    }


    public void begin(boolean multithreaded){

        loadGame(agents,resource);


        if (multithreaded){

            Thread[] threads = new Thread[agents.size()];

            int index = 0;
            for (Thread thread: threads){
                thread = new Thread(agents.get(index));
                index++;
                thread.start();
                try {
                    thread.join();
                } catch (InterruptedException e){
                    System.out.println("Thread error during naming game.");
                }
        }


        } else{

            for (Agent agent : agents){
                agent.NamingGame();
            }

        }


        Agent winner = agents.stream().max(comparing(Agent::getBid)).get();
        winner.consume(resource.reward*((double) (agents.size()-1)/(double)agents.size()),true);

        agents.remove(winner);


        for (Agent agent : agents){
            agent.consume((winner.getBid()/agents.size())+resource.reward*((1/(double)(agents.size()+1))/(double) agents.size()),false);
            agent.setTerm(resource.type,winner.getTerm(resource.type));
            agent.clearNeighbours();
        }




    }




    public void run(){

        begin(true);

    }


}
