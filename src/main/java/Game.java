import java.util.*;
import java.util.stream.Collectors;

import static java.util.Comparator.comparing;


public class Game{

    Resource resource;
    ArrayList<Agent> agents;
    final double noise = 0.005;


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


    public void begin(String type){


        switch(type){

            case "frequency":
                Random random = new Random();
                String freq_word;

                List<String> terms = agents.stream().map(a -> a.getTerm(resource.type)).collect(Collectors.toList());


                if (random.nextFloat() > noise) { // Noise parameter



                    HashMap<String, Integer> freq = new HashMap<>();

                    for (String term : terms) {

                        if (freq.containsKey(term)) {
                            freq.put(term, freq.get(term) + 1);
                        } else {
                            freq.put(term, 1);
                        }

                    }


                    freq_word = freq.entrySet().stream().max((entry1, entry2) -> entry1.getValue() > entry2.getValue() ? 1 : -1).get().getKey();

                } else{

                    freq_word=terms.get(random.nextInt(terms.size()));
                }

                for (Agent agent : agents){
                    agent.setTerm(resource.type,freq_word);
                }
                break;

            case "bidding":
                loadGame(agents,resource);


                Agent winner = agents.stream().max(comparing(Agent::getBid)).get();
                winner.consume(resource.reward*((double) (agents.size()-1)/(double)agents.size()),true);

                agents.remove(winner);


                for (Agent agent : agents){
                    agent.consume((winner.getBid()/agents.size())+resource.reward*((1/(double)(agents.size()+1))/(double) agents.size()),false);
                    agent.setTerm(resource.type,winner.getTerm(resource.type));
                    agent.clearNeighbours();
                }
                break;

        }


    }






}
