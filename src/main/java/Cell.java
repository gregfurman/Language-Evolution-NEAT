public class Cell {

    int x;
    int y;

    Agent agent;
    Resource resource;

    Cell(){

    }


    public Agent getAgent() {
        return agent;
    }

    public Resource getResource() {
        return resource;
    }

    public boolean hasAgent() {
        return agent != null;
    }

    public boolean hasResource() {
        return resource != null;
    }


    boolean isEmpty(){
        return !hasAgent() && !hasResource();
    }

    public void setAgent(Agent agent) {
        this.agent = agent;
    }

    public void setResource(Resource resource) {
        this.resource = resource;
    }

    public String toString(){

        if (hasAgent())
        return String.format("[%4d]",agent.id);

        return String.format("[%4s]","");
//        StringBuilder sb = new StringBuilder("[ ");
//
//        if (hasAgent() && hasResource())
//        return String.format("[ %d;%c ]",agent.id,resource.type);
//
//
//        if (!hasAgent() && hasResource())
//           return String.format("[  ;%c ]",resource.type);
//
//
//        if (!hasAgent() && !hasResource())
//            return "[  ;  ]";
//
//
//        if (hasAgent() && !hasResource())
//            return String.format("[  %d;  ]",agent.id);
//
//
//        if (hasAgent())
//            sb.append(agent.id);
//
//        sb.append(";");
//
//        if (hasResource())
//            sb.append((resource.type));
//
//
//        sb.append(" ]");
//
//        return sb.toString();




    }

    Cell(int x, int y){
        this.x = x;
        this.y = y;
    }




    public int[] move(){
        return new int[]{x,y};
    }

}
