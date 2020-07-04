import org.encog.ml.MLMethod;
import org.encog.ml.ea.genome.Genome;
import org.encog.neural.neat.NEATNetwork;

public class NetworkID {

    MLMethod network;
    int networkHash;
    Agent agent;


    public NetworkID(MLMethod network){

        this.network = network;
        this.networkHash = this.network.hashCode();

    }

    public NetworkID(MLMethod network, Agent agent){

        this.network = network;
        this.networkHash = this.network.hashCode();
        this.agent = agent;

    }



    public int getNetworkHash() {
        return networkHash;
    }

    public MLMethod getNetwork() {
        return network;
    }
}
