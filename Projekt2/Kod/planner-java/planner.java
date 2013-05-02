
public class planner {
	public static void main (String args[]) {
        // Read the data
        String holding = args[0];
        String world = args[1];
        String[] trees = args[2].split(";");

        // Print the data
		System.out.println("# Stupid Java planner!");
		System.out.println("# Holding: " + holding);
		System.out.println("# World: " + world);
        for (String t : trees) {
            System.out.println("# Tree: " + t);
        }

        // Print the plan
		System.out.println("This is a stupid move!");
        int stacknr = 0;
        for (int i = 0; i < world.length(); i++) {
            if (world.charAt(i) == ';') 
                stacknr++;
            else if (world.charAt(i) != ' ') 
                break;
        }
		System.out.println("pick " + stacknr);
		System.out.println("drop " + stacknr);
	}
}
