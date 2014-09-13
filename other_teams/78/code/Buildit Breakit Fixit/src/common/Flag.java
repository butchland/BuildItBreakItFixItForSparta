package common;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;


public abstract class Flag {

	private int numParams;
	
	public Flag(int numParams) {
		this.numParams = numParams;
	}
	
	public int getNumParams() {
		return numParams;
	}
	
	public void behavior(String[] params) {
		if(params.length != this.getNumParams()) {
			System.out.println("invalid");
			System.exit(-1);
		}
	}
	
	public static void processArgs(String[] args,HashMap<String, Flag> flags) {
		
		//int i = 0;

		ArrayList<Integer> flagIndices = new ArrayList<Integer>();
		
		for(int i = 0; i < args.length; i++) {
			
			String arg = args[i];
			
			if(arg.indexOf('-') != -1) {
				flagIndices.add(i);
			}
		}
		
		flagIndices.add(args.length);
		
		for(int i = 0; i < flagIndices.size() - 1; i++) {
			int ind1 = flagIndices.get(i);
			int ind2 = flagIndices.get(i+1);
			
			String[] params = new String[ind2 - ind1 - 1];
			
			for(int j = ind1 + 1; j < ind2; j++) {
				params[j - ind1 - 1] = args[j];
			}
			
			Flag curFlag = flags.get(args[ind1]);
			
			if(curFlag != null) {
				curFlag.behavior(params);
				flags.remove(curFlag);
			}
			else {
				System.out.println("invalid");
				System.exit(-1);
			}
		}
	}
}
