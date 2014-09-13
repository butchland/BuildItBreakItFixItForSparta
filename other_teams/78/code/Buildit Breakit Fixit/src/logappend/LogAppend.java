package logappend;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;

import common.Flag;
import common.PersonData;

public class LogAppend {

	private static final int NUM_FLAGS = 8;

	private HashMap<String, Flag> flags;
	private int curTime;
	private HashMap<String, ArrayList<PersonData>> logData;

	private String logName;
	private String name;

	private PersonData personData;
	private String key;
	private String batchName;
	private boolean isBatch;

	public LogAppend() {

		curTime = 0;
		flags = new HashMap<String, Flag>(NUM_FLAGS);
		logData = null;
		personData = new PersonData();
		batchName = null;
		isBatch = false;

		flags.put("-T", new Flag(1) {

			public void behavior(String [] params) {
				super.behavior(params);

				try {
					personData.setTime(Integer.parseInt(params[0]));
				} catch(NumberFormatException e) {
					System.out.println("invalid");
					System.exit(-1);
				}

				if(personData.getTime() < curTime) {
					System.out.println("invalid");
					System.exit(-1);
				}
			}
		});

		flags.put("-K", new Flag(1) {

			public void behavior(String [] params) {
				super.behavior(params);

				if(!params[0].matches("^[a-zA-Z0-9]+$")) {
					System.out.println("invalid");
					System.exit(-1);
				}

				key = params[0];
			}
		});

		flags.put("-E", new Flag(1) {

			public void behavior(String [] params) {
				super.behavior(params);

				if(!params[0].matches("^[a-zA-Z]+$")) {
					System.out.println("invalid");
					System.exit(-1);
				}

				if(personData.getStatus() != PersonData.Status.NOTHING) {
					System.out.println("invalid");
					System.exit(-1);
				}
				personData.setStatus(PersonData.Status.EMPLOYEE);

				name = params[0];
			}
		});

		flags.put("-G", new Flag(1) {

			public void behavior(String [] params) {
				super.behavior(params);

				if(!params[0].matches("^[a-zA-Z]+$")) {
					System.out.println("invalid");
					System.exit(-1);
				}

				if(personData.getStatus() != PersonData.Status.NOTHING) {
					System.out.println("invalid");
					System.exit(-1);
				}
				personData.setStatus(PersonData.Status.GUEST);
				name = params[0];
			}
		});

		flags.put("-A", new Flag(0) {

			public void behavior(String [] params) {
				super.behavior(params);

				if(personData.getAction() != PersonData.Action.UNKNOWN) {
					System.out.println("invalid");
					System.exit(-1);
				}

				personData.setAction(PersonData.Action.ARRIVING);
			}
		});

		flags.put("-L", new Flag(0) {

			public void behavior(String [] params) {
				super.behavior(params);

				if(personData.getAction() != PersonData.Action.UNKNOWN) {
					System.out.println("invalid");
					System.exit(-1);
				}

				personData.setAction(PersonData.Action.LEAVING);
			}
		});

		flags.put("-R", new Flag(1) {

			public void behavior(String [] params) {
				super.behavior(params);

				try {
					personData.setRoom(Integer.parseInt(params[0]));
				} catch(NumberFormatException e) {
					System.out.println("invalid");
					System.exit(-1);
				}

				if(personData.getRoom() < 0) {
					System.out.println("invalid");
					System.exit(-1);
				}
			}
		});

		flags.put("-B", new Flag(1) {
			public void behavior(String [] params) {
				super.behavior(params);

				if(isBatch) {
					System.out.println("invalid");
					System.exit(-1);
				}

				batchName = params[0];
			}
		});
	}

	public LogAppend(boolean isBatch) {
		this();
		this.isBatch = isBatch;
	}

	public void processArguments(String[] args) throws IOException, ClassNotFoundException {

		if(args.length == 0) {
			System.out.println("invalid");
			System.exit(-1);
		}
		if(!args[0].equals("-B")) {
		
			logName = args[args.length - 1];
			File log = new File(logName);
			String logKey = "";

			if(log.exists()) {

				FileInputStream in = new FileInputStream(logName);

				ObjectInputStream oIn = new ObjectInputStream(in);

				logKey = (String) oIn.readObject();

				curTime = (Integer) oIn.readObject();
				logData = (HashMap<String, ArrayList<PersonData>>) oIn.readObject();

				in.close();
				oIn.close();
			}

			String[] flagArgs = new String[args.length - 1];

			for(int i = 0; i < args.length - 1; i++) {
				flagArgs[i] = args[i];
			}

			Flag.processArgs(flagArgs, flags);

			if(!log.exists()) {
				log.createNewFile();
				logData = new HashMap<String, ArrayList<PersonData>>();
			}
			else {
				if(!key.equals(logKey)) {
					System.err.println("security error");
					System.exit(-1);
				}
			}

		}
		else {
			Flag.processArgs(args, flags);
			File batch = new File(batchName);
			if(batch.exists()) {

				Scanner batchIn = new Scanner(batch);

				while(batchIn.hasNextLine()) {
					String line = batchIn.nextLine();
					String[] list = line.split(" +");
					String[] inArgs = new String[list.length - 1];

					for(int i = 1; i < list.length; i++) {
						inArgs[i - 1] = list[i];
					}

					LogAppend logappend = new LogAppend(true);
					try {
						logappend.processArguments(inArgs);
						logappend.writeDataToLog();
					} catch(Exception e) {
						e.printStackTrace();
						System.out.println("invalid");
						System.exit(-1);
					}
				}
			}

			System.exit(0);
		}
	}

	public void writeDataToLog() throws Exception {
		ArrayList<PersonData> curData = logData.get(name);

		if(curData != null) {

			if(personData.getRoom() == PersonData.NO_ROOM) {

				if(curData.size() > 1) {
					// Case: person leaving gallery
					/*if(personData.getAction() == PersonData.Action.LEAVING) {
						if(curData.get(curData.size() - 1).getAction() != PersonData.Action.LEAVING) {
							System.out.println("invalid person leaving");
							System.exit(-1);
						}
					}
					// Case: person re-entering gallery
					else if(personData.getAction() == PersonData.Action.ARRIVING) {
						if(!(curData.get(curData.size() - 1).getAction() == PersonData.Action.LEAVING && curData.get(curData.size() - 1).getRoom() == PersonData.NO_ROOM)) {
							System.out.println("invalid person reentering");
							System.exit(-1);
						}
					}*/
				}
			}

			curData.add(personData);
		}
		else {

			ArrayList<PersonData> personList = new ArrayList<PersonData>();
			personList.add(personData);
			logData.put(name, personList);
		}

		FileOutputStream out = new FileOutputStream(logName);
		ObjectOutputStream oos;

		oos = new ObjectOutputStream(out);
		oos.writeObject(key);
		oos.writeObject(personData.getTime());
		oos.writeObject(logData);
		oos.close();
		out.close();
	}
	public static void main(String[] args) {
		LogAppend logappend = new LogAppend();
		try {
			logappend.processArguments(args);
			logappend.writeDataToLog();
		} catch(Exception e) {
			e.printStackTrace();
			System.out.println("invalid");
			System.exit(-1);
		}
	}
}
