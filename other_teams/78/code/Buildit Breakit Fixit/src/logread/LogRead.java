package logread;

import java.io.File;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

import common.Flag;
import common.PersonData;

public class LogRead {

	private HashMap<String, Flag> flags;
	private HashMap<String, ArrayList<PersonData>> logData;
	private String key;
	private boolean html;
	private PersonData targetData;
	private String name;
	private Task task;
	private int lowerBound;
	private int upperBound;

	public LogRead() {

		flags = new HashMap<String, Flag>(6);
		logData = null;
		targetData = new PersonData();
		html = false;
		key = null;
		lowerBound = -1;
		upperBound = -1;
		task = Task.NOTHING;

		flags.put("-S", new Flag(0) {

			public void behavior(String [] params) {
				super.behavior(params);

				if(task != Task.NOTHING) {
					System.out.println("invalid");
					System.exit(-1);
				}

				task = Task.STATE;
			}
		});

		flags.put("-R", new Flag(0) {

			public void behavior(String [] params) {
				super.behavior(params);

				if(task != Task.NOTHING) {
					System.out.println("invalid");
					System.exit(-1);
				}

				task = Task.ROOMS;
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

		flags.put("-H", new Flag(0) {

			public void behavior(String [] params) {
				super.behavior(params);

				if(task == Task.TIME) {
					System.out.println("invalid");
					System.exit(-1);
				}

				html = true;
			}
		});

		flags.put("-E", new Flag(1) {

			public void behavior(String [] params) {
				super.behavior(params);

				if(!params[0].matches("^[a-zA-Z]+$")) {
					System.out.println("invalid");
					System.exit(-1);
				}

				if(targetData.getStatus() != PersonData.Status.NOTHING) {
					System.out.println("invalid");
					System.exit(-1);
				}
				targetData.setStatus(PersonData.Status.EMPLOYEE);
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

				if(targetData.getStatus() != PersonData.Status.NOTHING) {
					System.out.println("invalid");
					System.exit(-1);
				}
				targetData.setStatus(PersonData.Status.GUEST);
				name = params[0];
			}
		});

		flags.put("-T", new Flag(0) {

			public void behavior(String [] params) {
				super.behavior(params);

				if(task != Task.NOTHING || html) {
					System.out.println("invalid");
					System.exit(-1);
				}

				task = Task.TIME;
			}
		});

		flags.put("-A", new Flag(0) {

			public void behavior(String [] params) {
				super.behavior(params);

				if(task != Task.NOTHING) {
					System.out.println("invalid");
					System.exit(-1);
				}

				task = Task.BOUNDS;
			}
		});

		flags.put("-L", new Flag(1) {

			public void behavior(String [] params) {
				super.behavior(params);

				try {
					lowerBound = Integer.parseInt(params[0]);
				} catch(NumberFormatException e) {
					System.out.println("invalid");
					System.exit(-1);
				}
			}
		});

		flags.put("-U", new Flag(1) {

			public void behavior(String [] params) {
				super.behavior(params);

				try {
					upperBound = Integer.parseInt(params[0]);
				} catch(NumberFormatException e) {
					System.out.println("invalid");
					System.exit(-1);
				}
			}
		});
	}

	public void processArguments(String[] args) throws Exception {

		if(args.length == 0) {
			System.out.println("invalid");
			System.exit(-1);
		}

		String[] flagArgs = new String[args.length - 1];

		for(int i = 0; i < args.length - 1; i++) {
			flagArgs[i] = args[i];
		}

		Flag.processArgs(flagArgs, flags);

		String logName = args[args.length - 1];
		File log = new File(logName);

		if(!log.exists()) {
			log.createNewFile();
			logData = new HashMap<String, ArrayList<PersonData>>();
		}
		else {

			FileInputStream in = new FileInputStream(logName);

			ObjectInputStream oIn = new ObjectInputStream(in);
			String logKey = (String)oIn.readObject();

			if(!key.equals(logKey)) {
				System.err.println("integrity violation");
				System.exit(-1);
			}

			oIn.readObject();
			logData = (HashMap<String, ArrayList<PersonData>>) oIn.readObject();

			in.close();
			oIn.close();
		}
	}

	public void printLog() {

		if(task == Task.NOTHING) {
			System.out.println("invalid");
			System.exit(-1);	
		}
		else if(task == Task.ROOMS) {
			if(targetData.getStatus() == PersonData.Status.NOTHING) {
				System.out.println("invalid");
				System.exit(-1);
			}

			ArrayList<PersonData> foundData = logData.get(name);

			if(html) {
				System.out.println("<html>\n<body>\n<table>\n<tr>\n  <th>Rooms</th>\n</tr>");
			}

			if(foundData != null) {

				if(foundData.size() > 0) {

					ArrayList<PersonData> filteredRooms = new ArrayList<PersonData>();


					for(int i = 0; i < foundData.size(); i++) {
						if(foundData.get(i).getRoom() != PersonData.NO_ROOM && foundData.get(i).getAction() == PersonData.Action.ARRIVING) {
							filteredRooms.add(foundData.get(i));
						}
					}

					for(int i = 0; i < filteredRooms.size() - 1; i++) {
						if(html) {
							System.out.println("<tr>\n  <td>" + filteredRooms.get(i).getRoom() + "</td>\n</tr>");
						}
						else {
							System.out.print(filteredRooms.get(i).getRoom() + ",");
						}
					}
					if(html) {
						System.out.println("<tr>\n  <td>" + filteredRooms.get(filteredRooms.size() - 1).getRoom() + "</td>\n</tr>");
					}
					else {
						System.out.println(filteredRooms.get(filteredRooms.size() - 1).getRoom());
					}
				}
			}

			if(html) {
				System.out.println("</table>\n</body>\n</html>");
			}

		}
		else if(task == Task.TIME) {

			if(targetData.getStatus() == PersonData.Status.NOTHING) {
				System.out.println("invalid");
				System.exit(-1);
			}

			ArrayList<PersonData> dataList = logData.get(name);

			if(dataList != null) {
				if(targetData.getStatus() == dataList.get(0).getStatus()) {
					int startTime = dataList.get(0).getTime();
					int endTime = dataList.get(dataList.size() - 1).getTime();
					System.out.println(endTime - startTime);
				}
			}
		}
		else if(task == Task.BOUNDS) {

			if(lowerBound < 0 || upperBound < 0 || upperBound < lowerBound) {
				System.out.println("invalid");
				System.exit(-1);
			}

			if(html) {
				System.out.println("<html>\n<body>\n<table>\n<tr>\n  <th>Employees</th>\n</tr>");
			}

			ArrayList<String> keyList = new ArrayList<String>(logData.keySet().size());

			for(String key : logData.keySet()) {
				keyList.add(key);
			}

			Collections.sort(keyList);
			ArrayList<String> filteredList = new ArrayList<String>();

			for(String key : keyList) {

				int enterTime = Integer.MAX_VALUE;
				int exitTime = Integer.MAX_VALUE;

				for(PersonData dataPoint : logData.get(key)) {

					if(dataPoint.getStatus() == PersonData.Status.EMPLOYEE) {
						if(enterTime == Integer.MAX_VALUE && dataPoint.getAction() == PersonData.Action.ARRIVING && dataPoint.getRoom() == PersonData.NO_ROOM) {
							enterTime = dataPoint.getTime();
						}
						else if(dataPoint.getAction() == PersonData.Action.LEAVING && dataPoint.getRoom() == PersonData.NO_ROOM) {
							exitTime = dataPoint.getTime();
							break;
						}
					}
				}

				//System.out.println(key + ": Enter time " + enterTime + " vs. " + lowerBound + "\nExit time " + exitTime + " vs. " + upperBound);
				if(enterTime <= upperBound && exitTime >= lowerBound) {
					filteredList.add(key);
				}
			}

			if(filteredList.size() > 0) {
				for(int i = 0; i < filteredList.size() - 1; i++) {
					if(html) {
						System.out.println("<tr>\n  <td>" + filteredList.get(i) + "</td>\n</tr>");
					}
					else {
						System.out.print(filteredList.get(i) + ",");
					}
				}
				if(html) {
					System.out.println("<tr>\n  <td>" + filteredList.get(filteredList.size() - 1) + "</td>\n</tr>");
				}
				else {
					System.out.println(filteredList.get(filteredList.size() - 1));
				}
			}

			if(html) {
				System.out.println("</table>\n</body>\n</html>");
			}

		}
		else {

			ArrayList<String> employees = new ArrayList<String>();
			ArrayList<String> guests = new ArrayList<String>();
			HashMap<Integer, ArrayList<String>> rooms = new HashMap<Integer, ArrayList<String>>();

			ArrayList<String> keyList = new ArrayList<String>(logData.keySet().size());

			for(String key : logData.keySet()) {
				keyList.add(key);
			}

			Collections.sort(keyList);

			for(String key : keyList) {
				ArrayList<PersonData> data = logData.get(key);
				if((data.get(data.size() - 1).getAction() == PersonData.Action.ARRIVING && data.get(data.size() - 1).getRoom() == PersonData.NO_ROOM) || data.get(data.size() - 1).getRoom() != PersonData.NO_ROOM) {
					if(data.get(0).getStatus() == PersonData.Status.EMPLOYEE) {
						employees.add(key);
					}
					else if(data.get(0).getStatus() == PersonData.Status.GUEST) {
						guests.add(key);
					}
				}

				if(data.get(data.size() - 1).getRoom() != PersonData.NO_ROOM) {
					ArrayList<String> names = rooms.get(data.get(data.size() - 1).getRoom());
					if(names == null) {
						rooms.put(data.get(data.size() - 1).getRoom(), new ArrayList<String>());
						names = rooms.get(data.get(data.size() - 1).getRoom());
					}
					names.add(key);
				}
			}

			if(html) {
				System.out.println("<html>\n<body>\n<table>");
				System.out.println("<tr>\n  <th>Employee</th>\n  <th>Guest</th>\n</tr>");
				System.out.println("<tr>");
				System.out.print("  <td>");
			}

			if(employees.size() > 0) {

				for(int i = 0; i < employees.size() - 1; i++) {
					System.out.print(employees.get(i) + ",");
				}
				System.out.print(employees.get(employees.size() - 1));

				if(!html) {
					System.out.println();
				}
			}

			if(html) {
				System.out.println("</td>");
				System.out.print("  <td>");
			}

			if(guests.size() > 0) {

				for(int i = 0; i < guests.size() - 1; i++) {
					System.out.print(guests.get(i) + ",");
				}
				System.out.print(guests.get(guests.size() - 1));

				if(!html) {
					System.out.println();
				}
			}

			if(html) {
				System.out.println("</td>");
				System.out.println("</tr>\n</table>\n<table>");
				System.out.println("<tr>\n  <th>Room ID</th>\n  <th>Occupants</th>\n</tr>\n<tr>");
			}

			if(rooms.size() > 0) {

				ArrayList<Integer> idKeyList = new ArrayList<Integer>(rooms.keySet().size());

				for(int key : rooms.keySet()) {
					idKeyList.add(key);
				}

				Collections.sort(idKeyList);

				for(int roomID : idKeyList) {

					if(html) {
						System.out.print("  <td>" + roomID + "</td>\n  <td>");
					}
					else {
						System.out.print(roomID + ": ");
					}

					ArrayList<String> roomNames = rooms.get(roomID);

					for(int i = 0; i < roomNames.size() - 1; i++) {
						System.out.print(roomNames.get(i) + ",");
					}
					System.out.print(roomNames.get(roomNames.size() - 1));

					if(html) {
						System.out.println("</td>");
					}
					else {
						System.out.println();
					}
				}
			}

			if(html) {
				System.out.println("</tr>\n</table>\n</body>\n</html>");
			}

		}
	}

	private enum Task {
		NOTHING, STATE, ROOMS, TIME, BOUNDS;
	}

	public static void main(String[] args) {
		LogRead logread = new LogRead();
		try {
			logread.processArguments(args);
			logread.printLog();
		} catch (Exception e) {
			System.out.println("invalid");
			System.exit(-1);
		}
	}
}
