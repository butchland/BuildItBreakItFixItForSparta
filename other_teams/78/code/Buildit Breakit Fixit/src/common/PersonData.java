package common;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

public class PersonData implements Serializable {

	private static final long serialVersionUID = 1439499645224516320L;
	public static int NO_TIME = -1;
	public static int NO_ROOM = -1;
	
	private int time;
	private int room;
	private Status status;
	private Action action;

	public PersonData() {
		this(NO_TIME,NO_ROOM,Status.NOTHING, Action.UNKNOWN);
	}
	
	public PersonData(int t, int r, Status s, Action a) {
		time = t;
		room = r;
		status = s;
		action = a;
	}

	public int getTime() {
		return time;
	}

	public int getRoom() {
		return room;
	}

	public Status getStatus() {
		return status;
	}

	public Action getAction() {
		return action;
	}

	public void setTime(int time) {
		this.time = time;
	}

	public void setRoom(int room) {
		this.room = room;
	}

	public void setStatus(Status status) {
		this.status = status;
	}

	public void setAction(Action action) {
		this.action = action;
	}

	
	private void readObject(ObjectInputStream aInputStream) throws ClassNotFoundException, IOException {
		//always perform the default de-serialization first
		aInputStream.defaultReadObject();
		time = aInputStream.readInt();
		room = aInputStream.readInt();
		status = Status.values()[aInputStream.readInt()];
		//validateData();
	}

	private void writeObject(ObjectOutputStream aOutputStream) throws IOException {
		aOutputStream.defaultWriteObject();
		aOutputStream.writeInt(time);
		aOutputStream.writeInt(room);
		aOutputStream.writeInt(status.ordinal());
	}
	
	public enum Status {
		NOTHING, GUEST, EMPLOYEE;
	}
	
	public enum Action {
		UNKNOWN, ARRIVING, LEAVING;
	}
}
