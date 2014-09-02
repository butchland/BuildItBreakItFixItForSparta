testcustomtotaltime: logappend logread
	./logappend -T 1 -K secret -A -E Fred testcustomtotaltime
	./logappend -T 3 -K secret -A -E Fred -R 1 testcustomtotaltime
	./logappend -T 5 -K secret -L -E Fred -R 1 testcustomtotaltime
	./logappend -T 6 -K secret -L -E Fred testcustomtotaltime
	./logappend -T 7 -K secret -A -E Fred testcustomtotaltime
	./logappend -T 11 -K secret -L -E Fred testcustomtotaltime
	./logread -K secret -T -E Fred testcustomtotaltime

clean:
	rm testcustom*