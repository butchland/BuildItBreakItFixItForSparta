SHELL = bash

.PHONY: test
test: testcustomtotaltime testcustomexample testcustombatch

testcustomtotaltime: logappend logread
	./logappend -T 1 -K secret -A -E Fred testcustomtotaltime
	./logappend -T 3 -K secret -A -E Fred -R 1 testcustomtotaltime
	./logappend -T 5 -K secret -L -E Fred -R 1 testcustomtotaltime
	./logappend -T 6 -K secret -L -E Fred testcustomtotaltime
	./logappend -T 7 -K secret -A -E Fred testcustomtotaltime
	./logappend -T 11 -K secret -L -E Fred testcustomtotaltime
	./logread -K secret -T -E Fred testcustomtotaltime > tmp
	echo -ne "9" > tmp1
	diff tmp tmp1

testcustomexample: logappend logread
	./logappend -T 1 -K secret -A -E Fred testcustomexample
	./logappend -T 2 -K secret -A -G Jill testcustomexample
	./logappend -T 3 -K secret -A -E Fred -R 1 testcustomexample
	./logappend -T 4 -K secret -A -G Jill -R 1 testcustomexample
	./logread -K secret -S testcustomexample > tmp2
	echo -ne "Fred\nJill\n1: Fred,Jill" > tmp3
	diff tmp2 tmp3

	./logappend -T 5 -K secret -L -E Fred -R 1 testcustomexample
	./logappend -T 6 -K secret -A -E Fred -R 2 testcustomexample
	./logappend -T 7 -K secret -L -E Fred -R 2 testcustomexample
	./logappend -T 8 -K secret -A -E Fred -R 3 testcustomexample
	./logappend -T 9 -K secret -L -E Fred -R 3 testcustomexample
	./logappend -T 10 -K secret -A -E Fred -R 1 testcustomexample
	./logread -K secret -R -E Fred testcustomexample > tmp4
	echo -ne "1,2,3,1" > tmp5
	diff tmp4 tmp5

testcustombatch: logappend logread
	echo -ne "-K secret -T 0 -A -E John testcustombatch\n-K secret -T 1 -A -R 0 -E John testcustombatch\n-K secret -T 2 -A -G James testcustombatch\n-K secret -T 3 -A -R 0 -G James testcustombatch" > testcustombatchfile
	./logappend -B testcustombatchfile
	./logread -K secret -S testcustombatch > tmp6
	echo -ne "John\nJames\n0: James,John" > tmp7
	diff tmp6 tmp7

clean:
	rm testcustom* tmp*