SHELL = bash

testclean: test clean
test: testcustomtotaltime testcustomexample testcustombatch testcustomsecurity testcustomallbound


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

testcustomsecurity: logappend logread
	./logappend -T 1 -K secret -A -E Fred testcustomsecurity
	./logappend -T 2 -K secret -A -G Jill testcustomsecurity
	./logappend -T 3 -K secret -A -E Fred -R 1 testcustomsecurity
	-./logread -K CLEARLYNOTTHESECRET -S testcustomsecurity 2> tmp8
	echo -ne "integrity violation" > tmp9
	diff tmp8 tmp9

	-./logappend -T 4 -K CLEARLYNOTTHESECRET -A -G Jill -R 1 testcustomsecurity 2> tmp10
	echo -ne "security error" > tmp11
	diff tmp10 tmp11

testcustomallbound: logappend logread
	./logappend -T 1 -K secret -A -E Fred testcustomallbound
	./logappend -T 2 -K secret -A -G Jill testcustomallbound
	./logappend -T 5 -K secret -L -E Fred testcustomallbound
	./logappend -T 9 -K secret -A -E Bob testcustomallbound
	./logappend -T 11 -K secret -L -E Bob testcustomallbound
	./logappend -T 12 -K secret -A -E Fred testcustomallbound
	./logappend -T 29 -K secret -A -G Swagger testcustomallbound
	./logread -K secret -A -L 1 -U 13 testcustomallbound > tmp12
	echo -ne "Bob,Fred" > tmp13
	diff tmp12 tmp13

	./logread -K secret -A -L 5 -U 8 testcustomallbound > tmp14
	echo -ne "Fred" > tmp15
	diff tmp14 tmp15

	./logread -K secret -A -L 6 -U 9 testcustomallbound > tmp16
	echo -ne "Bob" > tmp17
	diff tmp16 tmp17

	./logread -K secret -A -L 17 -U 20 testcustomallbound > tmp18
	diff tmp18 tmp15

	./logread -K secret -A -L 50 -U 51 testcustomallbound > tmp19
	echo -ne "" > tmp20
	diff tmp19 tmp20


clean:
	rm testcustom* tmp*