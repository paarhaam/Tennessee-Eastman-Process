      implicit none
      integer nx,ny,nu,i,j, no_simul,fault_counter,train,test
	integer plot_only_measured
      parameter (nx=26,nu=25,ny=32)
      double precision u(nu),x(nx),dxdt(nx),y(ny),y_ss(ny)
      double precision xls(8),F7,F10,F11
      double precision Cd

      double precision biasvalue,bias_tau,bias_base_gain,biasslope
	  double precision deltat
      double precision errold(3),  gain(3), taui(3), setpt(3),interr(3)
	double precision sensor_bias(3),setpt_bias(3),vp_bias(3)
	  double precision u0(25)
	double precision err(3),ctrl_Xm(3)
      common /CTRL/ setpt,gain,taui,errold,deltat,interr,u0,biasvalue,
     &sensor_bias,setpt_bias,vp_bias,err,ctrl_Xm
      
c the deltat used below seems to be the optimum in terms of not
c making the values hit NaN. Any higher value of deltat causes the
c variables to become NaN at some point. Maybe an advanced integration
c routine such as LSODE (atleast Runge Kutte) should be tried to get
c over this problem of optimal selection of deltat
cc      deltat=0.00001
      deltat=0.0001
      
	  bias_tau=0.1 ! in hrs
	  bias_base_gain = 1.0

      data errold/0.0,0.0,0.0/
		data interr/0.0,0.0,0.0/
      data gain/0.80,-1.36,-0.91/ ! the values used by Mani
ccc      data gain/12.80,-15.00,-30.50/
ccc      data gain/1.5,-1.2,-0.05/
      data taui/1.0,1.0,1.0/ ! in hrs
	
	do	fault_counter=1,33			 ! = 0 means no fault at all

	write(*,*) '-----------------------------------------------------'
	write(*,*) '------- STARTING A NEW SIMULATION ------------'
	
	if(fault_counter.eq.1) then
	open(unit=2,file='check2.txt',status='unknown')
	else
	open(unit=2,file='check2.txt',access='APPEND')
	end if	

      open(unit=1,file='uvals.dat',status='old')
      read(1,*) (u(i),i=1,nu)
      close(unit=1)

cc calculate the base values of input variables
		do 111 i=1,nu
		u0(i)=u(i)
111	continue

c default value of catalyst activity is 1.0
      Cd =1.0 

c giving roughly 33% change in variables as faults
c      u(1)=15.0
c      u(2)=150.0
c      u(3)=135.0
c      u(4)=550.0
c      u(5)=1600.0      
c      u(6)=20.0
c      u(7)=340.0
ccc      u(7)=u(7)+5.0
c      u(8)=280.0
ccc      u(8)=u(8)+2.0
cc Faults 9 and 10 are temperatures of reactor and separator
c respectively. Will give lesser change in them (about  %) as any
c higher change causes the variables to go to NaN.       
c      u(9)=130.0
c      u(10)=100.0
ccc      u(10)=u(10)+0.5

Cc Fault 1. Increase in u(1)
cc	      u(1) = u(1)*1.20	 !fault_counter=1
Cc Fault 2. Increase in u(2)
cc      u(2) = u(2)*1.050	 !fault_counter=2

Cc Fault 3. Increase in u(3)
cc      u(3) = u(3)*1.05	 !fault_counter=3

Cc Fault 4. Increase in u(4)
cc      u(4) = u(4)*1.02	 !fault_counter=4

Cc Fault 5. Increase in u(5)
cc      u(5) = u(5)*1.20	 !fault_counter=5

Cc Fault 6. Increase in u(6)
cc      u(6) = u(6)*1.10	 !fault_counter=6

Cc Fault 7. Increase in u(9). This is reactor temperature
cc      u(9) = u(9)*1.05	 !fault_counter=7

Cc Fault 8. Decrease in catalyst activity
cc      Cd = Cd*0.80	 !fault_counter=8

Cc Fault 9. Deccrease in u(1)
cc      u(1) = u(1)*0.80	 !fault_counter=9

Cc Fault 10. Decrease in u(2)
cc      u(2) = u(2)*0.95	 !fault_counter=10

Cc Fault 11. Decrease in u(3)
cc      u(3) = u(3)*0.95	 !fault_counter=11

Cc Fault 12. Decrease in u(4)
cc      u(4) = u(4)*0.90	 !fault_counter=12

Cc Fault 13. Decrease in u(5)
cc      u(5) = u(5)*0.95	 !fault_counter=13

Cc Fault 14. Decrease in u(6)
cc      u(6) = u(6)*0.90	 !fault_counter=14

Cc Fault 15. Decrease in u(9). This is reactor temperature. The
cc decrease considered here is very small (from 120.4 to 117.6). This
cc is because any further decrease leads to a negative F7 for 
cc a few starting iterations
cc      u(9) = 115.0	 !fault_counter=15


      
      open(unit=1,file='xvals.dat',status='old')
      read(1,*) (x(i),i=1,nx)
      close(unit=1)
      
      open(unit=1,file='yvals.dat',status='old')
      read(1,*) (y(i),i=1,ny)
      close(unit=1)


cc calculate the base values of output variables
      do i=1,ny
        y_ss(i)=y(i)
      end do

c      write(*,*) 'enter the number of simulation points'
c      read(*,*) no_simul

c	no_simul=150000
	no_simul=60000

      setpt(1) = 75.0
      setpt(2) = 50.0
      setpt(3) = 50.0
c	set all the biases to 0
		do i=1,3
		sensor_bias(i)=0.0
		setpt_bias(i)=0.0
		vp_bias(i)=0.0
		interr(i)=0.0
		end do

      biasvalue=1.0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc	INTRODUCING THE FAULT HERE (THE FAULTS ARE LISTED AT 
cccccc	OTHER PLACES TOO FOR UNDERSTANING e.g. THE LINES ABOVE,
cccccc	AND INSIDE THE SUBROUTINE FOR CONTROLLER

cccccc	if training (preparing the database), train=1,test=0
cccccc	if testing (performing fault diagnosis), train=0,test=1

	train=1
	plot_only_measured=0
	test=1-train

cccccc	The actual equations to introduce the fault are inside the loop
cccccc	where time is being varied. In that way, we can introduce the fault
cccccc	at different times.

	
	if(fault_counter.eq.1) then
	open(unit=11,file='fault_counter.txt',status='unknown')
      write(11,*) train, fault_counter,plot_only_measured,no_simul
      close(unit=11)
	
	else
	OPEN(UNIT=11,FILE='fault_counter.txt',access='APPEND')
	write(11,*) train, fault_counter,plot_only_measured,no_simul
      close(unit=11)
	end if

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	write the first line

c      write(2,*) y(3), y(11), y(12), ' 1475.16538', ' 0.136037007' 
c      write(2,211) y(2), y(4), y(5),u(10),u(7),u(8),y(1),y(3),y(6),
c     & y(7),y(8),y(9),y(10),y(11),y(12)

c	generate some more measurable variables
c	taken from the file model_check.f
	F10=u0(7) - u0(15)
	F11=u0(8)
	F7=1475.165050
	ctrl_Xm(1)=y(2)
      err(1) = setpt(1) - ctrl_Xm(1)
	ctrl_Xm(2)=y(4)
      err(2) = setpt(2) - ctrl_Xm(2)
	ctrl_Xm(3)=y(5)
      err(3) = setpt(3) - ctrl_Xm(3) 

	if(plot_only_measured.eq.1) then
ccc	There are 14 measured variables (identified by sensor location
ccc	for incipient fault diagnosis).
ccc		sensornames=[...
ccc		{1    65		'P_m'};...	y(6)
ccc		{2    66		'F_6'};...	y(7)
ccc		{3    67		'y_{A,6}'};...	y(8)
ccc		{4    70		'y_{D,6}'};...	y(11)
ccc		{5    71		'y_{E,6}'};...	y(12)
ccc		{6    75		'F_7'};...		F7
ccc		{7   154		'F_{10}'};...	F10=u(7)-u(15)
ccc		{8   155		'F_{11}'};...	F11=u(8)
ccc		{9   157		'VLr_Xm'};...	ctrl_Xm(1)
ccc		{10   158		'VLr_e'};...	err(1)
ccc		{11   160		'VLs_Xm'};...	ctrl_Xm(1)
ccc		{12   161		'VLs_e'};...	err(2)
ccc		{13   163		'VLp_Xm'};...	ctrl_Xm(1)
ccc		{14   164		'VLp_e'}];		err(3)

      
	write(2,212) 0.0,y(6),y(7),y(8),y(11),y(12), F7,F10,F11,
     &ctrl_Xm(1),err(1),ctrl_Xm(2),err(2),ctrl_Xm(3),err(3)
	else !plotting all u,x and y
	write(2,211) 0.0,F7,(u(i),i=1,nu),(x(i),i=1,nx),(y(i),i=1,ny)
	end if

c211	format(15(F15.9))
212	format(15(F19.5))
     
211	format(85(F19.5))
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	  do j=1,no_simul

c   faults actually being introduced here

	if(j.eq.10000) then
	  	if(train.eq.1) then		!	traing/preparing database
	if(fault_counter.eq.1) then
    	    u(1) = u(1)*1.20	 !fault_counter=1
	else if(fault_counter.eq.2) then
          u(2) = u(2)*1.050	 !fault_counter=2
	else if(fault_counter.eq.3) then
          u(3) = u(3)*1.05	 !fault_counter=3
	else if(fault_counter.eq.4) then
          u(4) = u(4)*1.02	 !fault_counter=4
	else if(fault_counter.eq.5) then
          u(5) = u(5)*1.20	 !fault_counter=5
	else if(fault_counter.eq.6) then
          u(6) = u(6)*1.10	 !fault_counter=6
	else if(fault_counter.eq.7) then
          u(9) = u(9)*1.05	 !fault_counter=7
	else if(fault_counter.eq.8) then
          Cd = Cd*0.80	 !fault_counter=8
	else if(fault_counter.eq.9) then
          u(1) = u(1)*0.80	 !fault_counter=9
	else if(fault_counter.eq.10) then
          u(2) = u(2)*0.95	 !fault_counter=10
	else if(fault_counter.eq.11) then
          u(3) = u(3)*0.95	 !fault_counter=11
	else if(fault_counter.eq.12) then
          u(4) = u(4)*0.90	 !fault_counter=12
	else if(fault_counter.eq.13) then
          u(5) = u(5)*0.95	 !fault_counter=13
	else if(fault_counter.eq.14) then
          u(6) = u(6)*0.90	 !fault_counter=14
	else if(fault_counter.eq.15) then
          u(9) = 117.6	 !fault_counter=15
	else if(fault_counter.eq.16) then
    		sensor_bias(1)=5*biasvalue  !fault_counter=16
	else if(fault_counter.eq.17) then	     
    		setpt_bias(1)=5 !fault_counter=17
	else if(fault_counter.eq.18) then
    		vp_bias(1)=u0(10)*0.05       !fault_counter=18
	else if(fault_counter.eq.19) then
    		sensor_bias(2)=5*biasvalue       !fault_counter=19
    	else if(fault_counter.eq.20) then
		setpt_bias(2)=5      !fault_counter=20
    	else if(fault_counter.eq.21) then
		vp_bias(2)=u0(7)*0.05       !fault_counter=21
	else if(fault_counter.eq.22) then
    		sensor_bias(3)=5*biasvalue       !fault_counter=22
    	else if(fault_counter.eq.23) then
		setpt_bias(3)=5       !fault_counter=23
    	else if(fault_counter.eq.24) then
		vp_bias(3)=u0(8)*0.05       !fault_counter=24
	else if(fault_counter.eq.25) then
    		sensor_bias(1)=-5*biasvalue       !fault_counter=25
    	else if(fault_counter.eq.26) then
		setpt_bias(1)=-5       !fault_counter=26
    	else if(fault_counter.eq.27) then
		vp_bias(1)=-u0(10)*0.05       !fault_counter=27
	else if(fault_counter.eq.28) then
    		sensor_bias(2)=-5*biasvalue	 !fault_counter=28
    	else if(fault_counter.eq.29) then
		setpt_bias(2)=-5       !fault_counter=29
    	else if(fault_counter.eq.30) then
		vp_bias(2)=-u0(7)*0.05       !fault_counter=30
	else if(fault_counter.eq.31) then
    		sensor_bias(3)=-5*biasvalue       !fault_counter=31
    	else if(fault_counter.eq.32) then
		setpt_bias(3)=-5       !fault_counter=32
    	else if(fault_counter.eq.33) then
		vp_bias(3)=-u0(8)*0.05		 !fault_counter=33
	end if
	else	! testing
	if(fault_counter.eq.1) then
    	    u(1) = u(1)*1.15	 !fault_counter=1
	else if(fault_counter.eq.2) then
          u(2) = u(2)*1.040	 !fault_counter=2
	else if(fault_counter.eq.3) then
          u(3) = u(3)*1.04	 !fault_counter=3
	else if(fault_counter.eq.4) then
          u(4) = u(4)*1.018	 !fault_counter=4
	else if(fault_counter.eq.5) then
          u(5) = u(5)*1.15	 !fault_counter=5
	else if(fault_counter.eq.6) then
          u(6) = u(6)*1.08	 !fault_counter=6
	else if(fault_counter.eq.7) then
          u(9) = u(9)*1.04	 !fault_counter=7
	else if(fault_counter.eq.8) then
          Cd = Cd*0.85	 !fault_counter=8
	else if(fault_counter.eq.9) then
          u(1) = u(1)*0.85	 !fault_counter=9
	else if(fault_counter.eq.10) then
          u(2) = u(2)*0.97	 !fault_counter=10
	else if(fault_counter.eq.11) then
          u(3) = u(3)*0.97	 !fault_counter=11
	else if(fault_counter.eq.12) then
          u(4) = u(4)*0.92	 !fault_counter=12
	else if(fault_counter.eq.13) then
          u(5) = u(5)*0.97	 !fault_counter=13
	else if(fault_counter.eq.14) then
          u(6) = u(6)*0.92	 !fault_counter=14
	else if(fault_counter.eq.15) then
          u(9) = 116.0	 !fault_counter=15
	else if(fault_counter.eq.16) then
    		sensor_bias(1)=6*biasvalue  !fault_counter=16
	else if(fault_counter.eq.17) then	     
    		setpt_bias(1)=6 !fault_counter=17
	else if(fault_counter.eq.18) then
    		vp_bias(1)=u0(10)*0.06       !fault_counter=18
	else if(fault_counter.eq.19) then
    		sensor_bias(2)=6*biasvalue       !fault_counter=19
    	else if(fault_counter.eq.20) then
		setpt_bias(2)=6      !fault_counter=20
    	else if(fault_counter.eq.21) then
		vp_bias(2)=u0(7)*0.06       !fault_counter=21
	else if(fault_counter.eq.22) then
    		sensor_bias(3)=6*biasvalue       !fault_counter=22
    	else if(fault_counter.eq.23) then
		setpt_bias(3)=6       !fault_counter=23
    	else if(fault_counter.eq.24) then
		vp_bias(3)=u0(8)*0.06       !fault_counter=24
	else if(fault_counter.eq.25) then
    		sensor_bias(1)=-6*biasvalue       !fault_counter=25
    	else if(fault_counter.eq.26) then
		setpt_bias(1)=-6       !fault_counter=26
    	else if(fault_counter.eq.27) then
		vp_bias(1)=-u0(10)*0.06       !fault_counter=27
	else if(fault_counter.eq.28) then
    		sensor_bias(2)=-6*biasvalue	 !fault_counter=28
    	else if(fault_counter.eq.29) then
		setpt_bias(2)=-6       !fault_counter=29
    	else if(fault_counter.eq.30) then
		vp_bias(2)=-u0(7)*0.06       !fault_counter=30
	else if(fault_counter.eq.31) then
    		sensor_bias(3)=-6*biasvalue       !fault_counter=31
    	else if(fault_counter.eq.32) then
		setpt_bias(3)=-6       !fault_counter=32
    	else if(fault_counter.eq.33) then
		vp_bias(3)=-u0(8)*0.06		 !fault_counter=33
	end if
	end if
	end if

	  
	  
         call TEsimf(nx,nu,ny,x,u,y,dxdt,Cd,xls,F7)
	  
c		The two lines written below are used to create first order measument bias
c		So they should be commented when step bias is to be simulated. Also, for
c		simulating first order bias, appropriate statements in the fault
c		introduction loop (the multiple if loops) should be modified.
		 
c		 biasslope=(-1/bias_tau*biasvalue + 1/bias_tau*bias_base_gain)
c		 biasvalue=biasvalue + biasslope*deltat
      do i=1,nx
	    x(i) = x(i) + dxdt(i)*deltat
	  end do

c calling control loops
	 
		call control_loops(nx,nu,ny,x,u,y)

	F10=u(7) - u(15)
	F11=u(8)


c         write(2,*) y(3), y(11), y(12), F7, xls(5)
c         write(2,*) y(3), y(11), y(12)
	
	if(mod(j,10).eq.0) then
	if(plot_only_measured.eq.1) then
      write(2,212) deltat*j,y(6),y(7),y(8),y(11),y(12),F7,F10,F11,
     &ctrl_Xm(1),err(1),ctrl_Xm(2),err(2),ctrl_Xm(3),err(3)
	else !plotting all u,x and y
	write(2,211) deltat*j,F7,(u(i),i=1,nu),(x(i),i=1,nx),(y(i),i=1,ny)
	end if
	end if

c      write(2,*) y(3), y(11), y(12), ' 1475.16538', ' 0.136037007' 
c      write(2,211) y(2), y(4), y(5),u(10),u(7),u(8),y(1),y(3),y(6),
c     & y(7),y(8),y(9),y(10),y(11),y(12)

		if(mod(j,1000).eq.0) then
					  write(*,*) 'interr(3)=',interr(3)
		end if
      end do

      write(*,*) 'DXDT:'
      do i=1,nx
              write(*,*) dxdt(i)
      end do
      write(*,*) 'Y:'
      do i=1,ny
              write(*,*) y(i)
      end do
	
	end do
      close(2)

	stop
      
      end

C- the main program  ends here
C-------------------------------------------------------------

C- now the control loops subroutine starts
C- note that velcoity form for control loops will be used

      subroutine control_loops(nx,nu,ny,x,u,y)
      
	  integer i
      double precision u(nu),x(nx),y(ny)
      double precision err(3),biasvalue
	  double precision sensor_bias(3),setpt_bias(3),vp_bias(3)
      double precision deltat
      double precision errold(3),gain(3),taui(3),setpt(3),interr(3)
	double precision u0(25)
	double precision ctrl_Xm(3)
      common/CTRL/ setpt,gain,taui,errold,deltat,interr,u0,biasvalue,
     &sensor_bias,setpt_bias,vp_bias,err,ctrl_Xm

c  reactor level controller: Loop 1
c  controlled var: reactor level, manip. var: Separator temperature
c (which is a manipulated variable in this model)      


cc		sensor_bias(1)=5*biasvalue  !fault_counter=16     
cc		sensor_bias(1)=-5*biasvalue       !fault_counter=25
cc		setpt_bias(1)=5 !fault_counter=17
cc		setpt_bias(1)=-5       !fault_counter=26
cc		vp_bias(1)=u0(10)*0.05       !fault_counter=18
cc		vp_bias(1)=-u0(10)*0.05       !fault_counter=27
	
	ctrl_Xm(1)=(y(2) + sensor_bias(1))
      err(1) = (setpt(1) + setpt_bias(1)) - ctrl_Xm(1)
		interr(1)=interr(1)+err(1)*deltat
      u(10) = u0(10) + gain(1)*(err(1) + interr(1)/taui(1)) +
     & vp_bias(1)

c  separator level controller: Loop 2
c  controlled var: separator level, manip var: separator underflow      

cc		sensor_bias(2)=5*biasvalue       !fault_counter=19
cc		sensor_bias(2)=-5*biasvalue	 !fault_counter=28
cc		setpt_bias(2)=5      !fault_counter=20
cc		setpt_bias(2)=-5       !fault_counter=29
cc		vp_bias(2)=u0(7)*0.05       !fault_counter=21
cc		vp_bias(2)=-u0(7)*0.05       !fault_counter=30
	
	ctrl_Xm(2)=(y(4) + sensor_bias(2))
      err(2) = (setpt(2) + setpt_bias(2)) - ctrl_Xm(2)
	  interr(2)=interr(2)+err(2)*deltat
      u(7) = u0(7) + gain(2)*(err(2) +interr(2)/taui(2))+vp_bias(2)

c  stripper level controller: Loop 3
c  controlled var: stripper level, manip var: product flow

cc		sensor_bias(3)=5*biasvalue       !fault_counter=22
cc		sensor_bias(3)=-5*biasvalue       !fault_counter=31
cc		setpt_bias(3)=5       !fault_counter=23
cc		setpt_bias(3)=-5       !fault_counter=32
cc		vp_bias(3)=u0(8)*0.05       !fault_counter=24
cc		vp_bias(3)=-u0(8)*0.05		 !fault_counter=33

	ctrl_Xm(3)=(y(5) + sensor_bias(3))
      err(3) = (setpt(3) + setpt_bias(3)) - ctrl_Xm(3) 
		interr(3)=interr(3)+err(3)*deltat
      u(8) = u0(8) + gain(3)*(err(3) + interr(3)/taui(3))+vp_bias(3)

      end

