 module readinputs
  implicit none
    contains

    
     logical function new_weatherfile()
           use utilities
           use constants_and_variables, ONLY:  weatherfilename
           integer :: start
           character(len=3) :: extension

           
           
           new_weatherfile = .FALSE.
           start = scan(weatherfilename,'.', BACK = .TRUE.)          
           extension = weatherfilename(start+1:start+3)
           call To_uppercase(extension )          
           if (extension == "WEA") new_weatherfile = .TRUE.
           
          return 
    end function new_weatherfile
    
    
    
    
    
    !***********************************************************************************************
      subroutine Read_Weatherfile
      use utilities
      use constants_and_Variables, ONLY:precip, pan_evap,air_temperature, wind_speed, solar_radiation, num_records, &
                                        EchoFileUnit,metfileunit,weatherfilename, startday, first_year
      
      integer :: dummy, status, first_month, first_day,i
      
      

      
      !open weather file, count file, allocate the weather variables, read in dat and place in vectors.
      

      OPEN(Unit = metfileunit,FILE=trim(adjustl(weatherfilename)),STATUS='OLD', IOSTAT=status)
      IF (status .NE. 0) THEN
         write(EchoFileUnit,*)'Problem with met file.'
         stop
      ENDIF   
      
      num_records = 0
      
      !Read first Date of Simulation
      

      read (metfileunit,*, IOSTAT=status) first_month, first_day, first_year

      
      if (status /= 0) then
          write(EchoFileUnit,*) status,  "No data or other problem in Weather File"
          stop
      end if
      
       
      startday = jd(first_year, first_month, first_day)

      !Count the records in the weather file
      num_records=1
      do        
          read (metfileunit,*, IOSTAT=status) dummy
          if (status /= 0)exit
          num_records=num_records+1
      end do
      write (EchoFileUnit,*) "Total days = ",num_records
      

      !Allocate the weather parameters
      
      allocate (precip(num_records))
      allocate (pan_evap(num_records))
      allocate (air_temperature(num_records))
      allocate (wind_speed(num_records))
      allocate (solar_radiation(num_records))
      
      !rewind and read in weather data
      rewind(metfileunit)
      
      do i = 1, num_records
          READ(MetFileUnit,*) dummy,dummy,dummy,precip(i),pan_evap(i),air_temperature(i),wind_speed(i),solar_radiation(i)
 
      end do
  
      close(metfileunit)
      
      end subroutine Read_Weatherfile
    
    !***************************************************************************************************
     subroutine Read_Old_Weatherfile
      use utilities
      use constants_and_Variables, ONLY:precip, pan_evap,air_temperature, wind_speed, solar_radiation, num_records, &
                                        EchoFileUnit,metfileunit,weatherfilename, startday, first_year
      
      integer :: dummy, status, first_month, first_day,i
      
      !open weather file, count file, allocate the weather variables, read in dat and place in vectors.
      



      
      
      OPEN(Unit = metfileunit,FILE=trim(adjustl(weatherfilename)),STATUS='OLD', IOSTAT=status)
      IF (status .NE. 0) THEN
         write(EchoFileUnit,*)'Problem with met file.'
         stop
      ENDIF   
      
      num_records = 0
      
      !Read first Date of Simulation
      read (metfileunit,'(1X,3I2)', IOSTAT=status) first_month, first_day, first_year

      first_year = first_year + 1900
      if (status /= 0) then
          write(EchoFileUnit,*) "No data or other problem in Weather File"
          stop
      end if
      
       
      startday = jd(first_year, first_month, first_day)

      !Count the records in the weather file
      num_records=1
      do        
          read (metfileunit,*, IOSTAT=status) dummy
          if (status /= 0)exit
          num_records=num_records+1
      end do
      write (EchoFileUnit,*) "Total days = ",num_records    

      !Allocate the weather parameters
      
      allocate (precip(num_records))
      allocate (pan_evap(num_records))
      allocate (air_temperature(num_records))
      allocate (wind_speed(num_records))
      allocate (solar_radiation(num_records))
      
      !rewind and read in weather data
      rewind(metfileunit)
      
      do i = 1, num_records
          READ(MetFileUnit,'(1X,3I2,5F10.0)', IOSTAT =status) dummy,dummy,dummy,precip(i),pan_evap(i),air_temperature(i),wind_speed(i),solar_radiation(i)
          ! if (status /= 0) then
          !     write (EchoFileUnit,*) "Weather file problem on line ", i
          !     exit
          ! end if
          
      end do
    !  write (EchoFileUnit,*) "finished reading old weather file"
      close(metfileunit)
      
     end subroutine Read_Old_Weatherfile 
     
! This subroutine unpacks an array (as read from a .wea or .dvf or .met file,
! without the date columns) and copies the data into the 1-d arrays used by PRZM

     subroutine unpackweather(wea)
      use constants_and_variables, ONLY: num_records, precip, pan_evap, air_temperature, &
          wind_speed, solar_radiation ! these are all set in this subroutine
      implicit none
      real*8, dimension(:, :) :: wea
    
      allocate (precip(num_records))
      precip = wea(:,1)
      allocate (pan_evap(num_records))
      pan_evap = wea(:,2)
      allocate (air_temperature(num_records))
      air_temperature = wea(:,3)
      allocate (wind_speed(num_records))
      wind_speed = wea(:,4)
      allocate (solar_radiation(num_records))
      solar_radiation = wea(:,5)
    end subroutine unpackweather    
      

    Subroutine PRZMRD_PRZM5(input_string)
      use  constants_and_Variables, ONLY:pfac, sfac, soil_temp_input,  &
      is_temperature_simulated, albedo,emmiss,bbt,IRTYPE,PCDEPL,max_irrig,FLEACH,  &
      min_evap_depth, AFIELD,IREG,USLEK,USLELS,USLEP,HL,SLP, USLEC,MNGN,  &
      theta_zero_input,sand_input,clay_input,fc_input,wp_input,CN_2,     &
      NUSLEC,GDUSLEC,GMUSLEC,GYUSLEC,erflag, IRFLAG, Num_delx,  DEPI,    &
      appeff,Tband_top,plant_pesticide_degrade_rate, foliar_formation_ratio_12,   &
      PTRN13,foliar_formation_ratio_23, MolarConvert_aq12_input, &
      MolarConvert_aq13_input, MolarConvert_aq23_input, MolarConvert_s12_input,   &
      MolarConvert_s13_input, MolarConvert_s23_input, ENPY,num_applications,   &
      DAIR,HENRYK,plant_volatilization_rate,oc_input,cam, Q_10,TBASE,NCHEM,    &
      NHORIZ,SOL,thickness,CONST,NPLOTS,chem_id, ARG,IARG2,PLNAME,mode,  &
      emergence_date, maturity_date, harvest_date, application_date,dispersion_input,  &
      runoff_effic,runoff_decline,runoff_extr_depth,erosion_decline,erosion_depth,erosion_effic ,Num_hydro_factors,use_usleyears,  &
      metfileunit,maxFileLength,TimeSeriesUnit,RunFilePath, FLAG1,nonequilibrium_flag, &
      calibrationflag,CalibrationDataUnit, CalibrationFilename, CalibrationFilenameOutput, &
      UserSpecifiesDepth, user_irrig_depth, k_f_input, N_f_input,lowest_conc, uWind_Reference_Height,Height_stagnant_air_layer_cm, &
      application_rate,number_subdelt, N_f_2_input,k_f_2_input,K2, bd_input, OC_input,  &
      aq_rate_input, sorb_rate_input, gas_rate_input,ADJUST_CN, &
      some_applications_were_foliar, plant_washoff_coeff,plant_volatilization_rate,weatherfilename, &
      max_root_depth,max_canopy_cover,max_canopy_holdup,max_canopy_height,  foliar_disposition,num_crop_periods,TAPP ,  &
      is_true_rain_distribution, is_TC_lag_method,uptkf, num_records,  &
      is_adjust_for_rain ,rain_limit, intolerable_rain_window,  optimum_application_window, min_days_between_apps
                                            
      use splitstring
      use utilities
      use Output_File ! probably not needed, as no file is written
      
     ! Use m_Wind
      implicit none
      character(len=*), intent(in) :: input_string
      character(len=:), dimension(:), allocatable :: inp
      integer :: dummy, i, k, l
          
      integer  :: apy, apm, apd                                  !local application date
      integer :: emd, emm, emy, mad, mam, may,had, ham, hay    !local crop dates
 
      CHARACTER(len = :), allocatable:: note  !used for sending error messages

      LOGICAL :: FATAL

      Character(Len = maxFileLength)  :: Filename
      
      ! if CAM == 1,2,3 then force soil incorporation depth == 4 cm
      Real, Parameter :: cam123_soil_depth = 4.0 ! cm
      integer :: status

      inp = strsplitc(input_string)      
      I   = 80

     sol = 0.0
     MolarConvert_aq12_input = 0.0
     MolarConvert_aq23_input = 0.0
     MolarConvert_aq13_input = 0.0
     MolarConvert_s12_input  = 0.0
     MolarConvert_s23_input  = 0.0
     MolarConvert_s13_input  = 0.0

      soil_temp_input = 0.0
      sand_input      = 0.0
      clay_input      = 0.0

  ! Convert input_string to array of strings, one for each line (dropping comments)
      READ(inp(1), '(A)', IOSTAT = status) weatherfilename
      READ(inp(2),'(A)',IOSTAT = status) filename  ! not needed as no file will be written
      READ(inp(3),*,IOSTAT = status) FLAG1, ADJUST_CN, is_TC_lag_method, nonequilibrium_flag, calibrationFlag, number_subdelt, is_true_rain_distribution
      ! Reading of calibration file removed
    
      READ(inp(4)(1:16),*,IOSTAT = status) PFAC,SFAC, min_evap_depth
      READ(inp(5),*,IOSTAT = status)  ERFLAG      
!    ERFLAG = 0 indicates that erosion will not be calculated
      if (ERFLAG.eq.1 .or. ERFLAG.gt.4 .or. ERFLAG.lt.0) then
         note = 'Erosion flag (ERFLAG) out of range'
         FATAL = .True.
         Call ERRCHK (note, FATAL)
      ENDIF 
      
      READ(inp(6),*,IOSTAT = status) USLEK,USLELS,USLEP,AFIELD,IREG,SLP,HL ! R3
      READ(inp(7),*) NUSLEC, use_usleyears ! R4
     ! R5
      l = 8  ! from here the line numbers in the file can vary 
      do i= 1, NUSLEC
        READ(inp(l),*)  GDUSLEC(i), GMUSLEC(i), GYUSLEC(i),USLEC(i),MNGN(i),CN_2(i)
        l = l+1
      end do
      !-------------End Erosion Inputs ------------------------
      READ(inp(l),*,IOSTAT = status)  num_crop_periods  ! R6
      l = afterread(l, status, "Record 6 "//inp(l))
      allocate (emergence_date       (num_crop_periods))
      allocate (maturity_date        (num_crop_periods))
      allocate (harvest_date         (num_crop_periods))   
      allocate (max_root_depth       (num_crop_periods))
      allocate (max_canopy_cover     (num_crop_periods))
      allocate (max_canopy_holdup    (num_crop_periods))
      allocate (max_canopy_height    (num_crop_periods))
      allocate (foliar_disposition   (num_crop_periods))

      DO  I=1,num_crop_periods
        READ(inp(l),*,IOSTAT = status) emd,emm,emy,mad,mam,may,had,ham,hay,max_root_depth(i), &
        max_canopy_cover(i), max_canopy_height(i), max_canopy_holdup(i),foliar_disposition(i)
        l = afterread(l, status, "Record 7 "//inp(l))                             
        emergence_date(i) = jd(emy,emm,emd)
        maturity_date(i)  = jd(may,mam,mad)
        harvest_date(i)   = jd(hay,ham,had) 
      end do
      READ(inp(l),*,IOSTAT = status)  IRFLAG, is_temperature_simulated  ! R8 (flags)
      l = afterread(l, status, "Record 8 "//inp(l))
          
!     reads in irrigation parameters if IRFLAG > 0
      IF(IRFLAG .NE. 0)THEN
       !New Variables: user_irrig_depth, UserSpecifiesDepth
        READ(inp(l),*,IOSTAT = status) IRTYPE,FLEACH,PCDEPL,max_irrig, UserSpecifiesDepth, user_irrig_depth
        l = afterread(l, status, "Record 9 "//inp(l))
        if (status /=0 ) stop
        IF (IRTYPE ==2) then
            stop
        end if
      END IF

      IF (is_temperature_simulated) THEN  !soil temperature simulation, if ITFLAG = 1
        READ(inp(l),*,IOSTAT = status)(ALBEDO(I),I=1,12),EMMISS
        l = afterread(l, status, "Record 10"//inp(l))
      
        READ(inp(l),*) uWind_Reference_Height,   Height_stagnant_air_layer_cm ! R11
        l = l+1
      !  reads in Lower boundary temperatures if is_temperature_simulated
        READ(inp(l),*,IOSTAT = status)(BBT(I),I=1,12)  ! R12
        l = afterread(l, status, "Record 12"//inp(l))

      !  reads in Q10FAC and TBASE
        READ(inp(l),*,IOSTAT = status) Q_10,TBASE  ! R13
        l = afterread(l, status, "Record 13"//inp(l))
      ENDIF
        
      READ(inp(l),*,IOSTAT = status) NHORIZ  ! R14
      l = afterread(l, status, "Record 14"//inp(l))

! Record 15. Soil parameters with temperature
      do I=1,NHORIZ
        READ(inp(l),*,IOSTAT=status) dummy,thickness(I),num_delx(I),dispersion_input(i),bd_input(I), &
        theta_zero_input(i),fc_input(i),wp_input(i),oc_input(i),  &
        sand_input(i),clay_input(i),soil_temp_input(I)
        l = l+1
        if (status/=0) stop
      end do
   
      READ(inp(l),*,IOSTAT = status) runoff_extr_depth,runoff_decline,runoff_effic ! R16
      l = afterread(l, status, "Record 16 "//inp(l))
      
      
      READ(inp(l),*,IOSTAT = status)  erosion_depth, erosion_decline, erosion_effic !R17
      l = afterread(l, status, "Record 17 "//inp(l))  

      !*********CHEMICAL INPUTS*************************************
      READ(inp(l),*,IOSTAT = status)  num_applications, NCHEM, is_adjust_for_rain, rain_limit, intolerable_rain_window,  optimum_application_window, min_days_between_apps ! R C1
      l = afterread(l, status, "Record C1 "//inp(l))
      
      IF (NCHEM .EQ. 0) NCHEM = 1
      !*** allocate pesticide application parameters ****
      allocate(application_date(num_applications))
      allocate(application_rate(num_applications))
      
      allocate(DEPI(num_applications))
      allocate(TAPP(num_applications))  !used first in initialization but put here for consistency
      allocate(APPEFF(num_applications))
      allocate(Tband_top(num_applications))
      allocate(CAM(num_applications))

      ! Record C2
      do i=1, num_applications
        READ(inp(l),*,IOSTAT = status) APD,APM,APY, CAM(i),DEPI(i), application_rate(i), APPEFF(i), Tband_top(i)
        l = afterread(l, status, "Record C2 "//inp(l))
        application_date(i) = jd(APY, apm,apd)  !determine julian application date
      end do

      some_applications_were_foliar = .FALSE.
      if(any(cam==2).or.any(cam==3).or.any(cam==9).or.any(cam==10))then
          some_applications_were_foliar = .TRUE.
      end if
      
      READ(inp(l),*)   (UPTKF(K),K=1,NCHEM)
      l = l+1  ! this read did not have a status check in the original

      IF(some_applications_were_foliar)THEN  !foliar applications
        do  K=1,NCHEM
          READ(inp(l),*,IOSTAT = status) plant_volatilization_rate(K),plant_pesticide_degrade_rate(K),plant_washoff_coeff(K)
          l = afterread(l, status, "Record C4 "//inp(l))
        end do
      ENDIF

      IF((some_applications_were_foliar).AND.(NCHEM.GT.1))THEN
        READ(inp(l),*,IOSTAT = status) foliar_formation_ratio_12, PTRN13, foliar_formation_ratio_23
        l = afterread(l, status, "Record C5 "//inp(l))  
      ENDIF
      
      do i=1, nchem
        READ(inp(l),*,IOSTAT = status) DAIR(I),HENRYK(I),ENPY(I)
        l = afterread(l, status, "Record C6 "//inp(l))
    end do      

      do I=1,NHORIZ
        READ(inp(l),*,IOSTAT = status)(k_f_input(K,I),K=1,NCHEM)
        l = afterread(l, status, "Record C7  Kf "//inp(l))
      end do
      
      do I=1,NHORIZ
        READ(inp(l),*,IOSTAT = status)(N_f_input(K,I),K=1,NCHEM)
        l = afterread(l, status, "Record C7A  Freundlich N "//inp(l))
      end do
      
      do I=1,NHORIZ
        READ(inp(l),*,IOSTAT = status)(k_f_2_input(K,I),K=1,NCHEM)
        l = afterread(l, status, "Record C7B  Region 2 Freundlich Coefficients' "//inp(l))
      end do
      
      do I=1,NHORIZ
        READ(inp(l),*,IOSTAT = status)(N_f_2_input(K,I),K=1,NCHEM)
        l = afterread(l, status, "Record C7  Region 2 Freundlich Exponents "//inp(l))
      end do    
 
      READ(inp(l),*,IOSTAT = status) lowest_conc
      l = afterread(l, status, "Record C7D lowest Freundlich conc"//inp(l))
      READ(inp(l),*,IOSTAT = status) (K2(K),K=1,NCHEM)
      l = afterread(l, status, "Record C7D Mass Transfer coef, K2 "//inp(l))
       
     do I=1,NHORIZ
      READ(inp(l),*,IOSTAT = status)(Aq_rate_input(K,I),Sorb_rate_input(K,I), Gas_rate_input(K,I), K=1, NCHEM)
      l = afterread(l, status, "Record C8 Degradation rates "//inp(l))
     end do

     do i=1,NHORIZ
      !read the molar prouction ratio of daughter to parent
          READ(inp(l),*,IOSTAT = status) MolarConvert_aq12_input(i),MolarConvert_aq13_input(i),MolarConvert_aq23_input(i), &
          MolarConvert_s12_input(i), MolarConvert_s13_input(i) ,MolarConvert_s23_input(i)
           !,DKRS12(I),DKRS13(I),DKRS23(I)  not used
          l = afterread(l, status, "Record C9 "//inp(l))
     end do

!****************TIME SERIES OUTPUT****************
l = l+1  ! skip reading NPLOTS, which is now passed to PRZM5
!READ(inp(l),*,IOSTAT = status) NPLOTS
!l = afterread(l, status, "Number of output columns "//inp(l))
  IF (NPLOTS .GT. 0) THEN
    DO I=1,NPLOTS
      READ(inp(l),*) PLNAME(I),chem_id(I),MODE(I),ARG(I),IARG2(I),CONST(I)
      l = afterread(l, status, "Record U2 "//inp(l))
    end do
!  call write_outputfile_header
  ENDIF
!
!open(unit=77, file="strdump.txt", status='UNKNOWN')
!write(77, *) "pfac = ", pfac
!write(77, *) "sfac = ", sfac
!write(77, *) "soil_temp_input = ", soil_temp_input
!write(77, *) "is_temperature_simulated = ", is_temperature_simulated
!write(77, *) "albedo = ", albedo
!write(77, *) "emmiss = ", emmiss
!write(77, *) "bbt = ", bbt
!write(77, *) "IRTYPE = ", IRTYPE
!write(77, *) "PCDEPL = ", PCDEPL
!write(77, *) "max_irrig = ", max_irrig
!write(77, *) "FLEACH = ", FLEACH
!write(77, *) "min_evap_depth = ", min_evap_depth
!write(77, *) "AFIELD = ", AFIELD
!write(77, *) "IREG = ", IREG
!write(77, *) "USLEK = ", USLEK
!write(77, *) "USLELS = ", USLELS
!write(77, *) "USLEP = ", USLEP
!write(77, *) "HL = ", HL
!write(77, *) "SLP = ", SLP
!write(77, *) "USLEC = ", USLEC
!write(77, *) "MNGN = ", MNGN
!write(77, *) "theta_zero_input = ", theta_zero_input
!write(77, *) "sand_input = ", sand_input
!write(77, *) "clay_input = ", clay_input
!write(77, *) "fc_input = ", fc_input
!write(77, *) "wp_input = ", wp_input
!write(77, *) "CN_2 = ", CN_2
!write(77, *) "NUSLEC = ", NUSLEC
!write(77, *) "GDUSLEC = ", GDUSLEC
!write(77, *) "GMUSLEC = ", GMUSLEC
!write(77, *) "GYUSLEC = ", GYUSLEC
!write(77, *) "erflag = ", erflag
!write(77, *) "IRFLAG = ", IRFLAG
!write(77, *) "Num_delx = ", Num_delx
!write(77, *) "DEPI = ", DEPI
!write(77, *) "appeff = ", appeff
!write(77, *) "Tband_top = ", Tband_top
!write(77, *) "plant_pesticide_degrade_rate = ", plant_pesticide_degrade_rate
!write(77, *) "foliar_formation_ratio_12 = ", foliar_formation_ratio_12
!write(77, *) "PTRN13 = ", PTRN13
!write(77, *) "foliar_formation_ratio_23 = ", foliar_formation_ratio_23
!write(77, *) "MolarConvert_aq12_input = ", MolarConvert_aq12_input
!write(77, *) "MolarConvert_aq13_input = ", MolarConvert_aq13_input
!write(77, *) "MolarConvert_aq23_input = ", MolarConvert_aq23_input
!write(77, *) "MolarConvert_s12_input = ", MolarConvert_s12_input
!write(77, *) "MolarConvert_s13_input = ", MolarConvert_s13_input
!write(77, *) "MolarConvert_s23_input = ", MolarConvert_s23_input
!write(77, *) "ENPY = ", ENPY
!write(77, *) "num_applications = ", num_applications
!write(77, *) "DAIR = ", DAIR
!write(77, *) "HENRYK = ", HENRYK
!write(77, *) "plant_volatilization_rate = ", plant_volatilization_rate
!write(77, *) "oc_input = ", oc_input
!write(77, *) "cam = ", cam
!write(77, *) "Q_10 = ", Q_10
!write(77, *) "TBASE = ", TBASE
!write(77, *) "NCHEM = ", NCHEM
!write(77, *) "NHORIZ = ", NHORIZ
!write(77, *) "SOL = ", SOL
!write(77, *) "thickness = ", thickness
!write(77, *) "CONST = ", CONST
!write(77, *) "NPLOTS = ", NPLOTS
!write(77, *) "chem_id = ", chem_id
!write(77, *) "ARG = ", ARG
!write(77, *) "IARG2 = ", IARG2
!write(77, *) "PLNAME = ", PLNAME
!write(77, *) "mode = ", mode
!write(77, *) "emergence_date = ", emergence_date
!write(77, *) "maturity_date = ", maturity_date
!write(77, *) "harvest_date = ", harvest_date
!write(77, *) "application_date = ", application_date
!write(77, *) "dispersion_input = ", dispersion_input
!write(77, *) "runoff_effic = ", runoff_effic
!write(77, *) "runoff_decline = ", runoff_decline
!write(77, *) "runoff_extr_depth = ", runoff_extr_depth
!write(77, *) "erosion_decline = ", erosion_decline
!write(77, *) "erosion_depth = ", erosion_depth
!write(77, *) "erosion_effic = ", erosion_effic
!write(77, *) "Num_hydro_factors = ", Num_hydro_factors
!write(77, *) "use_usleyears = ", use_usleyears
!write(77, *) "metfileunit = ", metfileunit
!write(77, *) "maxFileLength = ", maxFileLength
!write(77, *) "TimeSeriesUnit = ", TimeSeriesUnit
!write(77, *) "RunFilePath = ", RunFilePath
!write(77, *) "FLAG1 = ", FLAG1
!write(77, *) "nonequilibrium_flag = ", nonequilibrium_flag
!write(77, *) "calibrationflag = ", calibrationflag
!write(77, *) "CalibrationDataUnit = ", CalibrationDataUnit
!write(77, *) "CalibrationFilename = ", CalibrationFilename
!write(77, *) "CalibrationFilenameOutput = ", CalibrationFilenameOutput
!write(77, *) "UserSpecifiesDepth = ", UserSpecifiesDepth
!write(77, *) "user_irrig_depth = ", user_irrig_depth
!write(77, *) "k_f_input = ", k_f_input
!write(77, *) "N_f_input = ", N_f_input
!write(77, *) "lowest_conc = ", lowest_conc
!write(77, *) "uWind_Reference_Height = ", uWind_Reference_Height
!write(77, *) "Height_stagnant_air_layer_cm = ", Height_stagnant_air_layer_cm
!write(77, *) "application_rate = ", application_rate
!write(77, *) "number_subdelt = ", number_subdelt
!write(77, *) "N_f_2_input = ", N_f_2_input
!write(77, *) "k_f_2_input = ", k_f_2_input
!write(77, *) "K2 = ", K2
!write(77, *) "bd_input = ", bd_input
!write(77, *) "OC_input = ", OC_input
!write(77, *) "aq_rate_input = ", aq_rate_input
!write(77, *) "sorb_rate_input = ", sorb_rate_input
!write(77, *) "gas_rate_input = ", gas_rate_input
!write(77, *) "ADJUST_CN = ", ADJUST_CN
!write(77, *) "some_applications_were_foliar = ", some_applications_were_foliar
!write(77, *) "plant_washoff_coeff = ", plant_washoff_coeff
!write(77, *) "plant_volatilization_rate = ", plant_volatilization_rate
!write(77, *) "weatherfilename = ", weatherfilename
!write(77, *) "max_root_depth = ", max_root_depth
!write(77, *) "max_canopy_cover = ", max_canopy_cover
!write(77, *) "max_canopy_holdup = ", max_canopy_holdup
!write(77, *) "max_canopy_height = ", max_canopy_height
!write(77, *) "foliar_disposition = ", foliar_disposition
!write(77, *) "num_crop_periods = ", num_crop_periods
!write(77, *) "TAPP = ", TAPP
!write(77, *) "is_true_rain_distribution = ", is_true_rain_distribution
!write(77, *) "is_TC_lag_method = ", is_TC_lag_method
!write(77, *) "uptkf = ", uptkf
!close(unit=77)
! 

  END subroutine PRZMRD_PRZM5
  
  function readfile(filepath, unitnumber) result(str)
    ! Read an entire file into a string
    character(len=*), intent(in) :: filepath
    integer, optional, intent(in) :: unitnumber
    character(len=:), allocatable :: str
    integer :: flen, u
    if(present(unitnumber)) then
        u = unitnumber
    else
        u=1
    end if
  
    open(UNIT=u, FILE = filepath, status='old', access='stream')
    inquire(UNIT=u, SIZE=flen)
    allocate(character(len=flen) :: str)
    read(1,pos=1) str
    close(UNIT=u)
  end function readfile

end module readinputs