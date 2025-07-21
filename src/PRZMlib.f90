!  PRZMlib.f90
!
!  FUNCTIONS/SUBROUTINES exported from PRZM5.dll:
!  PRZM5 - subroutine
module p5
implicit none
contains
subroutine PRZM5(inputstring, day1, nrecords, nwea, nresults, weather, results)

  ! Expose subroutine PRZMlib to users of this DLL
  ! Calibration and PRZM_version removed from this version to minimize i/o
  !DEC$ ATTRIBUTES DLLEXPORT::PRZM5

  use constants_and_Variables, ONLY: EchoFileUnit,maxFileLength,calibrationFlag,RunFilePath, julday1900 ,&
                                  precip, pan_evap,air_temperature ,wind_speed, solar_radiation, &
                                  precipitation,PEVP,air_TEMP,WIND, SOLRAD ,harvest_day, &
                                  startday, num_records,is_harvest_day,canopy_holdup, &
                                  canopy_height, canopy_cover, cover, height, harvest_placement, &
                                  potential_canopy_holdup,evapo_root_node_daily, evapo_root_node,&
                                  root_node ,root_node_daily,atharvest_pest_app,day_number, NPLOTS,first_year
   use splitstring
   use initialization
   use utilities
   use readinputs
   use PRZM_core
   use plantgrowth
   use Output_File
   use allocations, only: deallocate_soil_compartments, deallocate_other
   implicit none

  ! Variables
  character(len=*), intent(in) :: inputstring
  integer*8, intent(in) :: day1 !, nplot, nday ! these are the dimensions of result, and the number of rows in the weather file
  integer*8, intent(in) :: nrecords, nwea, nresults
  real*8, dimension(nrecords,nwea), intent(in) :: weather
  real*8, dimension(nrecords,nresults), intent(out) :: results
  integer :: i , IERROR

   !integer YEAR,MONTH,DAY
   !Character(Len=maxFileLength) :: filename

   !Command line may contain a path to the runfile, else path is the working default path
   !all GetArgs (RunFilePath)


   !KECHO.prn records the operating status of PRZM during runtime
   !filename =  trim(RunFilePath) //  'KECHO.PRN'

   !OPEN(Unit=EchoFileUnit,FILE=Filename,STATUS='UNKNOWN',IOSTAT=IERROR)
   startday = day1
   first_year = yearfromjd(startday)
   NPLOTS = nresults - 3 ! There are three columns in the array for the date
   num_records = nrecords
   CALL PRZMRD_PRZM5(inputstring)   !read PRZM INPUT FILE
  
   call unpackweather(weather)

   !if (new_weatherfile()) then
   !    call Read_Weatherfile  !this reads the new format weather file
   !else
   !    call Read_Old_Weatherfile  !this reads the new format weather file
   !end if
   !allocate(results(num_records, NPLOTS+3)) ! do this here because num_records set from metfile; 3cols for date
   CALL INITL    !initialize variables

   Call Crop_Growth

   ! Calibration removed in this version
   !if (calibrationFlag) then
   !    call read_calibration_data
   !end if
   julday1900 = startday
   do i=1, num_records   !day loop driven by metfile only
       day_number = i
       !Vectors determined outside of loop transfered as scalers to loop
       precipitation           = precip(i)
       PEVP                    = pan_evap(i)
       air_TEMP                = air_temperature(i)
       WIND                    = wind_speed(i)
       SOLRAD                  = solar_radiation(i)
       cover                   = canopy_cover(i)
       height                  = canopy_height(i)
       harvest_day             = is_harvest_day(i)
       potential_canopy_holdup = canopy_holdup(i)
       evapo_root_node_daily   = evapo_root_node(i)
       root_node_daily         = root_node(i)       !only needed for irrigation
       harvest_placement       = atharvest_pest_app(i)

      CALL PRZM
      CALL write_outputfile(i, results)
      julday1900 = julday1900  + 1
   end do
   call deallocate_soil_compartments
   call deallocate_other
   close(77)
end subroutine PRZM5
end module p5
