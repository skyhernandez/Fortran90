!
!##################################################################
!##################################################################
!######                                                      ######
!######                   ingest_pilot.f90                   ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  The pilot file provided by the Central Weather Bureau is the data 
!  output that is fixed in location and sample the atmosphere every 
!  twelve hours.  The instrument measures:
!         Pressure [mb*10]                        Height [gpm]
!         Wind Speed [m/s]                        Wind Direction [deg]
!
!  This code:
!    1. Reads in the data measured from the rawinsonde
!    2. Coverts the data into the units needed for assimulation into the
!       ARPS model:
!         Pressure [mb]                          Height [gpm]
!         Wind Speed [m/s]                       Wind Direction [deg]
!    3. Outputs the data into an *.snd file
!
!-----------------------------------------------------------------------
!
!  AUTHOR: 
!
!  Skylar Hernandez, (06/06/2011)
!
!-----------------------------------------------------------------------
!
      PROGRAM ingest_pilotdrop_CWB
        IMPLICIT NONE

        CHARACTER(LEN=11) pilotFile         ! Pilot filename
        INTEGER*4 nargs                     ! # of command line args
        INTEGER*4 IARGC                     ! Intrinsic subroutine

        nargs = IARGC()
        IF (nargs < 1) THEN
          WRITE(*,*) 'ERROR:  You must enter a pilot file!'
          STOP
        ENDIF
        CALL GETARG(1,pilotFile)
        CALL readpilotfile(pilotFile)

      END PROGRAM
!
!----------------------------------------------------------------------
!
!  SUBROUTINE readpilotfile
!
!----------------------------------------------------------------------
!
!  PURPOSE:
!
!  This subroutine will Read in the data measured from the rawinsonde.
!
!----------------------------------------------------------------------
!

      SUBROUTINE readpilotfile(pilotFile)
        IMPLICIT NONE

        INTEGER*4 FileHandle
        CHARACTER(LEN=11) pilotFile       ! Pilot filename
        INTEGER*4 i                       ! Counter in the z-direction
        INTEGER*4 station_no              ! Station number
        INTEGER*4 station_hgt             ! Hieght of station
        INTEGER*4 read_lat                ! Latitude  [degree*100]
        INTEGER*4 read_lon                ! Longitude [degree*100]
        INTEGER*4 read_year               ! Year
        INTEGER*4 read_month              ! Month
        INTEGER*4 read_day                ! Day
        INTEGER*4 past_hour               ! Concatination by the hour var
        INTEGER*4 read_hour               ! Hour
        INTEGER*4 past_minute             ! Concatinate variable
        INTEGER*4 read_minute             ! Minute
        INTEGER*4 redoVar                 ! Var to read catted files
        INTEGER*4 read_record             ! Total number of logical record
        INTEGER*4 NumberOfLines           ! Actual number of data lines
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_pres  ! Pressure
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_hght  ! Height  
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_Wdir  ! Wind Dir
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_Wspd  ! Wind speed
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark1    ! pres flag 
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark2    ! var flag
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark3    ! Wspd flag         
!
!! Formating statements
!
1000    FORMAT (3x,  I5, I4, 2I5,  2x, 5I2, I3)
2000    FORMAT (2x, 2I5, 1x,  I1, 12x, 2I3, 2I1, 3x)
!
!! Start of the subroutine: opening and reading in the data into their
!! respective variables and arrays.
!
        FileHandle  = 11
        redoVar     = 0
        past_hour   = 0
        past_minute = 0

        OPEN(UNIT=FileHandle, FILE=pilotFile)
        DO
!
!! This line in the code will read in the header, and it sets up the
!! the rest of the code.
!
          READ(FileHandle, 1000, END =10) station_no, station_hgt,     &
              read_lat, read_lon, read_year, read_month, read_day,     &
              read_hour, read_minute, read_record
!
!! This loop will ensure to keep reading a concatinated file and packages
!! the *.snd files per hour.
!
10        IF (read_hour .ne. past_hour) THEN
            past_hour = read_hour
          ELSE
            read_minute = past_minute
          ENDIF
!
!! There is data from the instrument that cannot be injested into the
!! model, however the read_record includes those lines as well.  Thus
!! the NumberOfLines variable sets the number of lines to read that
!! contains data useful to the model.
!
          NumberOfLines = read_record - 1
!
!! Allocate the arrays by the length of the data that is useful for the
!! model.  Since the remark2 variable is used for two different
!! variables it gets allocated accordingly.
!
          ALLOCATE(remark1(NumberOfLines))
          ALLOCATE(remark2(2*NumberOfLines))
          ALLOCATE(remark3(NumberOfLines))
          ALLOCATE(read_pres(NumberOfLines))
          ALLOCATE(read_hght(NumberOfLines))
          ALLOCATE(read_Wdir(NumberOfLines))
          ALLOCATE(read_Wspd(NumberOfLines))
!
!! This is the part of the code that does the actual reading in of the
!! data (useful and useless).
!
          DO i = 1, NumberOfLines, 1
            READ(FileHandle,2000,IOSTAT = redovar) read_pres(i),       &
                 read_hght(i), remark1(i), read_Wdir(i), read_Wspd(i), &
                 remark2(i), remark3(i)
20        END DO
!
!! This is a set of conditions for the code to exit if it has reached
!! the end of the file or something is wrong with the input data, if
!! not, continue running within the do loop.
!
           IF (redovar > 0)  THEN
             ! ... something wrong ...
             WRITE(*,*) "... Something is wrong with the data file ..."
             EXIT
           ELSE IF (redovar < 0) THEN
             ! ... end of file reached ...
             EXIT
           ELSE
             ! ... do normal stuff ...
           END IF
!
!! Call on the subroutine to convet the units from what was read in to
!! what the model requires.
!
          CALL UnitConverter(station_no, station_hgt, remark1, remark2,&
               remark3, read_pres, read_hght, read_Wdir,      &
               read_Wspd, NumberOfLines, read_lat, read_lon, read_year,&
               read_month, read_day, read_hour, read_minute)
          
          DEALLOCATE(remark1)
          DEALLOCATE(remark2)
          DEALLOCATE(remark3)
          DEALLOCATE(read_pres)
          DEALLOCATE(read_hght)
          DEALLOCATE(read_Wdir)
          DEALLOCATE(read_Wspd)

        END DO

        CLOSE(FileHandle)

      END SUBROUTINE
!
!----------------------------------------------------------------------
!
!  SUBROUTINE UnitConverter
!
!----------------------------------------------------------------------
!
!  PURPOSE:
!
!  This subroutine coverts the data into the units needed for
!  assimulation into the ARPS model.  The variables with the prefix
!  read_ contained the original data provided by the pilot file in 
!  their original units.  Whereas, the variables with the prefix 
!  output_ contain the data modified from the pilot file into the units
!  needed for ARPS5.3.0 3dvar.
!
!----------------------------------------------------------------------
!
      SUBROUTINE UnitConverter(station_no, station_hgt, remark1,       &
                 remark2, remark3, read_pres, read_hght, read_Wdir,    &
                 read_Wspd, NumberOfLines, read_lat, read_lon,         &
                 read_year,read_month, read_day, read_hour, read_minute)


        IMPLICIT NONE
        CHARACTER(LEN=21) OutName
        INTEGER*4 i                     ! Counter in the z-direction
        INTEGER*4 station_no            ! station number
        INTEGER*4 station_hgt           ! input station hieght
        INTEGER*4 NumberOfLines         ! number of data in the vertical
        INTEGER*4 read_lat              ! Latitude  [degree*100]
        INTEGER*4 read_lon              ! Longitude [degree*100]
        INTEGER*4 read_year             ! year
        INTEGER*4 read_month            ! month
        INTEGER*4 read_day              ! day
        INTEGER*4 read_hour             ! hour
        INTEGER*4 read_minute           ! minute
        REAL*4 output_station_hgt       ! output station hieght
        REAL*4 output_lat               ! Latitude  [degree]
        REAL*4 output_lon               ! Longitude [degree]
        INTEGER*4, DIMENSION(NumberOfLines)   :: read_pres ! Pressure 
        INTEGER*4, DIMENSION(NumberOfLines)   :: read_hght ! Hieght
        INTEGER*4, DIMENSION(NumberOfLines)   :: read_Wdir ! Wind dir
        INTEGER*4, DIMENSION(NumberOfLines)   :: read_Wspd ! Wind spd
        INTEGER*4, DIMENSION(NumberOfLines)   :: remark1   ! pres flag
        INTEGER*4, DIMENSION(2*NumberOfLines) :: remark2   ! var flag
        INTEGER*4, DIMENSION(NumberOfLines)   :: remark3   ! wind flag         
        REAL*4, DIMENSION(NumberOfLines)  :: output_pres   ! Pressure  
        REAL*4, DIMENSION(NumberOfLines)  :: output_Wdir   ! Wind Dir 
        REAL*4, DIMENSION(NumberOfLines)  :: output_Wspd   ! Wind Speed 
        REAL*4, DIMENSION(NumberOfLines)  :: output_hght   ! Height
!
!! Simple unit and variable types conversions.
!
        output_station_hgt = station_hgt*1.
        output_lat  = read_lat/100.
        output_lon  = read_lon/100.
        output_pres = read_pres/10.
        output_hght = read_hght
        output_Wdir    = read_Wdir
        output_Wspd    = read_Wspd
!
!! Change missing data which is represented one way in the temp file
!! into the -99., which is required for the model.
!
        DO i = 1, NumberofLines, 1
          IF (output_pres(i) == -999.9) THEN
            output_pres(i) = -99.
          ENDIF
        ENDDO

        DO i = 1, NumberofLines, 1
          IF (output_hght(i) == -9999.) THEN
            output_hght(i) = -99.
          ENDIF
        ENDDO
!
!! This calls on a subroutine that outputs the flag data into a readable
!! format.
!
!        CALL pilotFlags(output_lat, output_lon, read_year,          &
!             read_month, read_day, read_hour, read_minute, remark1,    &
!             remark2, remark3, NumberOfLines, read_Wspd,               &
!             output_Wspd)
!
!! This calls a subroutine that creates the name of the *.snd file and
!! stores it in the OutName variable.
!
        CALL createOutName(read_year, read_month, read_day, read_hour, &
             read_minute, OutName)
!
!! This calls a subroutine that creates the *.snd file and inputs all
!! the data necessary for data assimulation.
!
        CALL outputIntoAscii(station_no, output_station_hgt,           &
             NumberOfLines, output_lat, output_lon, output_pres,       &
             output_hght, output_Wdir, output_Wspd, OutName)
        
        RETURN

      ENDSUBROUTINE
!
!-----------------------------------------------------------------------
!
!  SUBROUTINE createOutName
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Creates the name of the *.snd file and  returns the name back in
!  the OutName variable.
!
!-----------------------------------------------------------------------
!
      SUBROUTINE createOutName(year, month, day, hour, minute, OutName)
        IMPLICIT NONE
        CHARACTER(LEN=21) OutName             ! *.snd filename variable
        CHARACTER(LEN=2)  centuryChar         ! 19/20 century
        INTEGER*4 fileHandle
        INTEGER*4 year
        INTEGER*4 month
        INTEGER*4 day
        INTEGER*4 hour
        INTEGER*4 minute
!
!! OutName variable format
!
        880     FORMAT(A5,A2,I2.2,I2.2,I2.2,I2.2,I2.2,A4)
!
!! Calculates what century the data is from.
!
        IF(year >= 50) THEN
          centuryChar = "19"
        ELSE
          centuryChar = "20"
        ENDIF
!
!! Opens up a dummy file, which writes out the OutName variable, which
!! is later on read into the variable.  Once its done, this dummy file
!! gets deleted.
!         
        fileHandle = 12
        OPEN(UNIT=fileHandle, FILE='namefile') 
        WRITE(fileHandle,880) "pilot", centuryChar, year, month, day,  &
                              hour, minute, ".snd"
        CLOSE(fileHandle)
        fileHandle = 13
        OPEN(UNIT=fileHandle, FILE='namefile')
        READ(fileHandle,"A21") OutName
        CLOSE(UNIT=fileHandle, DISP='DELETE')        
        RETURN
      ENDSUBROUTINE
!
!-----------------------------------------------------------------------
!
!  SUBROUTINE outputIntoAscii
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  This subroutine outputs the data into an *.snd file.
!
!-----------------------------------------------------------------------
!

      SUBROUTINE outputIntoAscii(station_no, station_hgt,              &
                 NumberOfLines, lat, lon, pres, hgt, Wdir, Wspd,       &
                 OutName)

        IMPLICIT NONE

        INTEGER*4 i                      ! Counter in the z-direction
        INTEGER*4 fileHandle
        INTEGER*4 station_no             ! station number
        REAL*4    station_hgt            ! station height
        INTEGER*4 NumberOfLines          ! number of line of useful data
        CHARACTER(LEN=21) OutName        ! *snd filename variable
        REAL*4    lat                               ! Latitude  [degree]
        REAL*4    lon                               ! Longitude [degree]
        REAL*4, DIMENSION(NumberOfLines)  :: pres   ! Pressure    [mb]
        REAL*4, DIMENSION(NumberOfLines)  :: hgt    ! Height      [gpm]
        REAL*4, DIMENSION(NumberOfLines)  :: Wdir   ! Wind Dir    [deg]
        REAL*4, DIMENSION(NumberOfLines)  :: Wspd   ! Wind Speed  [m/s]
!
!! Formating statements for the *.snd file. FORMAT 860 is for the
!! header, while FORMAT 870 is for the body of data.
!
860     FORMAT(2I12,f11.4,f15.4,F15.0,5x,A3)
870     FORMAT(6F10.2)
!
!! Opening and writing into the file the header and body of data, and
!! append data (only if it fits within an one hour time window).
!
        fileHandle = 13

        OPEN(UNIT=fileHandle,FILE=OutName,POSITION='APPEND')
        WRITE(fileHandle,860) station_no, NumberOfLines, lat, lon,     &
                              station_hgt, "plt"
        DO i = 0, NumberOfLines-1, 1
          IF (pres(i) .EQ. -99.) THEN
            WRITE(fileHandle,870) hgt(i), pres(i), -99., -99., Wdir(i),&
                                  Wspd(i)
          ELSE
          ENDIF
        ENDDO
        CLOSE(fileHandle)
        
        RETURN

      ENDSUBROUTINE
!
!-----------------------------------------------------------------------
!
!  SUBROUTINE pilotFlags
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  The temp file had some flags posted on its data.  This subroutine
!  will interprete the numberical flag numbers to their actual meanings.
!
!  This part of the code is turned off for efficiency.
!
!-----------------------------------------------------------------------
!
      SUBROUTINE pilot(lat, lon, year, month, day, hour, minute,       &
                 remark1, remark2, remark3, NumberOfLines, in_Wspd,    &
                 out_Wspd)
                                
        IMPLICIT NONE
        
        INTEGER*4 i
        INTEGER*4 year
        INTEGER*4 month
        INTEGER*4 day
        INTEGER*4 hour
        INTEGER*4 minute
        REAL*4 lat 
        REAL*4 lon 
        INTEGER*4 NumberOfLines
        INTEGER*4, DIMENSION(NumberOfLines)   :: remark1
        INTEGER*4, DIMENSION(3*NumberOfLines) :: remark2
        INTEGER*4, DIMENSION(NumberOfLines)   :: remark3
        INTEGER*4, DIMENSION(NumberOfLines)   :: in_Wspd
        INTEGER*4, DIMENSION(NumberOfLines)   :: out_Wspd
    
        DO i = 1, NumberOfLines, 1
          SELECT CASE(remark1(i))
            CASE(1)
              WRITE(*,*) "surface level"
            CASE(4)
              WRITE(*,*) "tropopause significance level"
            CASE(14)
              WRITE(*,*) "standard level"
            CASE(15)
              WRITE(*,*) "maximum wind speed level"
          END SELECT
        ENDDO

        DO i = 1, 3*NumberOfLines, 1
          SELECT CASE(remark2(i))
            CASE(0)
              WRITE(*,*) "No check"
            CASE(1)
              WRITE(*,*) "Correct"
            CASE(3)
              WRITE(*,*) "Error"
            CASE(9)
              WRITE(*,*) "Missing"
          END SELECT
        ENDDO

        DO i = 1, NumberOfLines, 1
          SELECT CASE(remark3(i))
            CASE(0)
              WRITE(*,*) "No way to tell if the unit is converted"
            CASE(1)
              WRITE(*,*) "Original speed = ff*2 kts"
            CASE(2)
              WRITE(*,*) "Original speed = ff*2 + 1 kts"
            CASE(3)
              WRITE(*,*) "Unit for ff is m/s"
          END SELECT
        ENDDO

        RETURN
      ENDSUBROUTINE
!
!-----------------------------------------------------------------------
!
! End of the code.
!-----------------------------------------------------------------------
!
