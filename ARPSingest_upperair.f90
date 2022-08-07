!
!##################################################################
!##################################################################
!######                                                      ######
!######                  ingest_upperair.f09                 ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!##################################################################
!
!!
!! How to run this code:
!!
!! ingest_upperair.exe [-t YYMMDDHH] [-latmin min_lat] [-latmax max_lat]
!!   [-lonmin min_lon] [-lonmax max_lon] [-dt delta_mins] [+dt delta_mins]
!!   [-o output_file]
!!
!
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  The airep file provided by the Central Weather Bureau is the data  
!  output from an airplane flight, which are not fixed in location and
!  sample the atmosphere every hour.  The airplane measures:
!
!         Pressure [mb*10]                        Height [gpm]
!         Temperature [deg Celsius*10]            Wind Speed [m/s]
!         Wind Direction [deg]
!
!  The pilot file provided by the Central Weather Bureau is the data
!  output that is fixed in location and sample the atmosphere every
!  twelve hours.  The instrument measures:
!         Pressure [mb*10]                        Height [gpm]
!         Wind Speed [m/s]                        Wind Direction [deg]
!
!  The shiptemp file provided by the Central Weather Bureau is the data
!  output from a rawinsonde that was launched from a ship, which are
!  not-fixed in location and sample the atmosphere every twelve hours.
!  The rawinsonde measures:
!         Pressure [mb*10]                        Height   [gpm]
!         Temperature [deg Celsius*10]            Wind Speed [m/s]
!         Dewpoint Depression [deg Celsius*10]    Wind Direction [deg]
!
!  The tempdrop file provided by the Central Weather Bureau is the data
!  output from a dropwindsonde, which are not fixed in location and are
!  irregular temporally.  The dropwindsonde measures:
!         Pressure [mb*10]                        Height [gpm]
!         Temperature [deg Celsius*10]            Wind Speed [m/s]
!         Dewpoint Depression [deg Celsius*10]    Wind Direction [deg]
!
!  According to the National Hurricane Center typical measurement errors
!  for a GPS dropwindsonde are:
!                     pressure          ~ 1mb
!                     temperature       ~ 0.2K
!                     relative humidity ~ 5-10%
!                     wind              < 1m/s
!
!  The temp file provided by the Central Weather Bureau is the data
!  output from a rawinsonde, which are fixed in location and sample the
!  atmosphere every twelve hours.  The rawinsonde measures:
!         Pressure [mb*10]                        Height [gpm]
!         Temperature [deg Celsius*10]            Wind Speed [m/s]
!         Dewpoint Depression [deg Celsius*10]    Wind Direction [deg]
!
!  This code:
!    1. Reads in the data measured from the each of the aforementioned
!       intruments.
!    2. Coverts the data into the units needed for assimulation into the
!       ARPS model:
!         Pressure [mb]                          Height [gpm]
!         Temperature [deg Celsius]              Wind Speed [m/s]
!         Dewpoint Temperature [deg Celsius]     Wind Direction [deg]
!    3. Outputs the data into an *.snd file
!
!-----------------------------------------------------------------------
!
!  AUTHOR:
!
!  Skylar Hernandez, (07/11/2011)
!
!-----------------------------------------------------------------------
!
      PROGRAM ingest_upperair
        IMPLICIT NONE

        CHARACTER(LEN=11) airepFile         ! Airep filename
        CHARACTER(LEN=11) pilotFile         ! Pilot filename
        CHARACTER(LEN=11) shiptempFile      ! rawinsonde filename
        CHARACTER(LEN=11) tempFile          ! rawinsonde filename
        CHARACTER(LEN=11) tempdropFile      ! Dropwindsode filename
        CHARACTER(LEN=3)  airepprefix
        CHARACTER(LEN=3)  pilotprefix
        CHARACTER(LEN=3)  shiptempprefix
        CHARACTER(LEN=3)  tempprefix
        CHARACTER(LEN=3)  tempdropprefix
        CHARACTER(LEN=3)  instrument        ! three letter identifier
!! Command line arguement variables:
        INTEGER*4 IARGC                     ! Intrinsic subroutine
        INTEGER*4 nargs                     ! # of command line args
        INTEGER*4 iargs, jargs              ! index for command line args
        CHARACTER(LEN=7) cargs              ! var for command line options
        CHARACTER(LEN=8) date               ! Input date (MUST BE INCLUDED)
        REAL*4 min_lat, max_lat             ! User specified lat BC
        REAL*4 min_lon, max_lon             ! User specified lon BC
        INTEGER*4 delta_mins_minus, delta_mins_add ! User specified time BC
        CHARACTER(LEN=20) output_filename   ! User specified output filename
!! Command line indicators
        LOGICAL min_lat_ind, max_lat_ind
        LOGICAL min_lon_ind, max_lon_ind
        LOGICAL delta_mins_minus_ind, delta_mins_add_ind
        LOGICAL output_filename_ind
!! Debug level:
!!    if debug = 0 the basic information is printed out to the screen
!!    if debug = 1 the basic info plus user settings are printed out
!!    if debug = 9 the data gets outputted into the screen
        INTEGER*4, PARAMETER :: debug = 0
!
!! Each data file has a prefix.
!
        airepprefix    = "tua"
        pilotprefix    = "tup"
!       shiptempprefix = "tus"
        tempprefix     = "tus"
        tempdropprefix = "tux"                
!
!! Grab the command line arguement: 
!!
!! Process command line: [-t YYMMDDHH] [-latmin min_lat] [-latmax max_lat]
!!   [-lonmin min_lon] [-lonmax max_lon] [-dt delta_mins] [+dt delta_mins]
!!   [-o output_file]
!!
!
        nargs = IARGC()

        iargs = 1
        DO jargs = 1, nargs, 2
          CALL GETARG(iargs,cargs)
          IF (cargs(1:2) /= '-t') THEN
            WRITE(*,*) 'ERROR: Please enter a date \"-t YYMMDDHH\"!'
            STOP
          END IF
        ENDDO

        IF (debug == 1) THEN
          WRITE(*,*) 'Number of command line arguments: ', nargs
        END IF
!     
!! Setting the indicators to zero, if there is a command line arguement other
!! than date (which is required) then the index gets set to one.        
!
        min_lat_ind = .FALSE.
        max_lat_ind = .FALSE.
        min_lon_ind = .FALSE.
        max_lon_ind = .FALSE.
        delta_mins_minus_ind = .FALSE.
        delta_mins_add_ind   = .FALSE.
        output_filename_ind  = .FALSE.

        iargs = 1
        DO jargs = 1, nargs, 2 
          CALL GETARG(iargs,cargs)
          iargs = iargs + 1

          IF (cargs(1:2) == '-t') THEN
             CALL GETARG(iargs,date)
             IF (debug == 1) THEN
               WRITE(*,*) 'User Specified date: ', date
             END IF
          ELSE IF (cargs(1:7) == '-latmin') THEN
             CALL GETARG(iargs,cargs)
             READ(cargs,*) min_lat
             min_lat_ind = .TRUE.
             IF (debug == 1) THEN
               WRITE(*,'(A25,F5.2)') 'User Specified min lat: ', min_lat
             END IF
! Error Check
             IF (ABS(min_lat) .GT. 90.00) THEN
               WRITE(*,'(A35)') 'ERROR: The minimum latitude is < -90'
               STOP
             END IF

          ELSE IF (cargs(1:7) == '-latmax') THEN
             CALL GETARG(iargs,cargs)
             READ(cargs,*) max_lat
             max_lat_ind = .TRUE.
             IF (debug == 1) THEN
               WRITE(*,'(A25,F5.2)') 'User Specified max lat: ', max_lat
             END IF
! Error Check
             IF (ABS(max_lat) .GT. 90.00) THEN
               WRITE(*,'(A35)') 'ERROR: The maximum latitude is > 90'
               STOP
             END IF

          ELSE IF (cargs(1:7) == '-lonmin') THEN
             CALL GETARG(iargs,cargs)
             READ(cargs,*) min_lon
             min_lon_ind = .TRUE.
             IF (debug == 1) THEN
               WRITE(*,'(A25,F5.2)') 'User Specified min lon: ', min_lon
             END IF
! Error Check
             IF (ABS(min_lon) .GT. 180.00) THEN
               WRITE(*,'(A39)') 'ERROR: The minimum longitude is < -180'
               STOP
             END IF

          ELSE IF (cargs(1:7) == '-lonmax') THEN
             CALL GETARG(iargs,cargs)
             READ(cargs,*) max_lon
             max_lon_ind = .TRUE.
             IF (debug == 1) THEN
               WRITE(*,'(A25,F5.2)') 'User Specified max lon: ', max_lon
             END IF
! Error Check
             IF (ABS(max_lon) .GT. 90.00) THEN
               WRITE(*,'(A39)') 'ERROR: The maximum longitude is > 180'
               STOP
             END IF

          ELSE IF (cargs(1:3) == '-dt') THEN
             CALL GETARG(iargs,cargs)
             READ(cargs,*) delta_mins_minus
             delta_mins_minus_ind = .TRUE.
             IF (debug == 1) THEN
               WRITE(*,'(A29,I5)') 'User Specified -delta time: ',     &
               delta_mins_minus
          END IF

          ELSE IF (cargs(1:3) == '+dt') THEN
             CALL GETARG(iargs,cargs)
             READ(cargs,*) delta_mins_add
             delta_mins_add_ind = .TRUE.
             IF (debug == 1) THEN
               WRITE(*,'(A29,I5)') 'User Specified +delta time: ',     &
               delta_mins_add
             END IF

          ELSE IF (cargs(1:2) == '-o') THEN
             CALL GETARG(iargs,cargs)
             READ(cargs,*) output_filename
             output_filename_ind = .TRUE.
             IF (debug == 1) THEN
               WRITE(*,*) 'User Specified output file name: ',         &
               output_filename
             END IF
          END IF
          iargs = iargs + 1
          
        END DO
      
!        WRITE(*,*) 'User Specified data vector: '
!        WRITE(*,*) min_lat_ind, max_lat_ind, min_lon_ind, max_lon_ind, &
!                   delta_mins_minus_ind, delta_mins_add_ind,           &
!                   output_filename_ind


!
!! Create the file names
!
        airepFile = airepprefix // date
        pilotFile = pilotprefix // date
        shiptempFile = shiptempprefix // date
        tempFile = tempprefix // date
        tempdropFile = tempdropprefix // date

        instrument = "ARP"
        CALL readairepfile(debug,airepFile, instrument, min_lat,       &
        max_lat, min_lon, max_lon, delta_mins_minus, delta_mins_add,   &
        output_filename, min_lat_ind, max_lat_ind, min_lon_ind,        &
        max_lon_ind, delta_mins_minus_ind, delta_mins_add_ind,         &
        output_filename_ind)

        WRITE(*,*) "Completed:  ", airepFile

        instrument = "TMP"
        CALL readtempfile(debug, tempFile, instrument, min_lat,        &
        max_lat, min_lon, max_lon, delta_mins_minus, delta_mins_add,   &
        output_filename, min_lat_ind, max_lat_ind, min_lon_ind,        &
        max_lon_ind, delta_mins_minus_ind, delta_mins_add_ind,         &
        output_filename_ind)

        WRITE(*,*) "Completed:  ", tempFile

        instrument = "DRP"
        CALL readdroptempfile(debug, tempdropFile, instrument, min_lat,&
        max_lat, min_lon, max_lon, delta_mins_minus, delta_mins_add,   &
        output_filename, min_lat_ind, max_lat_ind, min_lon_ind,        &
        max_lon_ind, delta_mins_minus_ind, delta_mins_add_ind,         &
        output_filename_ind)

        WRITE(*,*) "Completed:  ", tempdropFile

!       instrument = "stp"
!       CALL readshiptempfile(debug,tempFile, instrument, min_lat,     &
!       max_lat, min_lon, max_lon, delta_mins_minus, delta_mins_add,   &
!       output_filename, min_lat_ind, max_lat_ind, min_lon_ind,        &
!       max_lon_ind, delta_mins_minus_ind, delta_mins_add_ind,         &
!       output_filename_ind)

!       WRITE(*,*) "Completed:  ", shiptempFile

        instrument = "PLT"
        CALL readpilotfile(debug, pilotFile, instrument, min_lat,      &
        max_lat, min_lon, max_lon, delta_mins_minus, delta_mins_add,   &
        output_filename, min_lat_ind, max_lat_ind, min_lon_ind,        &
        max_lon_ind, delta_mins_minus_ind, delta_mins_add_ind,         &
        output_filename_ind)

        WRITE(*,*) "Completed:  ", pilotFile


      END PROGRAM
!
!----------------------------------------------------------------------
!
!  SUBROUTINE readairepfile
!
!----------------------------------------------------------------------
!
!  PURPOSE:
!
!  This subroutine will Read in the data measured from the airep.
!
!----------------------------------------------------------------------
!

      SUBROUTINE readairepfile(debug, airepFile, instrument, min_lat,  &
      max_lat, min_lon, max_lon, delta_mins_minus, delta_mins_add,     &
      output_filename, min_lat_ind, max_lat_ind, min_lon_ind,          &
      max_lon_ind, delta_mins_minus_ind, delta_mins_add_ind,           &
      output_filename_ind)

        IMPLICIT NONE

        INTEGER*4 FileHandle
        INTEGER*4 stat                    ! file stat
        CHARACTER(LEN=11) airepFile       ! airep filename
        CHARACTER(LEN=3)  instrument      ! instrument indentifier
        INTEGER*4 i                       ! Counter in the z-direction
        INTEGER*4 station_no              ! Station Number
        INTEGER*4 station_hgt             ! Station Hieght
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
        INTEGER*4, DIMENSION(1) :: read_pres   ! Pressure
        INTEGER*4, DIMENSION(1) :: read_hght   ! Height
        INTEGER*4, DIMENSION(1) :: read_temp   ! Temperature
        INTEGER*4, DIMENSION(1) :: read_dewp   ! Dewpoint
        INTEGER*4, DIMENSION(1) :: read_Wdir   ! Wind Dir
        INTEGER*4, DIMENSION(1) :: read_Wspd   ! Wind speed
        INTEGER*4, DIMENSION(1) :: remark1     ! pres flag
        INTEGER*4, DIMENSION(1) :: remark2     ! hieght flag
        INTEGER*4, DIMENSION(1) :: remark3     ! temp flag
        INTEGER*4, DIMENSION(1) :: remark4     ! Wspd flag
!! Command line variables
        REAL*4 min_lat, max_lat             ! User specified lat BC
        REAL*4 min_lon, max_lon             ! User specified lon BC
        INTEGER*4 delta_mins_minus, delta_mins_add ! User specified time BC
        CHARACTER(LEN=20) output_filename   ! User specified output filename
!! Command line indicators
        LOGICAL min_lat_ind, max_lat_ind
        LOGICAL min_lon_ind, max_lon_ind
        LOGICAL delta_mins_minus_ind, delta_mins_add_ind
        LOGICAL output_filename_ind
        INTEGER*4 debug                     ! Debug level
! 
!! Formating statements
!
1000    FORMAT (12x, 2I5, 2x, 5I2,  I3)
2000    FORMAT ( I4,  I1,  I5, I1,  I4, I1, 21X)
3000    FORMAT (10x, 2I3,  I1, 37x)
!
!! Start of the subroutine: opening and reading in the data into their 
!! respective variables and arrays.
!
        NumberOfLines = 1
        FileHandle    = 11
        redoVar       = 0
        past_hour     = 0
        past_minute   = 0

        OPEN(UNIT=FileHandle,FILE=airepFile,ACTION='READ',IOSTAT=stat)
        IF (stat==0) THEN
          DO 
!
!! This line in the code will read in the header, and it sets up the 
!! the rest of the code. 
!
            READ(FileHandle, 1000,END =10) read_lat, read_lon,         &
                 read_year, read_month, read_day, read_hour,           &
                 read_minute, read_record
!
!! This loop will ensure to keep reading a concatinated file and packages
!! the *.snd files per hour.
!
10          IF (read_hour .ne. past_hour) THEN
              past_hour = read_hour
            ELSE
              read_minute = past_minute
            ENDIF
!
!! This is the part of the code that does the actual reading in of the 
!! data (useful and useless).
! 
            READ(FileHandle,2000,END = 20) read_pres, remark1,         &
                 read_hght, remark2, read_temp, remark3
20          READ(FileHandle,3000,IOSTAT = redovar) read_Wdir,          &
                 read_Wspd, remark4
!
!! This is a set of conditions for the code to exit if it has reached
!! the end of the file or something is wrong with the input data, if 
!! not, continue running within the do loop.
!   
            IF (redovar > 0)  THEN
                ! ... something wrong ...
               WRITE(*,*) "... Something is wrong with the data file..."
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

            CALL UnitConverter(debug,station_no,station_hgt,read_pres, &
            read_hght, read_temp, read_dewp, read_Wdir, read_Wspd,     &
            NumberOfLines, read_lat, read_lon, read_year, read_month,  &
            read_day, read_hour, read_minute, instrument, min_lat,     &
            max_lat, min_lon, max_lon, delta_mins_minus,               &
            delta_mins_add, output_filename, min_lat_ind, max_lat_ind, &
            min_lon_ind, max_lon_ind, delta_mins_minus_ind,            &
            delta_mins_add_ind, output_filename_ind)
 
          END DO
        END IF

        CLOSE(FileHandle)
      END SUBROUTINE
!
!----------------------------------------------------------------------
!
!  SUBROUTINE readpilotfile
!
!----------------------------------------------------------------------
!
!  PURPOSE:
!
!  This subroutine will Read in the data measured from the pilot data.
!
!----------------------------------------------------------------------
!

      SUBROUTINE readpilotfile(debug, pilotFile, instrument, min_lat,  &
      max_lat, min_lon, max_lon, delta_mins_minus, delta_mins_add,     &
      output_filename, min_lat_ind, max_lat_ind, min_lon_ind,          &
      max_lon_ind, delta_mins_minus_ind, delta_mins_add_ind,           &
      output_filename_ind)

        IMPLICIT NONE

        INTEGER*4 FileHandle
        INTEGER*4 stat                    ! file status
        CHARACTER(LEN=11) pilotFile       ! pilot data filename
        CHARACTER(LEN=3)  instrument      ! instrument indentifier
        INTEGER*4 i                       ! Counter in the z-direction
        INTEGER*4 station_no              ! Station Number
        INTEGER*4 station_hgt             ! Station Hieght
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
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_temp  ! Temperature
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_dewp  ! Dewpoint
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_Wdir  ! Wind Dir
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_Wspd  ! Wind speed
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark1    ! pres flag
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark2    ! var flag
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark3    ! Wspd flag
!! Command line variables
        REAL*4 min_lat, max_lat             ! User specified lat BC
        REAL*4 min_lon, max_lon             ! User specified lon BC
        INTEGER*4 delta_mins_minus, delta_mins_add ! User specified time BC
        CHARACTER(LEN=20) output_filename   ! User specified output filename
!! Command line indicators
        LOGICAL min_lat_ind, max_lat_ind
        LOGICAL min_lon_ind, max_lon_ind
        LOGICAL delta_mins_minus_ind, delta_mins_add_ind
        LOGICAL output_filename_ind
        INTEGER*4 debug                     ! Debug level
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

        OPEN(UNIT=FileHandle,FILE=pilotFile,ACTION='READ',IOSTAT=stat)
        IF (stat==0) THEN
          DO
!
!! This line in the code will read in the header, and it sets up the
!! the rest of the code.
!
            READ(FileHandle, 1000, END =10) station_no, station_hgt,   &
                read_lat, read_lon, read_year, read_month, read_day,   &
                read_hour, read_minute, read_record
!
!! This loop will ensure to keep reading a concatinated file and packages
!! the *.snd files per hour.
!
10          IF (read_hour .ne. past_hour) THEN
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
            ALLOCATE(read_temp(NumberOfLines))
            ALLOCATE(read_dewp(NumberOfLines))
            ALLOCATE(read_Wdir(NumberOfLines))
            ALLOCATE(read_Wspd(NumberOfLines))
!
!! This is the part of the code that does the actual reading in of the
!! data (useful and useless).
!
            DO i = 1, NumberOfLines, 1
              READ(FileHandle,2000,IOSTAT = redovar) read_pres(i),     &
                   read_hght(i), remark1(i), read_Wdir(i),             &
                   read_Wspd(i), remark2(i), remark3(i)
20          END DO
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
            CALL UnitConverter(debug,station_no,station_hgt,read_pres, &
            read_hght, read_temp, read_dewp, read_Wdir, read_Wspd,     &
            NumberOfLines, read_lat, read_lon, read_year, read_month,  &
            read_day, read_hour, read_minute, instrument, min_lat,     &
            max_lat, min_lon, max_lon, delta_mins_minus,               &
            delta_mins_add, output_filename, min_lat_ind, max_lat_ind, &
            min_lon_ind, max_lon_ind, delta_mins_minus_ind,            &
            delta_mins_add_ind, output_filename_ind)

            DEALLOCATE(remark2)
            DEALLOCATE(remark3)
            DEALLOCATE(read_pres)
            DEALLOCATE(read_hght)
            DEALLOCATE(read_temp)
            DEALLOCATE(read_dewp)
            DEALLOCATE(read_Wdir)
            DEALLOCATE(read_Wspd)
  
          END DO
        END IF

        CLOSE(FileHandle)
      END SUBROUTINE

!
!----------------------------------------------------------------------
!
!  SUBROUTINE readshiptempfile
!
!----------------------------------------------------------------------
!
!  PURPOSE:
!
!  This subroutine will Read in the data measured from the rawinsonde.
!
!----------------------------------------------------------------------
!
      SUBROUTINE readshiptempfile(debug, shiptempFile, instrument,     &
      min_lat, max_lat, min_lon, max_lon, delta_mins_minus,            &
      delta_mins_add, output_filename, min_lat_ind, max_lat_ind,       &
      min_lon_ind, max_lon_ind, delta_mins_minus_ind,                  &
      delta_mins_add_ind, output_filename_ind)

        IMPLICIT NONE

        INTEGER*4 FileHandle
        INTEGER*4 stat                    ! file status
        CHARACTER(LEN=11) shiptempFile    ! ship rawinsonde filename
        CHARACTER(LEN=3)  instrument      ! instrument indentifier
        INTEGER*4 i                       ! Counter in the z-direction
        INTEGER*4 station_no              ! Station Number
        INTEGER*4 station_hgt             ! Station Hieght
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
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_temp  ! Temperature
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_dewp  ! Dewpoint
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_Wdir  ! Wind Dir
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_Wspd  ! Wind speed
        CHARACTER(LEN=3), ALLOCATABLE, DIMENSION(:) :: remark1 ! pres flag
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark2    ! var flag
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark3    ! Wspd flag
!! Command line variables
        REAL*4 min_lat, max_lat             ! User specified lat BC
        REAL*4 min_lon, max_lon             ! User specified lon BC
        INTEGER*4 delta_mins_minus, delta_mins_add ! User specified time BC
        CHARACTER(LEN=20) output_filename   ! User specified output filename
!! Command line indicators
        LOGICAL min_lat_ind, max_lat_ind
        LOGICAL min_lon_ind, max_lon_ind
        LOGICAL delta_mins_minus_ind, delta_mins_add_ind
        LOGICAL output_filename_ind
        INTEGER*4 debug                     ! Debug level
!
!! Formating statements
!
1000    FORMAT (3x, I5, 4x, 2I5,  2x, 5I2,  I3)
2000    FORMAT (A2, 2I5,  I2,  I4,  I2,  I4,  I2, 2I3, I2, 3x)
3000    FORMAT (37x)
!
!! Start of the subroutine: opening and reading in the data into their
!! respective variables and arrays.
!
        FileHandle  = 11
        redoVar     = 0
        past_hour   = 0
        past_minute = 0

        OPEN(UNIT=FileHandle,FILE=shiptempFile,ACTION='READ',          &
             IOSTAT=stat)
        IF (stat==0) THEN
          DO
!
!! This line in the code will read in the header, and it sets up the
!! the rest of the code.
!
            READ(FileHandle, 1000, END = 10) station_no, read_lat,     &
                 read_lon, read_year, read_month, read_day, read_hour, &
                 read_minute, read_record
!
!! This loop will ensure to keep reading a concatinated file and packages
!! the *.snd files per hour.
!
10          IF (read_hour .ne. past_hour) THEN
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
            NumberOfLines = read_record - 2
!
!! Allocate the arrays by the length of the data that is useful for the
!! model.  Since the remark2 variable is used for three different
!! variables it gets allocated accordingly.
!
            ALLOCATE(remark1(NumberOfLines))
            ALLOCATE(remark2(3*NumberOfLines))
            ALLOCATE(remark3(NumberOfLines))
            ALLOCATE(read_pres(NumberOfLines))
            ALLOCATE(read_hght(NumberOfLines))
            ALLOCATE(read_temp(NumberOfLines))
            ALLOCATE(read_dewp(NumberOfLines))
            ALLOCATE(read_Wdir(NumberOfLines))
            ALLOCATE(read_Wspd(NumberOfLines))
!
!! This is the part of the code that does the actual reading in of the
!! data (useful and useless).
!
           DO i = 1, NumberOfLines, 1
              READ(FileHandle,2000, END = 20) remark1(i), read_pres(i),&
                   read_hght(i), remark2(3*i-2), read_temp(i),         &
                   remark2(3*i-1), read_dewp(i), remark2(3*i),         &
                   read_Wdir(i), read_Wspd(i), remark3(i)
20            CONTINUE
            END DO
            READ(FileHandle,3000, IOSTAT = redovar)
!
!! This is a set of conditions for the code to exit if it has reached
!! the end of the file or something is wrong with the input data, if
!! not, continue running within the do loop.
!
            IF (redovar > 0)  THEN
              ! ... something wrong ...
              WRITE(*,*) "ERROR: Something is wrong with the data file."
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
            CALL UnitConverter(debug,station_no,station_hgt,read_pres, &
            read_hght, read_temp, read_dewp, read_Wdir, read_Wspd,     &
            NumberOfLines, read_lat, read_lon, read_year, read_month,  &
            read_day, read_hour, read_minute, instrument, min_lat,     &
            max_lat, min_lon, max_lon, delta_mins_minus,               &
            delta_mins_add, output_filename, min_lat_ind, max_lat_ind, &
            min_lon_ind, max_lon_ind, delta_mins_minus_ind,            &
            delta_mins_add_ind, output_filename_ind)

            DEALLOCATE(remark1)
            DEALLOCATE(remark2)
            DEALLOCATE(remark3)
            DEALLOCATE(read_pres)
            DEALLOCATE(read_hght)
            DEALLOCATE(read_temp)
            DEALLOCATE(read_dewp)
            DEALLOCATE(read_Wdir)
            DEALLOCATE(read_Wspd)
 
          END DO
        END IF

        CLOSE(FileHandle)
      END SUBROUTINE
!
!----------------------------------------------------------------------
!
!  SUBROUTINE readdroptempfile
!
!----------------------------------------------------------------------
!
!  PURPOSE:
!
!  This subroutine will Read in the data measured from the dropsonde.
!
!----------------------------------------------------------------------
!
      SUBROUTINE readdroptempfile(debug, tempdropFile, instrument,     &
      min_lat, max_lat, min_lon, max_lon, delta_mins_minus,            &
      delta_mins_add, output_filename, min_lat_ind, max_lat_ind,       &
      min_lon_ind, max_lon_ind, delta_mins_minus_ind,                  &
      delta_mins_add_ind, output_filename_ind)

        IMPLICIT NONE

        INTEGER*4 FileHandle
        INTEGER*4 stat                    ! file status
        CHARACTER(LEN=11) tempdropFile    ! dropsonde filename
        CHARACTER(LEN=3)  instrument      ! instrument indentifier
        INTEGER*4 i                       ! Counter in the z-direction
        INTEGER*4 station_no              ! Station Number
        INTEGER*4 station_hgt             ! Station Hieght
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
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_temp  ! Temperature
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_dewp  ! Dewpoint
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_Wdir  ! Wind Dir
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_Wspd  ! Wind speed
        CHARACTER(LEN=3) EndOfFileChecker ! Checks for the end of file
        CHARACTER(LEN=3), ALLOCATABLE, DIMENSION(:) :: remark1 ! pres flag
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark2    ! var flag
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark3    ! Wspd flag
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark4    ! Wsfc flag
!! Command line variables
        REAL*4 min_lat, max_lat             ! User specified lat BC
        REAL*4 min_lon, max_lon             ! User specified lon BC
        INTEGER*4 delta_mins_minus, delta_mins_add ! User specified time BC
        CHARACTER(LEN=20) output_filename   ! User specified output filename
!! Command line indicators
        LOGICAL min_lat_ind, max_lat_ind
        LOGICAL min_lon_ind, max_lon_ind
        LOGICAL delta_mins_minus_ind, delta_mins_add_ind
        LOGICAL output_filename_ind
        INTEGER*4 debug                     ! Debug level
!
!! Formating statements
!
1000    FORMAT (A3,  9x, 2I5, 2x, 5I2, I3)
2000    FORMAT (A2, 2I5,  I2, I4,  I2, I4, I2, 2I3, I2, I3)
3000    FORMAT (37x)
!
!! Start of the subroutine: opening and reading in the data into their
!! respective variables and arrays.
!
        FileHandle  = 11
        redoVar     = 0
        past_hour   = 0
        past_minute = 0

        OPEN(UNIT=FileHandle,FILE=tempdropFile,ACTION='READ',          &
             IOSTAT=stat)
        IF (stat==0) THEN
          DO
!
!! This line in the code will read in the header, and it sets up the
!! the rest of the code.
!
            READ(FileHandle, 1000, END =10) EndOfFileChecker, read_lat,&
                 read_lon, read_year, read_month, read_day, read_hour, &
                 read_minute, read_record
!
!! This loop will ensure to keep reading a concatinated file and packages
!! the *.snd files per hour.
!
10          IF (read_hour .ne. past_hour) THEN
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
            NumberOfLines = read_record - 5
!
!! Allocate the arrays by the length of the data that is useful for the
!! model.  Since the remark2 variable is used for three different
!! variables it gets allocated accordingly.
!
            ALLOCATE(remark1(NumberOfLines))
            ALLOCATE(remark2(3*NumberOfLines))
            ALLOCATE(remark3(NumberOfLines))
            ALLOCATE(remark4(NumberOfLines))
            ALLOCATE(read_pres(NumberOfLines))
            ALLOCATE(read_hght(NumberOfLines))
            ALLOCATE(read_temp(NumberOfLines))
            ALLOCATE(read_dewp(NumberOfLines))
            ALLOCATE(read_Wdir(NumberOfLines))
            ALLOCATE(read_Wspd(NumberOfLines))
!
!! This is the part of the code that does the actual reading in of the
!! data (useful and useless).
!
            DO i = 1, NumberOfLines, 1
              READ(FileHandle,2000, END = 20) remark1(i), read_pres(i),&
                   read_hght(i), remark2(3*i-2), read_temp(i),         &
                   remark2(3*i-1), read_dewp(i), remark2(3*i),         &
                   read_Wdir(i), read_Wspd(i), remark4(i), remark3(i)
20          END DO
            READ(FileHandle,3000, END = 30)
30          READ(FileHandle,3000, END = 40)
40          READ(FileHandle,3000, END = 50)
50          READ(FileHandle,3000, IOSTAT = redovar)
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
            CALL UnitConverter(debug,station_no,station_hgt,read_pres, &
            read_hght, read_temp, read_dewp, read_Wdir, read_Wspd,     &
            NumberOfLines, read_lat, read_lon, read_year, read_month,  &
            read_day, read_hour, read_minute, instrument, min_lat,     &
            max_lat, min_lon, max_lon, delta_mins_minus,               &
            delta_mins_add, output_filename, min_lat_ind, max_lat_ind, &
            min_lon_ind, max_lon_ind, delta_mins_minus_ind,            &
            delta_mins_add_ind, output_filename_ind)

            DEALLOCATE(remark1)
            DEALLOCATE(remark2)
            DEALLOCATE(remark3)
            DEALLOCATE(remark4)
            DEALLOCATE(read_pres)
            DEALLOCATE(read_hght)
            DEALLOCATE(read_temp)
            DEALLOCATE(read_dewp)
            DEALLOCATE(read_Wdir)
            DEALLOCATE(read_Wspd)

          END DO
        END IF

        CLOSE(FileHandle)
      END SUBROUTINE
!
!----------------------------------------------------------------------
!
!  SUBROUTINE readtempfile
!
!----------------------------------------------------------------------
!
!  PURPOSE:
!
!  This subroutine will Read in the data measured from the rawinsonde.
!
!----------------------------------------------------------------------
!
      SUBROUTINE readtempfile(debug, tempFile, instrument, min_lat,    &
      max_lat, min_lon, max_lon, delta_mins_minus, delta_mins_add,     &
      output_filename, min_lat_ind, max_lat_ind, min_lon_ind,          &
      max_lon_ind, delta_mins_minus_ind, delta_mins_add_ind,           &
      output_filename_ind)

        IMPLICIT NONE

        INTEGER*4 FileHandle
        INTEGER*4 stat                    ! file status
        CHARACTER(LEN=11) tempFile        ! rawinsonde filename
        CHARACTER(LEN=3)  instrument      ! instrument indentifier
        INTEGER*4 i                       ! Counter in the z-direction
        INTEGER*4 station_no              ! Station Number
        INTEGER*4 station_hgt             ! Station Hieght
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
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_temp  ! Temperature
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_dewp  ! Dewpoint
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_Wdir  ! Wind Dir
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: read_Wspd  ! Wind speed
        CHARACTER(LEN=3), ALLOCATABLE, DIMENSION(:) :: remark1 ! pres flag
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark2    ! var flag
        INTEGER*4, ALLOCATABLE, DIMENSION(:) :: remark3    ! Wspd flag
!! Command line variables
        REAL*4 min_lat, max_lat             ! User specified lat BC
        REAL*4 min_lon, max_lon             ! User specified lon BC
        INTEGER*4 delta_mins_minus, delta_mins_add ! User specified time BC
        CHARACTER(LEN=20) output_filename   ! User specified output filename
!! Command line indicators
        LOGICAL min_lat_ind, max_lat_ind
        LOGICAL min_lon_ind, max_lon_ind
        LOGICAL delta_mins_minus_ind, delta_mins_add_ind
        LOGICAL output_filename_ind
        INTEGER*4 debug                     ! Debug level
!
!! Formating statements
!
1000    FORMAT (3x, I5, I4, 2I5,  2x, 5I2,  I3)
2000    FORMAT (A2, 2I5,  I2,  I4,  I2,  I4,  I2, 2I3, I2, 3x)
3000    FORMAT (37x)
!
!! Start of the subroutine: opening and reading in the data into their
!! respective variables and arrays.
!
        FileHandle  = 11
        redoVar     = 0
        past_hour   = 0
        past_minute = 0

        OPEN(UNIT=FileHandle,FILE=tempFile,ACTION='READ',IOSTAT=stat)
        IF (stat==0) THEN
          DO
!
!! This line in the code will read in the header, and it sets up the
!! the rest of the code.
!
            READ(FileHandle, 1000, END = 10) station_no, station_hgt,  &
                 read_lat, read_lon, read_year, read_month, read_day,  &
                 read_hour, read_minute, read_record
!
!! This loop will ensure to keep reading a concatinated file and packages
!! the *.snd files per hour.
!
10          IF (read_hour .ne. past_hour) THEN
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
            NumberOfLines = read_record - 2
!
!! Allocate the arrays by the length of the data that is useful for the
!! model.  Since the remark2 variable is used for three different
!! variables it gets allocated accordingly.
!
            ALLOCATE(remark1(NumberOfLines))
            ALLOCATE(remark2(3*NumberOfLines))
            ALLOCATE(remark3(NumberOfLines))
            ALLOCATE(read_pres(NumberOfLines))
            ALLOCATE(read_hght(NumberOfLines))
            ALLOCATE(read_temp(NumberOfLines))
            ALLOCATE(read_dewp(NumberOfLines))
            ALLOCATE(read_Wdir(NumberOfLines))
            ALLOCATE(read_Wspd(NumberOfLines))
!
!! This is the part of the code that does the actual reading in of the
!! data (useful and useless).
!
           DO i = 1, NumberOfLines, 1
              READ(FileHandle,2000, END = 20) remark1(i), read_pres(i),&
                   read_hght(i), remark2(3*i-2), read_temp(i),         &
                   remark2(3*i-1), read_dewp(i), remark2(3*i),         &
                   read_Wdir(i), read_Wspd(i), remark3(i)
20            CONTINUE
            END DO
            READ(FileHandle,3000, IOSTAT = redovar)
!
!! This is a set of conditions for the code to exit if it has reached
!! the end of the file or something is wrong with the input data, if
!! not, continue running within the do loop.
!
            IF (redovar > 0)  THEN
              ! ... something wrong ...
              WRITE(*,*) "ERROR: Something is wrong with the data file."
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
            CALL UnitConverter(debug,station_no,station_hgt,read_pres, &
            read_hght, read_temp, read_dewp, read_Wdir, read_Wspd,     &
            NumberOfLines, read_lat, read_lon, read_year, read_month,  &
            read_day, read_hour, read_minute, instrument, min_lat,     &
            max_lat, min_lon, max_lon, delta_mins_minus,               &
            delta_mins_add, output_filename, min_lat_ind, max_lat_ind, &
            min_lon_ind, max_lon_ind, delta_mins_minus_ind,            &
            delta_mins_add_ind, output_filename_ind)


            DEALLOCATE(remark1)
            DEALLOCATE(remark2)
            DEALLOCATE(remark3)
            DEALLOCATE(read_pres)
            DEALLOCATE(read_hght)
            DEALLOCATE(read_temp)
            DEALLOCATE(read_dewp)
            DEALLOCATE(read_Wdir)
            DEALLOCATE(read_Wspd)

          END DO

        END IF

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
!  read_ contained the original data provided by the temp file in their
!  original units.  Whereas, the variables with the prefix output_ 
!  contain the data modified from the temp file into the units needed 
!  for ARPS5.3.0 3dvar. 
!
!----------------------------------------------------------------------
!
      SUBROUTINE UnitConverter(debug,station_no,station_hgt,read_pres, &
      read_hght, read_temp, read_dewp, read_Wdir, read_Wspd,           &
      NumberOfLines, read_lat, read_lon, read_year, read_month,        &
      read_day, read_hour, read_minute, instrument, min_lat, max_lat,  &
      min_lon, max_lon, delta_mins_minus,delta_mins_add,               &
      output_filename, min_lat_ind, max_lat_ind, min_lon_ind,          &
      max_lon_ind, delta_mins_minus_ind, delta_mins_add_ind,           &
       output_filename_ind)


        IMPLICIT NONE

        CHARACTER(LEN=20) OutName         ! *.snd filename variable
        CHARACTER(LEN=3)  instrument      ! instrument indentifier
        INTEGER*4 i                       ! Counter in the z-direction
        INTEGER*4 station_no              ! station number
        INTEGER*4 station_hgt             ! input station hieght
        REAL*4    output_station_hgt      ! output station hieght 
        INTEGER*4 NumberOfLines           ! number of data in the vertical
        INTEGER*4 ActualLines             ! actual number of data 
        INTEGER*4 read_lat                ! Latitude  [degree*100]
        INTEGER*4 read_lon                ! Longitude [degree*100]
        INTEGER*4 read_year               ! year
        INTEGER*4 read_month              ! month
        INTEGER*4 read_day                ! day
        INTEGER*4 read_hour               ! hour
        INTEGER*4 read_minute             ! minute
        REAL*4    output_lat              ! Latitude  [degree]
        REAL*4    output_lon              ! Longitude [degree]
        INTEGER*4, DIMENSION(NumberOfLines) :: read_pres ! Pressure 
        INTEGER*4, DIMENSION(NumberOfLines) :: read_hght ! Hieght  
        INTEGER*4, DIMENSION(NumberOfLines) :: read_temp ! Temperature
        INTEGER*4, DIMENSION(NumberOfLines) :: read_dewp ! Dewpoint 
        INTEGER*4, DIMENSION(NumberOfLines) :: read_Wdir ! Wind dir
        INTEGER*4, DIMENSION(NumberOfLines) :: read_Wspd ! Wind spd
        REAL*4, DIMENSION(NumberOfLines)  :: output_pres ! Pressure  
        REAL*4, DIMENSION(NumberOfLines)  :: output_temp ! Temperature
        REAL*4, DIMENSION(NumberOfLines)  :: output_dewp ! Dewpoint 
        REAL*4, DIMENSION(NumberOfLines)  :: output_Wdir ! Wind Dir 
        REAL*4, DIMENSION(NumberOfLines)  :: output_Wspd ! Wind Speed 
        REAL*4, DIMENSION(NumberOfLines)  :: output_hght ! Hieght
!! Command line variables
        REAL*4 min_lat, max_lat             ! User specified lat BC
        REAL*4 min_lon, max_lon             ! User specified lon BC
        INTEGER*4 delta_mins_minus, delta_mins_add ! User specified time BC
        CHARACTER(LEN=20) output_filename   ! User specified output filename
!! Command line indicators
        LOGICAL min_lat_ind, max_lat_ind
        LOGICAL min_lon_ind, max_lon_ind
        LOGICAL delta_mins_minus_ind, delta_mins_add_ind
        LOGICAL output_filename_ind
        LOGICAL datum                       ! Data does(n't) fit the date range
        INTEGER*4 debug                     ! Debug level

!
!! Simple unit and variable types conversions.
!
        output_lat  = read_lat/100.
        output_lon  = read_lon/100.
        output_station_hgt = station_hgt*1.
        output_hght = read_hght
        output_pres = read_pres/10.
        output_temp = read_temp/10.
        DO i = 1, NumberofLines, 1
          IF (read_dewp(i) == -999) THEN
            output_dewp(i) = -999.
          else
            output_dewp(i) = (read_temp(i) - ABS(read_dewp(i)))/10.
          END IF
        END DO
        output_Wdir = read_Wdir
        output_Wspd = read_Wspd
!
!! Change missing data which is represented one way in the temp file
!! into the -999., which is required for the model.
!
  
        ActualLines = 0

        DO i = 1, NumberofLines, 1
          IF (output_pres(i) == -999.9) THEN
            output_pres(i) = -999.
          ENDIF
        ENDDO

        DO i = 1, NumberofLines, 1
          IF (output_hght(i) == -9999.) THEN
            output_hght(i) = -999.
          ELSE
            ActualLines = ActualLines + 1
          ENDIF
        ENDDO

        DO i = 1, NumberofLines, 1
          IF (output_temp(i) == -99.9) THEN
            output_temp(i) = -999.
            output_dewp(i) = -999.
          ENDIF
        ENDDO

        DO i = 1, NumberofLines, 1
          IF (output_Wspd(i) == -99.) THEN
            output_Wspd(i) = -999.
          ENDIF
        ENDDO

        DO i = 1, NumberofLines, 1
          IF (output_Wdir(i) == -99.) THEN
            output_Wdir(i) = -999.
          ENDIF
        ENDDO

!
!! This calls a subroutine that creates the name of the *.snd file and
!! stores it in the OutName variable.
!

        CALL createOutName(read_year, read_month, read_day, read_hour, &
        read_minute, delta_mins_minus, delta_mins_add,                 &
        delta_mins_minus_ind, delta_mins_add_ind, OutName, datum)
 
!
!! This calls a subroutine that creates the *.snd file and inputs all 
!! the data necessary for data assimulation.
!

        CALL outputIntoAscii(debug, station_no, output_station_hgt,    &
        NumberOfLines, ActualLines,output_lat, output_lon, output_pres,&
        output_hght, output_temp, output_dewp,output_Wdir, output_Wspd,&
        OutName, instrument,  min_lat, max_lat, min_lon, max_lon,      &
        output_filename, min_lat_ind, max_lat_ind, min_lon_ind,        &
        max_lon_ind, output_filename_ind, datum)

        
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
      SUBROUTINE createOutName(year, month, day, hour, minute,         &
      mins_minus, mins_add, mins_minus_ind, mins_add_ind, OutName,     &
      datum)

        IMPLICIT NONE

        CHARACTER(LEN=20) OutName            ! *.snd filename variable
        CHARACTER(LEN=2)  centuryChar        ! 19/20 century
        INTEGER*4 fileHandle
        INTEGER*4 startyear, year
        INTEGER*4 startmonth, month
        INTEGER*4 startday, day
        INTEGER*4 starthour, hour
        INTEGER*4 startminute, minute
!! Command line variables
        INTEGER*4 mins_minus, mins_add ! User specified time BC
!! Command line indicators
        LOGICAL mins_minus_ind, mins_add_ind, datum
!! delta dates
        INTEGER*4 yearminusone, yearplusone
        INTEGER*4 monthminusone, monthplusone
        INTEGER*4 dayminusone, dayplusone
        INTEGER*4 hourminusone, hourplusone
        INTEGER*4 minuteminusone, minuteplusone
!
!! OutName variable format
!
        200     FORMAT(A21,2(I2.2,A),I4,A,2I2)
        880     FORMAT(A4,I4.4,4(I2.2),A4)
!
!! Calculates what century the data is from.
!
        IF(year >= 50) THEN
          centuryChar = "19"
        ELSE
          centuryChar = "20"
        ENDIF

        fileHandle = 12
        OPEN(UNIT=fileHandle, FILE='yearfile')
        WRITE(fileHandle,'(A2,I2.2)') centuryChar, year
        CLOSE(fileHandle)

        fileHandle = 13
        OPEN(UNIT=fileHandle, FILE='yearfile')
        READ(UNIT=fileHandle,"(I4)") year
        CLOSE(UNIT=fileHandle, DISP='DELETE')
!
!! Check to see if the data fits the specified time window, if not then
!! data will not be plotted in the end as datum will equal .FALSE. .
!
        startyear   = year
        startmonth  = month
        startday    = day
        starthour   = hour
        startminute = minute
        datum = .FALSE.

        CALL timeMINUSone(year, month, day, hour, minute, mins_minus,  &
        mins_minus_ind, yearminusone, monthminusone, dayminusone,      &
        hourminusone, minuteminusone)

        CALL timePLUSone(year, month, day, hour, minute, mins_add,     &
        mins_add_ind, yearplusone, monthplusone, dayplusone,           &
        hourplusone, minuteplusone)

        IF (startyear .LE. yearplusone .AND.                           &
            startyear .GE. yearminusone) THEN
          IF (startmonth .LE. monthplusone .AND.                       &
              startmonth .GE. monthminusone) THEN
            IF (startday .LE. dayplusone .AND.                         &
                startday .GE. dayminusone)THEN
              IF (starthour .LE. hourplusone .AND.                     &
                  starthour .GE. hourminusone) THEN
                IF ((startminute .LE. minuteplusone .AND.              &
                     abs(startminute) .LE. 60-minuteminusone) .OR.     &
                    (starthour .EQ. hourminusone .AND.                 &
                     startminute .GE. minuteplusone)) THEN
                   datum=.TRUE.
                ELSE
! Error Check
                  datum=.FALSE.
                  WRITE (*,200) '     DATA OMITTED on ',startmonth,'/',&
                                startday,'/',startyear,'/',starthour,  &
                                startminute
                END IF
              ELSE
! Error Check
                datum=.FALSE.
                WRITE (*,200) '     DATA OMITTED on ',startmonth,'/',  &
                              startday,'/',startyear,'/',starthour,    &
                              startminute
              END IF
            ELSE
! Error Check
              datum=.FALSE.
              WRITE (*,200) '     DATA OMITTED on ',startmonth,'/',    &
                            startday,'/',startyear,'/',starthour,      &
                            startminute
            END IF
          ELSE
! Error Check
            datum=.FALSE.
            WRITE (*,200) '     DATA OMITTED on ',startmonth,'/',      &
                          startday,'/',startyear,'/',starthour,        &
                          startminute
          END IF
        ELSE
! Error Check
          datum=.FALSE.
          WRITE (*,200) '     DATA OMITTED on ',startmonth,'/',        &
                        startday,'/',startyear,'/',starthour,startminute
        END IF
!
!! Data are placed by the hour, thus if we want all the data to be in 
!! one file per hour we must set the minutes to a set time.
!
        minute=0
!
!! Opens up a dummy file, which writes out the OutName variable, which
!! is later on read into the variable.  Once its done, this dummy file
!! gets deleted.
!
        fileHandle = 12
        OPEN(UNIT=fileHandle, FILE='namefile')
        WRITE(fileHandle,880) "uair", year, month, day, &
                              hour, minute, ".snd"
 
        CLOSE(fileHandle)

        fileHandle = 13
        OPEN(UNIT=fileHandle, FILE='namefile')
        READ(fileHandle,"A20") OutName
        CLOSE(UNIT=fileHandle, DISP='DELETE')



        RETURN
      ENDSUBROUTINE
!
!-----------------------------------------------------------------------
!
!  SUBROUTINE timeMINUSone
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  This subroutine will calcuate the correct time given the user 
!  specified time interval.  Default is set to plus or minus one hour.
!
!-----------------------------------------------------------------------
!
      SUBROUTINE  timeMINUSone(startyear, startmonth, startday,        &
      starthour, startminute, mins_minus, mins_minus_ind, yearminusone,&
      monthminusone, dayminusone, hourminusone, minuteminusone)

        IMPLICIT NONE

        INTEGER*4 startyear, year, yearminusone
        INTEGER*4 startmonth, month, monthminusone
        INTEGER*4 startday, day, dayminusone
        INTEGER*4 starthour, hour, hourminusone
        INTEGER*4 startminute, minute, minuteminusone
!! Command line variables
        INTEGER*4 mins_minus      ! User specified time BC
!! Command line indicators
        LOGICAL mins_minus_ind    ! User specified time BC indicator

        year   = startyear
        month  = startmonth
        day    = startday
        hour   = starthour
        minute = startminute

        IF (mins_minus_ind .EQ. .FALSE.) THEN
           IF (hour == 0) THEN
              hour = 23
              day  = day  - 1
           ELSE 
              hour = hour - 1
           END IF
           minute = 00
        ELSE
           IF (hour == 0) THEN
              hour = 23
              day  = day  - 1
           ELSE
              hour = hour - 1
           END IF
           minute = 60 - mins_minus
        END IF
        
!
!! If we were to subtract 1 hour it could easily change the day, month, or 
!! year. The code below will allow us to subtract one hour properly.
!
        IF (day == 0 .AND. hour == 23) THEN
          IF (month == 1) THEN
            month = 12
            day   = 31
            year  = year - 1
          ELSE IF (month == 3) THEN
            month = 2
            IF(MOD(year,4) == 0 .OR. MOD(year,100) == 0 .OR.           &
            (MOD(year,400) == 0 .OR. MOD(year,100) == 0)) THEN
              !! Leap Year !!
              day = 29
            ELSE
              day = 28
            END IF
          ELSE IF (month == 5 .OR. month == 7 .OR. month == 8 .OR.     &
          month == 10 .OR. month == 12) THEN
            month = month - 1
            day = 30
          ELSE IF (month == 2 .OR. month == 4 .OR. month == 6 .OR.     &
          month == 9 .OR. month == 11) THEN
            month = month - 1
            day = 31
          END IF
        END IF

        yearminusone   = year
        monthminusone  = month
        dayminusone    = day
        hourminusone   = hour
        minuteminusone = minute

        RETURN
      ENDSUBROUTINE
!
!-----------------------------------------------------------------------
!
!  SUBROUTINE timePLUSone
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  This subroutine will calcuate the correct time given the user
!  specified time interval.  Default is set to plus or minus one hour.
!
!-----------------------------------------------------------------------
!
      SUBROUTINE  timePLUSone(startyear, startmonth, startday,         &
      starthour, startminute, mins_plus, mins_plus_ind, yearplusone,   &
      monthplusone, dayplusone, hourplusone, minuteplusone)

        IMPLICIT NONE

        INTEGER*4 startyear, year, yearplusone
        INTEGER*4 startmonth, month, monthplusone
        INTEGER*4 startday, day, dayplusone
        INTEGER*4 starthour, hour, hourplusone
        INTEGER*4 startminute, minute, minuteplusone
!! Command line variables
        INTEGER*4 mins_plus       ! User specified time BC
!! Command line indicators
        LOGICAL mins_plus_ind     ! User specified time BC indicator

        year   = startyear
        month  = startmonth
        day    = startday
        hour   = starthour
        minute = startminute

        IF (mins_plus_ind .EQ. .FALSE.) THEN
           IF (hour .EQ. 23) THEN
              hour = 00
              day  = day  + 1
           ELSE
              hour = hour + 1
           END IF
           minute = 00
        ELSE
           IF (hour .EQ. 23) THEN
              hour = 00
              day  = day  + 1
           ELSE
              hour = hour + 1
           END IF
           minute = mins_plus
        END IF
!
!! If we were to add 1 hour it could easily change the day, month, or
!! year. The code below will allow us to add one hour properly.
!

        IF (day == 31 .AND. hour == 00) THEN
          IF (month == 12) THEN
            month = 1
            day   = 1
            year  = year + 1
          ELSE IF (month == 2) THEN
            month = 2
            IF(MOD(year,4) .EQ. 0 .OR. MOD(year,100) .NE. 0 .OR.       &
            (MOD(year,400) .EQ. 0 .OR. MOD(year,100) .EQ. 0)) THEN
              !! Leap Year !!
              day = 29
            ELSE
              month = 3
              day   = 1
            END IF
          ELSE IF (month == 1 .OR. month == 3 .OR. month == 5 .OR.     &
          month == 7 .OR. month == 8 .OR. month == 10) THEN
            month = month + 1
            day = 1
          ELSE IF (month == 4 .OR. month == 6 .OR. month == 9 .OR.     &
          month == 11) THEN
            month = month + 1
            day = 1
          END IF
        END IF

        yearplusone   = year
        monthplusone  = month
        dayplusone    = day
        hourplusone   = hour
        minuteplusone = minute

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
      SUBROUTINE outputIntoAscii(debug, station_no, station_hgt,       &
      NumberOfLines, Actual,lat,lon,pres,hgt, temp, dewp, Wdir, Wspd,  &
      OutName, instrument,  min_lat, max_lat, min_lon, max_lon,        &
      output_filename, min_lat_ind, max_lat_ind, min_lon_ind,          &
      max_lon_ind, output_filename_ind, datum)

        IMPLICIT NONE

        INTEGER*4 i                       ! Counter in the z-direction
        INTEGER*4 fileHandle
        INTEGER*4 station_no              ! station number
        REAL*4    station_hgt             ! station height
        INTEGER*4 NumberOfLines           ! number of line of useful data
        INTEGER*4 Actual                  ! actual number of lines in data
        CHARACTER(LEN=20) OutName         ! *.snd filename variable
        CHARACTER(LEN=3) instrument
        REAL*4    lat                               ! Latitude  [degree]
        REAL*4    lon                               ! Longitude [degree]
        REAL*4, DIMENSION(NumberOfLines)  :: pres   ! Pressure    [mb]
        REAL*4, DIMENSION(NumberOfLines)  :: hgt    ! Height      [gpm]
        REAL*4, DIMENSION(NumberOfLines)  :: temp   ! Temperature [C]
        REAL*4, DIMENSION(NumberOfLines)  :: dewp   ! Dewpoint    [C]
        REAL*4, DIMENSION(NumberOfLines)  :: Wdir   ! Wind Dir    [deg]
        REAL*4, DIMENSION(NumberOfLines)  :: Wspd   ! Wind Speed  [m/s]
!! Command line variables
        REAL*4 min_lat, max_lat             ! User specified lat BC
        REAL*4 min_lon, max_lon             ! User specified lon BC
        INTEGER*4 delta_mins_minus, delta_mins_add ! User specified time BC
        CHARACTER(LEN=20) output_filename   ! User specified output filename
!! Command line indicators
        LOGICAL min_lat_ind, max_lat_ind
        LOGICAL min_lon_ind, max_lon_ind
        LOGICAL output_filename_ind
        LOGICAL datum                       ! Data does(n't) fit the date range
        INTEGER*4 debug                     ! Debug level
!
!! Formating statements for the *.snd file. FORMAT 860 is for the
!! header, while FORMAT 870 is for the body of data.
!
200     FORMAT(A18,F6.2,A2,F6.2,A)
860     FORMAT(2I12,f11.4,f15.4,F15.0,5x,A5,1x,A8)
870     FORMAT(6F10.2)
!
!! Setting boundaries specified by the user if any.
!
        IF (min_lat_ind .EQ. .FALSE.) THEN
          min_lat = -90.00
        END IF
        IF (max_lat_ind .EQ. .FALSE.) THEN
          max_lat = 90.00
        END IF
        IF (min_lon_ind .EQ. .FALSE.) THEN
          min_lon = -180.00
        END IF
        IF (max_lon_ind .EQ. .FALSE.) THEN
          max_lon =  180.00
        END IF

        IF (output_filename_ind .EQ. .TRUE.) THEN
          OutName = output_filename
        END IF
!
!! Opening and writing into the file the header and body of data, and
!! append data (only if it fits within an one hour time window).
!
        IF (datum .EQ. .TRUE.) THEN
          IF (lat .GT. min_lat .AND. lat .LT. max_lat .AND.            &
              lon .GT. min_lon .AND. lon .LT. max_lon ) THEN

            fileHandle = 13

            OPEN(UNIT=fileHandle,FILE=OutName,POSITION='APPEND')
            IF (instrument == "TMP") THEN
              WRITE(fileHandle,860) station_no, Actual, lat,           &
                                    lon,station_hgt, "intmp", "CWBRAWIN"
              DO i = 1, NumberOfLines, 1
                IF (hgt(i) == -999.) THEN
                ELSE
                  WRITE(fileHandle,870) hgt(i), pres(i), temp(i),      &
                                        dewp(i), Wdir(i), Wspd(i)
                ENDIF
              ENDDO
              IF (debug == 9) THEN
                DO  i = 1, NumberOfLines, 1
                  WRITE(*,860) station_no, NumberOfLines, lat, lon,    &
                               station_hgt
                  WRITE(*,870) hgt(i), pres(i), temp(i), dewp(i),      &
                               Wdir(i), Wspd(i)
                END DO
              END IF

            ELSE IF (instrument == "DRP") THEN
              station_no = 37678367
              station_hgt = 0.
              WRITE(fileHandle,860) station_no, Actual, lat,           &
                                    lon,station_hgt, "indrp", "DROPWIND"
              DO i = 1, NumberOfLines, 1
                IF (hgt(i) == -999.) THEN
                ELSE
                  WRITE(fileHandle,870) hgt(i), pres(i), temp(i),      &
                                        dewp(i), Wdir(i), Wspd(i)
                ENDIF
              ENDDO
              IF (debug == 9) THEN
                DO  i = 1, NumberOfLines, 1
                  WRITE(*,860) station_no, NumberOfLines, lat, lon,    &
                               station_hgt
                  WRITE(*,870) hgt(i), pres(i), temp(i), dewp(i),      &
                               Wdir(i), Wspd(i)
                END DO
              END IF

            ELSE IF (instrument == "STP") THEN
              station_hgt = 0.
              WRITE(fileHandle,860) station_no, Actual, lat,           &
                                    lon,station_hgt, "instp", "SHIPDATA"
              DO i = 1, NumberOfLines, 1
                IF (hgt(i) == -999.) THEN
                ELSE
                  WRITE(fileHandle,870) hgt(i), pres(i), temp(i),      &
                                        dewp(i), Wdir(i), Wspd(i)
                ENDIF
              ENDDO
              IF (debug == 9) THEN
                DO  i = 1, NumberOfLines, 1
                  WRITE(*,860) station_no, NumberOfLines, lat, lon,    &
                               station_hgt
                  WRITE(*,870) hgt(i), pres(i), temp(i), dewp(i),      &
                               Wdir(i), Wspd(i)
                END DO
              END IF


            ELSE IF (instrument == "PLT") THEN
              WRITE(fileHandle,860) station_no, Actual, lat,           &
                                    lon,station_hgt, "inplt", "PILOTDAT"
              DO i = 1, NumberOfLines, 1
                IF (pres(i) .EQ. -999.) THEN
                  temp(i) = -999.
                  dewp(i) = -999.
                ELSE
                ENDIF
                IF (hgt(i) == -999.) THEN
                ELSE
                  WRITE(fileHandle,870) hgt(i),pres(i),temp(i),dewp(i),&
                                        Wdir(i), Wspd(i)
                ENDIF
              ENDDO
              IF (debug == 9) THEN
                DO  i = 1, NumberOfLines, 1
                  WRITE(*,860) station_no, NumberOfLines, lat, lon,    &
                               station_hgt
                  WRITE(*,870) hgt(i), pres(i), temp(i), dewp(i),      &
                               Wdir(i), Wspd(i)
                END DO
              END IF

            ELSE IF (instrument == "ARP") THEN
              station_no = 24737
              NumberOfLines = 1
              station_hgt = 0.
              dewp(1) = -999.
              OPEN(UNIT=fileHandle,FILE=OutName,POSITION='APPEND')
              WRITE(fileHandle,860) station_no, Actual, lat,           &
                                    lon,station_hgt, "inarp", "ARPDATA "
              IF (hgt(1) == -999.) THEN
              ELSE
                WRITE(fileHandle,870) hgt(1), pres(1), temp(1),        &
                                      dewp(1), Wdir(1), Wspd(1)
              ENDIF
              IF (debug == 9) THEN
                WRITE(*,860) station_no, NumberOfLines, lat, lon,      &
                             station_hgt
                WRITE(*,870) hgt(1), pres(1), temp(1), dewp(1),        &
                             Wdir(1), Wspd(1)
              END IF

            END IF
          ELSE
! Error check
            WRITE (*,200) 'DATA OMITTED at ( ',lat,', ',lon,')'
          END IF
        END IF

        CLOSE(fileHandle)

        RETURN
      ENDSUBROUTINE
!
!-----------------------------------------------------------------------
!
! End of the code.
!
!-----------------------------------------------------------------------
!
