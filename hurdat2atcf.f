C=======================================================================
C                          HURDAT2ATCF writer
C=======================================================================
C  Converting Dr. Chris Landsea's Hurricane Reanalysis data into the 
C  widely used ATCF format.
C=======================================================================
C  @date   : 12/22/08
C  @version: 1.0.0
C  @author : Michael Kevin Hernandez
C  @email  : mkh182@psu.edu
C
C  @running: ./hurdat2atcf hurdat
C  @output : bdeck.hurdat
C=======================================================================
      PROGRAM hurdat2atcf
      
      implicit none

      CHARACTER(LEN = 80) Descriptor, PerDayData, HitLine

C
CC Vars for the Descriptor
C

      CHARACTER(LEN = 2)  dummy2
      CHARACTER(LEN = 3)  dummy3
      CHARACTER(LEN = 4)  dummy4
      CHARACTER(LEN = 7)  dummy7
      CHARACTER(LEN = 50) dummy50
      Integer*8           StormNo, NumberOfLines, Year, CardNo

C
CC Vars for the PerDayData
C

      CHARACTER(LEN = 1) SystemType1, SystemType2, SystemType3, 
     &                   SystemType4, dummy1
      CHARACTER(LEN = 2) Type1, Type2, Type3, Type4, month, day
      CHARACTER(LEN = 6) dummy6
      INTEGER*4          i, j, MaxWind1, MaxWind2, MaxWind3,
     &                   MaxWind4, latitude1, latitude2, latitude3,
     &                   latitude4, longitude1, longitude2, longitude3,
     &                   longitude4,  MinPressure1, MinPressure2, 
     &                   MinPressure3, MinPressure4
      CHARACTER(LEN = 80) dummy80

C
CC Open up the Hurdat data file
C

      open(unit = 10, file='hurdat')
      open(unit = 20, file='hurdat.atcf')

      NumberOfLines = 1

      do while (NumberOfLines .ge. 1)  

C
CC Read in the first line, which discribes the storm 
C

         read(10,1000,end = 100) CardNo, dummy7, Year, dummy4,
     &                           NumberOfLines, dummy2, StormNo, dummy50

C
CC Reading in the data describing the lifecycle of the cyclone.
C

100      do i = 1, NumberOfLines, 1
            read(10,2000,end = 200) dummy6, month, dummy1, day,
     &                              SystemType1, latitude1, longitude1, 
     &                              MaxWind1, MinPressure1, SystemType2,
     &                              latitude2, longitude2, MaxWind2, 
     &                              MinPressure2, SystemType3, 
     &                              latitude3, longitude3, MaxWind3,
     &                              MinPressure3, SystemType4, 
     &                              latitude4, longitude4, MaxWind4,
     &                              MinPressure4
            call types(SystemType1, SystemType2, SystemType3, 
     &                 SystemType4, MaxWind1, MaxWind2, MaxWind3, 
     &                 MaxWind4, Type1, Type2, Type3, Type4)

C
CC Printing out to the screen the data in atcf format. But if Latitude 
CC is zero then there should be no storm data.
C

200         if(latitude1 > 0) then
              write(*,4000) "AL, ", StormNo, ", ", year, month, day, 
     &                   "00, , BEST, 0, ", latitude1,"N, ", longitude1,
     &                   "W, " , MaxWind1, ", ", MinPressure1, ", ",
     &                   Type1, ","
              write(20,4000) "AL, ", StormNo, ", ", year, month, day,
     &                   "00, , BEST, 0, ", latitude1,"N, ", longitude1,
     &                   "W, " , MaxWind1, ", ", MinPressure1, ", ",
     &                   Type1, ","
            endif

            if(latitude2 > 0) then
              write(*,4000) "AL, ", StormNo, ", ", year, month, day,
     &                   "06, , BEST, 0, ", latitude2,"N, ", longitude2,
     &                   "W, " , MaxWind2, ", ", MinPressure2, ", ",
     &                   Type2, ","
              write(20,4000) "AL, ", StormNo, ", ", year, month, day,
     &                   "06, , BEST, 0, ", latitude2,"N, ", longitude2,
     &                   "W, " , MaxWind2, ", ", MinPressure2, ", ",
     &                   Type2, ","
            endif

            if(latitude3 > 0) then
              write(*,4000) "AL, ", StormNo, ", ", year, month, day,
     &                   "12, , BEST, 0, ", latitude3,"N, ", longitude3,
     &                   "W, " , MaxWind3, ", ", MinPressure3, ", ",
     &                   Type3, ","
              write(20,4000) "AL, ", StormNo, ", ", year, month, day,
     &                   "12, , BEST, 0, ", latitude3,"N, ", longitude3,
     &                   "W, " , MaxWind3, ", ", MinPressure3, ", ",
     &                   Type3, ","
            endif

            if(latitude4 > 0) then
              write(*,4000) "AL, ", StormNo, ", ", year, month, day,
     &                   "18, , BEST, 0, ", latitude4,"N, ", longitude4,
     &                   "W, " , MaxWind4, ", ", MinPressure4, ", ",
     &                   Type4, ","
              write(20,4000) "AL, ", StormNo, ", ", year, month, day,
     &                   "18, , BEST, 0, ", latitude4,"N, ", longitude4,
     &                   "W, " , MaxWind4, ", ", MinPressure4, ", ",
     &                   Type4, ","
            endif
            
            continue
         enddo

         read(10,3000,end = 300) dummy80
300      continue
      enddo

      close(10)
      close(20)

1000  format(I5,A7,I4,A3,I2,A1,I2,A50)
2000  format(A6,A2,A1,A2,A1,I3,I4,I4,I5,A1,I3,I4,I4,I5,A1,I3,I4,I4,I5,
     &       A1,I3,I4,I4,I5)
3000  format(A80)
4000  format(A4,I2,A2,I4,A2,A2,A15,I3,A3,I4,A3,I3,A2,I4,A2,A2,A1)

      END PROGRAM

C=======================================================================
C Subroutine Types
C=======================================================================
C This subroutine tell you what type of system there is either a 
C Hurricane = HU, Tropical Storm = TS, Subtropical Strom = SS, 
C and an Extratropical Strom = EX.  This converts the strom type from
C ATCF format from Dr. Landseas Hurdat format.
C=======================================================================
      SUBROUTINE types(SystemType1, SystemType2, SystemType3, 
     &                 SystemType4, MaxWind1, MaxWind2, MaxWind3, 
     &                 MaxWind4, Type1, Type2, Type3, Type4)

        implicit none

        CHARACTER(LEN = 1) SystemType1, SystemType2, SystemType3,
     &                     SystemType4
        CHARACTER(LEN = 2) Type1, Type2, Type3, Type4
        INTEGER*4          MaxWind1, MaxWind2, MaxWind3, MaxWind4
        
            if(SystemType1 == "*") then
               if(MaxWind1 >= 74) then
                 Type1 = "HU"
               else
                 Type1 = "TS"
               endif
            elseif(SystemType1 == "E") then
               Type1 = "EX"
            elseif(SystemType1 == "S") then
               Type1 = "SS"
            elseif(SystemType1 == "L") then
               Type1 = "LO"
            endif

            if(SystemType2 == "*") then
               if(MaxWind2 >= 74) then
                 Type2 = "HU"
               else
                 Type2 = "TS"
               endif
            elseif(SystemType2 == "E") then
               Type2 = "EX"
            elseif(SystemType2 == "S") then
               Type2 = "SS"
            elseif(SystemType2 == "L") then
               Type2 = "LO"
            endif

            if(SystemType3 == "*") then
               if(MaxWind3 >= 74) then
                 Type3 = "HU"
               else
                 Type3 = "TS"
               endif
            elseif(SystemType3 == "E") then
               Type3 = "EX"
            elseif(SystemType3 == "S") then
               Type3 = "SS"
            elseif(SystemType3 == "L") then
               Type3 = "LO"
            endif

            if(SystemType4 == "*") then
               if(MaxWind4 >= 74) then
                 Type4 = "HU"
               else
                 Type4 = "TS"
               endif
            elseif(SystemType4 == "E") then
               Type4 = "EX"
            elseif(SystemType4 == "S") then
               Type4 = "SS"
            elseif(SystemType4 == "L") then
               Type4 = "LO"
            endif

        return

      ENDSUBROUTINE
C=======================================================================

