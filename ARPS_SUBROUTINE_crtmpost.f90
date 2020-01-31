!
!##################################################################
!##################################################################
!######                                                      ######
!######                  SUBROUTINE crtmpost                 ######
!######                                                      ######
!######                     Developed by                     ######
!######     Center for Analysis and Prediction of Storms     ######
!######                University of Oklahoma                ######
!######                                                      ######
!##################################################################
!
      SUBROUTINE crtmpost(nx,ny,nz,x,y,z,zp,ptprt,pprt,qv,qc,qr,qi,qs, &
                          qh,ptbar,pbar,CRTMchannel1,CRTMchannel2,     &
                          CRTMchannel3,CRTMchannel4,CRTMchannel5,      &
                          CRTMchannels)
                          icrtm,nchannel,CRTM_OUT)
!
!-----------------------------------------------------------------------
!
!  PURPOSE:
!
!  Call CRTM to simulate GOES-R ABI IR channels and return brightness 
!  temperature for synthytic GOES-R imagery
!
!-----------------------------------------------------------------------
!
!  AUTHORS: Michael Hernandez
!  02/13/2012
!
!  MODIFICATION HISTORY:
!
!-----------------------------------------------------------------------
      USE CRTM_Module
      USE CRTM_SpcCoeff

      IMPLICIT NONE
!----------------------------------------------------------------------
! Constants
!----------------------------------------------------------------------
      REAL, PARAMETER :: p0  = 100000  ! reference Pressure (Pascal)
      REAL, PARAMETER :: rddcp = 0.2858! R/cp (unitless)
!----------------------------------------------------------------------
!  Output ARPS grid dimensions
!----------------------------------------------------------------------
      INTEGER :: nx ! Number of grid points in the x-dir
      INTEGER :: ny ! Number of grid points in the y-dir
      INTEGER :: nz ! Number of grid points in the z-dir
      INTEGER :: numbers ! numbers = nx * ny
!----------------------------------------------------------------------
!  ARPS grid variables
!-----------------------------------------------------------------------
      REAL, ALLOCATABLE :: x(:)
      REAL, ALLOCATABLE :: y(:)
      REAL, ALLOCATABLE :: z(:)
      REAL, ALLOCATABLE :: zp(:,:,:)
      REAL, ALLOCATABLE :: zps(:,:,:)

      INTEGER :: klev, istatus
      REAL    :: rlevel

      REAL, ALLOCATABLE :: pbar(:,:,:), pprt(:,:,:)
      REAL, ALLOCATABLE :: ptbar(:,:,:), ptprt(:,:,:)
      REAL, ALLOCATABLE :: qv(:,:,:),qc(:,:,:),qr(:,:,:)
      REAL, ALLOCATABLE :: qi(:,:,:),qs(:,:,:),qh(:,:,:)
!----------------------------------------------------------------------
! Internal arrays
!----------------------------------------------------------------------
      REAL, ALLOCATABLE :: hgtdp(:,:,:),qvdp(:,:,:),qcdp(:,:,:)
      REAL, ALLOCATABLE :: qrdp(:,:,:),qidp(:,:,:),qsdp(:,:,:)
      REAL, ALLOCATABLE :: qhdp(:,:,:),tempdp(:,:,:)
      REAL, ALLOCATABLE :: pres(:,:,:),temp(:,:,:)
      REAL, ALLOCATABLE :: qvinv(:,:,:),qcinv(:,:,:),qrinv(:,:,:)
      REAL, ALLOCATABLE :: qiinv(:,:,:),qsinv(:,:,:),qhinv(:,:,:)
      REAL, ALLOCATABLE :: zpsinv(:,:,:), tempinv(:,:,:)
!----------------------------------------------------------------------
! Output arrays
!----------------------------------------------------------------------
! CRTM Channel:
!   1 =  3.90 microns
!   2 =  6.14 microns
!   3 =  6.93 microns
!   4 =  7.34 microns
!   5 =  8.49 microns
!   6 =  9.60 microns
!   7 = 10.35 microns
!   8 = 11.22 microns
!   9 = 12.25 microns
!  10 = 13.28 microns
!-----------------------------------------------------------------------
      REAL, ALLOCATABLE :: CRTMchannel1(:,:),CRTMchannel2(:,:)
      REAL, ALLOCATABLE :: CRTMchannel3(:,:),CRTMchannel4(:,:)
      REAL, ALLOCATABLE :: CRTMchannel5(:,:),CRTMchannels(:)
!----------------------------------------------------------------------
! h-interpolation levels: 10 mb intervals
!----------------------------------------------------------------------
      INTEGER :: nprgem
      PARAMETER (nprgem = 100)
      INTEGER :: iprgem(nprgem)
      REAL, ALLOCATABLE :: algpzc(:,:,:)  ! -log(pressure)
      DATA iprgem  /0.009,   0.026,   0.055,   0.104,   0.177,   0.281,&
                    0.421,   0.604,   0.838,   1.129,   1.484,   1.910,&
                    2.416,   3.009,   3.696,   4.485,   5.385,   6.402,&
                    7.545,   8.822,  10.240,  11.807,  13.532,  15.423,&
                   17.486,  19.730,  22.163,  24.793,  27.626,  30.671,&
                   33.934,  37.425,  41.148,  45.113,  49.326,  53.794,&
                   58.524,  63.523,  68.797,  74.353,  80.198,  86.338,&
                   92.778,  99.526, 106.586, 113.965, 121.669, 129.703,&
                  138.072, 146.781, 155.836, 165.241, 175.001, 185.121,&
                  195.606, 206.459, 217.685, 229.287, 241.270, 253.637,&
                  266.392, 279.537, 293.077, 307.014, 321.351, 336.091,&
                  351.236, 366.789, 382.751, 399.126, 415.914, 433.118,&
                  450.738, 468.777, 487.236, 506.115, 525.416, 545.139,&
                  565.285, 585.854, 606.847, 628.263, 650.104, 672.367,&
                  695.054, 718.163, 741.693, 765.645, 790.017, 814.807,&
                  840.016, 865.640, 891.679, 918.130, 944.993, 972.264,&
                  999.942,1028.025,1056.510,1085.394/
!-----------------------------------------------------------------------
! Setting Parameters and variables for CRTM
!-----------------------------------------------------------------------
      CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'CRTM_FORWARD'
      CHARACTER(*), PARAMETER :: RESULTS_PATH = './results/'
      INTEGER, PARAMETER :: N_LAYERS    = 92     ! number of layers
      INTEGER, PARAMETER :: N_ABSORBERS = 3      ! number of absorbers
      INTEGER, PARAMETER :: N_CLOUDS    = 1      ! number of cloud types
      INTEGER, PARAMETER :: N_AEROSOLS  = 1      ! number of aerosols
      INTEGER, PARAMETER :: N_SENSORS = 1        ! ONE Sensor at a time
      REAL(fp), PARAMETER :: ZENITH_ANGLE = 30.0_fp
      REAL(fp), PARAMETER :: SCAN_ANGLE   = 26.37293341421_fp
      INTEGER :: N_PROFILES                      ! number of profiles

      CHARACTER(256) :: Message
      CHARACTER(256) :: Sensor_Id                ! Sensor id
      INTEGER :: Error_Status
      INTEGER :: Allocate_Status
      INTEGER :: n_Channels
      INTEGER :: k1, k2, l, m, i, j, k
      CHARACTER(256) :: rts_File
      TYPE(CRTM_RTSolution_type), ALLOCATABLE :: rts(:,:)
!-----------------------------------------------------------------------
! DEFINE THE CRTM INTERFACE STRUCTURES
!-----------------------------------------------------------------------
      TYPE(CRTM_ChannelInfo_type)             :: ChannelInfo(N_SENSORS)
      TYPE(CRTM_Geometry_type)                :: Geometry(N_PROFILES)
      TYPE(CRTM_Atmosphere_type)              :: Atm(N_PROFILES)
      TYPE(CRTM_Surface_type)                 :: Sfc(N_PROFILES)
      TYPE(CRTM_RTSolution_type), ALLOCATABLE :: RTSolution(:,:)
!-----------------------------------------------------------------------
! Sensor ID abi_gr = GOES R sensors
!-----------------------------------------------------------------------
      Sensor_Id = ADJUSTL("abi_gr")
      WRITE(*,'(//5x,"Running CRTM for ",a," sensor...")')             &
             TRIM(Sensor_Id)
!-----------------------------------------------------------------------
! Allocating arrays input arrays
!-----------------------------------------------------------------------
      ALLOCATE(x(nx),stat=istatus)
      ALLOCATE(y(ny),stat=istatus)
      ALLOCATE(z(nz),stat=istatus)
      ALLOCATE(zp(nx,ny,nz),stat=istatus)
      ALLOCATE(zps(nx,ny,nz),STAT=istatus)

      ALLOCATE(ptprt(nx,ny,nz),STAT=istatus)
      ALLOCATE(ptbar(nx,ny,nz),STAT=istatus)
      ALLOCATE(pbar(nx,ny,nz),STAT=istatus)
      ALLOCATE(pprt(nx,ny,nz),STAT=istatus)
      ALLOCATE(algpzc(nx,ny,nz),STAT=istatus)
      ALLOCATE(qv(nx,ny,nz),STAT=istatus)
      ALLOCATE(qc(nx,ny,nz),STAT=istatus)
      ALLOCATE(qr(nx,ny,nz),STAT=istatus)
      ALLOCATE(qi(nx,ny,nz),STAT=istatus)
      ALLOCATE(qs(nx,ny,nz),STAT=istatus)
      ALLOCATE(qh(nx,ny,nz),STAT=istatus)

!-----------------------------------------------------------------------
! Allocating internal arrays
!-----------------------------------------------------------------------
      ALLOCATE(pres(nx,ny,nz),STAT=istatus)
      ALLOCATE(temp(nx,ny,nz),STAT=istatus)
      ALLOCATE(tempinv(nx,ny,nz),STAT=istatus)

      ALLOCATE(zpsinv(nx,ny,nz),STAT=istatus)
      ALLOCATE(qvinv(nx,ny,nz),STAT=istatus)
      ALLOCATE(qcinv(nx,ny,nz),STAT=istatus)
      ALLOCATE(qrinv(nx,ny,nz),STAT=istatus)
      ALLOCATE(qiinv(nx,ny,nz),STAT=istatus)
      ALLOCATE(qsinv(nx,ny,nz),STAT=istatus)
      ALLOCATE(qhinv(nx,ny,nz),STAT=istatus)

      ALLOCATE(hgtdp(nx,ny,nprgem),STAT=istatus)
      ALLOCATE(qvdp(nx,ny,nprgem),STAT=istatus)
      ALLOCATE(qcdp(nx,ny,nprgem),STAT=istatus)
      ALLOCATE(qrdp(nx,ny,nprgem),STAT=istatus)
      ALLOCATE(qidp(nx,ny,nprgem),STAT=istatus)
      ALLOCATE(qsdp(nx,ny,nprgem),STAT=istatus)
      ALLOCATE(qhdp(nx,ny,nprgem),STAT=istatus)
!-----------------------------------------------------------------------
! Allocating output arrays
!-----------------------------------------------------------------------
      ALLOCATE(CRTMchannels(5),STAT=istatus)
      ALLOCATE(CRTMchannel1(nx,ny),STAT=istatus)
      ALLOCATE(CRTMchannel2(nx,ny),STAT=istatus)
      ALLOCATE(CRTMchannel3(nx,ny),STAT=istatus)
      ALLOCATE(CRTMchannel4(nx,ny),STAT=istatus)
      ALLOCATE(CRTMchannel5(nx,ny),STAT=istatus)


!------------------------------------------------------------------------
! Initialize CRTM
!------------------------------------------------------------------------
      N_PROFILES  = nx*ny

      WRITE( *,'(/5x,"Initializing the CRTM...")' )
      Error_Status = CRTM_Init( (/Sensor_Id/), ChannelInfo  ,          &
      EmisCoeff_File='Wu-Smith.CM-PDF.HQS_HQS-RefInd.EmisCoeff.bin',   &
      File_Path='coefficients/')
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error initializing CRTM'
        CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
        STOP
      END IF

!! Determine the total number of channels
!! for which the CRTM was initialized

      n_Channels = SUM(ChannelInfo%n_Channels)

!-----------------------------------------------------------------------
! Allocate Structure Arrays
!-----------------------------------------------------------------------

      ALLOCATE(RTSolution(n_Channels,N_PROFILES),STAT=Allocate_Status)
      IF (Allocate_Status/=0) THEN
        Message = 'Error allocating structure arrays'
        CALL Display_Message(PROGRAM_NAME,Message,FAILURE)
        STOP
      END IF

!! Allocate the STRUCTURES: The input FORWARD structure
      CALL CRTM_Atmosphere_Create(Atm,N_LAYERS,N_ABSORBERS,N_CLOUDS,   &
                                  N_AEROSOLS)

      IF (ANY(.NOT.CRTM_Atmosphere_Associated(Atm))) THEN
        Message = 'Error allocating CRTM Atmosphere structures'
        CALL Display_Message(PROGRAM_NAME,Message,FAILURE)
        STOP
      END IF

!-----------------------------------------------------------------------
!  Assign Input Data
!   * Load in the constant pressure layers, H2O, O3, & CO2 absorbers.
!-----------------------------------------------------------------------

      CALL Load_Atmconsts_Data()

!-----------------------------------------------------------------------
! Preparing the input data:
!  * Set coordinates at grid volume center
!  * Create the temperature field
!  * Create the temperature and other cloud species in pressure levels
!    rather than sigma levels
!-----------------------------------------------------------------------
      DO k=1,nz-1
        DO j=1,ny-1
          DO i=1,nx-1
            algpzc(i,j,k) = -ALOG(pbar(i,j,k)+pprt(i,j,k))
          END DO
        END DO
      END DO



      DO k=1,nz-1
        DO j=1,ny
          DO i=1,nx
            zps(i,j,k)= (zp(i,j,k)+zp(i,j,k+1))*0.5   ! in meter
          END DO
         END DO
      END DO

      DO k=1,nz
        DO j=1,ny
          DO i=1,nx
            pres(i,j,k) = pbar(i,j,k)+pprt(i,j,k)
            temp(i,j,k) = (ptbar(i,j,k)+ptprt(i,j,k))*((pres(i,j,k)/p0)&
                          **rddcp)
          END DO
        END DO
      END DO

      DO j=1,ny
        DO i=1,nx
          DO k=nz, 1, -1
            tempinv(i,j,(nz-k+1)) = temp(i,j,k)
            qvinv(i,j,(nz-k+1))   = qv(i,j,k)
!            qcinv(i,j,(nz-k+1))   = qc(i,j,k)
!            qrinv(i,j,(nz-k+1))   = qr(i,j,k)
!            qiinv(i,j,(nz-k+1))   = qi(i,j,k)
!            qsinv(i,j,(nz-k+1))   = qs(i,j,k)
!            qhinv(i,j,(nz-k+1))   = qh(i,j,k)
!            zpsinv(i,j,(nz-k+1))  = zps(i,j,k)
          END DO
        END DO
      END DO
      CALL edgfill(tempinv,1,nx,1,nx-1,1,ny,1,ny-1,1,nz,1,nz-1)
      CALL edgfill(qvinv,1,nx,1,nx-1,1,ny,1,ny-1,1,nz,1,nz-1)
!      CALL edgfill(qcinv,1,nx,1,nx-1,1,ny,1,ny-1,1,nz,1,nz-1)
!      CALL edgfill(qrinv,1,nx,1,nx-1,1,ny,1,ny-1,1,nz,1,nz-1)
!      CALL edgfill(qiinv,1,nx,1,nx-1,1,ny,1,ny-1,1,nz,1,nz-1)
!      CALL edgfill(qsinv,1,nx,1,nx-1,1,ny,1,ny-1,1,nz,1,nz-1)
!      CALL edgfill(qhinv,1,nx,1,nx-1,1,ny,1,ny-1,1,nz,1,nz-1)
!      CALL edgfill(zpsinv,1,nx,1,nx-1,1,ny,1,ny-1,1,nz,1,nz-1)

      IF( nprgem > 0 ) THEN
        DO klev=1,nprgem
          rlevel = -ALOG(float(iprgem(klev))*100.0)
          CALL hintrp1(nx,ny,nz,2,nz-2,tempinv,algpzc,rlevel,          &
                       tempdp(1,1,klev))
          CALL hintrp1(nx,ny,nz,2,nz-2,1000.*qvinv,algpzc,rlevel,      &
                       qvdp(1,1,klev))
!          CALL hintrp1(nx,ny,nz,2,nz-2,zpsinv,algpzc,rlevel,           &
!                       hgtdp(1,1,klev))
!          CALL hintrp1(nx,ny,nz,2,nz-2,qcinv,algpzc,rlevel,            &
!                       qcdp(1,1,klev))
!          CALL hintrp1(nx,ny,nz,2,nz-2,qrinv,algpzc,rlevel,            &
!                       qrdp(1,1,klev))
!          CALL hintrp1(nx,ny,nz,2,nz-2,qiinv,algpzc,rlevel,            &
!                       qidp(1,1,klev))
!          CALL hintrp1(nx,ny,nz,2,nz-2,qsinv,algpzc,rlevel,            &
!                       qsdp(1,1,klev))
!          CALL hintrp1(nx,ny,nz,2,nz-2,qhinv,algpzc,rlevel,            &
!                       qhdp(1,1,klev))
        ENDDO
      END IF


      DO j=1,ny
        DO i=1,nx
          Sfc(nx*(j-1)+i)%Land_Type = TILLED_SOIL
          Sfc(nx*(j-1)+i)%Land_Temperature = temp(i,j,nz)
          Atm(nx*(j-1)+i)%Temperature   = tempinv(i,j,:)
          Atm(nx*(j-1)+i)%Absorber(:,1) = qvinv(i,j,:)
        END DO
      END DO

!! GeometryInfo input:  All profiles are given the same value
!!                      The Sensor_Scan_Angle is optional.

     CALL CRTM_Geometry_SetValue( Geometry,                            &
                                 Sensor_Zenith_Angle = ZENITH_ANGLE,   &
                                 Sensor_Scan_Angle   = SCAN_ANGLE )

!-----------------------------------------------------------------------
! Call the CRTM FOWARD MODEL
!-----------------------------------------------------------------------
      Error_Status = CRTM_Forward( Atm, Sfc, Geometry, ChannelInfo,    &
                                   RTSolution  )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error in CRTM Forward Model'
        CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
        STOP
      END IF
!-----------------------------------------------------------------------
! Output the results to the screen
!-----------------------------------------------------------------------

      CRTMchannels(1)=(1/(SC(1)%wavenumber(2)))*1e4
      CRTMchannels(2)=(1/(SC(1)%wavenumber(3)))*1e4
      CRTMchannels(3)=(1/(SC(1)%wavenumber(4)))*1e4
      CRTMchannels(4)=(1/(SC(1)%wavenumber(7)))*1e4
      CRTMchannels(5)=(1/(SC(1)%wavenumber(9)))*1e4

      DO j = 1, ny
        DO i = 1, nx
          CRTMchannel1(i,j)= &
               RTSolution(2,(i+(j-1)*nx))%Brightness_Temperature
          CRTMchannel2(i,j)= &
               RTSolution(3,(i+(j-1)*nx))%Brightness_Temperature
          CRTMchannel3(i,j)= &
               RTSolution(4,(i+(j-1)*nx))%Brightness_Temperature
          CRTMchannel4(i,j)= &
               RTSolution(7,(i+(j-1)*nx))%Brightness_Temperature
          CRTMchannel5(i,j)= &
               RTSolution(9,(i+(j-1)*nx))%Brightness_Temperature
        END DO
      END DO

!-----------------------------------------------------------------------
! Destroy the CRTM
!-----------------------------------------------------------------------
      WRITE( *, '( /5x, "Destroying the CRTM..." )' )
      Error_Status = CRTM_Destroy( ChannelInfo )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error destroying CRTM'
        CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
        STOP
      END IF
!-----------------------------------------------------------------------
! Clean up
!-----------------------------------------------------------------------
      CALL CRTM_Atmosphere_Destroy(Atm)
      DEALLOCATE(RTSolution, rts, STAT=Allocate_Status)

      CONTAINS
        SUBROUTINE Load_Atmconsts_Data()
!! Internal subprogam to load some constant profile data

         numbers = nx*ny

          DO i=1, numbers, 1
            Sfc(i)%Land_Coverage    = 1.0_fp

!! Atmospheric profile data
            Atm(i)%Climatology    = US_STANDARD_ATMOSPHERE
            Atm(i)%Absorber_Id    = (/ H2O_ID, O3_ID, CO2_ID /)
            Atm(i)%Absorber_Units = (/ MASS_MIXING_RATIO_UNITS,        &
                                       VOLUME_MIXING_RATIO_UNITS,      &
                                       VOLUME_MIXING_RATIO_UNITS /)

        Atm(i)%Level_Pressure = &
        (/0.005,   0.016,   0.038,   0.077,   0.137,   0.224,   0.345, &
          0.506,   0.714,   0.975,   1.297,   1.687,   2.153,   2.701, &
          3.340,   4.077,   4.920,   5.878,   6.957,   8.165,   9.512, &
         11.004,  12.649,  14.456,  16.432,  18.585,  20.922,  23.453, &
         26.183,  29.121,  32.274,  35.650,  39.257,  43.100,  47.188, &
         51.528,  56.126,  60.990,  66.125,  71.540,  77.240,  83.231, &
         89.520,  96.114, 103.017, 110.237, 117.777, 125.646, 133.846, &
        142.385, 151.266, 160.496, 170.078, 180.018, 190.320, 200.989, &
        212.028, 223.441, 235.234, 247.409, 259.969, 272.919, 286.262, &
        300.000, 314.137, 328.675, 343.618, 358.967, 374.724, 390.893, &
        407.474, 424.470, 441.882, 459.712, 477.961, 496.630, 515.720, &
        535.232, 555.167, 575.525, 596.306, 617.511, 639.140, 661.192, &
        683.667, 706.565, 729.886, 753.627, 777.790, 802.371, 827.371, &
        852.788, 878.620, 904.866, 931.524, 958.591, 986.067,1013.948, &
       1042.232,1070.917,1100.000/)

       Atm(i)%Pressure       = &
        (/0.009,   0.026,   0.055,   0.104,   0.177,   0.281,   0.421, &
          0.604,   0.838,   1.129,   1.484,   1.910,   2.416,   3.009, &
          3.696,   4.485,   5.385,   6.402,   7.545,   8.822,  10.240, &
         11.807,  13.532,  15.423,  17.486,  19.730,  22.163,  24.793, &
         27.626,  30.671,  33.934,  37.425,  41.148,  45.113,  49.326, &
         53.794,  58.524,  63.523,  68.797,  74.353,  80.198,  86.338, &
         92.778,  99.526, 106.586, 113.965, 121.669, 129.703, 138.072, &
        146.781, 155.836, 165.241, 175.001, 185.121, 195.606, 206.459, &
        217.685, 229.287, 241.270, 253.637, 266.392, 279.537, 293.077, &
        307.014, 321.351, 336.091, 351.236, 366.789, 382.751, 399.126, &
        415.914, 433.118, 450.738, 468.777, 487.236, 506.115, 525.416, &
        545.139, 565.285, 585.854, 606.847, 628.263, 650.104, 672.367, &
        695.054, 718.163, 741.693, 765.645, 790.017, 814.807, 840.016, &
        865.640, 891.679, 918.130, 944.993, 972.264, 999.942,1028.025, &
        1056.510,1085.394/)

!! O3 profile
       Atm(i)%Absorber(:,2)  = &
       (/3.513E-01,4.097E-01,5.161E-01,7.225E-01,1.016E+00,1.354E+00,  &
         1.767E+00,2.301E+00,3.035E+00,3.943E+00,4.889E+00,5.812E+00,  &
         6.654E+00,7.308E+00,7.660E+00,7.745E+00,7.696E+00,7.573E+00,  &
         7.413E+00,7.246E+00,7.097E+00,6.959E+00,6.797E+00,6.593E+00,  &
         6.359E+00,6.110E+00,5.860E+00,5.573E+00,5.253E+00,4.937E+00,  &
         4.625E+00,4.308E+00,3.986E+00,3.642E+00,3.261E+00,2.874E+00,  &
         2.486E+00,2.102E+00,1.755E+00,1.450E+00,1.208E+00,1.087E+00,  &
         1.030E+00,1.005E+00,1.010E+00,1.028E+00,1.068E+00,1.109E+00,  &
         1.108E+00,1.071E+00,9.928E-01,8.595E-01,7.155E-01,5.778E-01,  &
         4.452E-01,3.372E-01,2.532E-01,1.833E-01,1.328E-01,9.394E-02,  &
         6.803E-02,5.152E-02,4.569E-02,4.855E-02,5.461E-02,6.398E-02,  &
         7.205E-02,7.839E-02,8.256E-02,8.401E-02,8.412E-02,8.353E-02,  &
         8.269E-02,8.196E-02,8.103E-02,7.963E-02,7.741E-02,7.425E-02,  &
         7.067E-02,6.702E-02,6.368E-02,6.070E-02,5.778E-02,5.481E-02,  &
         5.181E-02,4.920E-02,4.700E-02,4.478E-02,4.207E-02,3.771E-02,  &
         3.012E-02,1.941E-02,9.076E-03,2.980E-03,5.117E-03,1.160E-02,  &
         1.428E-02,1.428E-02,1.428E-02,1.428E-02/)

!! CO2 profile
       Atm(i)%Absorber(:,3)  = &
       (/3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,3.300e+02,  &
         3.300e+02,3.280e+02,3.200e+02,3.100e+02,2.700e+02,1.950e+02,  &
         1.100e+02,6.000e+01,4.000e+01,3.500e+01/)

!! Some pretend cloud data
            k1 = 76  ! Pressure[k1] = 506.115hPa
            k2 = 90  ! Pressure[k2] = 1028.025hPa
            Atm(i)%Cloud(1)%Type = WATER_CLOUD
            Atm(i)%Cloud(1)%Effective_Radius(k1:k2) = 20.0_fp ! microns
            Atm(i)%Cloud(1)%Water_Content(k1:k2)    = 5.0_fp  ! kg/m^2

          END DO

        END SUBROUTINE Load_Atmconsts_Data

      END SUBROUTINE

!----------------------------------------------------------------------------------
! End of subroutine
!----------------------------------------------------------------------------------

