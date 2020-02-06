C/*********************************************************************/
C/* */
C/* NetCDF Users Guide: Perform a search for NetCDF at www.google.com */
C/* The second hit will be the Web site: */
C/* http://www.unidata.ucar.edu/software/netcdf/guide_toc.html */
C/* This is a manual straight from the horse's mouth, so to speak. */
C/* It is all you need to become an expert on the NetCDF data format */
C/* and to write beautiful NetCDF FORTRAN file access software as a */
C/* result. */
C/*-------------------------------------------------------------------*/
C/* */
C/* Software Rules for the NetCDF exercise: */
C/* 1) Your code must compile, run and produce correct results. */
C/* 2) Your code must be beautifully and extensively documented, */
C/* similar in detail to this piece of code. */
C/* 3) Do not use fixed length arrays. Use only allocatable ones. */
C/* 4) Print to an ASCII status file, which is independent of your */
C/* ASCII data file, the following: */
C/      i) Number of dimensions, their IDs, names, and values. */
C/*    ii) Number of variables, their IDs and names. */
C/* 5) Print the variable data to an ASCII data file in such a way */
C/* that facilitates data conversion to the GRIB file format. */
C/* 6) Items 4) and 5) above must be done with DO LOOPS. */
C/*-------------------------------------------------------------------*/
C/* This tutorial example was written by Eugene Clothiaux on */
C/* September 24, 2008. */
C/*********************************************************************/
      PROGRAM NetCDFTutorial
C /*------------------------------------------------------------*/
C /* Force the declaration of all variables. */
C /*------------------------------------------------------------*/
      IMPLICIT NONE
C /*------------------------------------------------------------*/
C /* Must include the NetCDF INCLUDE file "netcdf.inc" so that */
C /* the program recognizes NetCDF function and subroutine */
C /* calls. */
C /*------------------------------------------------------------*/
      INCLUDE 'netcdf.inc'
C /*------------------------------------------------------------*/
C /* PARAMETER statements. The parameter "single" will be */
C /* used in the declaration of four-byte integers and floats. */
C /*------------------------------------------------------------*/
      INTEGER*4, PARAMETER :: single = 4
C /*------------------------------------------------------------*/
C /* Number of command line argument information. */
C /*------------------------------------------------------------*/
      INTEGER(KIND=SINGLE)
      1 nargs, ! Number of command line arguments
      2 IARGC ! Intrinsic function that retrieves them
      CHARACTER(LEN=256)
      1 NetCDFFilename ! Input file name command line argument
      INTEGER(KIND=SINGLE)
      1 ncid, ! NetCDF file ID
      2 ncrcode ! NetCDF function/subroutine status report
      INTEGER(KIND=SINGLE)
      1 ndims, ! Number of NetCDF dimensions
      2 nvars, ! Number of NetCDF variables
      3 ngatts, ! Number of NetCDF global attributes
      4 recdim ! Record dimenions of NetCDF file
C /*------------------------------------------------------------*/
C /* Make sure one command line argument is passed into the */
C /* program. The command line argument must be the NetCDF */
C /* file with a path attached to it. */
C /* To learn more about FUNCTION IARGC and SUBROUTINE GETARG */
C /* please go to */
C /* http://gcc.gnu.org/onlinedocs/gfortran/IARGC.html */
C /* and */
C /* http://gcc.gnu.org/onlinedocs/gfortran/GETARG.html */
C /* The location of these two Web sites is courtesy of Andy */
C /* Beck, who found them through a Web forum discussion of */
C /* some sort. */
C /*------------------------------------------------------------*/
!/* Retrieve the number of command line arguments. */
      nargs = IARGC()
!/* Make sure there is only one command line argument. */
      IF (nargs .ne. 1) THEN
         WRITE(*,*) 'You must enter 1 command line argument:'
         WRITE(*,*) ' 1) Name of the NetCDF file to be dumped'
         STOP
      END IF
C /*------------------------------------------------------------*/
C /* Retrieve the name of the NetCDF file passed in as a */
C /* command line argument. It is the only one at this point. */
C /*------------------------------------------------------------*/
      CALL GETARG(1, NetCDFFilename)
!/* Write the NetCDF file name to the terminal. */
      WRITE(*,*) NetCDFFilename
C /*------------------------------------------------------------*/
C /* OPEN the NetCDF file. */
C /*------------------------------------------------------------*/
      ncid = NCOPN(NetCDFFilename, NCNOWRIT, ncrcode)
!/* Handle return code problems from NCOPN. */
      IF (ncrcode .ne. 0) THEN
         WRITE(*,*) 'NCOPN return code: ', ncrcode
         STOP
      END IF
C /*------------------------------------------------------------*/
C /* OPEN the NetCDF file. */
C /*------------------------------------------------------------*/
CALL NCINQ(ncid, ndims, nvars, ngatts, recdim, ncrcode)
!/* Handle return code problems from NCINQ. */
      IF (ncrcode .ne. 0) THEN
         WRITE(*,*) 'NCINQ return code: ', ncrcode
         STOP
      ELSE
         WRITE(*,*) 'Number of Dimensions (ndims ): ', ndims,
         2 'Number of Variables (nvars ): ', nvars,
         3 'Number of Attributes (ngatts): ', ngatts,
         4 'Record Dimension Index (ngatts): ', recdim
      END IF
C /*------------------------------------------------------------*/
C /* CLOSE the NetCDF file. */
C /*------------------------------------------------------------*/
CALL NCCLOS(ncid, ncrcode)
!/* Handle return code problems from NCCLOS. */
      IF (ncrcode .ne. 0) THEN
         WRITE(*,*) 'NCCLOS return code: ', ncrcode
         STOP
      END IF
C /*------------------------------------------------------------*/
C /* DONE processing the NetCDF file. */
C /*------------------------------------------------------------*/
      END
C/*********************************************************************/
