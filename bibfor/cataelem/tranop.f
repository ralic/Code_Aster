      SUBROUTINE TRANOP(INIT,FIN,IVAL,CVAL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CATAELEM  DATE 11/03/98   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C TOLE CRS_512
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     INCLUDE($CIMPERR)
C
      COMMON /CIMP/IMP,IULMES,IULIST,IULVIG
C
C     EXCLUDE($CIMPERR)
C
C
C   OPTION_SIMPLE         2
C                 <1>        <2>
C
C   RIGIDITE_MECA
C                 <3>
C
C   IN     3
C      <4>   <5>
C
C      GEO       GEOME
C         <6>          <7>/<8>
C
C      HOO        HOOCKE
C
C      CARAC      CARACTERISTIQUE
C
C      OUT    1
C  <8>    <9>  <10>
C
C     K           MATUU
C      <11>             <12>/<2>
C
C
C
C   HOOK_DE_T
C
C   IN    3
C
C
C      MAT       MATER
C
C
C      TEMP       TEMPER
C
C      GEO        GEOME
C
C   OUT    1
C
C     HOO         HOOKE
C
C
C   OPTION_COMPOSEE       1
C                   <13>    <14>
C
C   RIGIDITE_DE_T
C                  <15>
C
C   IN       4 % PARAM_IN
C      <16>    <17>
C
C       MAT     MAT
C          <34>    <18>/<19>
C
C       TEMP   TEMP
C
C       GEO    GEO
C
C       CARAC   CARAC
C
C
C       OUT       1
C  <19>     <20>    <21>
C
C          K         K
C        <35>          <22>/<23>
C
C        INTER       1
C  <23>       <24>    <25>
C
C        HOO            HOOK
C           <36>            <26>/<27>
C
C        DECOMPOSITION        2
C  <27>               <28>     <29>
C
C      HOOK_DE_T
C               <30>
C
C       2
C         <31>
C
C       MAT         MAT
C         <32>         <33>/<29>
C
C      TEMP        TEMP
C
C
C      RIGIDITE_MECA
C
C
C     2
C
C      GEO         GEO
C
C
C     CARAC        CARAC
C
C
C   FIN
C      -----------------------------------------------------------------
C      !OPTION- ! OPTION- ! IN ! OUT ! INTER !DECOMPO-!MOT ! INT ! EOF/!
C      !SIMPLE  ! COMPOSEE!    !     !       !SITION  !    !     ! FIN !
C      !  =     !   =     ! =  ! =   !  =    !  =     !    !     !     !
C      ! CLEOPT ! CLEOPT  !CLE ! CLE !CLEOPT ! CLEOPT !    !     !     !
C      !        !         !OPT ! OPT !       !        !    !     !     !
C      ! (1)    !  (2)    !(3) ! (4) ! (5)   ! (6)    !    !     !     !
C-----------------------------------------------------------------------
C  0   ! 1      !  13     !    !     !       !        !    !     ! 60  !
C-----------------------------------------------------------------------
C  1   !        !         !    !     !       !        !    !  2  !     !
C-----------------------------------------------------------------------
C  2   !        !         !    !     !       !        ! 3  !     ! 50  !
C-----------------------------------------------------------------------
C  3   !        !         !  4 !     !       !        !    !     !     !
C-----------------------------------------------------------------------
C  4   !        !         !    !     !       !        !    ! 5   !     !
C-----------------------------------------------------------------------
C  5   !        !         !    !     !       !        ! 6  !     !     !
C-----------------------------------------------------------------------
C  6   !        !         !    !     !       !        ! 7  !     !     !
C-----------------------------------------------------------------------
C  7   !        !         !    !     !       !        ! 6  !     !     !
C-----------------------------------------------------------------------
C  8   !        !         !    !  9  !       !        !    !     !     !
C-----------------------------------------------------------------------
C  9   !        !         !    !     !       !        !    ! 10  !     !
C-----------------------------------------------------------------------
C  10  !        !         !    !     !       !        ! 11 !     ! 51  !
C-----------------------------------------------------------------------
C  11  !        !         !    !     !       !        ! 12 !     !     !
C-----------------------------------------------------------------------
C  12  !        !         !    !     !       !        ! 11 !     ! 52  !
C-----------------------------------------------------------------------
C  13  !        !         !    !     !       !        !    ! 14  !     !
C-----------------------------------------------------------------------
C  14  !        !         !    !     !       !        ! 15 !     ! 53  !
C-----------------------------------------------------------------------
C  15  !        !         ! 16 !     !       !        !    !     !     !
C-----------------------------------------------------------------------
C  16  !        !         !    !     !       !        !    ! 17  !     !
C-----------------------------------------------------------------------
C  17  !        !         !    !     !       !        ! 34 !     !     !
C-----------------------------------------------------------------------
C  18  !        !         !    !     !       !        ! 34 !     !     !
C-----------------------------------------------------------------------
C  19  !        !         !    ! 20  !       !        !    !     !     !
C-----------------------------------------------------------------------
C  20  !        !         !    !     !       !        !    ! 21  !     !
C-----------------------------------------------------------------------
C  21  !        !         !    !     !       !        ! 35 !     !     !
C-----------------------------------------------------------------------
C  22  !        !         !    !     !       !        ! 35 !     !     !
C-----------------------------------------------------------------------
C  23  !        !         !    !     !  24   !        !    !     !     !
C-----------------------------------------------------------------------
C  24  !        !         !    !     !       !        !    ! 25  !     !
C-----------------------------------------------------------------------
C  25  !        !         !    !     !       !        ! 36 !     !     !
C-----------------------------------------------------------------------
C  26  !        !         !    !     !       !        ! 36 !     !     !
C-----------------------------------------------------------------------
C  27  !        !         !    !     !       !  28    !    !     !     !
C-----------------------------------------------------------------------
C  28  !        !         !    !     !       !        !    ! 29  !     !
C-----------------------------------------------------------------------
C  29  !        !         !    !     !       !        ! 30 !     ! 54  !
C-----------------------------------------------------------------------
C  30  !        !         !    !     !       !        !    ! 31  !     !
C-----------------------------------------------------------------------
C  31  !        !         !    !     !       !        ! 32 !     ! 55  !
C-----------------------------------------------------------------------
C  32  !        !         !    !     !       !        ! 33 !     !     !
C-----------------------------------------------------------------------
C  33  !        !         !    !     !       !        ! 32 !     ! 56  !
C-----------------------------------------------------------------------
C  34  !        !         !    !     !       !        ! 18 !     !     !
C-----------------------------------------------------------------------
C  35  !        !         !    !     !       !        ! 22 !     !     !
C-----------------------------------------------------------------------
C  36  !        !         !    !     !       !        ! 26 !     !     !
C-----------------------------------------------------------------------
      INTEGER INIT,FIN,IVAL
      CHARACTER*(*) CVAL
C
      REAL*8 RVAL
      INTEGER ERRLEX,ERRGRA,LIRE
C
C     INCLUDE($CMOTOPT)
      CHARACTER*16 CLEOPT
      COMMON /CMTOPN/NCLOPT
      COMMON /CMTOPC/CLEOPT(6)
C     EXCLUDE($CMOTOPT)
C
C     INCLUDE($CDEBUG)
      CHARACTER*8 CLEDBG
      CHARACTER*24 OBJDMP
      INTEGER PASDMP,TYOBDM
      COMMON /CMODBG/CLEDBG
      COMMON /CDEBUG/ICCDBG
      COMMON /CBDMPC/OBJDMP(30)
      COMMON /CBDMPN/NDMP,PASDMP(30),TYOBDM(30)
C
C       NDMP : NOMBRE D OBJETS A DUMPER
C       PASDMP(IDMP)  : PASSE OU ON DUMPE L OBJET IDMP
C       OBJDMP(IDMP)  : NOM DE L OBJET IDMP
C       TYOBDM(IDMP)  : GENRE DE L OBJET IDMP :  0 OBJET SIMPLE
C                                                1 COLLECTION NUMEROTEE
C                                                2 COLLECTION NOMME
C
C     EXCLUDE($CDEBUG)
C
      ASSIGN 500 TO LIRE
      ASSIGN 900 TO ERRLEX
      ASSIGN 901 TO ERRGRA
C
  500 CONTINUE
      CALL SNLIRE(ICLASS,IVAL,RVAL,CVAL)
      IF (ICLASS.EQ.0) GO TO ERRLEX
      IF (ICLASS.EQ.3) THEN
        IF (CVAL(1:IVAL).EQ.'FIN') THEN
          ICLASS = -1
        END IF

      END IF
C
      IF (ICLASS.EQ.3) THEN
        IF (CVAL(1:IVAL).EQ.CLEDBG) THEN
          CALL LCTDBG(ICLASS,IVAL,RVAL,CVAL,IRTET)
          IF ( IRTET.GT.0 ) GOTO (901), IRTET
          GO TO LIRE

        END IF

      END IF
C
      IF (INIT.EQ.0) THEN
        CALL TRSALT(FIN,ICLASS,IVAL,CVAL,CLEOPT(1),1,CLEOPT(2),13,60,
     +              IRTET)
        IF ( IRTET.GT.0 ) GOTO (901), IRTET
        GOTO 9999

      ELSE
        GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
     +         23,24,25,26,27,28,29,30,31,32,33,34,35,36),INIT

      END IF
C
    1 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,1,' ',FIN,2,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    4 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,1,' ',FIN,5,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    9 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,1,' ',FIN,10,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   13 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,1,' ',FIN,14,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   16 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,1,' ',FIN,17,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   20 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,1,' ',FIN,21,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   24 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,1,' ',FIN,25,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   28 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,1,' ',FIN,29,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   30 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,1,' ',FIN,31,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    5 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,6,IRTET)
       IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    6 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,7,IRTET)
       IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    7 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,6,IRTET)
       IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   11 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,12,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   17 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,34,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   18 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,34,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   21 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,35,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   22 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,35,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   25 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,36,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   26 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,36,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   32 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,33,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   34 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,18,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   35 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,22,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   36 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,26,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    3 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,4,CLEOPT(3),FIN,4,IRTET)
       IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    8 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,4,CLEOPT(4),FIN,9,IRTET)
       IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   15 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,4,CLEOPT(3),FIN,16,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   19 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,4,CLEOPT(4),FIN,20,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   23 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,4,CLEOPT(5),FIN,24,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   27 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,4,CLEOPT(6),FIN,28,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    2 CONTINUE
      CALL WMEOF(ICLASS,IVAL,CVAL,FIN,3,50,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   10 CONTINUE
      CALL WMEOF(ICLASS,IVAL,CVAL,FIN,11,51,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   12 CONTINUE
      CALL WMEOF(ICLASS,IVAL,CVAL,FIN,11,52,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   14 CONTINUE
      CALL WMEOF(ICLASS,IVAL,CVAL,FIN,15,53,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   29 CONTINUE
      CALL WMEOF(ICLASS,IVAL,CVAL,FIN,30,54,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   31 CONTINUE
      CALL WMEOF(ICLASS,IVAL,CVAL,FIN,32,55,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   33 CONTINUE
      CALL WMEOF(ICLASS,IVAL,CVAL,FIN,32,56,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
  900 CONTINUE
      WRITE (IULMES,9000)
      CALL UTMESS('F','TRANOP','PB. LECTURE CATALOGUES.')
C
  901 CONTINUE
      WRITE (IULMES,9001) ICLASS,INIT,FIN
      IF (ICLASS.EQ.1) THEN
        WRITE (IULMES,9002) IVAL

      ELSE IF (ICLASS.EQ.2) THEN
        WRITE (IULMES,9003) RVAL

      ELSE IF (ICLASS.EQ.3) THEN
        WRITE (IULMES,9004) IVAL,CVAL(1:IVAL)

      ELSE IF (ICLASS.EQ.4) THEN
        WRITE (IULMES,9005) IVAL,CVAL(1:IVAL)

      ELSE IF (ICLASS.GE.7) THEN
        WRITE (IULMES,9006) CVAL(1:1)
      END IF

      CALL UTMESS('F','TRANOP','ERREUR GRAMMAIRE')
C
 9000 FORMAT (' ************* LECTURE CATALOGUE OPTIONS  **',/,
     +       ' ************* ERREUR LEXICALE *************',/)
 9001 FORMAT (' ************* LECTURE CATALOGUE OPTIONS  **',/,
     +       ' ************* ERREUR GRAMMAIRE ************',/,
     +       ' TYPE DERNIERE QUANTITE LUE ',I5,/,' ETAT INITIAL ',I5,/,
     +       ' ETAT FINAL   ',I5,/)
 9002 FORMAT (' ENTIER LU : ',I10)
 9003 FORMAT (' REAL   LU : ',D12.5)
 9004 FORMAT (' IDENTIFICATEUR LU : ',/,' LONGUEUR ',I4,/,2X,A72)
 9005 FORMAT (' TEXTE  LU : ',/,' LONGUEUR ',I4,/,2X,A72)
 9006 FORMAT (' SEPARATEUR LU : ',A1)

 9999 CONTINUE
      END
