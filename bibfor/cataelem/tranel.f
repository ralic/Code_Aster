      SUBROUTINE TRANEL(INIT,FIN,IVAL,CVAL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CATAELEM  DATE 12/05/97   AUTEUR JMBHH01 J.M.PROIX 
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
C TOLE CRP_20
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     INCLUDE($CIMPERR)
C
      COMMON /CIMP/IMP,IULMES,IULIST,IULVIG
C
C     EXCLUDE($CIMPERR)
C
C
C
C
C     ELEMENT     TRI3_MEC
C           <1>          <2>
C     MAILLE       TRI3      17
C           <3>       <4>      <5>
C     CARTE    2
C         <6>   <7>
C %  NBRE DE  MODE_LOCAUX "N" PRIS PAR DEFAUT LORS DE L EXTRACTION  SUR
C     %  LES CARTES
C
C     N1 =  MASVO           E      1    IDEN    1    RHO
C      <8>      <9>         <10>   <11>    <12> <13>    <14>  <14>
C
C     CHAMNO    3
C          <18>   <19>
C %  NBRE DE MODE_LOCAUX "N" PRIS PAR DEFAUT LORS DE L EXTRACTION   SUR
C     %  LES CHAMPS GLOBAUX AUX NOEUDS
C
C     N2 =  GEOME           N      3    IDEN     2     X     Y
C      <20>     <21>        <22>   <23>    <24> <25>    <26>  <26>
C     N3 =  DEPLA           N      3    DIFF     2     UX   UY
C      <20>     <21>        <22>   <23>    <27> <28>    <29>  <29>
C                                               3      RX   RY  RZ
C                                               <28>    <29>  <29>
C                                               2      UX   UY
C                                               <28>    <29>  <29>
C
C     N4 =  TEMPE           N   3  IDEN  1  THETA
C
C     CHAMELEM    2
C         <30>     <31>
C %  NBRE DE MODE_LOCAUX "N " SUPPLEMENTAIRES POUR LES CHAMPS GLOBAUX
C     %  PAR ELEMENTS
C
C     N5 =  TEMPE           E       4      IDEN     1     THETA
C  <32>      <33>       <34>    <35>      <36>  <37>        <38>  <38>
C     N6 =  MASVO           E       4      DIFF     1     RHO
C  <32>      <33>       <34>    <35>      <39>  <40>        <41>  <41>
C                                                  2     UX    UY
C                                               <40>        <41>  <41>
C                                                  3     RX    RY   RZ
C                                               <40>        <41>  <41>
C                                                  1     RHO
C                                               <40>        <41>  <41>
C     VECTEUR    1
C         <42>   <43>
C     %  NBRE DE MODE_LOCAUX  "V "  (VECTEUR ELEMENTAIRE)
C
C     V1  =   VECTU         N2
C      <44>       <45>        <46>
C
C     MATRICE      1
C          <47>   <48>
C     %  NBRE DE MODE_LOCAUX  "M "  (MATRICE ELEMENTAIRE)
C
C     M1 =     MATUU           N3       N3     % MATRICE(U,U)
C      <49>         <50>         <51>     <52>
C     OPTION      2
C         <53>  <54>
C     %  NBRE D OPTIONS CALCULEES PAR LE TYPE_ELEMENT
C     %
C     %  OPTION        NUMERO
C     %    !           CALCUL
C     %    !          (CALC_NUMC)
C     %    !              I
C     %    V              V
C
C     MASSE              17
C           <55>           <56>
C                 2      N2     NOM      N6         NOM
C                  <57>    <69>     <58>   <69>         <58>/<68>
C                 1      M1        NOM
C                  <66>    <70>       <67>
C
C     CONV_TRI3_MEC_01   34
C
C                 2   N4   NOM   N2  NOM
C
C                 1   N5   NOM
C
C     CONVERT    2
C          <59>  <60>
C       NBRE DE CONVERSIONS PREVUES POUR LE TYPE_ELEMENT
C
C     GRANDEUR     OPTION    MODE_LOCAL   MODE_LOCAL    NUM_CALC
C      !         APPELANTE    "IN"         "OUT"        I
C      !             I         !             !          I
C      !             I         !             I          !
C      V             V         V             V          V
C
C     MASVO           *         N1            *         -1  % EXPAND
C          <61>        <62>      <63>          <64>      <65>
C TEMPE           *         N4            N5        34  % CONV_TRI3_01
C
C     % FIN DE  TRI3_MEC
C
C
C ----------------------------------------------------------------------
C   ! M ! I !SEP! E/! I ! D !E ! M !C!C !C ! V ! M! O! C! E !  !
C   ! O ! N !   ! N ! D ! I !L ! A !A!H !H ! E ! A! P! O! O !* !
C   ! T ! T !   !   ! E ! F !E ! I !R!A !A ! C ! T! T! N! F !  !
C   !   !   !   !   ! N ! F !M ! L !T!M !E ! T ! R! I! V!   !  !
C   !   !   !   !   !   !   !E ! L !E!N !L ! E ! I! O! E!   !  !
C   !   !   !   !   !   !   !N ! E ! !O !E ! U ! C! N! R!   !  !
C   !   !   !   !   !   !   !T !   ! !  !M ! R ! E!  ! T!   !  !
C ----------------------------------------------------------!--!
C 0 !   !   !   !   !   !   ! 1! 3 !6!18!30!42 !47!53!59!100!  !
C ----------------------------------------------------------!--!
C 1 ! 2 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 2 !   !   !   !   !   !   !  ! 3 ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 3 ! 4 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 4 !   ! 5 !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 5 !   !   !   !   !   !   !1 !   !6!18!30!42 !47!53!59!101!  !
C ----------------------------------------------------------!--!
C 6 !   ! 7 !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 7 ! 8 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !102!  !
C ----------------------------------------------------------!--!
C 8 ! 9 !   !  8!   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 9 !   !   !   !10 !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 10!   ! 1 ->11!   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 11!   !   !   !   !12 !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 12!   ! 13!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 13!14 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 14!14 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !103!  !
C ----------------------------------------------------------!--!
C 18!   ! 19!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 19!20 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !105!  !
C ----------------------------------------------------------!--!
C 20!21 !   ! 20!   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 21!   !   !   !E>22   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 22!   !23 !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 23!   !   !   !   !24 ! 27!  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 24!   !25 !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 25! 26!   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 26! 26!   !   !   !   !   !  !   ! !  !  !   !  !  !  !106!  !
C ----------------------------------------------------------!--!
C 27!   ! 28!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 28!29 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 29!29 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !107!  !
C ----------------------------------------------------------!--!
C 30!   ! 31!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 31! 32!   !   !   !   !   !  !   ! !  !  !   !  !  !  !108!  !
C ----------------------------------------------------------!--!
C 32!33 !   ! 32!   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 33!   !   !   !E>34   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 34!   ! 35!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 35!   !   !   !   !36 ! 39!  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 36!   ! 37!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 37!38 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 38!38 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !109!  !
C ----------------------------------------------------------!--!
C 39!   ! 40!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 40! 41!   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 41! 41!   !   !   !   !   !  !   ! !  !  !   !  !  !  !110!  !
C ----------------------------------------------------------!--!
C 42!   !43 !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 43!44 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !111!  !
C ----------------------------------------------------------!--!
C 44!45 !   ! 44!   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 45!46 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 46!   !   !   !   !   !   ! 1!   !6!18!30!42 !47!53!59!112!  !
C ----------------------------------------------------------!--!
C 47!   ! 48!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 48!49 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !113!  !
C ----------------------------------------------------------!--!
C 49!50 !   ! 49!   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 50!51 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 51!52 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 52!   !   !   !   !   !   !1 !   !6!18!30!42 !47!53!59!114!  !
C ----------------------------------------------------------!--!
C 53!   ! 54!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 54!55 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !115!  !
C ----------------------------------------------------------!--!
C 55!   ! 56!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 56!   !57 !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 57! 69!   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 58! 69!   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 59!   !60 !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 60! 61!   !   !   !   !   !  !   ! !  !  !   !  !  !  !118!61!
C ----------------------------------------------------------!--!
C 61! 62!   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !62!
C ----------------------------------------------------------!--!
C 62! 63!   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !63!
C ----------------------------------------------------------!--!
C 63! 64!   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !64!
C ----------------------------------------------------------!--!
C 64!   ! 65!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 65! 61!   !   !   !   !   !  !   ! !  !  !   !  !  !  !119!  !
C ----------------------------------------------------------!--!
C 66! 70!   !   !   !   !   !  !   ! !  !  !   !  !  !  !116!  !
C ----------------------------------------------------------!--!
C 67! 70!   !   !   !   !   !  !   ! !  !  !   !  !  !  !117!  !
C ----------------------------------------------------------!--!
C 68!   ! 66!   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 69!58 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C 70!67 !   !   !   !   !   !  !   ! !  !  !   !  !  !  !   !  !
C ----------------------------------------------------------!--!
C
C     ALTERATION : EN FAIT EN 10 ON NE PEUT LIRE QUE 1 ET ON VA EN 11
C          EN 11 ON NE PEUT LIRE QUE IDEN
C          LES ETATS 15 , 16 , 17 N EXISTENT PLUS
C
C
      INTEGER INIT,FIN,IVAL
      CHARACTER*(*) CVAL
C
      REAL*8 RVAL
      INTEGER ERRLEX,ERRGRA,LIRE
C
C     INCLUDE($CMOTELE)
      CHARACTER*16 CLELE
      COMMON /CMELEN/NCLELE
      COMMON /CMELEC/CLELE(9)
C     EXCLUDE($CMOTELE)
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
        CALL TRCLEL(FIN,ICLASS,IVAL,CVAL,1,3,6,18,30,42,47,53,59,100,
     +              IRTET)
        IF ( IRTET.GT.0 ) GOTO (901), IRTET
        GOTO 9999

      ELSE
        GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,900,900,900,18,19,20,21,
     +         22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
     +         41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,
     +         60,61,62,63,64,65,66,67,68,69,70) INIT

      END IF
C
    1 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 2
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    2 CONTINUE
      IF (ICLASS.EQ.3) THEN
        IF (CVAL(1:IVAL).EQ.CLELE(2)) THEN
          FIN = 3
          GOTO 9999

        ELSE
          GO TO ERRGRA

        END IF

      ELSE
        GO TO ERRGRA

      END IF
C
    3 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 4
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    4 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 5
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    5 CONTINUE
      CALL TRCLEL(FIN,ICLASS,IVAL,CVAL,1,-1,6,18,30,42,47,53,59,101,
     +            IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    6 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 7
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    7 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 102
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 8
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    8 CONTINUE
      IF (ICLASS.EQ.11) THEN
        GO TO LIRE

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 9
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    9 CONTINUE
      CALL TRSALT(FIN,ICLASS,IVAL,CVAL,'E',10,'N',10,-1,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   10 CONTINUE
      IF ((ICLASS.EQ.1) .AND. (IVAL.EQ.1)) THEN
        FIN = 11
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   11 CONTINUE
      CALL TRSALT(FIN,ICLASS,IVAL,CVAL,'IDEN',12,'IDEN',12,-1,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   12 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 13
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   13 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 14
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   14 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 103
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 14
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   18 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 19
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   19 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 105
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 20
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   20 CONTINUE
      IF (ICLASS.EQ.11) THEN
        GO TO LIRE

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 21
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   21 CONTINUE
      CALL TRSALT(FIN,ICLASS,IVAL,CVAL,'N',22,'N',22,-1,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   22 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 23
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   23 CONTINUE
      CALL TRSALT(FIN,ICLASS,IVAL,CVAL,'IDEN',24,'DIFF',27,-1,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   24 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 25
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   25 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 26
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   26 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 106
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 26
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   27 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 28
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   28 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 29
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   29 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 107
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 29
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   30 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 31
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   31 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 108
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 32
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   32 CONTINUE
      IF (ICLASS.EQ.11) THEN
        GO TO LIRE

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 33
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   33 CONTINUE
      CALL TRSALT(FIN,ICLASS,IVAL,CVAL,'E',34,'N',34,-1,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   34 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 35
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   35 CONTINUE
      CALL TRSALT(FIN,ICLASS,IVAL,CVAL,'IDEN',36,'DIFF',39,-1,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   36 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 37
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   37 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 38
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   38 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 109
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 38
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   39 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 40
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   40 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 41
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   41 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 110
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 41
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   42 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 43
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   43 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 111
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 44
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   44 CONTINUE
      IF (ICLASS.EQ.11) THEN
        GO TO LIRE

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 45
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   45 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 46
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   46 CONTINUE
      CALL TRCLEL(FIN,ICLASS,IVAL,CVAL,1,-1,6,18,30,42,47,53,59,112,
     +            IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   47 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 48
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   48 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 113
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 49
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   49 CONTINUE
      IF (ICLASS.EQ.11) THEN
        GO TO LIRE

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 50
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   50 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 51
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   51 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 52
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   52 CONTINUE
      CALL TRCLEL(FIN,ICLASS,IVAL,CVAL,1,-1,6,18,30,42,47,53,59,114,
     +            IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
   53 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 54
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   54 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 115
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 55
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   55 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 56
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   56 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 57
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   57 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 69
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   58 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 69
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   59 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 60
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   60 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 118
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 61
        GOTO 9999

      ELSE IF ((ICLASS.EQ.4) .AND. (CVAL(1:IVAL).EQ.'*')) THEN
        FIN = 61
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   61 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 62
        GOTO 9999

      ELSE IF ((ICLASS.EQ.4) .AND. (CVAL(1:IVAL).EQ.'*')) THEN
        FIN = 62
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   62 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 63
        GOTO 9999

      ELSE IF ((ICLASS.EQ.4) .AND. (CVAL(1:IVAL).EQ.'*')) THEN
        FIN = 63
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   63 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 64
        GOTO 9999

      ELSE IF ((ICLASS.EQ.4) .AND. (CVAL(1:IVAL).EQ.'*')) THEN
        FIN = 64
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   64 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 65
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   65 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 119
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 61
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   66 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 116
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 70
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   67 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 117
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        FIN = 70
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   68 CONTINUE
      IF (ICLASS.EQ.1) THEN
        FIN = 66
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   69 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 58
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
   70 CONTINUE
      IF (ICLASS.EQ.3) THEN
        FIN = 67
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
  900 CONTINUE
      WRITE (IULMES,9000)
      CALL UTMESS('F','TRANEL','PB. LECTURE CATALOGUES.')
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

      CALL UTMESS('F','TRANEL','ERREUR GRAMMAIRE')
C
 9000 FORMAT (' ************* LECTURE CATALOGUE ELEMENTS  **',/,
     +       ' ************* ERREUR LEXICALE **************',/)
 9001 FORMAT (' ************* LECTURE CATALOGUE ELEMENTS  **',/,
     +       ' ************* ERREUR GRAMMAIRE *************',/,
     +       ' TYPE DERNIERE QUANTITE LUE ',I5,/,' ETAT INITIAL ',I5,/,
     +       ' ETAT FINAL   ',I5,/)
 9002 FORMAT (' ENTIER LU : ',I10)
 9003 FORMAT (' REAL   LU : ',D12.5)
 9004 FORMAT (' IDENTIFICATEUR LU : ',/,' LONGUEUR ',I4,/,2X,A72)
 9005 FORMAT (' TEXTE  LU : ',/,' LONGUEUR ',I4,/,2X,A72)
 9006 FORMAT (' SEPARATEUR LU : ',A1)

 9999 CONTINUE
      END
