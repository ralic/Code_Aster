      SUBROUTINE TRANTM(INIT,FIN,IVAL,CVAL)
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     INCLUDE($CIMPERR)
C
      COMMON /CIMP/IMP,IULMES,IULIST,IULVIG
C
C     EXCLUDE($CIMPERR)
C
C
C <0>
C
C   TYPE_MAILLE      2
C               <1>    <2>
C
C    POI1       1
C         <3>     <4> / <5>
C
C    SEG2       2
C
C   FIN
C      ---------------------------------------
C      !TYPE-   ! MOT     ! ENTIER ! FIN/EOF !
C      !MAILLE  !         !        !         !
C      !        !         !        !         !
C---------------------------------------------
C  0   !  1     !         !        !         !
C---------------------------------------------
C  1   !        !         !  2     !         !
C---------------------------------------------
C  2   !        !   3     !        !  10     !
C---------------------------------------------
C  3   !        !         !  4     !         !
C---------------------------------------------
C  4   !        !   3     !        !         !
C---------------------------------------------
C  5   !        !         !        !  11     !
C---------------------------------------------
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
      INTEGER INIT,FIN,IVAL
      CHARACTER*(*) CVAL
C
      REAL*8 RVAL
      INTEGER ERRLEX,ERRGRA,LIRE
      CHARACTER*16 TM
C
      PARAMETER (TM='TYPE_MAILLE     ')
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
        CALL WCLASS(ICLASS,IVAL,CVAL,4,TM,FIN,1,IRTET)
        IF ( IRTET.GT.0 ) GOTO (901), IRTET
        GOTO 9999

      ELSE
        GO TO (1,2,3,4,5),INIT

      END IF
C
    1 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,1,' ',FIN,2,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    2 CONTINUE
      CALL WMEOF(ICLASS,IVAL,CVAL,FIN,3,10,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    3 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,1,' ',FIN,4,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    4 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,' ',FIN,3,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    5 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 11
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
  900 CONTINUE
      WRITE (IULMES,9000)
      CALL UTMESS('F','TRANTM','PB. LECTURE CATALOGUES.')
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

      CALL UTMESS('F','TRANTM','PB. LECTURE CATALOGUES.')
C
 9000 FORMAT (' ************* LECTURE CATALOGUE TYPE_MAILLE  **',/,
     +       ' ************* ERREUR LEXICALE *************',/)
 9001 FORMAT (' ************* LECTURE CATALOGUE TYPE_MAILLE  **',/,
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
