      SUBROUTINE TRANPH(INIT,FIN,IVAL,CVAL)
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
C   EVOLUTION DES CATALOGUES TYPE- MAILLE ET TYPE-ELEMENT
C
C
C    PHENOMENES_MODELISATION
C                           <1>
C
C    PHENOMENE       MECANIQUE       MODELISATION         3D
C             <2>              <3>                <4>        <5>
C
C    MAILLE       TETRA4    ELEMENT       MECA_TET4
C            <6>         <7>        <8>             <9>
C
C    MAILLE       TETRA4    ELEMENT       MECA_TET4
C    MAILLE       TETRA4    ELEMENT       MECA_TET4
C    MAILLE       TETRA4    ELEMENT       MECA_TET4
C
C   FIN
C    -----------------------------------------------------------------
C    ! M ! T ! CLE(1) ! CLE(2) ! CLE(3) ! CLE(4) ! CLE(5) !  EOF/FIN!
C    ! O ! E !        !        !        !        !        !          !
C    ! T ! X !        !        !        !        !        !          !
C ---------------------------------------------------------------------
C 0  !   !   !   1    !        !        !        !        !          !
C ---------------------------------------------------------------------
C 1  !   !   !        !  2     !        !        !        !          !
C ---------------------------------------------------------------------
C 2  ! 3 !   !        !        !        !        !        !          !
C ---------------------------------------------------------------------
C 3  !   !   !        !        !  4     !        !        !          !
C ---------------------------------------------------------------------
C 4  ! 5 ! 5 !        !        !        !        !        !          !
C ---------------------------------------------------------------------
C 5  !   !   !        !        !        !  6     !        !          !
C ---------------------------------------------------------------------
C 6  ! 7 !   !        !        !        !        !        !          !
C ---------------------------------------------------------------------
C 7  !   !   !        !        !        !        !  8     !          !
C ---------------------------------------------------------------------
C 8  ! 9 !   !        !        !        !        !        !          !
C ---------------------------------------------------------------------
C 9  !   !   !        !   2    !  4     !  6     !        !   10     !
C ---------------------------------------------------------------------
C
C
      INTEGER INIT,FIN,IVAL
      CHARACTER*(*) CVAL
C
      REAL*8 RVAL
      INTEGER ERRLEX,ERRGRA,LIRE,ERRPGM
C
C     INCLUDE($CMOTPH)
      CHARACTER*24 CLEPH
      COMMON /CMTPHN/NCLEPH
      COMMON /CMTPHC/CLEPH(5)
C
C     CLEPH(1) = 'PHENOMENES_MODELISATION '
C     CLEPH(2) = 'PHENOMENE               '
C     CLEPH(3) = 'MODELISATION            '
C     CLEPH(4) = 'MAILLE                  '
C     CLEPH(5) = 'ELEMENT                 '
C
C     EXCLUDE($CMOTPH)
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
      ASSIGN 902 TO ERRPGM
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
        CALL WCLASS(ICLASS,IVAL,CVAL,4,CLEPH(1),FIN,1,IRTET)
        IF ( IRTET.GT.0 ) GOTO (901), IRTET
        GOTO 9999

      ELSE
        GO TO (1,2,3,4,5,6,7,8,9,10),INIT

      END IF
C
    1 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,4,CLEPH(2),FIN,2,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    2 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,'      ',FIN,3,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    3 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,4,CLEPH(3),FIN,4,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    4 CONTINUE
      IF ((ICLASS.EQ.3) .OR. (ICLASS.EQ.4)) THEN
        FIN = 5
        GOTO 9999

      ELSE
        GO TO ERRGRA

      END IF
C
    5 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,4,CLEPH(4),FIN,6,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    6 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,'      ',FIN,7,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    7 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,4,CLEPH(5),FIN,8,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    8 CONTINUE
      CALL WCLASS(ICLASS,IVAL,CVAL,3,'      ',FIN,9,IRTET)
      IF ( IRTET.GT.0 ) GOTO (901), IRTET
      GOTO 9999
C
    9 CONTINUE
      IF (ICLASS.EQ.-1) THEN
        FIN = 10
        GOTO 9999

      ELSE IF (ICLASS.EQ.3) THEN
        IF (CVAL(1:IVAL).EQ.CLEPH(2)) THEN
          FIN = 2
          GOTO 9999

        ELSE IF (CVAL(1:IVAL).EQ.CLEPH(3)) THEN
          FIN = 4
          GOTO 9999

        ELSE IF (CVAL(1:IVAL).EQ.CLEPH(4)) THEN
          FIN = 6
          GOTO 9999

        ELSE
          GO TO ERRGRA

        END IF

      ELSE
        GO TO ERRGRA

      END IF
C
   10 CONTINUE
      GO TO ERRPGM

C
C
  900 CONTINUE
      WRITE (IULMES,9000)
      CALL UTMESS('F','TRANPH','PB. LECTURE CATALOGUES.')
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

      CALL UTMESS('F','TRANPH','PB. LECTURE CATALOGUES.')
C

  902 CONTINUE
      WRITE (IULMES,9010)
      CALL UTMESS('F','TRANPH','PB. LECTURE CATALOGUES.')
C
 9000 FORMAT (' *** LECTURE CATALOGUE PHENOMENES_MODELISATION**',/,
     +       ' *** ERREUR LEXICALE **************',/)
 9001 FORMAT (' **** LECTURE CATALOGUE PHENOMENES_MODELISATION**',/,
     +       ' **** ERREUR GRAMMAIRE *************',/,
     +       ' TYPE DERNIERE QUANTITE LUE ',I5,/,' ETAT INITIAL ',I5,
     +       /,' ETAT FINAL   ',I5,/)
 9002 FORMAT (' ENTIER LU : ',I10)
 9003 FORMAT (' REAL   LU : ',D12.5)
 9004 FORMAT (' IDENTIFICATEUR LU : ',/,' LONGUEUR ',I4,/,2X,A72)
 9005 FORMAT (' TEXTE  LU : ',/,' LONGUEUR ',I4,/,2X,A72)
 9006 FORMAT (' SEPARATEUR LU : ',A1)
 9010 FORMAT (' *** LECTURE CATALOGUE PHENOMENES_MODELISATION**',/,
     +       ' *** ERREUR PROGRAMME **************')

 9999 CONTINUE
      END
