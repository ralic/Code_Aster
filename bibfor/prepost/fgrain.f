        SUBROUTINE FGRAIN (PIC,NPIC,ITRV,NCYC,SIGMIN,SIGMAX)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ----------------------------------------------------------------
C      COMPTAGE DES CYCLES PAR LA METHODE RAINFLOW (POSTDAM)
C       ----------------------------------------------------------------
C      IN  PIC     VECTEUR  DES PICS
C          NPIC    NOMBRE   DE  PICS
C          ITRV    VECTEUR  DE TRAVAIL ENTIER
C      OUT SIGMAX  CONTRAINTES MAXIMALES DES CYCLES
C          SIGMIN  CONTRAINTES MINIMALES DES CYCLES
C      OUT  NCYC    NOMBRE  DE  CYCLE
C       ----------------------------------------------------------------
        IMPLICIT REAL*8 (A-H,O-Z)
        REAL*8          PIC(*), X, Y,E1,E2,E3,SIGMAX(*),SIGMIN(*)
        REAL*8          R1,R2,RD,RAD
        INTEGER         NPIC,  NCYC,   ITRV(*) ,NPICB
        LOGICAL         LRESI
C       ----------------------------------------------------------------
        LRESI = .FALSE.
        NPICB = NPIC
        DO 20 I=1,NPICB
         ITRV(I) = I
 20     CONTINUE
        NCYC  = 0
 1      CONTINUE
        I     = 1
        J     = 1
C
  2     CONTINUE
        IF(I+3.GT.NPICB) THEN
          GOTO 100
        ENDIF
        E1 = ABS ( PIC(ITRV(I+1)) - PIC(ITRV(I)) )
        E2 = ABS ( PIC(ITRV(I+2)) - PIC(ITRV(I+1)))
        E3 = ABS ( PIC(ITRV(I+3)) - PIC(ITRV(I+2)))
C
        IF ( E1. GE. E2 .AND. E3 .GE. E2 ) THEN
          NCYC = NCYC + 1
          IF(PIC(ITRV(I+1)).GE.PIC(ITRV(I+2))) THEN
            SIGMAX(NCYC) = PIC(ITRV(I+1))
            SIGMIN(NCYC) = PIC(ITRV(I+2))
          ELSE
            SIGMAX(NCYC) = PIC(ITRV(I+2))
            SIGMIN(NCYC) = PIC(ITRV(I+1))
          ENDIF
          DO 3 K=I+2,J+2,-1
           ITRV(K) = ITRV(K-2)
  3       CONTINUE
          J=J+2
          I=J
          GOTO 2
        ELSE
          I=I+1
          GOTO 2
        ENDIF
C
C  --- TRAITEMENT DU RESIDU -------
C
 100    CONTINUE
        IF(.NOT.LRESI) THEN
          NPICR = NPICB-2*NCYC
          DO 101 I=1,NPICR
           ITRV(I)= ITRV(2*NCYC+I)
 101      CONTINUE
          R1 = PIC(ITRV(1))
          R2 = PIC(ITRV(2))
          RAD= PIC(ITRV(NPICR-1))
          RD = PIC(ITRV(NPICR))
          X  = (RD-RAD)*(R2-R1)
          Y  = (RD-RAD)*(R1-RD)
          IF(X.GT.0.D0. AND .Y.LT.0.D0) THEN
             DO 102 I=1,NPICR
              ITRV(I+NPICR)= ITRV(I)
 102         CONTINUE
             NPICB = 2*NPICR
          ELSE IF (X.GT.0.D0. AND .Y.GE.0.D0) THEN
C -- ON ELIMINE  R1 ET RN
             DO 103 I=NPICR,2,-1
              ITRV(I+NPICR-2)= ITRV(I)
 103         CONTINUE
             NPICB = 2*NPICR-2
          ELSE IF (X.LT.0.D0. AND .Y.LT.0.D0) THEN
C -- ON ELIMINE R1
             DO 104 I=NPICR,2,-1
              ITRV(I+NPICR-1)= ITRV(I)
 104         CONTINUE
             NPICB = 2*NPICR-1
          ELSE IF (X.LT.0.D0. AND .Y.GE.0.D0) THEN
C -- ON ELIMINE RN
             DO 105 I=NPICR,1,-1
              ITRV(I+NPICR-1)= ITRV(I)
 105         CONTINUE
              NPICB = 2*NPICR-1
          ENDIF
          LRESI = .TRUE.
          GOTO 1
        ENDIF
C
         END
