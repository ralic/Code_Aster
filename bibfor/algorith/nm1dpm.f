      SUBROUTINE NM1DPM(FAMI,KPG,KSP,OPTION,NVAR,
     >           ALPHA,
     >           NCSTPM,CSTPM,
     >           SIGM,VIM,
     >           DEPS,
     >           VIP,SIGP,DSDE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
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
C -----------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FAMI,OPTION
      INTEGER       KPG,KSP,NVAR
      REAL*8        ALPHA
      INTEGER       NCSTPM
      REAL*8        CSTPM(NCSTPM)
      REAL*8        SIGM,VIM(NVAR)
      REAL*8        DEPS
      REAL*8        VIP(NVAR),SIGP,DSDE
C -----------------------------------------------------------------
C
C    TRAITEMENT DE LA RELATION DE COMPORTEMENT -ELASTOPLASTICITE-
C    ECROUISSAGE NON LINEAIRE - MODELE DE PINTO MENEGOTTO
C    POUR UN ELEMENT BARRE DE TYPE MECA_ BARRE
C
C -----------------------------------------------------------------
C IN
C       OPTION : OPTION DEMANDEE (R_M_T,FULL OU RAPH_MECA)
C       NVAR   : NOMNBRE DE VARIABLES INTERNES
C       ALPHA  : COEFFICIENT DE DILATATION THERMIQUE
C       TERF   : TEMPERATURE DE REFERENCE
C       NCSTPM : NOMBRE DE CONSTANTES DE MATERIAU
C       CSTPM  : CONSTANTES DE MATERIAU :
C           E      : MODULE D'YOUNG
C           SY     : LIMITE ELASTIQUE
C           EPSU   : DEFORMATION ULTIME
C           SU     : CONTRAINTE ULTIME
C           EPSH   : DEFORMATION A LA FIN DU PALIER PLASTIQUE PARFAIT
C           R0     : COEFFICIENT EXPERIMENTAL
C           B      : COEFFICIENT
C           A1     : COEFFICIENT EXPERIMENTAL
C           A2     : COEFFICIENT EXPERIMENTAL
C           ELAN   : RAPPORT LONGUEUR/DIAMETRE DE LA BARRE
C           A6     : COEFFICIENT EXPERIMENTAL FLAMMBAGE
C           C      : COEFFICIENT EXPERIMENTAL FLAMMBAGE
C           COA    : COEFFICIENT EXPERIMENTAL FLAMMBAGE
C       SIGM   : CONTRAINTE INSTANT MOINS
C       VI M   : VARIABLES INTERNES INSTANT MOINS
C       DEPS   : DEFORMATION TOTALE INSTANT PLUS
C               - DEFORMATION TOTALE INSTANT PLUS
C       TEMPP  : TEMPERATURE IMPOSEE A L'INSTANT PLUS
C
C OUT : SIGP   : CONTRAINTE A L'INSTANT ACTUEL
C       VIP    : VARIABLE INTERNE A L'INSTANT ACTUEL
C       DSDE   : TANGENTE
C
C----------VARIABLES LOCALES
C
      REAL*8      CYCL,PLASTI,EH
      REAL*8      EPSY,R,SIGMAX,TEMPP,TEMPM
      REAL*8      EPSRM,EPSRP,SIGRP,EPSM,DEPSM
      REAL*8      SIGEL,PALEL,PALEC,PALSU,PALGIU
      REAL*8      SIGEPS,EPSMEC,DEPMEC,EPS0,SIG0
      REAL*8      FLBG
      REAL*8      A5,XISEC,XIPRIM,BC,BT,GAS,B0,SIGINF
      REAL*8      CHGDIR,ER,EPSETP,XI,SIGETP
      REAL*8      E,SY,EPSU,SU,EPSH,R0,B,A1
      REAL*8      A2,ELAN,A6,C,COA
      INTEGER     IRET

C
C---------- INITIALISATION DES VARIABLES DE SORTIE
C
      IF ((OPTION.EQ.'FULL_MECA').OR.(OPTION.EQ.'RAPH_MECA')) THEN
        VIP(1) = 0.D0
        VIP(2) = 0.D0
        VIP(3) = 0.D0
        VIP(4) = 0.D0
        VIP(5) = 0.D0
        VIP(6) = 0.D0
        VIP(7) = 0.D0
        VIP(8) = 0.D0
        SIGP   = 0.D0
      ENDIF
      IF ((OPTION.EQ.'FULL_MECA').OR.(OPTION(1:9).EQ.'RIGI_MECA')) THEN
        DSDE   = 0.D0
      ENDIF
C
C----------RECUPERATION DES CARACTERISTIQUES
C
C
      CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,TEMPP,IRET)
      CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TEMPM,IRET)
C
C
      E=CSTPM(1)
      SY=CSTPM(2)
      EPSU=CSTPM(3)
      SU=CSTPM(4)
      EPSH=CSTPM(5)
      R0=CSTPM(6)
      B=CSTPM(7)
      A1=CSTPM(8)
      A2=CSTPM(9)
      ELAN=CSTPM(10)
      A6=CSTPM(11)
      C=CSTPM(12)
      COA=CSTPM(13)
C
      EPSY= SY/E
      EH  = (SU-SY)/(EPSU-EPSY)
      IF (B.EQ.-1.D0) THEN
       B=EH/E
      ENDIF
      B0  = B
C
      EPSRM     =VIM(1)
      EPSRP     =VIM(2)
      SIGRP     =VIM(3)
C
C     EPSM EST EN FAIT EPSMEC DU TEMPS PRECEDENT
C
      EPSM      =VIM(4)
C
C    DEPSM EST EN FAIT DEPMEC DU TEMPS PRECEDENT
C
      DEPSM     =VIM(5)
      CYCL      =VIM(6)
      PLASTI    =VIM(7)
      FLBG      =VIM(8)
C
C  ON APPELLE DEFORMATION MECANIQUE DEF TOTALE - ALPHA(T-TREF)
C
C   DEFORMATION I   OBTENU PAR       I SIGNIFICATION      I
C---------------I--------------------I--------------------I
C   DEPS        I ARGUMENT           I DEF TOTALE TEMPS + I
C               I                    I-DEF TOTALE TEMPS - I
C---------------I--------------------I--------------------I
C   EPSM        I VARIABLE INTERNE - I DEF MECA TEMPS -   I
C---------------I--------------------I--------------------I
C   DEPSM       I VARIABLE INTERNE - I DEF MECA TEMPS -   I
C               I                    I-(DEF MECA TEMPS --)I
C---------------I--------------------I--------------------I
C   DEPMEC      I CALCULEE           I DEF MECA   TEMPS + I
C               I                    I-(DEF MECA TEMPS - )I
C               I                    I =DEPS-ALPH(TP-TM)  I
C---------------I--------------------I--------------------I
C   EPSMEC      I CALCULEE           I DEF MECA   TEMPS + I
C               I                    I =EPSM+DEPMEC       I
C---------------I--------------------I--------------------I
C
C12345678901234567890123456789012345678901234567890123456789012345678901
      DEPMEC  =DEPS-ALPHA*(TEMPP-TEMPM)
      EPSMEC  =EPSM + DEPMEC
      CHGDIR  =DEPSM*DEPMEC
      SIGEPS  =SIGM*DEPMEC
      A5      =1.D0+(5.D0-ELAN)/7.5D0
      SIGINF  =4.D0*SY/ELAN
      GAS     =(11.D0-ELAN)/(10.D0*(EXP(C*ELAN)-1.D0))
C
C
      IF ( OPTION(1:9) .EQ. 'FULL_MECA'  .OR.
     +     OPTION(1:9) .EQ. 'RAPH_MECA' ) THEN
C
C
C**********PREMIER CHARGEMENT********************************
C
        IF(CYCL.LT.0.5D0) THEN
C
C----------CALCUL DES SEUILS PALEL,PALEC,PALSU
C
          SIGEL=SIGM + E * (DEPMEC)
          SIGMAX=SU-(SU-SY)*((EPSU-ABS(EPSMEC))/(EPSU-EPSH))**4
          PALEL=ABS(SIGEL)-SY
          PALEC=ABS(EPSMEC)-EPSH
          PALSU=ABS(SIGEL)-SIGMAX
          PALGIU=ABS(EPSRP-EPSMEC)-SY/(3.D0*E)
          FLBG=0.D0
C
C----------CALCUL DE SIGP
C
C-----------CAS OU ON A DEJA DECHARGE
C
          IF((PLASTI.GT.0.5D0).AND.(SIGEPS.LT.0.D0)) THEN
            IF(PALGIU.LT.0.D0) THEN
               SIGP=SIGM+E*(DEPMEC)
            ELSE
              CYCL=1
              IF(SIGM.GT.0.D0) THEN
                EPSRM=-EPSY
              ELSE
                EPSRM=EPSY
              ENDIF
              EPSRP=EPSM
              SIGRP=SIGM
              IF (DEPMEC.GE.0.D0) THEN
                EPS0=-(SIGRP-SY+EH*EPSY-E*EPSRP)/(E-EH)
                SIG0=EH*(EPS0-EPSY)+SY
C
C-----------CAS FLAMBAGE
                IF(ELAN.GT.5.0D0.AND.FLBG.GT.0.5D0) THEN
                  XISEC=EPSRM-EPS0
                  ER   =E*(A5+(1.D0-A5)*EXP(-A6*XISEC**2))
                  BT   =B*E/ER
                  B0   =BT
                ENDIF
C------------------------
              ELSE IF (DEPMEC.LT.0.D0) THEN
                EPS0=-(SIGRP+SY-EH*EPSY-E*EPSRP)/(E-EH)
                SIG0=EH*(EPS0+EPSY)-SY
C
C-----------CAS FLAMBAGE
                IF(ELAN.GT.5.0D0) THEN
                  FLBG=1.D0
                  XIPRIM=EPSRP-EPSRM
                  BC   =COA*(5.D0-ELAN)*EXP(XIPRIM*B*E/(SY-SIGINF))
                  B0   =BC
                  SIG0 =SIG0-GAS*B*E*(B-B0)/(1.D0-B0)
                ENDIF
C----------------------
              ENDIF
C
C---------------CALCUL DE EPSETP,R
C
              EPSETP= (EPSMEC-EPSRP)/(EPS0-EPSRP)
              XI    = (EPSRM-EPS0)/(EPS0-EPSRP)
              R     = R0-A1*XI/(A2+XI)
              SIGETP=B0*EPSETP+((1-B0)/(1+(EPSETP)**R)**(1/R))*EPSETP
              SIGP=(SIG0-SIGRP)* SIGETP+SIGRP
            ENDIF
          ELSE
C
C------CAS OU ON RESTE MONOTONE
C
            CYCL=0.D0
            IF (PALEL.LE.0.D0) THEN
              SIGP=SIGM+E*DEPMEC
            ELSE
              PLASTI=1
              IF (PALEC.LE.0.D0) THEN
                IF (SIGEL.GE.0.D0) THEN
                  SIGP=SY
                ELSE
                  SIGP=-SY
                ENDIF
              ELSE
                IF(PALSU.LE.0.D0) THEN
                  SIGP=SIGM + E *(DEPMEC)
                ELSE
                  IF (SIGEL.GE.0.D0) THEN
                    IF (ABS(EPSMEC).LT.ABS(EPSU)) THEN
                      SIGP=SU-(SU-SY)*((EPSU-EPSMEC)/(EPSU-EPSH))**4
                    ELSE
                      SIGP=SU
                    ENDIF
                  ELSE
                    IF (ABS(EPSMEC).LT.ABS(EPSU)) THEN
                      SIGP=-(SU-(SU-SY)*((EPSU+EPSMEC)/(EPSU-EPSH))**4)
                    ELSE
                      SIGP=-SU
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
C
C
C**********CYCLE*****************************************************

        ELSE IF(CYCL.GT.0.5D0) THEN
C
C---------------CALCUL DE EPSR,SIGR,EPS0,SIG0 A L'INSTANT DU CALCUL
C
C-------RECUPERATION DES COORD. EPSRP,SIGRP SI CHANGT DE DIRECTION
C
         IF (CHGDIR.LT.0.D0) THEN
           EPSRM=EPSRP
           EPSRP=EPSM
           SIGRP=SIGM
         ENDIF
C------------------
C
         IF (DEPMEC.GE.0.D0) THEN
           EPS0=-(SIGRP-SY+EH*EPSY-E*EPSRP)/(E-EH)
           SIG0=EH*(EPS0-EPSY)+SY
C
C----------CAS FLAMBAGE
           IF((ELAN.GT.5.0D0).AND.(FLBG.GT.0.5D0)) THEN
             XISEC=EPSRM-EPS0
             ER   =E*(A5+(1.D0-A5)*EXP(-A6*XISEC**2))
             EPS0=-(SIGRP-SY+EH*EPSY-ER*EPSRP)/(ER-EH)
             SIG0=EH*(EPS0-EPSY)+SY
             BT   =B*E/ER
             B0   =BT
           ENDIF
C----------------------
         ELSE IF (DEPMEC.LT.0.D0) THEN
           EPS0=-(SIGRP+SY-EH*EPSY-E*EPSRP)/(E-EH)
           SIG0=EH*(EPS0+EPSY)-SY
C
C---------CAS FLAMBAGE
           IF(ELAN.GT.5.0D0) THEN
             FLBG=1.D0
             XIPRIM=EPSRP-EPSRM
             BC   =COA*(5.D0-ELAN)*EXP(XIPRIM*B*E/(SY-SIGINF))
             B0   =BC
             SIG0 =SIG0-GAS*B*E*(B-B0)/(1.D0-B0)
           ENDIF
C--------------------
         ENDIF
C
C---------------CALCUL DE EPSETP,R
C
          EPSETP= (EPSMEC-EPSRP)/(EPS0-EPSRP)
          XI    = (EPSRM-EPS0)/(EPS0-EPSRP)
          R     = R0-A1*XI/(A2+XI)
          IF ( EPSETP.NE.0.D0) THEN
            SIGETP=B0*EPSETP+((1-B0)/(1+(EPSETP)**R)**(1/R))*EPSETP
            SIGP=(SIG0-SIGRP)* SIGETP+SIGRP
          ELSE
            SIGETP=0.D0
            SIGP=SIGRP
          ENDIF
C
        ENDIF
C
        VIP(1)=EPSRM
        VIP(2)=EPSRP
        VIP(3)=SIGRP
        VIP(4)=EPSMEC
        VIP(5)=DEPMEC
        VIP(6)=CYCL
        VIP(7)=PLASTI
        VIP(8)=FLBG
C
        IF (OPTION .EQ. 'FULL_MECA' ) THEN
C
C --- CALCUL DE LA PENTE
C
          SIGEPS=SIGP*(DEPMEC)
C
          IF (CYCL.LT.0.5D0) THEN
C
            IF((PLASTI.GT.0.5D0).AND.(SIGEPS.GE.0.D0)) THEN
              DSDE= (E*EH/(EH+E))
            ELSE
              DSDE=E
            ENDIF
C
          ELSEIF (CYCL.GT.0.5D0) THEN
            IF(ABS(EPSMEC).GT.ABS(EPS0)) THEN
              DSDE=E
            ELSE
              IF (EPSETP.NE.0.D0) THEN
                DSDE = (B0+(B0-1)*(1+EPSETP**R)**(-(1+1/R))*EPSETP**R
     &                 +(1-B0)/(1+EPSETP**R)**(1/R))
     &                 *(SIG0-SIGRP)/(EPS0-EPSRP)
              ELSE
                DSDE=(SIG0-SIGRP)/(EPS0-EPSRP)
              ENDIF
            ENDIF
          ENDIF

        ELSE IF (OPTION.EQ.'FULL_MECA_ELAS') THEN

          DSDE=E

        ENDIF
C
      ELSEIF (OPTION(1:14) .EQ. 'RIGI_MECA_TANG') THEN
C
C --- CALCUL DE LA PENTE
C
        IF (CYCL.LT.0.5D0) THEN
C
          IF (PLASTI.GT.0.5D0) THEN
            DSDE= (E*EH/(EH+E))
          ELSE
            DSDE=E
          ENDIF
C
        ELSE
          DSDE=E
        ENDIF

      ELSEIF (OPTION(1:14).EQ.'RIGI_MECA_ELAS') THEN

        DSDE=E

      ENDIF
C
C ----------------------------------------------------------------
C
      END
