      SUBROUTINE INSVAR ( EPSD, DEPS, SIGD, DSIG, VIND,
     1  NVI , NMAT , MATERF , JFIS1 )
        IMPLICIT REAL*8 (A-H,O-Z)
C       -----------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
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
C       -----------------------------------------------------------
C       NADAI_B :
C                  INITIALISATION DES VARIABLES INTERNES (JFIS1 = 0)
C                  CORRECTION DES CONTRAINTES ET DEFORMATIONS
C                   DE CISAILLEMENT
C
C       IN  SIGD   :  CONTRAINTE A T
C           DSIG   :  INCREMENT DE CONTRAINTE ELASTIQUE
C           EPSD   :  DEFORMATION A T
C           DEPS   :  INCREMENT DE DEFORMATION TOTALE
C           VIND   :  VARIABLES INTERNES A T
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           JFIS1  :  INDICATEUR DE FISSURATION A T (0 = NON, 1 = OUI)
C       OUT
C           SIGD  DSIG     :  CORRIGES (SIG(3)= SIG(4) / SQRT(2))
C           EPSD EPSF DEPS :  CORRIGES ( EPS(3)=EPS(3) * 2 / SQRT(2) )
C           VIND   :  INITIALISATION VARIABLES INTERNES A T SI JFIS1=0
C       -----------------------------------------------------------
        INTEGER         NMAT , NVI , JFIS1 , ICC, IPPL
        REAL*8          SIGD(6) ,  DSIG(6) , EPSD(6) , DEPS(6)
        REAL*8          MATERF(NMAT,2), VIND(*), TETA, RTM1
        REAL*8          LTR, KRUPC, KRUPT, KPIC, RAC2 , E , EPSFT
        REAL*8          ALPHA, EPO1,EPO, LCS , NU , FTC , FLIM
        COMMON /DBETO/ POU1(4),LCS,ALPHA,E,NU,KRUPC,KRUPT,FTC,FLIM,
     1    EPO,EPO1,POU2(6),ICC,IPOU1(9)
C       ------------------------------------------------------------
C  AFFECTATION  DU COMMON DBETO
C
        RAC2 = SQRT(2.D0)
        E       = MATERF(1,1)
        NU      = MATERF(2,1)
        LCS     = MATERF(1,2)
        LTR     = MATERF(2,2)
        KPIC    = MATERF(4,2)
        KRUPC   = MATERF(5,2)
        KRUPT   = MATERF(6,2)
        ALPHA   = LTR/LCS
        EPO1    = KPIC + LCS / E
        ICC     = 3
C
       IF( JFIS1 .EQ. 0 ) THEN
C
        TETA=VIND(6)
        RTM1=VIND(14)
        DO 10  I = 3 , NVI-1
        VIND ( I ) = 0.D0
   10   CONTINUE
C
        VIND(7)  = E
        VIND(8)  = E
        IPPL  = 0
        CALL INSEFT ( EPSFT , LTR , IPPL )
        VIND(9)  = - EPSFT
        VIND(10) = - EPSFT
        VIND(11) = ABS( LTR / ( LTR / E - KRUPT ) )
        VIND(12) = VIND(11)
        VIND(13) = LTR
        VIND(6)  = TETA
        VIND(14) = RTM1
       ENDIF
C
C  CORRECTION DES DEFORMATIONS POUR INSFIS
       EPSD (3) = EPSD (4) * 2.D0 / RAC2
       DEPS (3) = DEPS (4) * 2.D0 / RAC2
C
C  CORRECTION DES CONTRAINTES POUR INSFIS
       SIGD (3) = SIGD (4) / RAC2
       DSIG (3) = DSIG (4) / RAC2
C
      END
