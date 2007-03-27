      SUBROUTINE NM1DAS(FAMI,KPG,KSP,E,ALPHA,SYC,SYT,ETC,ETT,CR,
     &                  TMOINS,TPLUS,
     &                  SIGM,DEPS,VIM,SIG,VIP,DSDEM,DSDEP)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
C ----------------------------------------------------------------------
C          PLASTICITE VON MISES ISOTROPE BILINEAIRE MONODIM
C    ECROUISSAGE ISOTROPE ASYMETRIQUE LINEAIRE - VON MISES-
C
C
C IN  FAMI     : FAMILLE DES POINTS DE GAUSS
C IN  KPG      : NUMERO DU POINT DE GAUSS
C IN  KSP      : NUMERO DU SOUS-POINT DE GAUSS
C IN  E        : MODULE D YOUNG
C IN  ALPHA    : COEF DILAT THERMIQUE
C       ETT    : ET EN TRACTION
C       ETC    : ET EN COMPRESSION
C       SYC    : LIMITE ELASTIQUE EN COMPRESSION
C       SYT    : LIMITE ELASTIQUE EN TRACTION
C       CR     : COEFFICIENT DE RESTAURATION. =0 POUR LE MOMENT
C IN  SIGM     : CONTRAINTE AU TEMPS MOINS
C IN  DEPS     : DEFORMATION  TOTALE PLUS - DEFORMATION TOTALE MOINS
C IN  VIM      : DEFORMATION  PLASTIQUE CUMULEE  AU TEMPS MOINS
C
C OUT SIG     : CONTRAINTES AU TEMPS PLUS
C OUT VIP    : DEFORMATION  PLASTIQUE CUMULEE TRACTION AU TEMPS PLUS
C OUT DSDEM   : DSIG/DEPS TEMPS MOINS
C OUT DSDEP   : DSIG/DEPS TEMPS PLUS
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL*8        E,ALPHA,SYC,SYT,ETC,ETT,CR,TMOINS,TPLUS
      REAL*8        SIGM,DEPS,PMT,PMC,XMT,XMC,XPT,XPC,VIM(4),VIP(4)
      REAL*8        SIG,PPT,PPC,DSDEM,DSDEP
      INTEGER       KPG,KSP
      CHARACTER*(*) FAMI
C     ------------------------------------------------------------------
C     VARIABLES LOCALES
C     ------------------------------------------------------------------
      REAL*8        RMC,RMT,SIGE,HT,HC,DEPMEC,DPT,RPT,DPC,RPC,SIGD,T,TM
      INTEGER       IRET

C     ------------------------------------------------------------------
C     VARIABLES INTERMEDIAIRES
C     ------------------------------------------------------------------
C
      HT   = E*ETT/(E-ETT)
      HC   = E*ETC/(E-ETC)
      PMT = VIM(1)
      XMT = VIM(2)
      PMC = VIM(3)
      XMC = VIM(4)
C     ------------------------------------------------------------------
C     DELTA DEFORMATION MECANIQUE
C     ------------------------------------------------------------------
C
      CALL RCVARC('F','TEMP','+',FAMI,KPG,KSP,T,IRET)
      CALL RCVARC('F','TEMP','-',FAMI,KPG,KSP,TM,IRET)


      DEPMEC = DEPS-ALPHA*(T-TM)
C     ------------------------------------------------------------------
C     FONCTIONS D'ECROUISSAGE AU TEMPS MOINS
C     ------------------------------------------------------------------
C
      RMT = HT*PMT+SYT
      RMC = HC*PMC+SYC

C     RIGI_MECA_TANG = MATRICE ELASTIQUE

      DSDEM = E
C     ------------------------------------------------------------------
C     ESTIMATION ELASTIQUE
C     ------------------------------------------------------------------
      SIGD=SIGM+E*DEPMEC
C     ------------------------------------------------------------------
C     CALCUL EPSP, P , SIG
C     ------------------------------------------------------------------

      IF (DEPMEC.GT.0.D0) THEN

C        CAS DE LA "TRACTION"

         SIGE = SIGD - XMT

         IF (SIGE.LT.RMT) THEN

C           ON RESTE ELASTIQUE

            PPT = PMT
            XPT = XMT
            PPC = PMC
            SIG = SIGD
CCC         XPC = CR * SIG
CJMP        XPC = XMC + CR * (SIG-XMC)
            XPC = SIG + (XMC-SIG)*EXP(-CR*(TPLUS-TMOINS))
            DSDEP = E
         ELSE

C           ON PLASTIFIE EN TRACTION

            DPT = (SIGE - RMT)/(E+HT)
            PPT = PMT + DPT
            PPC = PMC
            RPT = SYT + HT*PPT
            SIG = SIGE/(1.D0+E*DPT/RPT)+XMT
CCC         XPC = CR * SIG
CJMP        XPC = XMC + CR * (SIG-XMC)
            XPC = SIG + (XMC-SIG)*EXP(-CR*(TPLUS-TMOINS))
            XPT = XMT
            DSDEP = ETT

         ENDIF

      ELSEIF (DEPMEC.LT.0.D0) THEN

C        CAS DE LA "COMPRESSION"

         SIGE = SIGD - XMC

         IF (SIGE.GT.(-RMC)) THEN

C           ON RESTE ELASTIQUE

            PPT = PMT
            XPC = XMC
            PPC = PMC
            SIG = SIGD
CCC         XPT = CR * SIG
CJMP        XPT = XMT + CR * (SIG-XMT)
            XPT = SIG + (XMT-SIG)*EXP(-CR*(TPLUS-TMOINS))
            DSDEP = E
         ELSE

C           ON PLASTIFIE EN COMPRESSION

            DPC = (ABS(SIGE) - RMC)/(E+HC)
            PPT = PMT
            PPC = PMC + DPC
            RPC = SYC + HC*PPC
            SIG = SIGE/(1.D0+E*DPC/RPC)+XMC
CCC         XPT = CR * SIG
CJMP        XPT = XMT + CR * (SIG-XMT)
            XPT = SIG + (XMT-SIG)*EXP(-CR*(TPLUS-TMOINS))
            XPC = XMC
            DSDEP = ETC

         ENDIF

      ELSE

C           ON RESTE ELASTIQUE ET DEPS = 0

            PPT = PMT
            XPC = XMC
            PPC = PMC
            XPT = XMT
            SIG = SIGM
            DSDEP = E

      ENDIF
       VIP(1) = PPT
       VIP(2) = XPT
       VIP(3) = PPC
       VIP(4) = XPC

      END
