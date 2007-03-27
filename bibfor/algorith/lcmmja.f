      SUBROUTINE LCMMJA (FAMI,KPG,KSP,MOD, NMAT, MATERF,TIMED, TIMEF,
     &   ITMAX,TOLER,   COMP,NBCOMM, CPMONO, PGL,TOUTMS,HSR,
     &                     NR,NVI,VIND,YF,   DY,   DRDY,IRET)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C TOLE CRP_21
C MODIF ALGORITH  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE JMBHH01 J.M.PROIX
C       ----------------------------------------------------------------
C       MONOCRISTAL : CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
C                    DY    = ( DSIG + DGAMMA PAR SYST )
C                    Y     = ( SIG   GAMMA P par syst. gliss)
C       IN  FAMI   :  FAMILLE DE POINTS DE GAUSS
C       IN  KPG    :  NUMERO DU POINT DE GAUSS
C       IN  KSP    :  NUMERO DU SOUS-POINT DE GAUSS
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TIMED  :  ISTANT PRECEDENT
C           TIMEF  :  INSTANT ACTUEL
C           ITMAX  :  ITER_INTE_MAXI
C           TOLER  :  RESI_INTE_RELA
C           COMP   :  NOM COMPORTEMENT
C           NBCOMM :  INCIDES DES COEF MATERIAU
C           CPMONO :  NOM DES COMPORTEMENTS
C           PGL    :  MATRICE DE PASSAGE
C           TOUTMS :  TENSEURS D'ORIENTATION
C           HSR    :  MATRICE D'INTERACTION
C           NVI    :  NOMBRE DE VARIABLES INTERNES
C           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
C           YF     :  VARIABLES A T + DT =  ( SIGF X1F X2F PF (EPS3F) )
C           DY     :  SOLUTION           =  ( DSIG DX1 DX2 DP (DEPS3) )
C           NR     :  DIMENSION DECLAREE DRDY
C       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C       OUT IRET   :  CODE RETOUR
C       ----------------------------------------------------------------
      INTEGER         NDT , NDI , NMAT , NR, NVI, NBFSYS, NUVS, IEXP
      INTEGER         NBCOMM(NMAT,3),NUVI,IFA,NBSYS,IS,IV,I,J,NUMC,IRET
      INTEGER         NUML, NUMCO,IEI,IFL,IEC,IR,NSFA,NUVR,NSFV,ITMAX
      INTEGER         KPG,KSP
      REAL*8          UN  , ZERO , VIND(*),DFDTAU
      REAL*8          TOUTMS(5,24,6),HSR(5,24,24),SQ,PR
C     ALLOCATION DYNAMIQUE
      REAL*8          SIGF(6),DDVIS(3,3),DDVIR(NVI),DRSDPR(NVI)
      REAL*8          FHOOK(6,6),CRITR,DGAMM2,DP2,DALPH2
      REAL*8          EXPBP(24)
      REAL*8          PGL(3,3),MS(6),VIS(3),TAUS,TIMED, TIMEF
      REAL*8          P,DP,YF(*),DY(*),DRDY(NR,NR),SMSMS(6,6),DFDGA
      REAL*8          MATERF(NMAT*2), DT,RP,DADV(3),DFDTAR,DFDALR,DFDRR
      REAL*8          DVDTAU(3),DTAUDS(3,6),MSMS(6,6),DRRDPS,DELTSR
      REAL*8          MSDGDT(6,6),D,R0,Q,B,N,K,C,DGAMMS,ABSDGA,H,RR
      REAL*8          ALPHAM,DALPHA,ALPHAP,CRIT,DALDGA,DGAMMA,DALPHR
      REAL*8          DRDGA,SGNS,TAUR,SGNR,ALPHAR,GAMMAP,PM,GAMMAR
      REAL*8          DAR,DGR,ARM,DFDAL,DFDR,DGAMMR,ALPHMR,PS,TOLER
      CHARACTER*(*)   FAMI
      CHARACTER*16    CPMONO(5*NMAT+1),COMP(*)
      CHARACTER*16    NOMFAM,NECOUL,NECRIS,NECRCI
      CHARACTER*8     MOD
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT , NDI
C     ----------------------------------------------------------------
      IRET=0
      CALL R8INIR ( NR*NR, 0.D0 , DRDY, 1 )
      CALL R8INIR ( 36, 0.D0 , MSDGDT, 1 )
      DT=TIMEF-TIMED
      CALL LCEQVN ( NDT , YF(1)       , SIGF )
      NBFSYS=NBCOMM(NMAT,2)

C     NSFA : debut de la famille IFA dans DY et YD, YF
      NSFA=6
C     NSFV : debut de la famille IFA dans les variables internes
      NSFV=6
C     LE NUMERO GLOBAL DU SYSTEME IS DANS Y EST NUVS

      DO 6 IFA=1,NBFSYS

         NOMFAM=CPMONO(5*(IFA-1)+1)
C         NMATER=CPMONO(5*(IFA-1)+2)
         NECOUL=CPMONO(5*(IFA-1)+3)
         NECRIS=CPMONO(5*(IFA-1)+4)
         NECRCI=CPMONO(5*(IFA-1)+5)

         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS)

         IF (NBSYS.EQ.0) CALL U2MESS('F','ALGORITH_70')

         DO 7 IS=1,NBSYS

C           CALCUL DE LA SCISSION REDUITE =
C           PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
C           TAU      : SCISSION REDUITE TAU=SIG:MS
            DO 101 I=1,6
               MS(I)=TOUTMS(IFA,IS,I)
 101        CONTINUE
            TAUS=0.D0
            DO 9 I=1,6
               TAUS=TAUS+SIGF(I)*MS(I)
 9          CONTINUE

            NUVS=NSFA+IS
C              DGAMMS = DeltaGamma(Is)
            DGAMMS=DY(NUVS)

C           CALCUL DE DALPHA
            ABSDGA=ABS(DGAMMS)
            NUVI=NSFV+3*(IS-1)
            ALPHAM=VIND(NUVI+1)
            GAMMAP=VIND(NUVI+2)+DGAMMS
            PM=VIND(NUVI+3)

            IF(NECOUL.NE.'KOCKS_RAUCH') THEN

            CALL LCMMFC( MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRCI,
     &       ITMAX,TOLER, ALPHAM,DGAMMS,DALPHA,IRET )
            ALPHAP=ALPHAM+DALPHA

C           CALCUL DE R(P) : RP=R0+Q*(1.D0-EXP(-B*P))
            IEXP=0
            IF (IS.EQ.1) IEXP=1

            CALL LCMMFI(MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRIS,
     &           IS,NBSYS,VIND(NSFV+1),DY(NSFA+1),HSR,IEXP,EXPBP,RP)
            ENDIF
            CALL LCMMFE( FAMI,KPG,KSP,TAUS,MATERF(NMAT+1),MATERF(1),IFA,
     &                  NMAT,NBCOMM,NECOUL,IS,NBSYS,VIND(NSFV+1),
     &      DY(NSFA+1),RP,ALPHAP,GAMMAP,DT,DALPHA,DGAMMA,DP,CRIT,SGNS,
     &      HSR,IRET)
            GAMMAP=VIND(NUVI+2)+DGAMMA

            IF(NECOUL.EQ.'KOCKS_RAUCH') THEN
            ALPHAP=ALPHAM+DALPHA
            ENDIF

            IF (CRIT.GT.0.D0) THEN
C
C              CALCUL de dF/dR, dF/dalpha, dF/dtau
               CALL LCMMJF( FAMI,KPG,KSP,TAUS,MATERF(NMAT+1),MATERF(1),
     &               IFA,NMAT,NBCOMM,DT,NECOUL,IS,0,NBSYS,VIND(NSFV+1),
     &               DY(NSFA+1),HSR,RP,ALPHAP,DALPHA,GAMMAP,
     &               DGAMMA,SGNR,DFDTAU,DFDAL,DFDR)

C              dR1/dS
               CALL LCPRTE(MS,MS,MSMS)
               CALL LCPRSM(DFDTAU, MSMS, SMSMS)
               CALL LCSOMA(MSDGDT, SMSMS, MSDGDT)

C              dR2/dS
               DO 29 I=1,6
                  DRDY(NUVS,I)=-MS(I)*DFDTAU
 29            CONTINUE

               DO 22 IR = 1, NBSYS
C                 Calcul de TauR (scission reduite du systeme R)
                  TAUR=0.D0
                  DO 91 I=1,6
                     TAUR=TAUR+SIGF(I)*TOUTMS(IFA,IR,I)
 91               CONTINUE
                  NUVR=NSFA+IR
                  DGAMMR=DY(NUVR)
                  ALPHMR=VIND(NUVR-6+1)

                  IF(NECOUL.NE.'KOCKS_RAUCH') THEN

                  CALL LCMMFC( MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRCI,
     &            ITMAX,TOLER,ALPHAM,DGAMMS,DALPHA,IRET )
                  ALPHAR=ALPHMR+DALPHA

C                 CALCUL DE R(P)
                  CALL LCMMFI(MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRIS,
     &             IR,NBSYS,VIND(NSFV+1),DY(NSFA+1),HSR,IEXP,EXPBP,RR)

                  ENDIF

                  GAMMAR=VIND(NUVR-6+2)+DGAMMR

                  CALL LCMMFE(FAMI,KPG,KSP,TAUS,MATERF(NMAT+1),
     &                    MATERF(1),IFA,NMAT,NBCOMM,NECOUL,IS,NBSYS,
     &                    VIND(NSFV+1),DY(NSFA+1),RR,ALPHAR,GAMMAR,DT,
     &                    DALPH2,DGAMM2,DP2,CRITR,SGNR,HSR,IRET)
                  GAMMAR=VIND(NUVR-6+2)+DGAMM2

                  IF(NECOUL.EQ.'KOCKS_RAUCH') THEN
                  ALPHAR=ALPHMR+DALPH2
                  ENDIF

C                 CALCUL de dF/dRr, dF/dalphar, dF/dtau
                  CALL LCMMJF( FAMI,KPG,KSP,TAUR,MATERF(NMAT+1),
     &               MATERF(1),IFA,NMAT,NBCOMM,DT,NECOUL,IS,IR,NBSYS,
     &               VIND(NSFV+1),DY(NSFA+1),HSR,RR,ALPHAR,DALPHR,
     &               GAMMAR,DGAMM2,SGNR,DFDTAR,DFDALR,DFDRR)

C                 CALCUL DE dRr/dps
                  PS=PM+ABSDGA

                  IF(NECOUL.NE.'KOCKS_RAUCH') THEN

                  CALL LCMMJI( MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRIS,
     &                HSR,IS,IR,PS,DRRDPS)

C                 CALCUL DE DALPHAr/dGAMMAs
                  CALL LCMMJC(MATERF(NMAT+1),IFA,NMAT,NBCOMM,
     &                    IR,IS,NECRCI,DGAMMS,ALPHMR,DALPHA,SGNR,DALDGA)

C                 terme dR1/dGammaS
                  DFDGA=DFDALR*DALDGA+DFDRR*DRRDPS*SGNS
                  ELSE
                  DFDGA=DFDALR
                  ENDIF

                  DO 191 I=1,6
                     DRDY(I,NUVS)=DRDY(I,NUVS)+TOUTMS(IFA,IR,I)*DFDGA
 191              CONTINUE
C                 terme dR2r/dGammas
                  DELTSR=0.D0
                  IF (IR.EQ.IS) DELTSR=1.D0
                  DRDY(NUVR,NUVS)=DELTSR-DFDGA

  22           CONTINUE

            ELSE
               DRDY(NUVS,NUVS)=1.0D0
            ENDIF

  7     CONTINUE
        NSFA=NSFA+NBSYS
        NSFV=NSFV+NBSYS*3

  6   CONTINUE

      IF (MATERF(NMAT).EQ.0) THEN
         CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERF(1) , FHOOK )
      ELSEIF (MATERF(NMAT).EQ.1) THEN
         CALL LCOPIL  ( 'ORTHOTRO' , MOD , MATERF(1) , FHOOK )
      ENDIF

      CALL LCSOMA(MSDGDT, FHOOK, MSDGDT)
      CALL LCICMA (MSDGDT, 6,6,NDT,NDT,1,1,DRDY,NR,NR,1,1)

      END
