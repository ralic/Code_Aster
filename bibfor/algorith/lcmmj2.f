      SUBROUTINE LCMMJ2 (TYPMOD, NMAT, MATERF,TIMED, TIMEF,
     &                   NBCOMM,CPMONO,PGL,TOUTMS,HSR,
     &                   NR,VIND,YF,DY,DRDY,IRET)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE PROIX J.M.PROIX
C TOLE CRS_1404
C       ----------------------------------------------------------------
C       MONOCRISTAL : CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
C                    DY    = ( DSIG + DGAMMA PAR SYST )
C                    Y     = ( SIG   GAMMA P par syst. gliss)
C       IN  TYPMOD    :  TYPE DE MODELISATION
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
      INTEGER         NDT , NDI , NMAT , NR, NBFSYS, NUVS
      INTEGER         NBCOMM(NMAT,3),NUVI,IFA,NBSYS,IS,I,IRET
      INTEGER         IR,NSFA,NUVR,NSFV
      REAL*8            VIND(*),DGDTAU
      REAL*8          TOUTMS(5,24,6),HSR(5,24,24)
C     ALLOCATION DYNAMIQUE
      REAL*8          SIGF(6)
      REAL*8          FHOOK(6,6)
      REAL*8          DHDALR,HS,Q(3,3),LG(3)
      REAL*8          PGL(3,3),MS(6),NG(3),TAUS,TIMED, TIMEF
      REAL*8          DP,YF(*),DY(*),DRDY(NR,NR),SMSMS(6,6),DFDGA
      REAL*8          MATERF(NMAT*2), DT,RP,DFDTAR,DGDALR,DFDRR
      REAL*8          MSMS(6,6),DELTSR
      REAL*8          MSDGDT(6,6),RR
      REAL*8          ALPHAM,DALPHA,ALPHAP,CRIT,DGAMMA
      REAL*8          SGNS,TAUR,ALPHAR,GAMMAP,GAMMAR
      REAL*8          DGDAL,DFDR,ALPHMR
      CHARACTER*16    CPMONO(5*NMAT+1)
      CHARACTER*16    NOMFAM,NECOUL
      CHARACTER*8     TYPMOD
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
         NECOUL=CPMONO(5*(IFA-1)+3)

         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS,NG,LG,0,Q)
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
            NUVI=NSFV+3*(IS-1)
            ALPHAM=VIND(NUVI+1)
            GAMMAP=VIND(NUVI+2)
            ALPHAP=ALPHAM+DY(NUVS)
            CALL LCMMFE(TAUS,MATERF(NMAT+1),MATERF(1),IFA,
     &           NMAT,NBCOMM,NECOUL,IS,NBSYS,VIND(NSFV+1),
     &           DY(NSFA+1),RP,ALPHAP,GAMMAP,DT,DALPHA,DGAMMA,DP,CRIT,
     &           SGNS,HSR,IRET)
               IF (IRET.GT.0)  GOTO 9999
            IF (CRIT.GT.0.D0) THEN
C              CALCUL de dF/dtau
               CALL LCMMJF( TAUS,MATERF(NMAT+1),MATERF(1),
     &               IFA,NMAT,NBCOMM,DT,NECOUL,IS,0,NBSYS,VIND(NSFV+1),
     &               DY(NSFA+1),HSR,RP,ALPHAP,DALPHA,GAMMAP,
     &               DGAMMA,SGNS,DGDTAU,DGDAL,DFDR,IRET)
               IF (IRET.GT.0)  GOTO 9999
C------------------------
C              dR1/dS
C------------------------
               CALL LCPRTE(MS,MS,MSMS)
               CALL LCPRSM(DGDTAU, MSMS, SMSMS)
               CALL LCSOMA(MSDGDT, SMSMS, MSDGDT)
C------------------------
C              dR2/dS
C------------------------
               DO 29 I=1,6
                     HS=DFDR
                     DRDY(NUVS,I)=-MS(I)*ABS(DGDTAU)*HS
 29            CONTINUE
C------------------------
C                 terme dR1/dGammaS
C------------------------
                  DFDGA=DGDAL*SGNS
                  DO 193 I=1,6
                     DRDY(I,NUVS)=DRDY(I,NUVS)+TOUTMS(IFA,IS,I)*DFDGA
 193              CONTINUE
C------------------------ calc de dF/dRs, dF/dalphas,
C             calcul des ns termes dR1/dGammaS (ou dR1/dalphas pour KR)
C             et     des ns termes dR2/dGammaS (ou dR2/dalphas pour KR)
C------------------------
               DO 22 IR = 1, NBSYS
C                 Calcul de TauR (scission reduite du systeme R)
                  TAUR=0.D0
                  DO 91 I=1,6
                     TAUR=TAUR+SIGF(I)*TOUTMS(IFA,IR,I)
 91               CONTINUE
                  NUVR=NSFA+IR
                  ALPHMR=VIND(NUVR-6+1)
                  ALPHAR=ALPHMR+DALPHA
                  GAMMAR=VIND(NUVR-6+2)
C                 CALCUL de dF/dRr, dF/dalphar, dF/dtau
                  CALL LCMMJF( TAUR,MATERF(NMAT+1),
     &               MATERF(1),IFA,NMAT,NBCOMM,DT,NECOUL,IS,IR,NBSYS,
     &               VIND(NSFV+1),DY(NSFA+1),HSR,RR,ALPHAR,DALPHA,
     &               GAMMAR,DGAMMA,SGNS,DFDTAR,DGDALR,DFDRR,IRET)
                  IF (IRET.GT.0)  GOTO 9999
C------------------------
C                 terme dR2r/dGammas
C------------------------
                  DELTSR=0.D0
                  IF (IR.EQ.IS) DELTSR=1.D0
                     DHDALR=DFDRR
                     DRDY(NUVR,NUVS)=DELTSR-DGDALR*HS-DP*DHDALR
  22           CONTINUE

            ELSE
               DRDY(NUVS,NUVS)=1.0D0
            ENDIF

  7     CONTINUE
        NSFA=NSFA+NBSYS
        NSFV=NSFV+NBSYS*3

  6   CONTINUE

      IF (MATERF(NMAT).EQ.0) THEN
         CALL LCOPIL  ( 'ISOTROPE' , TYPMOD , MATERF(1) , FHOOK )
      ELSEIF (MATERF(NMAT).EQ.1) THEN
         CALL LCOPIL  ( 'ORTHOTRO' , TYPMOD , MATERF(1) , FHOOK )
      ENDIF

      CALL LCSOMA(MSDGDT, FHOOK, MSDGDT)
      CALL LCICMA (MSDGDT, 6,6,NDT,NDT,1,1,DRDY,NR,NR,1,1)
9999  CONTINUE
      END
