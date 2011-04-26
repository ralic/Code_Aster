      SUBROUTINE LCMMJ3 (TYPMOD, NMAT, MATERF,TIMED, TIMEF,
     &                   NBCOMM,CPMONO,PGL,TOUTMS,HSR,
     &                    NR,VIND,YF,DY,DRDY,IRET)
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
C RESPONSABLE PROIX J-M.PROIX
C       ----------------------------------------------------------------
C       MONOCRISTAL : CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
C                    DY    = ( DSIG + DGAMMA PAR SYST )
C                    Y     = ( SIG   GAMMA P par syst. gliss)
C
C       COMPORTEMENTS ISSUS DE LA DD : DD-CFC
C
C       INCONNUES : SIGMA, ALPHA=DENSITE DE DISLOCATION * B**2
C
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
      INTEGER         NBCOMM(NMAT,3),IFA,NBSYS,IS,I,IRET,J
      INTEGER         IR,NSFA,NUVR,NSFV

      REAL*8          VIND(*),TOUTMS(5,24,6),HSR(5,24,24),SIGF(6)
      REAL*8          FHOOK(6,6),Q(3,3),LG(3)
      REAL*8          PGL(3,3),MS(6),NG(3),TAUS,TIMED, TIMEF,MR(6)
      REAL*8          DPS,YF(*),DY(*),DRDY(NR,NR)
      REAL*8          MATERF(NMAT*2),DT,MSDGDT(6,6)
      REAL*8          SGNS,TAUR,SGNR,HS,HR,DPR,R8B
      REAL*8          DPDTAU,DPRDAS,DHRDAS
      CHARACTER*16    CPMONO(5*NMAT+1),NOMFAM
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

C              CALCUL de dF/dtau

            CALL LCMMJD( TAUS,MATERF,IFA,NMAT,NBCOMM,DT,
     &                   IS,IS,NBSYS,HSR,VIND(NSFV+1),DY(NSFA+1),
     &                   DPDTAU,R8B,R8B,HS,DPS,SGNS,IRET)
            IF (IRET.GT.0)  GOTO 9999
C------------------------
C              dR1/dS
C------------------------
            IF (DPDTAU.GT.0.D0) THEN
               DO 1002 I=1,6
               DO 1002 J=1,6
                 MSDGDT(I,J)=MSDGDT(I,J)+MS(I)*MS(J)*DPDTAU*SGNS
 1002          CONTINUE
            ENDIF
C------------------------
C              dR2/dS
C------------------------
            DO 29 I=1,6
               DRDY(NUVS,I)=-MS(I)*DPDTAU*HS
 29         CONTINUE

C------------------------
C           calcul des ns termes dR1/dalphas
C           et     des ns termes dR2/dalphas
C------------------------
            DO 22 IR = 1, NBSYS
               DO 102 I=1,6
                  MR(I)=TOUTMS(IFA,IR,I)
 102           CONTINUE
C              Calcul de TauR (scission reduite du systeme R)
               TAUR=0.D0
               DO 91 I=1,6
                  TAUR=TAUR+SIGF(I)*MR(I)
 91            CONTINUE
               NUVR=NSFA+IR
               CALL LCMMJD(TAUR,MATERF,IFA,NMAT,NBCOMM,DT,
     &                   IR,IS,NBSYS,HSR,VIND(NSFV+1),DY(NSFA+1),
     &                   R8B,DPRDAS,DHRDAS,HR,DPR,SGNR,IRET)
               IF (IRET.GT.0)  GOTO 9999
C------------------------
C              terme dR1/dAlpha_s
C------------------------
               DO 193 I=1,6
                  DRDY(I,NUVS)=DRDY(I,NUVS)+MR(I)*SGNR*DPRDAS
 193           CONTINUE
C------------------------
C              terme dR2r/dGammas
C------------------------
               DRDY(NUVR,NUVS)=-DPRDAS*HR-DPR*DHRDAS
  22        CONTINUE
               DRDY(NUVS,NUVS)=DRDY(NUVS,NUVS)+1.D0
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
