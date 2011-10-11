      SUBROUTINE LCMMJA(COMP,TYPMOD,NMAT,MATERF,TIMED,TIMEF,
     &                  ITMAX, TOLER,NBCOMM,CPMONO,PGL,NFS,NSG,
     &                  TOUTMS,HSR,NR,NVI,VIND,DF,YF,YD,DY,DRDY,IRET)
      IMPLICIT NONE
C TOLE CRP_21
C ----------------------------------------------------------------------
C MODIF ALGORITH  DATE 10/10/2011   AUTEUR PROIX J-M.PROIX 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C       IN  COMP   :  NOM COMPORTEMENT
C           TYPMOD :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TIMED  :  ISTANT PRECEDENT
C           TIMEF  :  INSTANT ACTUEL
C           ITMAX  :  ITER_INTE_MAXI
C           TOLER  :  RESI_INTE_RELA
C           NBCOMM :  INCIDES DES COEF MATERIAU
C           CPMONO :  NOM DES COMPORTEMENTS
C           PGL    :  MATRICE DE PASSAGE
C           TOUTMS :  TENSEURS D'ORIENTATION
C           HSR    :  MATRICE D'INTERACTION
C           NR     :  DIMENSION DECLAREE DRDY
C           NVI    :  NOMBRE DE VARIABLES INTERNES
C           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
C           DF     :  Increment de Gradient de deformation 
C           YD     :  VARIABLES A T  
C           YF     :  VARIABLES A T + DT  
C           DY     :  SOLUTION           
C       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C           IRET   :  CODE RETOUR
C       ----------------------------------------------------------------
      INTEGER NMAT,NR,NBFSYS,NDT,NDI,NSFA,NSFV,NBSYS,IS,IR
      INTEGER NBCOMM(NMAT,3),IFA,I,J,K,L,IRET,IFL,ITMAX,NUVR,NUVS
      INTEGER NUECOU,IND(3,3),NVI,NFS,NSG
      REAL*8  VIND(*),YF(*),DY(*),DRDY(NR,NR),MATERF(NMAT*2)
      REAL*8  PGL(3,3),TOUTMS(NFS,NSG,6),HSR(NSG,NSG),GAMSNS(3,3)
      REAL*8  TIMED, TIMEF, MSDGDT(6,6),DT,FKOOH(6,6),SIGF(6)
      REAL*8  TOLER,DGSDTS,DKSDTS,DGRDBS,DKRDBS,TAUS,TAUR,MSNS(3,3)
      REAL*8  Q(3,3),MUS(6),NS(3),MS(3),MUR(6),DTODS(3,3)
      REAL*8  DFPDS(3,3,3,3),YD(*),MSNST(3,3,NSG),FP(3,3)
      REAL*8  MRNR(3,3),DF(3,3),FE(3,3)
      REAL*8  DFPDBS(3,3,NSG),DFPDGA(3,3,NSG)
      CHARACTER*16 NOMFAM,CPMONO(5*NMAT+1),COMP(*)
      CHARACTER*8     TYPMOD
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT , NDI
C     ----------------------------------------------------------------
      DATA IND/1,4,5,4,2,6,5,6,3/
C     ----------------------------------------------------------------

      IRET=0
      DT=TIMEF-TIMED
      
      CALL R8INIR ( NR*NR, 0.D0 , DRDY, 1 )
      CALL R8INIR ( 36, 0.D0 , MSDGDT, 1 )
      
      CALL LCEQVN(NDT,YF(1),SIGF)
      
C     Inverse de la matrice de Hooke      
      IF (MATERF(NMAT).EQ.0) THEN
         CALL LCOPIL  ( 'ISOTROPE' , TYPMOD , MATERF(1) , FKOOH )
      ELSEIF (MATERF(NMAT).EQ.1) THEN
         CALL LCOPIL  ( 'ORTHOTRO' , TYPMOD , MATERF(1) , FKOOH )
      ENDIF

      IF (COMP(3)(1:5).NE.'PETIT') THEN
         CALL R8INIR ( 81, 0.D0 , DFPDS, 1 )
         CALL R8INIR ( 3*3*NSG, 0.D0 , DFPDBS, 1 )
C        calcul de DFPDGA : dFp / dGamma_S pour tous les systemes S
         CALL LCMMJG(COMP,NMAT,NBCOMM,CPMONO,HSR,DT,NVI,VIND,YD,DY,
     &               ITMAX,TOLER,MATERF,SIGF,FKOOH,NFS,NSG,TOUTMS,PGL,
     &               MSNST,GAMSNS,DFPDGA,IRET)
      ENDIF
      
C     NSFA : debut de la famille IFA dans DY et YD, YF
      NSFA=6
C     NSFV : debut de la famille IFA dans les variables internes
      NSFV=6
C     LE NUMERO GLOBAL DU SYSTEME IS DANS Y EST NUVS
      NBFSYS=NBCOMM(NMAT,2)

      DO 6 IFA=1,NBFSYS
      
         NOMFAM=CPMONO(5*(IFA-1)+1)
         IFL=NBCOMM(IFA,1)
         NUECOU=NINT(MATERF(NMAT+IFL))

         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MUS,NS,MS,0,Q)

         DO 7 IS=1,NBSYS
         
C           calcul de Tau_s HPP ou GDEF

            CALL CALTAU(COMP,IFA,IS,SIGF,FKOOH,NFS,NSG,TOUTMS,
     &                  TAUS,MUS,MSNS)
            
            NUVS=NSFA+IS
            
C           CALCUL DES DERIVEES : 
C           DGSDTS=dGamma_S/dTau_S,  DKSDTS=dK_s/dTau_S, 
C           DGRDBS=dGamma_R/dBeta_S, DKRDBS=dK_S/dBeta_R

            CALL LCMMJB( TAUS,TAUS,MATERF,CPMONO,IFA,NMAT,NBCOMM,DT,
     &       NUECOU,NSFV,NSFA,IS,IS,NBSYS,NFS,NSG,HSR,VIND,DY,ITMAX,
     &            TOLER,DGSDTS,DKSDTS,DGRDBS,DKRDBS,IRET)
C           ici  DGRDBS,DKRDBS sont inutiles    
            IF (IRET.GT.0)  GOTO 9999

            IF (ABS(DGSDTS).GT.0.D0) THEN
C              Cas ou Delta-Gamma_S est non nul           
               IF (COMP(3)(1:5).EQ.'PETIT') THEN
C                 dR1/dS
                  DO 1002 I=1,6
                  DO 1002 J=1,6
                     MSDGDT(I,J)=MSDGDT(I,J)+MUS(I)*MUS(J)*DGSDTS
 1002             CONTINUE
C                 dR2/dS
                  DO 29 I=1,6
                     DRDY(NUVS,I)=-MUS(I)*DKSDTS
 29               CONTINUE
               ELSE
                  CALL CALDTO(SIGF,FKOOH,MSNS,DTODS)
C                 dR1/dS
                  DO 1003 I=1,3
                  DO 1003 J=1,3
                  DO 1003 K=1,3
                  DO 1003 L=1,3
                     DFPDS(I,J,K,L)=DFPDS(I,J,K,L)+
     &                              DFPDGA(I,J,IS)*DGSDTS*DTODS(K,L)
 1003             CONTINUE
C                 dR2/dS
                  DO 30 I=1,3
                  DO 30 J=1,3
                     DRDY(NUVS,IND(I,J))=-DKSDTS*DTODS(I,J)
 30               CONTINUE
 
               ENDIF
            ENDIF

C------------------------
C           calcul des ns termes dR1_i/dBeta_s
C           et     des ns termes dR2_r/dBeta_s
C------------------------
            DO 22 IR = 1, NBSYS
               CALL CALTAU(COMP,IFA,IR,SIGF,FKOOH,NFS,NSG,TOUTMS,
     &                     TAUR,MUR,MRNR)

               NUVR=NSFA+IR

               CALL LCMMJB(TAUR,TAUS,MATERF,CPMONO,IFA,NMAT,NBCOMM,DT,
     &             NUECOU,NSFV,NSFA,IR,IS,NBSYS,NFS,NSG,HSR,VIND,DY,
     &             ITMAX,TOLER,DGSDTS,DKSDTS,DGRDBS,DKRDBS,IRET)
C              ici DGSDTS,DKSDTS sont inutiles    
               IF (IRET.GT.0)  GOTO 9999
               
               IF (ABS(DGRDBS).GT.0.D0) THEN
                  IF (COMP(3)(1:5).EQ.'PETIT') THEN
C                    terme dR1/dAlpha_s
                     DO 193 I=1,6
                        DRDY(I,NUVS)=DRDY(I,NUVS)+MUR(I)*DGRDBS
 193                 CONTINUE
                  ELSE
                     DO 1006 I=1,3
                     DO 1006 J=1,3
                        DFPDBS(I,J,IS)=DFPDBS(I,J,IS)+
     &                                    DFPDGA(I,J,IR)*DGRDBS
 1006                CONTINUE
                  ENDIF
C                 terme dR2r/dGammas
                  DRDY(NUVR,NUVS)=-DKRDBS
               ENDIF
               
  22        CONTINUE
  
            DRDY(NUVS,NUVS)=DRDY(NUVS,NUVS)+1.D0
            
  7     CONTINUE
  
        NSFA=NSFA+NBSYS
        NSFV=NSFV+NBSYS*3

  6   CONTINUE

      IF (COMP(3)(1:5).NE.'PETIT') THEN
         CALL CALCFE(NR,NDT,VIND,DF,GAMSNS,FE,FP,IRET)
         CALL CALDFE(DF,NR,VIND,DFPDS,FE,DFPDBS,MSDGDT,DRDY)
      ENDIF
      
      CALL LCSOMA(MSDGDT, FKOOH, MSDGDT)
      CALL LCICMA(MSDGDT, 6,6,NDT,NDT,1,1,DRDY,NR,NR,1,1)
9999  CONTINUE
      END
