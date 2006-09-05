        SUBROUTINE LCMMRE ( MOD, NMAT, MATERD, MATERF,TEMPF,
     3      COMP,NBCOMM, CPMONO, PGL,TOUTMS,HSR, NR, NVI,VIND,
     1                    TIMED, TIMEF,YD ,  YF,   DEPS,   DY, R )
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/09/2006   AUTEUR JOUMANA J.EL-GHARIB 
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
C TOLE CRP_21
C       ----------------------------------------------------------------
C     MONOCRISTAL  : CALCUL DES TERMES DU SYSTEME NL A RESOUDRE = R(DY)
C                  DY =  DSIG DEPSVP (DALPHAS DGAMMAS DPS) PAR SYSTEME
C                  Y  = SIG EPSVP (ALPHAS GAMMAS PS) PAR SYSTEME
C                  R  = ( S    E  A   G  P    )
C  ATTENTION IL FAUT CALCULER -R
C
C     IN  MOD    :  TYPE DE MODELISATION
C         NMAT   :  DIMENSION MATER
C         MATERD :  COEFFICIENTS MATERIAU A T
C         MATERF :  COEFFICIENTS MATERIAU A T+DT
C         YD     :  VARIABLES A T       = ( SIGD VIND (EPSD3)  )
C         YF     :  VARIABLES A T + DT  = ( SIGF VINF (EPSF3)  )
C         DY     :  SOLUTION ESSAI      = ( DSIG DVIN (DEPS3) )
C         DEPS   :  INCREMENT DE DEFORMATION
C     OUT R      :  SYSTEME NL A T + DT
C         DY     :  NOuVELLE SOLUTION  = ( DSIG DVIN (DEPS3) )
C         YF     :  VARIABLES A T + DT  = ( SIGF VINF (EPSF3)  )
C     ----------------------------------------------------------------
      INTEGER         NDT , NDI , NMAT, NR, NVI
      INTEGER ITENS,NBFSYS,I,NUVI,IFA,NBSYS,IS,IV,IR
C
      REAL*8          DKOOH(6,6), FKOOH(6,6),TIMED, TIMEF
      REAL*8          SIGF(6)   , SIGD(6)
      REAL*8          DEPS(6)   , DEPSP(6),    DEPSE(6), DEVI(6),DT
      REAL*8          EPSED(6) , EPSEF(6), H1SIGF(6),VIND(*)
      REAL*8          MATERD(NMAT*2) ,MATERF(NMAT*2),TEMPF
      REAL*8          VIS(3),MS(6),TAUS,DGAMMA,DALPHA,DP,RP,SQ,PR
      REAL*8          R(NR),DY(NR),YD(NR),YF(NR)
      REAL*8          TOUTMS(5,24,6), HSR(5,24,24)
C
      CHARACTER*8     MOD
      
      INTEGER         NBCOMM(NMAT,3),NUMS,MONO1,NUV1,IEC,IEI,IFL,NSFA
      REAL*8          PGL(3,3),D,R0,Q,B,N,K,C,DGAMM1,ABSDGA,ALPHAM,H
      REAL*8          CRIT,ALPHAP
      CHARACTER*16    CPMONO(5*NMAT+1),COMP(*)
      CHARACTER*16 NOMFAM,NECOUL,NECRIS,NECRCI
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT , NDI
C     ----------------------------------------------------------------
C
      DT=TIMEF-TIMED
      CALL LCEQVN ( NDT , YF(1)       , SIGF)
      CALL LCEQVN ( 6 , DY(NDT+1)  ,  DEPSP  )
      CALL R8INIR(6, 0.D0, DEVI, 1)
      
      NBFSYS=NBCOMM(NMAT,2)
      MONO1=NBCOMM(NMAT,1)
            
      NUVI=NDT+6
      NUV1=NDT
      NUMS=0
C     NSFA : debut des variables internes de la famille IFA      
      NSFA=6
      
      DO 6 IFA=1,NBFSYS
      
         NOMFAM=CPMONO(5*(IFA-1)+1)
C         NMATER=CPMONO(5*(IFA-1)+2)
         NECOUL=CPMONO(5*(IFA-1)+3)
         NECRIS=CPMONO(5*(IFA-1)+4)
         NECRCI=CPMONO(5*(IFA-1)+5)
         IEC=NBCOMM(IFA,2)
         D=MATERF(NMAT+IEC)
         IEI=NBCOMM(IFA,3)
         R0=MATERF(NMAT+IEI-1+1)
         Q=MATERF(NMAT+IEI-1+2)
         B=MATERF(NMAT+IEI-1+3)
         IFL=NBCOMM(IFA,1)
         N=MATERF(NMAT+IFL-1+1)
         K=MATERF(NMAT+IFL-1+2)
         C=MATERF(NMAT+IFL-1+3)
         
         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS)
         
         DO 7 IS=1,NBSYS
            NUMS=NUMS+1
         
C           CALCUL DE LA SCISSION REDUITE =
C           PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
C           TAU      : SCISSION REDUITE TAU=SIG:MS
C            CALL LCMMSG(NOMFAM,NBSYS,IS,PGL,MS)
               DO 101 I=1,6
                  MS(I)=TOUTMS(IFA,IS,I)
 101            CONTINUE  
            
            TAUS=0.D0
            DO 10 I=1,6
               TAUS=TAUS+SIGF(I)*MS(I)
 10         CONTINUE
 
            IF (MONO1.EQ.1) THEN
               NUV1=NUV1+1
               DGAMM1=DY(NUV1)
               
C              CALCUL DE DALPHA

               ABSDGA=ABS(DGAMM1)
               ALPHAM=VIND(NUVI-6+1)
               DALPHA=(DGAMM1-D*ALPHAM*ABSDGA)/(1.0D0+D*ABSDGA)
               ALPHAP=ALPHAM+DALPHA
               
C              CALCUL DE R(P) 
               SQ=0.D0
                 DO 11 IR = 1, NBSYS 
                  PR= ABS(YF(NSFA+IR))
                  SQ = SQ + HSR(IFA,IS,IR)*(1.D0-EXP(-B*PR))
  11            CONTINUE                
                  RP=R0+Q*SQ
                  CRIT=ABS(TAUS-C*ALPHAP)-RP
               
               IF (CRIT.LE.0.D0) THEN
                  DP=0.D0
                  DGAMMA=0.D0
               ELSE
                  DP=DT*(CRIT/K)**N
                  DGAMMA=DP*(TAUS-C*ALPHAP)/ABS(TAUS-C*ALPHAP)
               ENDIF
               DO 19 ITENS=1,6
                  DEVI(ITENS)=DEVI(ITENS)+MS(ITENS)*DGAMMA
 19            CONTINUE
               R(NUV1)=-(DGAMM1-DGAMMA)
               NUVI=NUVI+3
               
            ELSE
            
C              VARIABLES INTERNES DU SYST GLIS
               DO 8 IV=1,3
                  NUVI=NUVI+1
                  VIS(IV)=YF(NUVI)
  8            CONTINUE
  
C
C              ECROUISSAGE ISOTROPE
C
               CALL LCMMEI(MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRIS,
     &                     NUMS,VIS,NVI,YF(NDT+1+7),RP,SQ)
C
C              ECOULEMENT VISCOPLASTIQUE
C              ROUTINE COMMUNE A L'IMPLICITE (PLASTI-LCPLNL)
C              ET L'EXPLICITE (NMVPRK-GERPAS-RK21CO-RDIF01)
C              CAS IMPLCITE : IL FAUT PRENDRE EN COMPTE DT
C              CAS EXPLICITE : IL NE LE FAUT PAS (C'EST FAIT PAR RDIF01)
C
               CALL LCMMFL(TAUS,MATERF(NMAT+1),IFA,NMAT,NBCOMM,
     &         NECOUL,RP,NUMS,VIS,NVI,YF(NDT+1),DT,DT,DGAMMA,DP,TEMPF)
C
C              ECROUISSAGE CINEMATIQUE
C
               CALL LCMMEC(TAUS,MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRCI,
     &                     VIS,DGAMMA,DP,DALPHA)
 
               DO 9 ITENS=1,6
                  DEVI(ITENS)=DEVI(ITENS)+MS(ITENS)*DGAMMA
  9            CONTINUE
 
               R(NUVI-2)=-(DY(NUVI-2)-DALPHA)
               R(NUVI-1)=-(DY(NUVI-1)-DGAMMA)
               R(NUVI  )=-(DY(NUVI  )-DP)
            
            ENDIF

  7     CONTINUE
  
        NSFA=NSFA+NBSYS
  
  6   CONTINUE

      IF (MONO1.NE.1) THEN
         CALL LCDIVE ( DEVI , DEPSP   , R(NDT+1) )
      ENDIF
      
      CALL LCEQVN ( NDT , YD(1)       , SIGD)
C
C                 -1                     -1
C -   HOOKF, HOOKD , DFDS , EPSEF = HOOKD  SIGD + DEPS - DEPSP
C
      IF (MATERF(NMAT).EQ.0) THEN
         CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERD(1) , DKOOH )
         CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERF(1) , FKOOH )
      ELSEIF (MATERF(NMAT).EQ.1) THEN
         CALL LCOPIL  ( 'ORTHOTRO' , MOD , MATERD(1) , DKOOH )
         CALL LCOPIL  ( 'ORTHOTRO' , MOD , MATERF(1) , FKOOH )
      ENDIF
      CALL LCPRMV ( DKOOH,   SIGD  , EPSED )
      
      IF (MONO1.NE.1) THEN
         CALL LCDIVE ( DEPS ,   DEPSP , DEPSE )
      ELSE
         CALL LCDIVE ( DEPS ,   DEVI , DEPSE )
      ENDIF
      
      CALL LCSOVE ( EPSED,   DEPSE , EPSEF )
C      TENTATIVE DE NORMALISATION DU SYSTEME : 
C     LA PREMIERE EQUATION EST  (HF-1)SIGF - (HD-1)SIGD - (DEPS-DEPSP)=0
      CALL LCPRMV ( FKOOH,   SIGF  , H1SIGF )
      CALL LCDIVE ( EPSEF,   H1SIGF  , R(1) )    

      END
