        SUBROUTINE LCMMRE ( MOD, NMAT, MATERD, MATERF,TEMPF,
     3             COMP,NBCOMM, CPMONO, PGL, NR, NVI, TIMED, TIMEF,
     1                      YD ,  YF,   DEPS,   DY,     R )
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/08/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C     ----------------------------------------------------------------
      INTEGER         NDT , NDI , NMAT, NR, NVI
      INTEGER ITENS,NBFSYS,I,NUVI,IFA,ICOMPO,NBSYS,IS,IV
C
      REAL*8          HOOKF(6,6), DKOOH(6,6), FKOOH(6,6),TIMED, TIMEF
      REAL*8          SIGF(6)   , DSIG(6) ,    SIGD(6) ,    DFDS(6)
      REAL*8          DEPS(6)   , DEPSP(6),    DEPSE(6), DEVI(6),DT
      REAL*8          EPSED(6)  , EPSEF(6), EPSP(6),H1SIGF(6)
      REAL*8          MATERD(NMAT*2) ,MATERF(NMAT*2),TEMPF
      REAL*8          VIS(3),MS(6),TAUS,DGAMMA,DALPHA,DP,RP
      REAL*8          R(NR),DY(NR),YD(NR),YF(NR),RBID,R8VIDE
C
      CHARACTER*8     MOD
      
      INTEGER         NBCOMM(NMAT,3),NUMS
      REAL*8          PGL(3,3)
      CHARACTER*16    CPMONO(5*NMAT+1),COMP(*)
      CHARACTER*16 NOMFAM,NMATER,NECOUL,NECRIS,NECRCI
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT , NDI
C     ----------------------------------------------------------------
C
      DT=TIMEF-TIMED
      CALL LCEQVN ( NDT , YF(1)       , SIGF)
      CALL LCEQVN ( NDT , YD(1)       , SIGD)
      CALL LCEQVN ( NDT , DY(1)       , DSIG )
      CALL LCEQVN ( 6 , YF(NDT+1)   , EPSP  )
      CALL LCEQVN ( 6 , DY(NDT+1)  ,  DEPSP  )
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
      CALL LCDIVE ( DEPS ,   DEPSP , DEPSE )
      CALL LCSOVE ( EPSED,   DEPSE , EPSEF )
C      TENTATIVE DE NORMALISATION DU SYSTEME : 
C     LA PREMIERE EQUATION EST  (HF-1)SIGF - (HD-1)SIGD - (DEPS-DEPSP)=0
      CALL LCPRMV ( FKOOH,   SIGF  , H1SIGF )
      CALL LCDIVE ( EPSEF,   H1SIGF  , R(1) )    

      CALL R8INIR(6, 0.D0, DEVI, 1)
      
      NBFSYS=NBCOMM(NMAT,2)
      
      NUVI=NDT+6
      NUMS=0
      
      DO 6 IFA=1,NBFSYS
      
         NOMFAM=CPMONO(5*(IFA-1)+1)
C         NMATER=CPMONO(5*(IFA-1)+2)
         NECOUL=CPMONO(5*(IFA-1)+3)
         NECRIS=CPMONO(5*(IFA-1)+4)
         NECRCI=CPMONO(5*(IFA-1)+5)
      
         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS)
         
         IF (NBSYS.EQ.0) CALL UTMESS('F','LCMMRE','NBSYS=0')
         
         DO 7 IS=1,NBSYS
            NUMS=NUMS+1
         
C           VARIABLES INTERNES DU SYST GLIS
            DO 8 IV=1,3
               NUVI=NUVI+1
               VIS(IV)=YF(NUVI)
  8         CONTINUE
  
C           CALCUL DE LA SCISSION REDUITE =
C           PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
C           TAU      : SCISSION REDUITE TAU=SIG:MS
            CALL LCMMSG(NOMFAM,NBSYS,IS,PGL,MS)
            
            TAUS=0.D0
            DO 10 I=1,6
               TAUS=TAUS+SIGF(I)*MS(I)
 10         CONTINUE
C
C           ECROUISSAGE ISOTROPE
C
            CALL LCMMEI(MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRIS,
     &                  NUMS,VIS,NVI,YF(NDT+1),RP)
C
C           ECOULEMENT VISCOPLASTIQUE
C           ROUTINE COMMUNE A L'IMPLICITE (PLASTI-LCPLNL)
C           ET L'EXPLICITE (NMVPRK-GERPAS-RK21CO-RDIF01)
C           CAS IMPLCITE : IL FAUT PRENDRE EN COMPTE DT
C           CAS EXPLICITE : IL NE LE FAUT PAS (C'EST FAIT PAR RDIF01)
C            
            CALL LCMMFL(TAUS,MATERF(NMAT+1),IFA,NMAT,NBCOMM,
     &      NECOUL,RP,NUMS,VIS,NVI,YF(NDT+1),DT,DT,DGAMMA,DP,TEMPF)
C
C           ECROUISSAGE CINEMATIQUE
C
            CALL LCMMEC(TAUS,MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRCI,
     &                  VIS,DGAMMA,DP,DALPHA)
            
            DO 9 ITENS=1,6
               DEVI(ITENS)=DEVI(ITENS)+MS(ITENS)*DGAMMA
  9         CONTINUE
  
            R(NUVI-2)=-(DY(NUVI-2)-DALPHA)
            R(NUVI-1)=-(DY(NUVI-1)-DGAMMA)
            R(NUVI  )=-(DY(NUVI  )-DP)

  7     CONTINUE
  
  6   CONTINUE

      CALL LCDIVE ( DEVI , DEPSP   , R(NDT+1) )

      END
