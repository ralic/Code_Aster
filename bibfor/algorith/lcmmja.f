      SUBROUTINE LCMMJA ( MOD, NMAT, MATERF,TIMED, TIMEF,
     3                     COMP,NBCOMM, CPMONO, PGL,NR,NVI,
     1                     YF,   DY,   DRDY)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/06/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C                    DY    = ( DSIG  DEPSP (DALPHA DGAMMA,DP) PAR SYST )
C                    Y     = ( SIG   EPSP ALPHA GAMMA P par syst. gliss)
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           YF     :  VARIABLES A T + DT =  ( SIGF X1F X2F PF (EPS3F) )
C           DY     :  SOLUTION           =  ( DSIG DX1 DX2 DP (DEPS3) )
C           NR   :  DIMENSION DECLAREE DRDY
C       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
C       ----------------------------------------------------------------
      INTEGER         NDT , NDI , NMAT , NR, NVI,NBFSYS
      INTEGER         NBCOMM(NMAT,3),NUMS,NUVI,IFA,NBSYS,IS,IV,I,J,NUMC
      INTEGER         NUML, NUMCO, NS
      REAL*8          UN  , ZERO 
      PARAMETER       ( UN   = 1.D0   )
      PARAMETER       ( ZERO = 0.D0   )
C     ALLOCATION DYNAMIQUE        
      REAL*8          SIGF(6),DDVIS(3,3),DDVIR(NVI),DRSDPR(NVI)
      REAL*8          UNIT(6,6), DHOOK(6,6)
      REAL*8          PGL(3,3),MS(6),VIS(3),TAUS,TIMED, TIMEF
      REAL*8          P,DP,YF(*),DY(*),DRDY(NR,NR)
      REAL*8          MATERF(NMAT*2), MMS(6), DT,RP,DADV(3)
      REAL*8          DVDTAU(3),DVI(3,3),DTAUDS(3,6)
      CHARACTER*16    CPMONO(5*NMAT+1),COMP(*)
      CHARACTER*16    NOMFAM,NMATER,NECOUL,NECRIS,NECRCI
      CHARACTER*8     MOD
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT , NDI
C     ----------------------------------------------------------------
      DATA  UNIT        /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     1                   ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     2                   ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     3                   ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     4                   ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     5                   ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/
C
      
      CALL R8INIR ( NR*NR, 0.D0 , DRDY, 1 )
C      
      DT=TIMEF-TIMED
C     NS=NOMBRE TOTAL DE SYSTEMES DE GLISSEMENT         
      NS=(NVI-1-6)/3
      CALL LCEQVN ( NDT , YF(1)       , SIGF )
C
      CALL LCOPIL  ( 'ISOTROPE' , MOD , MATERF(1) , DHOOK )
C
C - DSDS(T+DT)
C
      CALL LCICMA (DHOOK, 6,6,NDT,NDT,1,1,DRDY,NR,NR,1,1)
C  DHSIG/DEPSP
      CALL LCICMA (UNIT, 6,6,NDT,NDT,1,1,DRDY,NR,NR,1,NDT+1)
C  DEPSP/DEPSP
      CALL LCICMA (UNIT,6,6,NDT,NDT,1,1,DRDY,NR,NR,NDT+1,NDT+1)


      
      NBFSYS=NBCOMM(NMAT,2)
      
      NUMS=0
      NUVI=NDT+6
      
      DO 6 IFA=1,NBFSYS
      
         NOMFAM=CPMONO(5*(IFA-1)+1)
C         NMATER=CPMONO(5*(IFA-1)+2)
         NECOUL=CPMONO(5*(IFA-1)+3)
         NECRIS=CPMONO(5*(IFA-1)+4)
         NECRCI=CPMONO(5*(IFA-1)+5)
      
         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS)
         
         IF (NBSYS.EQ.0) CALL UTMESS('F','LCMMJA','NBSYS=0')

         DO 7 IS=1,NBSYS
         
C           VARIABLES INTERNES DU SYST GLIS
C           DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P
            DO 8 IV=1,3
               NUVI=NUVI+1
               VIS(IV)=YF(NUVI)
  8         CONTINUE
  
C           CALCUL DE LA SCISSION REDUITE =
C           PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
C           TAU      : SCISSION REDUITE TAU=SIG:MS
            CALL LCMMSG(NOMFAM,NBSYS,IS,PGL,MS)
            
            TAUS=0.D0
            DO 9 I=1,6
               TAUS=TAUS+SIGF(I)*MS(I)
               MMS(I)=-MS(I)
 9          CONTINUE
 
C           POSITION DE DEVP/DGAMMA
            NUMS=NUMS+1
            NUMC=NDT + 6 + 3*(NUMS-1)+1
            CALL LCICMA(MMS,6,1,6,1,1,1,DRDY,NR,NR,NDT+1,NUMC+1)

      
C           DR/DP SUIVANT LA LOI D'ECROUISSAGE ISOTROPE CHOISIE
            CALL LCMMJI( MATERF(NMAT+1),IFA,NMAT,NBCOMM,NECRIS,
     &                     NUMS,VIS,NVI,YF(NDT+1),DRSDPR,RP)
      
C           ECOULEMENT VISCOPLASTIQUE :   
C           CALCUL DE DVDTAU(3) ET DE DDVIS/DDVIS(3,3)
C           DANS DDVIS : 1,1 = DA/DALPHA, 1,2=DG/DALPHA, 1,3=DP/DALPHA
C           DANS DVDTAU : 1 = DALPHA/DTAU, 2=GAMMA, 3=P
            CALL LCMMJF( TAUS,MATERF(NMAT+1),IFA,NMAT,NBCOMM,DT,NECOUL,
     &                   NUMS,VIS,NVI,RP,DRSDPR,DVDTAU,DDVIS,DDVIR,DP)

C           DERIVEE DE L'EQUATION D'ECROUISSAGE CINEMATIQUE      
            CALL LCMMJC(MATERF(NMAT+1),IFA,NMAT,NBCOMM,DP,
     &                     NECRCI,VIS,DADV)
       
            DO 10 I=1,3
               DDVIS(1,I)=DADV(I)
 10         CONTINUE    

C           STOCKAGE DANS DRDY     
     
            CALL LCICMA(DDVIS,3,3,3,3,1,1,DRDY,NR,NR,NUMC,NUMC)
C           INTERACTION ENTRE SYSTEMES DE GLISSEMENT : D(P)/DPR  
            DO 13 I=1,NS
              IF (I.NE.NUMS) THEN
                 NUML = NUMC + 2
                 NUMCO= NDT + 6 + 3*(I-1)+3
                 CALL LCICMA(DDVIR(I),1,1,1,1,1,1,DRDY,NR,NR,NUML,NUMCO)
              ENDIF
 13         CONTINUE
 
C           MULTIPLICATION DE DTAUS/DS=MS PAR DP/DTAU, DG/DTAU,DA/DTAU
            DO 11 I=1,3
               DO 12 J=1,6
                  DTAUDS(I,J)=DVDTAU(I)*MS(J)
 12            CONTINUE
 11         CONTINUE
            CALL LCICMA(DTAUDS,3,6,3,6,1,1,DRDY,NR,NR,NUMC,1)
            
  7     CONTINUE
  
  6   CONTINUE
      END
