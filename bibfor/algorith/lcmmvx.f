      SUBROUTINE LCMMVX (  SIGF ,VIN, NMAT, MATERF,TEMPF,
     &                   COMP,NBCOMM, CPMONO, PGL, NR, NVI, SEUIL)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/11/2005   AUTEUR JOUMANA J.EL-GHARIB 
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
C     ----------------------------------------------------------------
C     MONOCRISTAL  :   CONVEXE ELASTO PLASTIQUE POUR (MATER,SIG,X1,X2,P)
C                   SEUIL  =MAX(SEUIL(S))
C     ----------------------------------------------------------------
C     IN  SIG    :  CONTRAINTE
C     IN  VIN    :  VARIABLES INTERNES = ( X1 X2 P )
C     IN  NMAT   :  DIMENSION MATER
C     IN  MATER  :  COEFFICIENTS MATERIAU A TEMP
C     OUT SEUIL  :  SEUIL  ELASTICITE
C     ----------------------------------------------------------------
      INTEGER         NDT , NDI , NMAT, NR, NVI
      INTEGER         ITENS,NBFSYS,I,NUVI,IFA,ICOMPO,NBSYS,IS,IV
C
      REAL*8          SIGF(6),VIN(NVI),RP,TEMPF
      REAL*8          MATERF(NMAT*2),SEUIL,DT,SQ
      REAL*8          VIS(3),MS(6),TAUS,DGAMMA,DALPHA,DP
C
      CHARACTER*8     MOD      
      INTEGER         NBCOMM(NMAT,3), NUMS
      REAL*8          PGL(3,3)
      CHARACTER*16    CPMONO(5*NMAT+1),COMP(*)
      CHARACTER*16 NOMFAM,NMATER,NECOUL,NECRIS
C
      NBFSYS=NBCOMM(NMAT,2)
      
      NUVI=6
      SEUIL=-1.D0
      DT=1.D0
      NUMS=0
      DO 6 IFA=1,NBFSYS
      
         NOMFAM=CPMONO(5*(IFA-1)+1)
         NECOUL=CPMONO(5*(IFA-1)+3)
         NECRIS=CPMONO(5*(IFA-1)+4)
      
         CALL LCMMSG(NOMFAM,NBSYS,0,PGL,MS)
         
         IF (NBSYS.EQ.0) CALL UTMESS('F','LCMMVX','NBSYS=0')
         
         DO 7 IS=1,NBSYS
            NUMS=NUMS+1
         
C           VARIABLES INTERNES DU SYST GLIS
            DO 8 IV=1,3
               NUVI=NUVI+1
               VIS(IV)=VIN(NUVI)
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
     &                  NUMS,VIS,NVI,VIN(7),RP,SQ)
C
C           ECOULEMENT VISCOPLASTIQUE
C            
            CALL LCMMFL(TAUS,MATERF(NMAT+1),IFA,NMAT,NBCOMM,
     &         NECOUL,RP,NUMS,VIS,NVI,VIN,DT,DT,DGAMMA,DP,TEMPF)

            IF (DP.GT.0.D0) THEN
            SEUIL=1.D0
            ENDIF
 7     CONTINUE
  
  6   CONTINUE
        END
