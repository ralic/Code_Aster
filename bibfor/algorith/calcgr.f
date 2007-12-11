      SUBROUTINE CALCGR(IGAU,NBSIG,NBVARI,VIP,NU,EPSFL)
      IMPLICIT   NONE
      INTEGER NBSIG,IGAU,NBVARI
       REAL*8 NU,EPSFL(NBSIG),VIP(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/10/2006   AUTEUR SMICHEL S.MICHEL-PONNELLE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

C ======================================================================
C   BUT : CALCUL DES DEFORMATIONS DE FLUAGE DE GRANGER EN 
C         UN POINT DE GAUSS
C         ROUTINE APPELEE POUR LE POST-TRAITEMENT
C----------------------------------------------------------------------
      INTEGER I,K
      REAL * 8 EPSTMP(6)
      
      
      DO 100 K = 1,NBSIG
            EPSTMP(K) = VIP((IGAU-1)*NBVARI + 8*NBSIG+K)
            DO 90 I = 1,8
              EPSTMP(K) = EPSTMP(K) - VIP((IGAU-1)*NBVARI+
     &                   (I-1)*NBSIG+K)
   90       CONTINUE
  100     CONTINUE


           EPSFL(1) = EPSTMP(1) -
     &                      NU* (EPSTMP(2) +  EPSTMP( 3))
           EPSFL(2) = EPSTMP(2) -
     &                      NU* (EPSTMP(1) +  EPSTMP( 3))
           EPSFL(3) = EPSTMP(3) -
     &                      NU* (EPSTMP(1) +  EPSTMP( 2))
          DO 110 I = 4,NBSIG
            EPSFL(I) = (1.D0 + NU) *EPSTMP(I)
  110     CONTINUE
  
  
      END
