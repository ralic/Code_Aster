        SUBROUTINE LCMMJC( COEFT,IFA,NMAT,NBCOMM,
     &              IR,IS,NECRCI,DGAMMS,ALPHMR,DALPHA,SGNR,DALDGR)
        IMPLICIT NONE
        INTEGER IFA,NMAT,NBCOMM(NMAT,3),IR,IS
        REAL*8 COEFT(NMAT),DALDGR,DGAMMS,ALPHMR,SGNR,LCINE2,R8PREM
        CHARACTER*16 NECRCI
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/08/2007   AUTEUR PROIX J-M.PROIX 
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
C ======================================================================
C  CALCUL DES DERIVEES DES VARIABLES INTERNES DES LOIS MONOCRISTALLINES
C  POUR L'ECROUISSAGE CINEMATIQUE
C       IN  COEFT  :  PARAMETRES MATERIAU
C           IFA    :  NUMERO DE FAMILLE
C           IR     :  
C           NBCOMM :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C           IS     :  NUMERO DU SYSTEME DE GLISSEMENT EN COURS
C           IR     :  NUMERO DU SYSTEME DE GLISSEMENT POUR INTERACTION
C           NECRCI :  NOM DE LA LOI D'ECROUISSAGE CINEMATIQUE
C           DGAMMS :  ACCROISS. GLISSEMENT PLASTIQUE 
C           ALPHMR :  VAR. ECR. CIN. INST T
C           DALPHA :  DELTA ALPHA
C           SGNR   : DELTA P ACTUEL
C     OUT:
C           DALDGR : dAlpha/dGamma
C           
C     ----------------------------------------------------------------
      REAL*8 C,P,D,ALPHA,GAMMA,DP,GM,PM,CC,DALPHA
      INTEGER IEC,NUECIN
C     ----------------------------------------------------------------

      
      IEC=NBCOMM(IFA,2)
      NUECIN=COEFT(IEC)

C--------------------------------------------------------------------
C     POUR UN NOUVEL ECROUISSAGE CINEMATIQUE, AJOUTER UN BLOC IF 
C--------------------------------------------------------------------

C      IF (NECRCI.EQ.'ECRO_CINE1') THEN
      IF (NUECIN.EQ.1) THEN
C          D=COEFT(IEC-1+1)
          D=COEFT(IEC+1)
          DALDGR=0.D0
          IF (IS.EQ.IR) THEN
             DALDGR=(1.D0-D*ALPHMR*SGNR)/(1.D0+D*ABS(DGAMMS))**2
          ENDIF
      ENDIF
      
C      IF (NECRCI.EQ.'ECRO_CINE2') THEN
      IF (NUECIN.EQ.2) THEN
          DALDGR=0.D0
          IF (IS.EQ.IR) THEN
             IF (ABS(DGAMMS).GT.R8PREM()) THEN
                DALDGR=DALPHA/DGAMMS
             ELSE
                DALDGR=1.D0
             ENDIF
          ENDIF
      ENDIF
           
      END
