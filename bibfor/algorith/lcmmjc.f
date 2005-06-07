        SUBROUTINE LCMMJC( COEFT,IFA,NMAT,NBCOMM,DP,
     &                     NECRCI,VIS,DADV)
        IMPLICIT NONE
        INTEGER IFA,NMAT,NBCOMM(NMAT,3)
        REAL*8 COEFT(NMAT),VIS(3),DADV(3)
        CHARACTER*16 NECRCI
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
C ======================================================================
C  CALCUL DES DERIVEES DES VARIABLES INTERNES DES LOIS MONOCRISTALLINES
C  POUR L'ECROUISSAGE CINEMATIQUE
C       IN  COEFT   :  PARAMETRES MATERIAU
C           IFA :  NUMERO DE FAMILLE
C           NBCOMM :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C           NECRCI  :  NOM DE LA LOI D'ECROUISSAGE CINEMATIQUE
C           VIS : VARIABLES INTERNES DU SYSTEME DE GLISSEMENT COURANT
C           DP  : DELTA P ACTUEL
C     OUT:
C       DADV  :  DERIVEES DE L'EQUATION ECR. CIN. PAR RAPPORT A 
C                ALPHA, GAMMA ET P
C     ----------------------------------------------------------------
      REAL*8 C,P,D,ALPHA,GAMMA,DP,GM,PM,CC
      INTEGER IEC
C     ----------------------------------------------------------------

C     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P
C     DANS DADV : 1,1 = DA/DALPHA, 1,2=DG/DALPHA, 1,3=DP/DALPHA...

      CALL LCINVN(3, 0.D0, DADV)
      
      IEC=NBCOMM(IFA,2)
      ALPHA=VIS(1)

C--------------------------------------------------------------------
C     POUR UN NOUVEL ECROUISSAGE CINEMATIQUE, AJOUTER UN BLOC IF 
C--------------------------------------------------------------------

      IF (NECRCI.EQ.'ECRO_CINE1') THEN
          D=COEFT(IEC-1+1)
          DADV(1)=1.D0+D*DP
          DADV(2)=-1.D0
          DADV(3)=D*ALPHA
      ENDIF
      
      IF (NECRCI.EQ.'ECRO_CINE2') THEN
          D=COEFT(IEC-1+1)
          GM=COEFT(IEC-1+2)
          PM=COEFT(IEC-1+3)
          C=COEFT(IEC-1+4)
          CC=C*ALPHA
          IF(CC.NE.0.D0) THEN
            DADV(1)=1.D0+D*DP+
     &           PM*C/GM*((ABS(CC)/GM)**(PM-1))*ALPHA/ABS(ALPHA)
            DADV(2)=-1.D0
            DADV(3)=D*ALPHA
          ELSE
            DADV(1)=1.D0+D*DP
            DADV(2)=-1.D0
            DADV(3)=D*ALPHA
          ENDIF
      ENDIF
           
      END
