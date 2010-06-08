      SUBROUTINE INICAL(NBIN  ,LPAIN ,LCHIN ,
     &                  NBOUT ,LPAOUT,LCHOUT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 30/04/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER       NBIN,NBOUT
      CHARACTER*8   LPAOUT(NBOUT),LPAIN(NBIN)
      CHARACTER*19  LCHIN(NBIN),LCHOUT(NBOUT) 
C      
C ----------------------------------------------------------------------
C
C ROUTINE UTILITAIRE POUR CALCUL
C
C INITIALISATIONS DES CHAMPS IN/OUT POUR CALCUL
C
C ----------------------------------------------------------------------
C
C
C IN  NBIN   : NOMBRE MAXI DE CHAMPS IN POUR CALCUL
C IN  LPAIN  : NOM DES TYPES DE CHAMP D'ENTREE
C IN  LCHIN  : NOM DES CHAMPS D'ENTREE
C IN  NBOUT  : NOMBRE MAXI DE CHAMPS OUT POUR CALCUL
C IN  LPAOUT : NOM DES TYPES DE CHAMP DE SORTIE
C IN  LCHOUT : NOM DES CHAMPS DE SORTIE
C
C ----------------------------------------------------------------------
C 
      INTEGER      ICH
      CHARACTER*8  K8BLA
      CHARACTER*19 K19BLA      
C
C ---------------------------------------------------------------------
C
      K8BLA  = ' '
      K19BLA = ' '
C      
      DO 100 ICH = 1,NBIN
        LCHIN(ICH)  = K19BLA
        LPAIN(ICH)  = K8BLA
  100 CONTINUE
C    
      DO 200 ICH = 1,NBOUT
        LCHOUT(ICH) = K19BLA
        LPAOUT(ICH) = K8BLA
  200 CONTINUE  
      END
