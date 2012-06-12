      SUBROUTINE RUPMAT (FAMI, KPG, KSP, IMAT,VIM, LGPG, E, SIGD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/06/2012   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =================================================================
      IMPLICIT     NONE
      INTEGER      KPG, KSP,IMAT, LGPG, CERR, I
      REAL*8       E,VIM(*),SIGD(6), COEF
      CHARACTER*(*)   FAMI
C =================================================================
C   IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C     KPG,KSP : NUMERO DU (SOUS)POINT DE GAUSS
C     IMAT    : ADRESSE DU MATERIAU CODE
C     VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C     LGPG  : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
C           CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
C   OUT E    :  MODULE D YOUNG DEGRADE PAR UN COEF DONNE MATERIAU
C     SIGD   :  CHAMPS DE CONTRAINTES DES ELE. ENDOMMAGES
C =================================================================
      IF (VIM (LGPG).LT.0.5D0) THEN
        GOTO 999
      ENDIF 
      
      CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ', 'CRIT_RUPT', 0,' ',
     &           0.D0, 1,'COEF',  COEF,  CERR, 1 )
C 
      E = E /COEF
C 
      DO 100 I =1,6
         SIGD(I)=0.D0
 100  CONTINUE
      
 999  CONTINUE
      END
