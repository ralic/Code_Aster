      SUBROUTINE DVD2RT(NPT,ETA,KSI,DFD2DE,DFD2DK,DFDEDK)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/01/2001   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C-----------------------------------------------------------------------
C 
C    - FONCTION REALISEE:  CALCUL DU HESSIEN DES FONCTIONS DE FORME 
C      POUR UN TRIANGLE DE REFERENCE AUX POINTS DE GAUSS
C
C    - ARGUMENTS:
C        DONNEES:          NPT      -->  NOMBRE DE POINTS DU QUADRANGLE
C                          ETA,KSI  -->  COORDONNEES DU POINT
C
C        RESULTATS:
C                          DFD2DE   <--  DERIVEE SECONDE / ETA
C                          DFD2DK   <--  DERIVEE SECONDE / KSI
C                          DFDEDK   <--  DERIVEE SECONDE / ETA ET KSI
C
C
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       AUCUNE.
C     FONCTIONS INTRINSEQUES:
C       AUCUNE.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       11/12/00 (OB): CREATION AVEC (NTPT.EQ.6).
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS 
      INTEGER  NPT
      REAL*8   ETA,KSI,DFD2DE(*),DFD2DK(*),DFDEDK(*)
      
C AFFECTATION DES FONCTIONS DE FORME ET DE LEURS DERIVEES
      IF(NPT.EQ.6) THEN
         DFD2DE(1)= 0.D0
         DFD2DE(2)= 1.D0
         DFD2DE(3)= 1.D0
         DFD2DE(4)= 0.D0
         DFD2DE(5)= -2.D0
         DFD2DE(6)= 0.D0
         DFD2DK(1)= 1.D0
         DFD2DK(2)= 1.D0
         DFD2DK(3)= 0.D0
         DFD2DK(4)= -2.D0
         DFD2DK(5)= 0.D0
         DFD2DK(6)= 0.D0
         DFDEDK(1)= 0.D0
         DFDEDK(2)= 1.D0
         DFDEDK(3)= 0.D0
         DFDEDK(4)= -1.D0
         DFDEDK(5)= -1.D0
         DFDEDK(6)= 1.D0
      ENDIF
      
      END
