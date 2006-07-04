      SUBROUTINE MODTHM(NOMTE,MODINT)
      IMPLICIT      NONE
      CHARACTER*3   MODINT
      CHARACTER*16  NOMTE
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2006   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
C --- DETERMINATION DES MODELISATIONS LUMPEE, NON LUMPEE OU MIXTE -----
C =====================================================================
      INTEGER       DIMETE,LXLGUT
C =====================================================================
C
C     REPERAGE DE LA LONGUEUR UTILE DE LA CHAINE 'NOMTE'
C
      DIMETE = LXLGUT(NOMTE)
C
C     SI ON EST DANS UNE MODELISATION PERMANENTE, LA FIN EST '_P'
C     ON DOIT DONC TESTER SUR L'ANTEPENULTIEME CARACTERE
C
      IF (NOMTE(DIMETE-1:DIMETE).EQ.'_P') THEN
        DIMETE = DIMETE - 2
      ENDIF
C
C     REPERAGE DU MODE D'INTEGRATION
C
      IF (NOMTE(DIMETE:DIMETE).EQ.'D') THEN
        MODINT = 'LUM'
      ELSEIF (NOMTE(DIMETE:DIMETE).EQ.'S') THEN
        MODINT = 'RED'
      ELSE
        MODINT = 'CLA'
      ENDIF
C =====================================================================
      END
