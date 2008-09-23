      SUBROUTINE NMAGGL(VALVAL,TYPVAL,NOMVAL)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*24 VALVAL(8),NOMVAL 
      CHARACTER*3  TYPVAL      
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
C
C CHANGE LE CONTENU D'UNE VARIABLE CHAPEAU DES NOMS
C
C ----------------------------------------------------------------------
C
C
C IN  VALVAL : VARIABLE CHAPEAU POUR NOM DES CHAM_NO
C IN  TYPVAL : TYPE DE VARIABLE A REMPLACER
C IN  NOMVAL : NOM  DU CHAM_NO DE LA VARIABLE A REMPLACER
C      
C ----------------------------------------------------------------------
C
      INTEGER      INDEX
C      
C ----------------------------------------------------------------------
C
      IF (TYPVAL.EQ.'DEO') THEN
        INDEX = 1
      ELSEIF (TYPVAL.EQ.'SIG') THEN
        INDEX = 2
      ELSEIF (TYPVAL.EQ.'VAR') THEN
        INDEX = 3
      ELSEIF (TYPVAL.EQ.'COM') THEN
        INDEX = 4   
      ELSEIF (TYPVAL.EQ.'VIT') THEN
        INDEX = 5   
      ELSEIF (TYPVAL.EQ.'ACC') THEN
        INDEX = 6                                         
      ELSE    
        CALL ASSERT(.FALSE.)
      ENDIF    
C
C --- REMPLACEMENT
C
      VALVAL(INDEX) = NOMVAL      
C
      END
