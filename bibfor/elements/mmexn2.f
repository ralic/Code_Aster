      SUBROUTINE MMEXN2(TYPRAC,NDEXCL)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/10/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER  TYPRAC
      INTEGER  NDEXCL(9)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C RETOURNE LE NOEUD MILIEU A EXCLURE POUR ACCORD LINE/QUAD
C
C ----------------------------------------------------------------------
C
C
C IN  TYPRAC : TYPE DE RACCORD LINE/QUAD (ON EST SUR UN QUAD8/9)
C               1 - NOEUD MILIEU 5 EXCLU
C               2 - NOEUD MILIEU 6 EXCLU
C               3 - NOEUD MILIEU 7 EXCLU
C               4 - NOEUD MILIEU 8 EXCLU
C OUT NDEXCL : NUMERO DU NOEUD MILIEU A EXCLURE
C
C ----------------------------------------------------------------------
C
      IF (TYPRAC.EQ.1) THEN
        NDEXCL(5) = 1
      ELSEIF (TYPRAC.EQ.2) THEN
        NDEXCL(6) = 1
      ELSEIF (TYPRAC.EQ.3) THEN
        NDEXCL(7) = 1
      ELSEIF (TYPRAC.EQ.4) THEN
        NDEXCL(8) = 1
      ENDIF 
C
      END
