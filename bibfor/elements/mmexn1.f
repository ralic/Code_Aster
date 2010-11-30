      SUBROUTINE MMEXN1(TYPBAR,NDEXCL)
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
      INTEGER  TYPBAR
      INTEGER  NDEXCL(9)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C RETOURNE LE NOEUD MILIEU A EXCLURE POUR BARSOUM
C
C ----------------------------------------------------------------------
C
C
C IN  TYPBAR : NOEUDS EXCLUS PAR CET ELEMENT DE BARSOUM
C               TYPBAR = 0
C                PAS DE FOND DE FISSURE
C               TYPBAR = 1
C                QUAD 8 - FOND FISSURE: 1-2
C               TYPBAR = 2
C                QUAD 8 - FOND FISSURE: 2-3
C               TYPBAR = 3
C                QUAD 8 - FOND FISSURE: 3-4
C               TYPBAR = 4
C                QUAD 8 - FOND FISSURE: 4-1
C               TYPBAR = 5
C                SEG 3  - FOND FISSURE: 1
C               TYPBAR = 6
C                SEG 3  - FOND FISSURE: 2
C OUT NDEXCL : NUMERO DU NOEUD MILIEU A EXCLURE
C
C ----------------------------------------------------------------------
C
      IF (TYPBAR.EQ.1) THEN
        NDEXCL(1) = 1
        NDEXCL(2) = 1
        NDEXCL(5) = 1
      ELSEIF (TYPBAR.EQ.2) THEN
        NDEXCL(2) = 1
        NDEXCL(3) = 1
        NDEXCL(6) = 1             
      ELSEIF (TYPBAR.EQ.3) THEN
        NDEXCL(3) = 1
        NDEXCL(4) = 1
        NDEXCL(7) = 1   
      ELSEIF (TYPBAR.EQ.4) THEN
        NDEXCL(4) = 1
        NDEXCL(1) = 1
        NDEXCL(8) = 1       
      ELSEIF (TYPBAR.EQ.5) THEN
        NDEXCL(1) = 1      
      ELSEIF (TYPBAR.EQ.6) THEN
        NDEXCL(2) = 1                 
      ENDIF    
C
      END
