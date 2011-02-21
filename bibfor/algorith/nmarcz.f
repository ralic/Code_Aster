      SUBROUTINE NMARCZ(TYPCHZ,NOMCHA,TYPECH,NOMSCH,NBCHEF)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/02/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*(*) TYPCHZ
      CHARACTER*19  NOMCHA,NOMSCH(*)
      CHARACTER*24  TYPECH(*)
      INTEGER       NBCHEF
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (ALGORITHME - ARCHIVAGE )
C
C CREATION LISTE DES CHAMPS A ARCIVER
C
C ----------------------------------------------------------------------
C
C
C IN  TYPCHA : TYPE DU CHAMP
C              ' ' POUR INITIALISER
C IN  NOMCHA : NOM DU CHAMP
C I/O TYPECH : LISTE DES TYPES DE CHAMPS
C I/O NOMSCH : LISTE DES NOMS DE CHAMPS
C I/O NBCHEF : NOMBRE DE CHAMPS DANS LA LISTE
C
C ----------------------------------------------------------------------
C
      INTEGER I
      CHARACTER*24 TYPCHA
C
      IF (NOMCHA.EQ.' ') THEN
        NBCHEF = 1
        DO 10 I=1,99
          TYPECH(I) = ' '
          NOMSCH(I) = ' '
  10    CONTINUE      
      ELSE
        CALL ASSERT(NBCHEF.LE.99)
        TYPCHA = TYPCHZ
        TYPECH(NBCHEF) = TYPCHA
        NOMSCH(NBCHEF) = NOMCHA
        NBCHEF = NBCHEF + 1 
      ENDIF  
C
      END
