      SUBROUTINE TMACOQ(TYPEMA,DIME,L)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ----------------------------------------------------------------------
C       TYPE DE MAILLE VOLUMIQUE ASSOCIE A UN TYPE DE MAILLE COQUE
C ----------------------------------------------------------------------
C VARIABLE D'ENTREE / SORTIE
C CHARACTER*8  TYPEMA  : TYPE DE MAILLE
C
C VARIABLE D'ENTREE
C INTEGER      DIME    : DIMENSION DE L'ESPACE
C
C VARIABLE DE SORTIE
C INTEGER      L       : L = 0 : TYPEMA N'EST PAS UNE MAILLE DE COQUE
C                                TYPEMA INCHANGE
C                        L = 1 : TYPEMA EST UNE MAILLE DE COQUE
C                                TYPEMA = TYPE DE MAILLE VOLUM. ASSOCIE
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8 TYPEMA
      INTEGER     DIME,L

C --- COQUE 2D

      IF (DIME.EQ.2) THEN

        IF (TYPEMA(1:3).EQ.'SEG') THEN 

          TYPEMA = 'QUAD6'
          L = 1

        ELSE

          L = 0

        ENDIF

C --- COQUE 3D

      ELSE

        IF (TYPEMA(1:4).EQ.'TRIA') THEN

          L = 1
          IF (TYPEMA(5:5).EQ.'3') THEN
            TYPEMA = 'PENTA6'
          ELSEIF (TYPEMA(5:5).EQ.'6') THEN
            TYPEMA = 'PENTA12'
          ELSE
            TYPEMA = 'PENTA14'
          ENDIF

        ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN

          L = 1
          IF (TYPEMA(5:5).EQ.'4') THEN
            TYPEMA = 'HEXA8'
          ELSEIF (TYPEMA(5:5).EQ.'8') THEN
            TYPEMA = 'HEXA16'
          ELSEIF (TYPEMA(5:5).EQ.'9') THEN
            TYPEMA = 'HEXA18'
          ENDIF

        ELSE

          L = 0

        ENDIF

      ENDIF

      END
