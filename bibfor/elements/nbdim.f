      FUNCTION NBDIM(NOMTEZ)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) NOMTEZ
      INTEGER NBDIM
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/01/2001   AUTEUR CIBHHPD D.NUNEZ 
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
C     BUT       : DIMENSION D'UN TYPE D'ELEMENT DONNE
C IN  K* NOMTEZ : NOM TYPE DE L'ELEMENT
C-----------------------------------------------------------------------
C
      CHARACTER*16 NOMTE
C
      NOMTE  = NOMTEZ
C
C ---- CAS DU 3D :
C      ---------
      IF  (NOMTE(6:10).EQ.'TETRA'.OR.NOMTE(6:10).EQ.'PYRAM'
     + .OR.NOMTE(6:10).EQ.'PENTA'.OR.NOMTE(6:9) .EQ.'HEXA'
     + .OR.NOMTE(6:10).EQ.'HEXS') THEN
           NBDIM = 3
C
C ---- CAS DU AXI_FOURIER :
C      ------------------
      ELSEIF  ( NOMTE(3:4) .EQ. 'FO' ) THEN
           NBDIM = 3
C
C ---- CAS DU 2D :
C      ---------
      ELSEIF  (NOMTE(5:6).EQ.'QU'.OR.NOMTE(5:6).EQ.'TR'
     + .OR.NOMTE(5:6).EQ.'QS') THEN
           NBDIM = 2
      ELSE
         CALL UTMESS('F','NBDIM','LE TYPE D''ELEMENT : '//NOMTE//
     +               'N''EST PAS TRAITE.')
      ENDIF
C
      END
