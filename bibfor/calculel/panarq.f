      SUBROUTINE PANARQ(TYPEMA,PAN,NARE)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C        PANS TOUCHANT LES ARETES QUADRATIQUES D'UN TYPE DE MAILLE
C ----------------------------------------------------------------------
C VARIABLE D'ENTREE
C CHARACTER*8  TYPEMA          : TYPE DE MAILLE
C
C VARIABLES DE SORTIE
C INTEGER      PAN(DIM-1,NARE) : PANS TOUCHANT LES ARETES QUADRATIQUES
C                               (PAN1.1,[PAN1.2],PAN2.1,[PAN2.2],...)
C                                PAN*.1 [ET PAN*.2] TOUCHE[NT] L'ARETE 
C                                QUADRATIQUE *
C INTEGER      NARE            : NOMBRE D'ARETES QUADRATIQUES
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8 TYPEMA
      INTEGER     PAN(*),NARE

      IF (TYPEMA(1:5).EQ.'TRIA6') THEN
        NARE = 3
        PAN(1) = 1
        PAN(2) = 2      
        PAN(3) = 3
      ELSEIF (TYPEMA(1:5).EQ.'QUAD6') THEN
        NARE = 2
        PAN(1) = 1
        PAN(2) = 3
      ELSEIF ((TYPEMA(1:5).EQ.'QUAD8').OR.(TYPEMA(1:5).EQ.'QUAD9')) THEN
        NARE = 4
        PAN(1) = 1
        PAN(2) = 2
        PAN(3) = 3
        PAN(4) = 4
      ELSEIF (TYPEMA(1:7).EQ.'TETRA10') THEN
        NARE = 6
        PAN(1) = 1
        PAN(2) = 2
        PAN(3) = 1
        PAN(4) = 4
        PAN(5) = 1
        PAN(6) = 3
        PAN(7) = 2
        PAN(8) = 3
        PAN(9) = 2
        PAN(10) = 4
        PAN(11) = 3
        PAN(12) = 4
      ELSEIF ((TYPEMA(1:7).EQ.'PENTA12')
     &    .OR.(TYPEMA(1:7).EQ.'PENTA14')) THEN
        NARE = 6
        PAN(1) = 1
        PAN(2) = 3
        PAN(3) = 1
        PAN(4) = 4
        PAN(5) = 1
        PAN(6) = 5
        PAN(7) = 2
        PAN(8) = 3
        PAN(9) = 2
        PAN(10) = 4
        PAN(11) = 2
        PAN(12) = 5 
      ELSEIF (TYPEMA(1:7).EQ.'PENTA15') THEN
        NARE = 9
        PAN(1) = 1
        PAN(2) = 3
        PAN(3) = 1
        PAN(4) = 4
        PAN(5) = 1
        PAN(6) = 5
        PAN(7) = 3
        PAN(8) = 5
        PAN(9) = 3
        PAN(10) = 4
        PAN(11) = 4
        PAN(12) = 5
        PAN(13) = 2
        PAN(14) = 3
        PAN(15) = 2
        PAN(16) = 4
        PAN(17) = 2
        PAN(18) = 5
      ELSEIF((TYPEMA(1:6).EQ.'HEXA16').OR.(TYPEMA(1:6).EQ.'HEXA18'))THEN
        NARE = 8
        PAN(1) = 1
        PAN(2) = 3
        PAN(3) = 1
        PAN(4) = 5
        PAN(5) = 5
        PAN(6) = 2
        PAN(7) = 2
        PAN(8) = 3
        PAN(9) = 3
        PAN(10) = 4        
        PAN(11) = 4
        PAN(12) = 5
        PAN(13) = 3
        PAN(14) = 6
        PAN(15) = 5        
        PAN(16) = 6        
      ELSEIF((TYPEMA(1:6).EQ.'HEXA20').OR.(TYPEMA(1:6).EQ.'HEXA27'))THEN
        NARE = 12
        PAN(1) = 1
        PAN(2) = 2
        PAN(3) = 1
        PAN(4) = 3
        PAN(5) = 1
        PAN(6) = 4
        PAN(7) = 1
        PAN(8) = 5
        PAN(9) = 2
        PAN(10) = 5
        PAN(11) = 2
        PAN(12) = 3
        PAN(13) = 3
        PAN(14) = 4
        PAN(15) = 4
        PAN(16) = 5
        PAN(17) = 2
        PAN(18) = 6        
        PAN(19) = 3
        PAN(20) = 6
        PAN(21) = 4
        PAN(22) = 6
        PAN(23) = 5        
        PAN(24) = 6
      ELSE
        CALL UTMESS('F','PANARQ',TYPEMA//' INDISPONIBLE')
      ENDIF  

      END
