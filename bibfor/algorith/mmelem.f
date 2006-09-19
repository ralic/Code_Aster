      SUBROUTINE MMELEM(NOMTE,
     &                  NDIM,NDDL,ESC,NNE,MAIT,NNM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/09/2006   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      CHARACTER*16 NOMTE
      INTEGER      NDIM
      INTEGER      NDDL
      CHARACTER*8  ESC
      INTEGER      NNE
      CHARACTER*8  MAIT
      INTEGER      NNM
C
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0364/TE0365
C ----------------------------------------------------------------------
C
C RETOURNE QUELQUES INFOS SUR LES ELEMENTS DE CONTACT FORMES ENTRE
C DEUX ELEMENTS DE SURFACE
C
C IN  NOMTE  : NOM DU TE DE L'ELEMENT DE CONTACT EN JEU
C OUT NDIM   : DIMENSION DE LA MAILLE DE CONTACT
C OUT NDDL   : NOMBRE TOTAL DE DEGRES DE LIBERTE DE LA MAILLE DE CONTACT
C OUT ESC    : NOM DE LA MAILLE ESCLAVE
C OUT NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C OUT MAIT   : NOM DE LA MAILLE MAITRE
C OUT NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C
C ----------------------------------------------------------------------
C
      IF (NOMTE.EQ.'CFS2S2') THEN
        ESC  = 'SG2'
        NNM  = 2
        NNE  = 2
        NDDL = 12
        NDIM = 2
        MAIT = 'SG2'
      ELSE IF (NOMTE.EQ.'CFS2S3') THEN
        ESC  = 'SG2'
        NNM  = 3
        NNE  = 2
        NDDL = 14
        NDIM = 2
        MAIT = 'SG3'
      ELSE IF (NOMTE.EQ.'CFS3S2') THEN
        ESC  = 'SG3'
        NNM  = 2
        NNE  = 3
        NDDL = 16
        NDIM = 2
        MAIT = 'SG2'
      ELSE IF (NOMTE.EQ.'CFS3S3') THEN
        ESC  = 'SG3'
        NNM  = 3
        NNE  = 3
        NDDL = 18
        NDIM = 2
        MAIT = 'SG3'
      ELSE IF (NOMTE.EQ.'CFT3T3') THEN
        ESC  = 'TR3'
        NNM  = 3
        NNE  = 3
        NDDL = 27
        NDIM = 3
        MAIT = 'TR3'
      ELSE IF (NOMTE.EQ.'CFT3T6') THEN
        ESC  = 'TR3'
        NNM  = 6
        NNE  = 3
        NDDL = 36
        NDIM = 3
        MAIT = 'TR6'
      ELSE IF (NOMTE.EQ.'CFT6T3') THEN
        ESC  = 'TR6'
        NNM  = 3
        NNE  = 6
        NDDL = 45
        NDIM = 3
        MAIT = 'TR3'
      ELSE IF (NOMTE.EQ.'CFT6T6') THEN
        ESC  = 'TR6'
        NNM  = 6
        NNE  = 6
        NDDL = 54
        NDIM = 3
        MAIT = 'TR6'
      ELSE IF (NOMTE.EQ.'CFQ4Q4') THEN
        ESC  = 'QU4'
        NNM  = 4
        NNE  = 4
        NDDL = 36
        NDIM = 3
        MAIT = 'QU4'
      ELSE IF (NOMTE.EQ.'CFQ4Q8') THEN
        ESC  = 'QU4'
        NNM  = 8
        NNE  = 4
        NDDL = 48
        NDIM = 3
        MAIT = 'QU8'
      ELSE IF (NOMTE.EQ.'CFQ8Q4') THEN
        ESC  = 'QU8'
        NNM  = 4
        NNE  = 8
        NDDL = 60
        NDIM = 3
        MAIT = 'QU4'
      ELSE IF (NOMTE.EQ.'CFQ8Q8') THEN
        ESC  = 'QU8'
        NNM  = 8
        NNE  = 8
        NDDL = 72
        NDIM = 3
        MAIT = 'QU8'
      ELSE IF (NOMTE.EQ.'CFQ8T6') THEN
        ESC  = 'QU8'
        NNM  = 6
        NNE  = 8
        NDDL = 66
        NDIM = 3
        MAIT = 'TR6'
      ELSE IF (NOMTE.EQ.'CFT6Q8') THEN
        ESC  = 'TR6'
        NNM  = 8
        NNE  = 6
        NDDL = 60
        NDIM = 3
        MAIT = 'QU8'
      ELSE IF (NOMTE.EQ.'CFQ4T3') THEN
        ESC  = 'QU4'
        NNM  = 3
        NNE  = 4
        NDDL = 33
        NDIM = 3
        MAIT = 'TR3'
      ELSE IF (NOMTE.EQ.'CFQ4T6') THEN
        ESC  = 'QU4'
        NNM  = 6
        NNE  = 4
        NDDL = 42
        NDIM = 3
        MAIT = 'TR6'  
      ELSE IF (NOMTE.EQ.'CFT3Q4') THEN
        ESC  = 'TR3'
        NNM  = 4
        NNE  = 3
        NDDL = 30
        NDIM = 3
        MAIT = 'QU4'
      ELSE IF (NOMTE.EQ.'CFT3Q8') THEN
        ESC  = 'TR3'
        NNM  = 8
        NNE  = 3
        NDDL = 42
        NDIM = 3
        MAIT = 'QU8'  
      ELSE IF (NOMTE.EQ.'CFQ9Q9') THEN
        ESC  = 'QU9'
        NNM  = 9
        NNE  = 9
        NDDL = 81
        NDIM = 3
        MAIT = 'QU9'
      ELSEIF (NOMTE .EQ. 'CFP2P2') THEN
        ESC  = 'SG2'
        NNM  = 2
        NNE  = 2
        NDDL = 18
        NDIM = 3
        MAIT = 'SG2'
      ELSE
        CALL UTMESS('F','MMELEM', 'NOM DE L''ELEMENT INCONNU')
      END IF
      END
