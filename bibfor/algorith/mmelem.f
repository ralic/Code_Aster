      SUBROUTINE MMELEM(NOMTE ,NDIM  ,NDDL  ,ESC   ,NNE   ,
     &                  MAIT  ,NNM )

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/09/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER      NDIM, NDDL, NNE, NNM
      CHARACTER*8  ESC, MAIT
      CHARACTER*16 NOMTE
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C RETOURNE DES INFOS SUR LES ELEMENTS DE CONTACT FORMES ENTRE
C DEUX ELEMENTS DE SURFACE
C      
C ----------------------------------------------------------------------
C
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
      INTEGER I2D,I3D
C
C ----------------------------------------------------------------------
C
      IF (NOMTE(1:2).EQ.'CF') THEN
C ----- COMPOSANTES 2D : LAGS_C   LAGS_F1
C ----- COMPOSANTES 3D : LAGS_C   LAGS_F1  LAGS_F2
        I2D = 2
        I3D = 3
      ELSEIF (NOMTE(1:2).EQ.'CO') THEN
C ----- COMPOSANTE : LAGS_C
        I2D = 1
        I3D = 1
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- 2D
C
      IF (NOMTE.EQ.'CFS2S2' .OR. NOMTE.EQ.'COS2S2') THEN
        NDIM = 2
        ESC  = 'SG2'
        NNE  = 2
        MAIT = 'SG2'
        NNM  = 2
        NDDL = NNM*NDIM + NNE*(NDIM+I2D)
      ELSE IF (NOMTE.EQ.'CFS2S3' .OR. NOMTE.EQ.'COS2S3') THEN
        NDIM = 2
        ESC  = 'SG2'
        NNE  = 2
        MAIT = 'SG3'
        NNM  = 3
        NDDL = NNM*NDIM + NNE*(NDIM+I2D)
      ELSE IF (NOMTE.EQ.'CFS3S2' .OR. NOMTE.EQ.'COS3S2') THEN
        NDIM = 2
        ESC  = 'SG3'
        NNE  = 3
        MAIT = 'SG2'
        NNM  = 2
        NDDL = NNM*NDIM + NNE*(NDIM+I2D)
      ELSE IF (NOMTE.EQ.'CFS3S3' .OR. NOMTE.EQ.'COS3S3') THEN
        NDIM = 2
        ESC  = 'SG3'
        NNE  = 3
        MAIT = 'SG3'
        NNM  = 3
        NDDL = NNM*NDIM + NNE*(NDIM+I2D)
C
C --- 3D
C
      ELSE IF (NOMTE.EQ.'CFT3T3' .OR. NOMTE.EQ.'COT3T3') THEN
        NDIM = 3
        ESC  = 'TR3'
        NNE  = 3
        MAIT = 'TR3'
        NNM  = 3
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT3T6' .OR. NOMTE.EQ.'COT3T6') THEN
        NDIM = 3
        ESC  = 'TR3'
        NNE  = 3
        MAIT = 'TR6'
        NNM  = 6
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT6T3' .OR. NOMTE.EQ.'COT6T3') THEN
        NDIM = 3
        ESC  = 'TR6'
        NNE  = 6
        MAIT = 'TR3'
        NNM  = 3
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT6T6' .OR. NOMTE.EQ.'COT6T6') THEN
        NDIM = 3
        ESC  = 'TR6'
        NNE  = 6
        MAIT = 'TR6'
        NNM  = 6
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ4Q4' .OR. NOMTE.EQ.'COQ4Q4') THEN
        NDIM = 3
        ESC  = 'QU4'
        NNE  = 4
        MAIT = 'QU4'
        NNM  = 4
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ4Q8' .OR. NOMTE.EQ.'COQ4Q8') THEN
        NDIM = 3
        ESC  = 'QU4'
        NNE  = 4
        MAIT = 'QU8'
        NNM  = 8
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ8Q4' .OR. NOMTE.EQ.'COQ8Q4') THEN
        NDIM = 3
        ESC  = 'QU8'
        NNE  = 8
        MAIT = 'QU4'
        NNM  = 4
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ8Q8' .OR. NOMTE.EQ.'COQ8Q8') THEN
        NDIM = 3
        ESC  = 'QU8'
        NNE  = 8
        MAIT = 'QU8'
        NNM  = 8
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ8T6' .OR. NOMTE.EQ.'COQ8T6') THEN
        NDIM = 3
        ESC  = 'QU8'
        NNE  = 8
        MAIT = 'TR6'
        NNM  = 6
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT6Q8' .OR. NOMTE.EQ.'COT6Q8') THEN
        NDIM = 3
        ESC  = 'TR6'
        NNE  = 6
        MAIT = 'QU8'
        NNM  = 8
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ4T3' .OR. NOMTE.EQ.'COQ4T3') THEN
        NDIM = 3
        ESC  = 'QU4'
        NNE  = 4
        MAIT = 'TR3'
        NNM  = 3
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ4T6' .OR. NOMTE.EQ.'COQ4T6') THEN
        NDIM = 3
        ESC  = 'QU4'
        NNE  = 4
        MAIT = 'TR6'
        NNM  = 6
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT3Q4' .OR. NOMTE.EQ.'COT3Q4') THEN
        NDIM = 3
        ESC  = 'TR3'
        NNE  = 3
        MAIT = 'QU4'
        NNM  = 4
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT3Q8' .OR. NOMTE.EQ.'COT3Q8') THEN
        NDIM = 3
        ESC  = 'TR3'
        NNE  = 3
        MAIT = 'QU8'
        NNM  = 8
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ9Q9' .OR. NOMTE.EQ.'COQ9Q9') THEN
        NDIM = 3
        ESC  = 'QU9'
        NNE  = 9
        MAIT = 'QU9'
        NNM  = 9
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSEIF (NOMTE.EQ.'CFP2P2' .OR. NOMTE.EQ.'COP2P2') THEN
        NDIM = 3
        ESC  = 'SG2'
        NNE  = 2
        MAIT = 'SG2'
        NNM  = 2
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
C
      END
