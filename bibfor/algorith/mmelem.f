      SUBROUTINE MMELEM(NOMTE ,NDIM  ,NDDL  ,TYPMAE,NNE   ,
     &                  TYPMAM,NNM   ,NNL   ,NBCPS ,NBDM  ,
     &                  LAXIS ,LELTF )

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/04/2011   AUTEUR ABBAS M.ABBAS 
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
      INTEGER      NDIM,NDDL,NNE,NNM,NNL
      INTEGER      NBCPS,NBDM
      CHARACTER*8  TYPMAE, TYPMAM
      CHARACTER*16 NOMTE
      LOGICAL      LAXIS,LELTF
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
C OUT TYPMAE : TYPE DE LA MAILLE ESCLAVE
C OUT NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C OUT TYPMAM : TYPE DE LA MAILLE MAITRE
C OUT NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C OUT NNL    : NOMBRE DE NOEUDS PORTANT UN LAGRANGE DE CONTACT/FROTT
C OUT NBCPS  : NOMBRE DE COMPOSANTES/NOEUD DES LAGR_C+LAGR_F
C OUT NBDM   : NOMBRE DE COMPOSANTES/NOEUD DES DEPL+LAGR_C+LAGR_F
C OUT LAXIS  : .TRUE. SI MODELE AXISYMETRIQUE
C OUT LELTF  : .TRUE. SI ELEMENT COMPORTANT DES DDL DE FROTTEMENT
C
C ----------------------------------------------------------------------
C
      INTEGER I2D,I3D
      LOGICAL LTEATT
C
C ----------------------------------------------------------------------
C

C
C --- MODELE AXISYMETRIQUE ?
C
      LAXIS    = LTEATT(' ','AXIS','OUI')
C
C --- ELEMENT AVEC DDL DE FROTTEMENT ?
C
      LELTF    = LTEATT(' ','FROTTEMENT','OUI')       
C
C --- NOMBRE DE COMPOSANTES LAGRANGIENS (NON DEPLACEMENT)
C     
      IF (LELTF) THEN
C ----- COMPOSANTES 2D : LAGS_C   LAGS_F1
C ----- COMPOSANTES 3D : LAGS_C   LAGS_F1  LAGS_F2
        I2D = 2
        I3D = 3
      ELSE
C ----- COMPOSANTE : LAGS_C
        I2D = 1
        I3D = 1
      ENDIF
C
C --- 2D
C
C --- 'SE2'
      IF (NOMTE(1:6).EQ.'CFS2S2' .OR. NOMTE(1:6).EQ.'COS2S2') THEN
        NDIM    = 2
        TYPMAE  = 'SE2'
        NNE     = 2
        TYPMAM  = 'SE2'
        NNM     = 2
        NDDL    = NNM*NDIM + NNE*(NDIM+I2D)
      ELSE IF (NOMTE(1:6).EQ.'CFS2S3' .OR. NOMTE(1:6).EQ.'COS2S3') THEN
        NDIM    = 2
        TYPMAE  = 'SE2'
        NNE     = 2
        TYPMAM  = 'SE3'
        NNM     = 3
        NDDL    = NNM*NDIM + NNE*(NDIM+I2D)
C --- 'SE3'
      ELSE IF (NOMTE(1:6).EQ.'CFS3S2' .OR. NOMTE(1:6).EQ.'COS3S2') THEN
        NDIM    = 2
        TYPMAE  = 'SE3'
        NNE     = 3
        TYPMAM  = 'SE2'
        NNM     = 2
        NDDL    = NNM*NDIM + NNE*(NDIM+I2D)
      ELSE IF (NOMTE(1:6).EQ.'CFS3S3' .OR. NOMTE(1:6).EQ.'COS3S3') THEN
        NDIM    = 2
        TYPMAE  = 'SE3'
        NNE     = 3
        TYPMAM  = 'SE3'
        NNM     = 3
        NDDL    = NNM*NDIM + NNE*(NDIM+I2D)
C
C --- 3D
C
C --- 'SE2'
      ELSEIF (NOMTE.EQ.'CFP2P2' .OR. NOMTE.EQ.'COP2P2') THEN
        NDIM = 3
        TYPMAE  = 'SE2'
        NNE  = 2
        TYPMAM = 'SE2'
        NNM  = 2
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
C --- 'TR3'
      ELSE IF (NOMTE.EQ.'CFT3T3' .OR. NOMTE.EQ.'COT3T3') THEN
        NDIM    = 3
        TYPMAE  = 'TR3'
        NNE     = 3
        TYPMAM  = 'TR3'
        NNM     = 3
        NDDL    = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT3T6' .OR. NOMTE.EQ.'COT3T6') THEN
        NDIM    = 3
        TYPMAE  = 'TR3'
        NNE     = 3
        TYPMAM  = 'TR6'
        NNM     = 6
        NDDL    = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT3Q4' .OR. NOMTE.EQ.'COT3Q4') THEN
        NDIM = 3
        TYPMAE  = 'TR3'
        NNE  = 3
        TYPMAM = 'QU4'
        NNM  = 4
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT3Q8' .OR. NOMTE.EQ.'COT3Q8') THEN
        NDIM = 3
        TYPMAE  = 'TR3'
        NNE  = 3
        TYPMAM = 'QU8'
        NNM  = 8
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT3Q9' .OR. NOMTE.EQ.'COT3Q9') THEN
        NDIM = 3
        TYPMAE  = 'TR3'
        NNE  = 3
        TYPMAM = 'QU9'
        NNM  = 9
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
C --- 'TR6'
      ELSE IF (NOMTE.EQ.'CFT6T3' .OR. NOMTE.EQ.'COT6T3') THEN
        NDIM    = 3
        TYPMAE  = 'TR6'
        NNE     = 6
        TYPMAM  = 'TR3'
        NNM     = 3
        NDDL    = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT6T6' .OR. NOMTE.EQ.'COT6T6') THEN
        NDIM    = 3
        TYPMAE  = 'TR6'
        NNE     = 6
        TYPMAM  = 'TR6'
        NNM     = 6
        NDDL    = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT6Q4' .OR. NOMTE.EQ.'COT6Q4') THEN
        NDIM = 3
        TYPMAE  = 'TR6'
        NNE  = 6
        TYPMAM = 'QU4'
        NNM  = 4
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT6Q8' .OR. NOMTE.EQ.'COT6Q8') THEN
        NDIM = 3
        TYPMAE  = 'TR6'
        NNE  = 6
        TYPMAM = 'QU8'
        NNM  = 8
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFT6Q9' .OR. NOMTE.EQ.'COT6Q9') THEN
        NDIM = 3
        TYPMAE  = 'TR6'
        NNE  = 6
        TYPMAM = 'QU9'
        NNM  = 9
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
C --- 'QU4'
      ELSE IF (NOMTE.EQ.'CFQ4T3' .OR. NOMTE.EQ.'COQ4T3') THEN
        NDIM = 3
        TYPMAE  = 'QU4'
        NNE  = 4
        TYPMAM = 'TR3'
        NNM  = 3
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ4T6' .OR. NOMTE.EQ.'COQ4T6') THEN
        NDIM = 3
        TYPMAE  = 'QU4'
        NNE  = 4
        TYPMAM = 'TR6'
        NNM  = 6
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ4Q4' .OR. NOMTE.EQ.'COQ4Q4') THEN
        NDIM = 3
        TYPMAE  = 'QU4'
        NNE  = 4
        TYPMAM = 'QU4'
        NNM  = 4
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ4Q8' .OR. NOMTE.EQ.'COQ4Q8') THEN
        NDIM = 3
        TYPMAE  = 'QU4'
        NNE  = 4
        TYPMAM = 'QU8'
        NNM  = 8
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ4Q9' .OR. NOMTE.EQ.'COQ4Q9') THEN
        NDIM = 3
        TYPMAE  = 'QU4'
        NNE  = 4
        TYPMAM = 'QU9'
        NNM  = 9
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
C --- 'QU8'
      ELSE IF (NOMTE.EQ.'CFQ8T3' .OR. NOMTE.EQ.'COQ8T3') THEN
        NDIM = 3
        TYPMAE  = 'QU8'
        NNE  = 8
        TYPMAM = 'TR3'
        NNM  = 3
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ8T6' .OR. NOMTE.EQ.'COQ8T6') THEN
        NDIM = 3
        TYPMAE  = 'QU8'
        NNE  = 8
        TYPMAM = 'TR6'
        NNM  = 6
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ8Q4' .OR. NOMTE.EQ.'COQ8Q4') THEN
        NDIM = 3
        TYPMAE  = 'QU8'
        NNE  = 8
        TYPMAM = 'QU4'
        NNM  = 4
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ8Q8' .OR. NOMTE.EQ.'COQ8Q8') THEN
        NDIM = 3
        TYPMAE  = 'QU8'
        NNE  = 8
        TYPMAM = 'QU8'
        NNM  = 8
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ8Q9' .OR. NOMTE.EQ.'COQ8Q9') THEN
        NDIM = 3
        TYPMAE  = 'QU8'
        NNE  = 8
        TYPMAM = 'QU9'
        NNM  = 9
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
C --- 'QU9'
      ELSE IF (NOMTE.EQ.'CFQ9T3' .OR. NOMTE.EQ.'COQ9T3') THEN
        NDIM = 3
        TYPMAE  = 'QU9'
        NNE  = 9
        TYPMAM = 'TR3'
        NNM  = 3
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ9T6' .OR. NOMTE.EQ.'COQ9T6') THEN
        NDIM = 3
        TYPMAE  = 'QU9'
        NNE  = 9
        TYPMAM = 'TR6'
        NNM  = 6
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ9Q4' .OR. NOMTE.EQ.'COQ9Q4') THEN
        NDIM = 3
        TYPMAE  = 'QU9'
        NNE  = 9
        TYPMAM = 'QU4'
        NNM  = 4
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ9Q8' .OR. NOMTE.EQ.'COQ9Q8') THEN
        NDIM = 3
        TYPMAE  = 'QU9'
        NNE  = 9
        TYPMAM = 'QU8'
        NNM  = 8
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE IF (NOMTE.EQ.'CFQ9Q9' .OR. NOMTE.EQ.'COQ9Q9') THEN
        NDIM = 3
        TYPMAE  = 'QU9'
        NNE  = 9
        TYPMAM = 'QU9'
        NNM  = 9
        NDDL = NNM*NDIM + NNE*(NDIM+I3D)
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
C      
C --- NOMBRE DE NOEUDS PORTANT DES LAGRANGES
C
      NNL    = NNE
C
C --- NOMBRE DE COMPOSANTES LAGR_C + LAGR_F
C
      IF (LELTF) THEN
        NBCPS = NDIM
      ELSE
        NBCPS = 1
      ENDIF
C
C --- NOMBRE DE COMPOSANTES TOTAL DEPL + LAGR_C + LAGR_F
C
      NBDM   = NDIM + NBCPS  
C      
      CALL ASSERT(NDDL.LE.81)
      CALL ASSERT((NDIM.EQ.2).OR.(NDIM.EQ.3))           
C
      END
