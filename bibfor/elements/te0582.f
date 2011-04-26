      SUBROUTINE TE0582(OPTION,NOMTE)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      CHARACTER*16 OPTION,NOMTE
C ......................................................................

C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          TUYAU ET LES VECTEURS ELEMENTAIRES DE FORCES
C                          D ACCELERATION
C                          OPTION : RIGI_MECA, MASS_MECA, M_GAMMA
C                          SERT A DIMENSIONNER LES MATRICES
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      INTEGER NBRDDM
      PARAMETER (NBRDDM=156)
      INTEGER NPG,IPOIDS,IVF
      INTEGER NDIM,NNOS,NNO,JCOOPG,IDFDK,JDFD2,JGANO
      REAL*8 MASS(NBRDDM*NBRDDM),K(NBRDDM*NBRDDM)
      INTEGER M,NBRDDL,NC,IACCE,IVECT,IMASS

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX --------------------

      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------


      CALL ELREF5(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,JCOOPG,IVF,IDFDK,
     &            JDFD2,JGANO)

      M = 3
      IF (NOMTE.EQ.'MET6SEG3') M = 6

C     FORMULE GENERALE

      NBRDDL = NNO* (6+3+6* (M-1))

C     VERIFS PRAGMATIQUES

      IF (NBRDDL.GT.NBRDDM) THEN
        CALL U2MESS('F','ELEMENTS4_40')
      END IF
      IF (NOMTE.EQ.'MET3SEG3') THEN
        IF (NBRDDL.NE.63) THEN
          CALL U2MESS('F','ELEMENTS4_41')
        END IF
      ELSE IF (NOMTE.EQ.'MET6SEG3') THEN
        IF (NBRDDL.NE.117) THEN
          CALL U2MESS('F','ELEMENTS4_41')
        END IF
      ELSE IF (NOMTE.EQ.'MET3SEG4') THEN
        IF (NBRDDL.NE.84) THEN
          CALL U2MESS('F','ELEMENTS4_41')
        END IF
      ELSE
        CALL U2MESS('F','ELEMENTS4_42')
      END IF

      IF (OPTION.EQ.'RIGI_MECA') THEN
        CALL TURIGI(NOMTE,NBRDDL,K)
      ELSE IF ((OPTION.EQ.'MASS_MECA').OR.(OPTION.EQ.'M_GAMMA')) THEN
        CALL TUMASS(NOMTE,NBRDDL,MASS)
      END IF

      IF (OPTION.EQ.'MASS_MECA') THEN
        CALL JEVECH('PMATUUR','E',IMASS)
C     DIMENSION DE LA MATRICE STOCKEE SOUS FORME VECTEUR
        NC = NBRDDL* (NBRDDL+1)/2
        CALL MAVEC(MASS,NBRDDL,ZR(IMASS),NC)
      ELSE IF (OPTION.EQ.'M_GAMMA') THEN
        CALL JEVECH('PDEPLAR','L',IACCE)
        CALL JEVECH('PVECTUR','E',IVECT)
        CALL PMAVEC('ZERO',NBRDDL,MASS,ZR(IACCE),ZR(IVECT))
      ENDIF

      END
