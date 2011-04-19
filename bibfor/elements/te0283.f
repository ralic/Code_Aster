      SUBROUTINE TE0283(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT NONE
      CHARACTER*16 NOMTE,OPTION
C ----------------------------------------------------------------------

C    - FONCTION REALISEE:  CALCUL DES VECTEURS RESIDUS
C                          OPTION : 'RESI_RIGI_MASS'
C                          ELEMENTS 3D ISO PARAMETRIQUES

C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT

C THERMIQUE NON LINEAIRE

C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

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

C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      INTEGER ICODRE
      REAL*8 BETA,LAMBDA,THETA,DELTAT,KHI,TPG,TPGM
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),POIDS,R8BID,HYDRGM(27)
      REAL*8 DTPGDX,DTPGDY,DTPGDZ,RBID,CHAL,HYDRGP(27)
      REAL*8 TPSEC, DIFF,ERR
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER JGANO,NNO,KP,NPG1,I,ITEMPS,IFON(3),L,NDIM
      INTEGER IHYDR,IHYDRP,ITEMPR
      INTEGER ISECHI,ISECHF,JGANO2
      INTEGER ICOMP,ITEMPI,IVERES,NNOS
      INTEGER NPG2,IPOID2,IVF2,IDFDE2
      LOGICAL LTEATT
C ----------------------------------------------------------------------
C PARAMETER ASSOCIE AU MATERIAU CODE

C --- INDMAT : INDICE SAUVEGARDE POUR LE MATERIAU

CCC      PARAMETER        ( INDMAT = 8 )
C ----------------------------------------------------------------------

C DEB ------------------------------------------------------------------
      IF ( (LTEATT(' ','LUMPE','OUI')) .AND.
     &    (NOMTE(6:10).NE.'PYRAM')) THEN
         CALL ELREF4(' ','NOEU',NDIM,NNO,NNOS,NPG2,IPOID2,IVF2,
     &            IDFDE2,JGANO2)
      ELSE
         CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG2,IPOID2,IVF2,
     &            IDFDE2,JGANO2)
      ENDIF
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPEI','L',ITEMPI)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      CALL JEVECH('PRESIDU','E',IVERES)

      DELTAT = ZR(ITEMPS+1)
      THETA = ZR(ITEMPS+2)
      KHI = ZR(ITEMPS+3)

      IF (ZK16(ICOMP) (1:5).EQ.'THER_') THEN

        CALL NTFCMA(ZI(IMATE),IFON)
C----
C   INITIALISATION THER_HYDR
C----
        IF (ZK16(ICOMP) (1:9).EQ.'THER_HYDR') THEN
          CALL JEVECH('PHYDRPM','L',IHYDR)
          CALL JEVECH('PHYDRPP','E',IHYDRP)
          CALL JEVECH('PTEMPER','L',ITEMPR)
          CALL RCVALA(ZI(IMATE),' ','THER_HYDR',0,' ',R8BID,1,
     &              'CHALHYDR',  CHAL,ICODRE,1)
          DO 150 KP = 1,NPG2
             L = NNO*(KP-1)
             HYDRGM(KP)=0.D0
             DO 160 I = 1,NNO
                HYDRGM(KP)=HYDRGM(KP)+ZR(IHYDR)*ZR(IVF2+L+I-1)
 160         CONTINUE
 150      CONTINUE

        END IF

        DO 30 KP = 1,NPG1
          L = (KP-1)*NNO
          CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
          TPG = 0.D0
          DTPGDX = 0.D0
          DTPGDY = 0.D0
          DTPGDZ = 0.D0
          DO 10 I = 1,NNO
            TPG = TPG + ZR(ITEMPI+I-1)*ZR(IVF+L+I-1)
            DTPGDX = DTPGDX + ZR(ITEMPI+I-1)*DFDX(I)
            DTPGDY = DTPGDY + ZR(ITEMPI+I-1)*DFDY(I)
            DTPGDZ = DTPGDZ + ZR(ITEMPI+I-1)*DFDZ(I)
   10     CONTINUE

          CALL RCFODE(IFON(2),TPG,LAMBDA,RBID)

          DO 20 I = 1,NNO
            ZR(IVERES+I-1) = ZR(IVERES+I-1) +
     &                     POIDS*THETA*LAMBDA* (DFDX(I)*DTPGDX+
     &                     DFDY(I)*DTPGDY+DFDZ(I)*DTPGDZ)
   20     CONTINUE
   30   CONTINUE

        DO 60 KP = 1,NPG2
          L = (KP-1)*NNO
          CALL DFDM3D ( NNO, KP, IPOID2, IDFDE2,
     &                ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
          TPG = 0.D0
          DO 40 I = 1,NNO
            TPG = TPG + ZR(ITEMPI+I-1)*ZR(IVF2+L+I-1)
   40     CONTINUE
C ---  RESOLUTION DE L EQUATION D HYDRATATION

          IF (ZK16(ICOMP) (1:9).EQ.'THER_HYDR') THEN
            TPGM = 0.D0
            HYDRGP(KP)=0.D0
            DO 51 I = 1,NNO
              TPGM = TPGM + ZR(ITEMPR+I-1)*ZR(IVF2+L+I-1)
   51       CONTINUE
            CALL RUNGE6(IFON(3),DELTAT,TPG,TPGM,
     &                  HYDRGM(KP),HYDRGP(KP),ERR)
          ENDIF

          CALL RCFODE(IFON(1),TPG,BETA,RBID)
          IF (ZK16(ICOMP) (1:9).EQ.'THER_HYDR') THEN
C ---   THERMIQUE NON LINEAIRE AVEC HYDRATATION
            DO 61 I = 1,NNO
              ZR(IVERES+I-1) = ZR(IVERES+I-1) +
     &        POIDS* ((BETA-CHAL*HYDRGP(KP))/
     &        DELTAT*KHI*ZR(IVF2+L+I-1))
   61       CONTINUE
          ELSE
C ---   THERMIQUE NON LINEAIRE SEULE
          DO 50 I = 1,NNO
            ZR(IVERES+I-1) = ZR(IVERES+I-1) +
     &                   POIDS*BETA/DELTAT*KHI*ZR(IVF2+L+I-1)
   50     CONTINUE
         ENDIF
   60   CONTINUE

      ELSE IF (ZK16(ICOMP) (1:5).EQ.'SECH_') THEN

C --- SECHAGE

        IF (ZK16(ICOMP) (1:12).EQ.'SECH_GRANGER' .OR.
     &      ZK16(ICOMP) (1:10).EQ.'SECH_NAPPE') THEN
          CALL JEVECH('PTMPCHI','L',ISECHI)
          CALL JEVECH('PTMPCHF','L',ISECHF)
        ELSE
C          POUR LES AUTRES LOIS, PAS DE CHAMP DE TEMPERATURE
C          ISECHI ET ISECHF SONT FICTIFS
          ISECHI = ITEMPI
          ISECHF = ITEMPI
        ENDIF
        DO 70 KP = 1,NPG1
          L = (KP-1)*NNO
          CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                  ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
          TPG    = 0.D0
          DTPGDX = 0.D0
          DTPGDY = 0.D0
          DTPGDZ = 0.D0
          TPSEC  = 0.D0
          DO 80 I = 1,NNO
            TPG   = TPG   + ZR(ITEMPI+I-1)*ZR(IVF+L+I-1)
            TPSEC = TPSEC + ZR(ISECHF+I-1)*ZR(IVF+L+I-1)
            DTPGDX = DTPGDX + ZR(ITEMPI+I-1)*DFDX(I)
            DTPGDY = DTPGDY + ZR(ITEMPI+I-1)*DFDY(I)
            DTPGDZ = DTPGDZ + ZR(ITEMPI+I-1)*DFDZ(I)
   80     CONTINUE
          CALL RCDIFF(ZI(IMATE),ZK16(ICOMP),TPSEC,TPG,DIFF)
CCDIR$ IVDEP
          DO 90 I = 1,NNO
            ZR(IVERES+I-1) = ZR(IVERES+I-1) +
     &                       POIDS* (
     &                       THETA*DIFF* (DFDX(I)*DTPGDX+DFDY(I)*DTPGDY+
     &                       DFDZ(I)*DTPGDZ))
   90     CONTINUE
   70   CONTINUE
        DO 71 KP = 1,NPG2
          L = (KP-1)*NNO
          CALL DFDM3D ( NNO, KP, IPOID2, IDFDE2,
     &                  ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
          TPG    = 0.D0
          DO 81 I = 1,NNO
            TPG   = TPG   + ZR(ITEMPI+I-1)*ZR(IVF2+L+I-1)
   81     CONTINUE
CCDIR$ IVDEP
          DO 91 I = 1,NNO
            ZR(IVERES+I-1) = ZR(IVERES+I-1) +
     &                    POIDS* (1.D0/DELTAT*KHI*ZR(IVF2+L+I-1)*TPG)
   91     CONTINUE
   71   CONTINUE
      ENDIF
      IF (ZK16(ICOMP) (1:9).EQ.'THER_HYDR')
     &              CALL PPGAN2(JGANO2,1,HYDRGP,ZR(IHYDRP))
C FIN ------------------------------------------------------------------
      END
