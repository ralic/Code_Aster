      SUBROUTINE TE0279(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/02/2010   AUTEUR HAELEWYN J.HAELEWYN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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

C    - FONCTION REALISEE:  CALCUL DES MATRICES TANGENTES ELEMENTAIRES
C                          OPTION : 'MTAN_RIGI_MASS'
C                          ELEMENTS 3D ISO PARAMETRIQUES LUMPES

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


      REAL*8 RHOCP,LAMBDA,THETA,DELTAT,KHI,TPGI
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),POIDS,R8BID
      REAL*8 TPSEC, DIFF
      INTEGER IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER JGANO,NNO,KP,NPG,I,J,IJ,L,IMATTT,ITEMPS,IFON(3)
      INTEGER ISECHI,ISECHF
      INTEGER ICOMP,ITEMPI,NNOS,NDIM
      INTEGER NPG2,IPOID2,IVF2,IDFDE2
      LOGICAL LTEATT
C DEB ------------------------------------------------------------------
      IF ( (LTEATT(' ','LUMPE','OUI')).AND.
     &    (NOMTE(6:10).NE.'PYRAM')) THEN
         CALL ELREF4(' ','NOEU',NDIM,NNO,NNOS,NPG2,IPOID2,IVF2,
     &            IDFDE2,JGANO)
      ELSE
         CALL ELREF4(' ','MASS',NDIM,NNO,NNOS,NPG2,IPOID2,IVF2,
     &            IDFDE2,JGANO)
      ENDIF
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPEI','L',ITEMPI)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      CALL JEVECH('PMATTTR','E',IMATTT)

      DELTAT = ZR(ITEMPS+1)
      THETA = ZR(ITEMPS+2)
      KHI = ZR(ITEMPS+3)

      IF (ZK16(ICOMP) (1:5).EQ.'THER_') THEN

        CALL NTFCMA(ZI(IMATE),IFON)

        DO 40 KP = 1,NPG
          L = (KP-1)*NNO
          CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                  ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
          TPGI = 0.D0
          DO 10 I = 1,NNO
            TPGI = TPGI + ZR(ITEMPI+I-1)*ZR(IVF+L+I-1)
   10     CONTINUE
          CALL RCFODE(IFON(2),TPGI,LAMBDA,R8BID)

          DO 30 I = 1,NNO
CCDIR$ IVDEP
            DO 20 J = 1,I
              IJ = (I-1)*I/2 + J
              ZR(IMATTT+IJ-1) = ZR(IMATTT+IJ-1) +
     &                        POIDS*THETA*LAMBDA* (DFDX(I)*DFDX(J)+
     &                        DFDY(I)*DFDY(J)+DFDZ(I)*DFDZ(J))
   20       CONTINUE
   30     CONTINUE
   40   CONTINUE

        DO 80 KP = 1,NPG2
          L = (KP-1)*NNO
          CALL DFDM3D ( NNO, KP, IPOID2, IDFDE2,
     &                  ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
          TPGI = 0.D0
          DO 50 I = 1,NNO
            TPGI = TPGI + ZR(ITEMPI+I-1)*ZR(IVF2+L+I-1)
   50     CONTINUE
          CALL RCFODE(IFON(1),TPGI,R8BID,RHOCP)

          DO 70 I = 1,NNO
CCDIR$ IVDEP
            DO 60 J = 1,I
              IJ = (I-1)*I/2 + J
              ZR(IMATTT+IJ-1) = ZR(IMATTT+IJ-1) +
     &                        POIDS*KHI*RHOCP*ZR(IVF2+L+I-1)*
     &                        ZR(IVF2+L+J-1)/DELTAT
   60       CONTINUE
   70     CONTINUE
   80   CONTINUE
C
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
        END IF
        DO 90 KP = 1,NPG
          L = (KP-1)*NNO
          CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                  ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
          TPGI = 0.D0
          TPSEC = 0.D0
          DO 100 I = 1,NNO
            TPGI  = TPGI  + ZR(ITEMPI+I-1)*ZR(IVF+L+I-1)
            TPSEC = TPSEC + ZR(ISECHF+I-1)*ZR(IVF+L+I-1)
  100     CONTINUE
          CALL RCDIFF(ZI(IMATE),ZK16(ICOMP),TPSEC,TPGI,DIFF)
          DO 110 I = 1,NNO
C
            DO 120 J = 1,I
              IJ = (I-1)*I/2 + J
              ZR(IMATTT+IJ-1) = ZR(IMATTT+IJ-1) +
     &                          POIDS* (THETA*DIFF* (DFDX(I)*DFDX(J)+
     &                          DFDY(I)*DFDY(J)+DFDZ(I)*DFDZ(J)))
  120       CONTINUE
  110     CONTINUE
   90   CONTINUE
        DO 91 KP = 1,NPG2
          L = (KP-1)*NNO
          CALL DFDM3D ( NNO, KP, IPOID2, IDFDE2,
     &                  ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
          DO 111 I = 1,NNO
C
            DO 121 J = 1,I
              IJ = (I-1)*I/2 + J
              ZR(IMATTT+IJ-1) = ZR(IMATTT+IJ-1) +
     &                          POIDS*
     &                     (KHI*ZR(IVF2+L+I-1)*ZR(IVF2+L+J-1)/DELTAT)
  121       CONTINUE
  111     CONTINUE
   91   CONTINUE
C
      ENDIF

C FIN ------------------------------------------------------------------
      END
