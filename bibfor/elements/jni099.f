      SUBROUTINE JNI099(ELREFE,NMAXOB,LIOBJ,NBOBJ)
      IMPLICIT NONE
      CHARACTER*8 ELREFE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/08/2003   AUTEUR JMBHH01 J.M.PROIX 
C RESPONSABLE VABHHTS J.PELLET
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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

C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN DECLARATIONS NORMALISEES  JEVEUX ---------------------

      INTEGER NBPG(10),LCARAC,NNO,IRET,ICARAC,LFF,LFFT
      INTEGER IFF,IFFT,IPOIDS,IVF,IDFDE,IDFDK,I,K,ITRAV
      INTEGER NDIM,NG1(10),IDFD2E,IDFD2K,IDFDEK,IAUX,NNOS
      INTEGER IFAP,NDERIV,NFF,II,ICOG
      INTEGER NSOMFA,NBFPG,NMAXOB,NBOBJ
      REAL*8 XSO(3),X(27),XG(2),XI(2)
      REAL*8 DFF(3,27)
      LOGICAL TRIA,QUAD
      CHARACTER*16 ELREFL
      CHARACTER*24 CARAC,FFO,TRAV,CHMAT1,CHMAT2,LIOBJ(NMAXOB)
C DEB -----------------------------------------------------------------
      TRIA = .FALSE.
      QUAD = .FALSE.

      ELREFL = ELREFE
      CARAC = '&INEL.'//ELREFE//'.CARAC'
      FFO = '&INEL.'//ELREFE//'.FF'
      TRAV = '&INEL.'//ELREFE//'.TRAVAIL'
      CHMAT1 = '&INEL.'//ELREFL//'.A'
      CHMAT2 = '&INEL.'//ELREFL//'.B'

      NBOBJ = 5
      CALL ASSERT(NMAXOB.GT.NBOBJ)
      LIOBJ(1) = CARAC
      LIOBJ(2) = FFO
      LIOBJ(3) = TRAV
      LIOBJ(4) = CHMAT1
      LIOBJ(5) = CHMAT2

      CALL JEEXIN(CARAC,IRET)
      IF (IRET.NE.0) GO TO 160


      DO 10 I = 1,10
        NBPG(I) = 0
   10 CONTINUE

      CALL CARREF(ELREFE,NDIM,NNO,NNOS,NBFPG,NBPG,X)
      IF (ELREFE.EQ.'TRIA3' .OR. ELREFE.EQ.'TRIA6' .OR.
     &    ELREFE.EQ.'TRIA3L' .OR. ELREFE.EQ.'TRIA3H' .OR.
     &    ELREFE.EQ.'TRIA6H' .OR. ELREFE.EQ.'TRIA6D' .OR.
     &    ELREFE.EQ.'TRIL6' .OR. ELREFE.EQ.'TRII3' .OR.
     &    ELREFE.EQ.'TRII6' .OR. ELREFE.EQ.'TRIA7') THEN

        TRIA = .TRUE.

      ELSE IF (ELREFE.EQ.'QUAD4' .OR. ELREFE.EQ.'QUAD8' .OR.
     &         ELREFE.EQ.'QUAD4L' .OR. ELREFE.EQ.'QUA8D' .OR.
     &         ELREFE.EQ.'QUAS8' .OR. ELREFE.EQ.'QUAI4' .OR.
     &         ELREFE.EQ.'QUAI8' .OR. ELREFE.EQ.'QUAD9' .OR.
     &         ELREFE .EQ. 'QUAS4') THEN

        QUAD = .TRUE.

      ELSE
        CALL ASSERT(.FALSE.)
      END IF



C     ------------------------------------------------------------------
C                 CAS DES TRIANGLES ET QUADRANGLES
C     ------------------------------------------------------------------
      IF (TRIA .OR. QUAD) THEN
C ------ ALLOCATIONS MEMOIRES AUGMENTEES POUR LES DERIVEES SECONDES
C         NNO*(133+3*9+... AU LIEU DE NNO*(133+...
        CALL WKVECT(TRAV,'G V R',NNO* (160+25*NNO),ITRAV)

        LCARAC = 6
        CALL WKVECT(CARAC,'G V I',LCARAC,ICARAC)

        ZI(ICARAC) = NNO
        ZI(ICARAC+1) = 4
        ZI(ICARAC+2) = NBPG(1)
        ZI(ICARAC+3) = NBPG(2)
        ZI(ICARAC+4) = NBPG(3)
        ZI(ICARAC+5) = NBPG(4)

C ------ ALLOCATIONS MEMOIRES AUGMENTEES POUR LES DERIVEES SECONDES
C         (EN QUADRATIQUE)
        IF ((NNO.EQ.6) .OR. (NNO.EQ.8) .OR. (NNO.EQ.9)) THEN
          LFF = (NBPG(1)+NBPG(2)+NBPG(3)+NBPG(4))* (1+NNO*3) +
     &          3*NBPG(1)*NNO
          CALL WKVECT(FFO,'G V R',LFF,IFF)
C --------- POINTEURS POUR LE CALCUL DES DERIVEES SECONDES
          IDFD2E = IFF + (NBPG(1)+NBPG(2)+NBPG(3)+NBPG(4))* (1+NNO*3)
          IDFD2K = IDFD2E + NBPG(1)*NNO
          IDFDEK = IDFD2K + NBPG(1)*NNO
        ELSE
          LFF = (NBPG(1)+NBPG(2)+NBPG(3)+NBPG(4))* (1+NNO*3)
          CALL WKVECT(FFO,'G V R',LFF,IFF)
        END IF
        LFFT = 2* (NBPG(1)+NBPG(2)+NBPG(3)+NBPG(4))
        CALL WKVECT('&&JNI099.FFT','V V R',LFFT,IFFT)

C=======================================================================
C TRAITEMENTS DES FAMILLES DE POINT DE GAUSS
C=======================================================================
        NSOMFA = 0
        DO 150 IFAP = 1,NBFPG
          IF (IFAP.EQ.1) THEN
            IPOIDS = IFF
            ICOG = IFFT
          ELSE
            IPOIDS = IDFDK + NBPG(IFAP-1)*NNO
            NSOMFA = NSOMFA + NBPG(IFAP-1)
            ICOG = IFFT + 2*NSOMFA
          END IF
          IVF = IPOIDS + NBPG(IFAP)
          IDFDE = IVF + NBPG(IFAP)*NNO
          IDFDK = IDFDE + NBPG(IFAP)*NNO


          IF (TRIA) THEN
C FAIRE ATTENTION DANS CES APPEL AUX CALCULS DE FF ET DFF
C A PRENDRE LA VALEUR EXACTE DES NBNO (DIMENSION DES VECTEURS
            IF (IFAP.NE.3) THEN
              CALL ELRFGF(ELREFE,IFAP,NBPG,27*3,ZR(ICOG),27,ZR(IPOIDS))
            ELSE IF (NBPG(3).EQ.3) THEN
              DO 20 K = 1,NBPG(3)
                ZR(IPOIDS+K-1) = 2.D0/3.D0
   20         CONTINUE
            ELSE IF (NBPG(3).EQ.6) THEN
              DO 30 K = 1,3
                ZR(IPOIDS+K-1) = 0.D0
   30         CONTINUE
              DO 40 K = 4,NBPG(3)
                ZR(IPOIDS+K-1) = 2.D0/3.D0
   40         CONTINUE
            ELSE IF (NBPG(3).EQ.7) THEN
              DO 50 K = 1,7
                ZR(IPOIDS+K-1) = 0.D0
   50         CONTINUE
            END IF
            DO 70 I = 1,NBPG(IFAP)
              K = NNO* (I-1)
              IAUX = ICOG + 2*I - 2
              XG(1) = ZR(IAUX)
              XG(2) = ZR(IAUX+1)
              XI(1) = X(NDIM* (I-1)+1)
              XI(2) = X(NDIM* (I-1)+2)
              IF (IFAP.EQ.3) THEN
                CALL ELRFVF(ELREFE,XI,NNO,ZR(IVF+K),NFF)
                CALL ELRFDF(ELREFE,XI,NNO*NDIM,DFF,NFF,NDERIV)
              ELSE
                CALL ELRFVF(ELREFE,XG,NNO,ZR(IVF+K),NFF)
                CALL ELRFDF(ELREFE,XG,NNO*NDIM,DFF,NFF,NDERIV)
              END IF
              DO 60 II = 1,NNO
                ZR(IDFDE+K+II-1) = DFF(1,II)
                ZR(IDFDK+K+II-1) = DFF(2,II)
   60         CONTINUE

C CALCUL DES DERIVEES SECONDES DES FONCTIONS DE FORME POUR TE0096
C AVEC OPTION CALCUL DE DG (TRIANGLE EN QUADRATIQUE).

              IF ((IFAP.EQ.1) .AND. (NNO.EQ.6)) CALL DVD2RT(NNO,XG(1),
     &            XG(2),ZR(IDFD2E+K),ZR(IDFD2K+K),ZR(IDFDEK+K))
   70       CONTINUE
          ELSE
            IF (IFAP.NE.3) THEN
              CALL ELRFGF(ELREFE,IFAP,NBPG,27*3,ZR(ICOG),27,ZR(IPOIDS))
            ELSE IF (NBPG(3).EQ.4) THEN
              DO 80 K = 1,NBPG(3)
                ZR(IPOIDS+K-1) = 1.D0
   80         CONTINUE
            ELSE IF (NBPG(3).EQ.8) THEN
              DO 90 K = 1,4
                ZR(IPOIDS+K-1) = -1.D0/3.D0
   90         CONTINUE
              DO 100 K = 5,NBPG(3)
                ZR(IPOIDS+K-1) = 4.D0/3.D0
  100         CONTINUE
            ELSE IF (NBPG(3).EQ.9) THEN
              DO 110 K = 1,4
                ZR(IPOIDS+K-1) = 1.D0/9.D0
  110         CONTINUE
              DO 120 K = 5,8
                ZR(IPOIDS+K-1) = 4.D0/9.D0
  120         CONTINUE
              ZR(IPOIDS+9-1) = 16.D0/9.D0
            END IF
            DO 140 I = 1,NBPG(IFAP)
              K = NNO* (I-1)
              IAUX = ICOG + 2*I - 2
              XG(1) = ZR(IAUX)
              XG(2) = ZR(IAUX+1)
              XI(1) = X(NDIM* (I-1)+1)
              XI(2) = X(NDIM* (I-1)+2)
              IF (IFAP.EQ.3) THEN
                CALL ELRFVF(ELREFE,XI,NNO,ZR(IVF+K),NFF)
                CALL ELRFDF(ELREFE,XI,NNO*NDIM,DFF,NFF,NDERIV)
              ELSE
                CALL ELRFVF(ELREFE,XG,NNO,ZR(IVF+K),NFF)
                CALL ELRFDF(ELREFE,XG,NNO*NDIM,DFF,NFF,NDERIV)
              END IF
              DO 130 II = 1,NNO
                ZR(IDFDE+K+II-1) = DFF(1,II)
                ZR(IDFDK+K+II-1) = DFF(2,II)
  130         CONTINUE

C CALCUL DES DERIVEES SECONDES DES FONCTIONS DE FORME POUR TE0096
C AVEC OPTION CALCUL DE DG (QUADRANGLE EN QUADRATIQUE).
              IF ((IFAP.EQ.1) .AND. ((NNO.EQ.8).OR.
     &            (NNO.EQ.9))) CALL DVD2RQ(NNO,XG(1),XG(2),ZR(IDFD2E+K),
     &            ZR(IDFD2K+K),ZR(IDFDEK+K))
  140       CONTINUE
          END IF
  150   CONTINUE

        NDIM = 2
        XSO(1) = 0.D+00
        XSO(2) = 0.D+00
        XSO(3) = 0.D+00
        NBFPG = 3
        NG1(1) = NBPG(1)
        NG1(2) = NBPG(2)


C ------ INITIALISATION DE LA MATRICE "MAGIQUE" DE PASSAGE DES
C        CONTRAINTES AUX POINTS DE GAUSS AUX CONTRAINTES AUX SOMMETS

        CALL INMAT2(NDIM,NNO,NNOS,NBFPG,ELREFE,XSO,NG1)

      ELSE
        CALL ASSERT(.FALSE.)
      END IF

      CALL JEDETR('&&JNI099.FFT')
  160 CONTINUE

      END
