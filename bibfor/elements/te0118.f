      SUBROUTINE TE0118(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C ----------------------------------------------------------------------
C  FONCTION REALISEE:
C     DANS LE CADRE X-FEM, PROPAGATION :
C    * CALCUL DE LA LONGUEUR DU PLUS PETIT ARETE DU MAILLAGE, C'EST A
C      DIRE LA VALEUR DE LA CONDITION CFL POUR LES PHASES DE
C      REINITIALISATION ET REORTHOGONALISATION
C    * CALCULS ELEMENTAIRES NECESSAIRES AUX PHASES DE REINITIALISATION
C     ET REORTHOGONALISATION (CF DOC R7.02.12)
C
C  OPTIONS
C  -------
C     'CFL_XFEM'
C      --------
C     CALCULE LA CONDITION CFL POUR LES PHASES DE REINITIALISATION ET
C     REORTHOGONALISATION
C        IN :  'PGEOMR'    GEOMETRIE
C        OUT : 'PLONCAR'   CHAM_ELEM
C
C     'MOY_NOEU_S'
C      ----------
C     CALCULE LE CHAM_ELEM CONTENANT LA MOYENNE ARITHMETIQUE AUX NOEUDS
C     SOMMETS D'UN CHAM_NO
C        IN :  'PNEUTR'    CHAM_NO
C        OUT : 'PMOYEL'    CHAM_ELEM
C
C     'XFEM_SMPLX_INIT'
C      ---------------
C     METHODE 'SIMPLEXE' : CALCULE LE CHAM_ELEM CONTENANT |T| DE
C     L'ELEMENT, ET LE CHAM_ELNO DES DIRECTIONS NI (CF DOC R)
C        IN :  'PGEOMR'    GEOMETRIE
C        OUT : 'PMEAST'    CHAM_ELEM
C              'PNIELNO'   CHAM_ELNO
C
C     'XFEM_SMPLX_REAC'
C      ---------------
C     METHODE 'SIMPLEXE' :  CALCULE LE CHAM_ELEM DELTA_PHI,
C                           ET LE CHAM_ELNO ALPHA (CF DOC R)
C        IN :  'PLSNO'     CHAM_NO
C              'PGRLS'     CHAM_NO
C              'PGRANDF'   CHAM_ELEM
C              'PNIELNO'   CHAM_ELNO
C        OUT : 'PDPHI'     CHAM_ELEM
C              'PALPHA'    CHAM_ELNO
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------

      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
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

      INTEGER     IADZI,IAZK24,IGEOM,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &            JGANO,I,J,INO,IMEAST,INI,ILSNO,IGRLS,IGDF,IDPHI,NBMA,
     &            IALPHA,INEUTR,IMOYEL,ILC,ADDIM,IADRMA,IBID,IRET,
     &            NDIME
      REAL*8      DFDX(27),DFDY(27),DFDZ(27),MEAST,NX,NY,NZ,NEUTR,JAC,
     &            GRADX,GRADY,GRADZ,NORMGR,K(8),DELPHI,DPHI(8),SIGMAK,
     &            SIGKFI,ALPHA(8),SMXDFI,DMIN,DISTIJ,XI,YI,ZPTI,XJ,YJ,
     &            ZJ,SIGMNI(3),NORM12,NORM14,NORM15,R8PREM,
     &            TOLENI
      CHARACTER*8 TYPMA,NOMA,NOMO,NOMAIL,K8BID
      CHARACTER*24 VALK(3)

      PARAMETER   (TOLENI=1.D-6)
      INTEGER      IARG

C DEBUT ----------------------------------------------------------------
      CALL JEMARQ()

C  VERIFICATION DU TYPE D'ELEMENT
      CALL TECAEL(IADZI,IAZK24)
      TYPMA=ZK24(IAZK24-1+3+ZI(IADZI-1+2)+3)
      NOMAIL= ZK24(IAZK24-1+3)(1:8)

C-----------------------------------------------------------------------
      IF (OPTION.EQ.'CFL_XFEM') THEN
C-----------------------------------------------------------------------
C  --------------------------------
C  RECUPERATION DES ENTREES/SORTIES
C  --------------------------------

         CALL JEVECH('PGEOMER','L',IGEOM)
         CALL JEVECH('PLONCAR','E',ILC)

         CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &               JGANO)
         CALL GETVID(' ','MODELE',1,IARG,1,NOMO,IBID)
C
C --- NOM DU MAILLAGE ATTACHE AU MODELE
C
         CALL JEVEUO(NOMO(1:8)//'.MODELE    .LGRF','L',IADRMA)
         NOMA  = ZK8(IADRMA)
         CALL DISMOI('F','DIM_GEOM',NOMA,'MAILLAGE',NDIME,K8BID,IRET)
         CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMA,K8BID,IBID)


         IF (NDIME.EQ.3) THEN
C  ----------------------------------------------------------
C  CALCUL DE LA PLUS PETITE DISTANCE ENTRE LES NOEUDS SOMMETS
C  ----------------------------------------------------------
           DMIN = SQRT((ZR(IGEOM-1+NDIM*(2-1)+1)-ZR(IGEOM-1+1))**2
     &                +(ZR(IGEOM-1+NDIM*(2-1)+2)-ZR(IGEOM-1+2))**2
     &                +(ZR(IGEOM-1+NDIM*(2-1)+3)-ZR(IGEOM-1+3))**2)

C     BOUCLE SUR LES NOEUDS SOMMETS
           DO 10 I=1,NNOS-1
              XI = ZR(IGEOM-1+NDIM*(I-1)+1)
              YI = ZR(IGEOM-1+NDIM*(I-1)+2)
              ZPTI = ZR(IGEOM-1+NDIM*(I-1)+3)

C     ON CHERCHE LE NOEUDS SOMMET LE PLUS PROCHE
              DO 20 J=I+1,NNOS
                 XJ = ZR(IGEOM-1+NDIM*(J-1)+1)
                 YJ = ZR(IGEOM-1+NDIM*(J-1)+2)
                 ZJ = ZR(IGEOM-1+NDIM*(J-1)+3)

                 DISTIJ = SQRT((XJ-XI)**2+(YJ-YI)**2+(ZJ-ZPTI)**2)
                 IF ((DISTIJ.LE.DMIN).AND.(DISTIJ.NE.0))  DMIN = DISTIJ

 20           CONTINUE

 10        CONTINUE

         ELSEIF (NDIME.EQ.2) THEN

C  ----------------------------------------------------------
C  CALCUL DE LA PLUS PETITE DISTANCE ENTRE LES NOEUDS SOMMETS
C  ----------------------------------------------------------
           DMIN = SQRT((ZR(IGEOM-1+NDIM*(2-1)+1)-ZR(IGEOM-1+1))**2
     &                +(ZR(IGEOM-1+NDIM*(2-1)+2)-ZR(IGEOM-1+2))**2)

C     BOUCLE SUR LES NOEUDS SOMMETS
           DO 30 I=1,NNOS-1
              XI = ZR(IGEOM-1+NDIM*(I-1)+1)
              YI = ZR(IGEOM-1+NDIM*(I-1)+2)

C     ON CHERCHE LE NOEUDS SOMMET LE PLUS PROCHE
              DO 40 J=I+1,NNOS
                 XJ = ZR(IGEOM-1+NDIM*(J-1)+1)
                 YJ = ZR(IGEOM-1+NDIM*(J-1)+2)
                 DISTIJ = SQRT((XJ-XI)**2+(YJ-YI)**2)
                 IF ((DISTIJ.LE.DMIN).AND.(DISTIJ.NE.0))  DMIN = DISTIJ

 40           CONTINUE

 30        CONTINUE
         ENDIF

         ZR(ILC) = DMIN

      ELSE
           IF (TYPMA(1:5).NE.'TETRA'.AND.TYPMA(1:4).NE.'HEXA'.AND.
     &       TYPMA(1:3).NE.'TRI'.AND.TYPMA(1:3).NE.'QUA') THEN
             VALK(1) = OPTION
             VALK(2) = NOMAIL
             VALK(3) = TYPMA
             CALL U2MESK('F','ELEMENTS3_19', 3 ,VALK)
           ENDIF
      ENDIF

C-----------------------------------------------------------------------
      IF (OPTION.EQ.'MOY_NOEU_S') THEN
C-----------------------------------------------------------------------

C  --------------------------------
C  RECUPERATION DES ENTREES/SORTIES
C  --------------------------------
         CALL JEVECH('PNEUTR','L',INEUTR)
         CALL JEVECH('PMOYEL','E',IMOYEL)

C  --------------------------
C  CALCUL DE GRANDF ET PETITF
C  --------------------------
         CALL ELREF4(' ','NOEU',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &               JGANO)
         NEUTR = 0.D0
         DO 50 INO=1,NNO
            NEUTR = NEUTR + ZR(INEUTR-1+INO)
 50      CONTINUE

         NEUTR = NEUTR / NNO
         ZR(IMOYEL) = NEUTR

C-----------------------------------------------------------------------
      ELSEIF (OPTION.EQ.'XFEM_SMPLX_INIT') THEN
C-----------------------------------------------------------------------

C  --------------------------------
C  RECUPERATION DES ENTREES/SORTIES
C  --------------------------------
         CALL JEVECH('PGEOMER','L',IGEOM)
         CALL JEVECH('PMEAST','E',IMEAST)
         CALL JEVECH('PNIELNO','E',INI)

         CALL ELREF4(' ','NOEU',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &               JGANO)
         CALL ASSERT(NNO.LE.8)


         CALL GETVID(' ','MODELE',1,IARG,1,NOMO,IBID)
C
C --- NOM DU MAILLAGE ATTACHE AU MODELE
C
         CALL JEVEUO(NOMO(1:8)//'.MODELE    .LGRF','L',IADRMA)
         NOMA  = ZK8(IADRMA)
         CALL JEVEUO(NOMA//'.DIME','L',ADDIM)
         NDIME=ZI(ADDIM-1+6)

C  --------------
C  CALCUL DE |T|
C  --------------

         IF (NDIME.EQ.3) THEN

           IF (TYPMA(1:5).EQ.'TETRA') THEN
            CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &                  JGANO)
            CALL ASSERT(NPG.EQ.1)
            CALL DFDM3D(NNO,1,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,DFDZ,
     &                  MEAST)
            CALL ELREF4(' ','NOEU',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &                  JGANO)

           ELSEIF (TYPMA(1:4).EQ.'HEXA') THEN
               NORM12=0.D0
               NORM14=0.D0
               NORM15=0.D0
               DO 115 I=1,3
                  NORM12=NORM12+(ZR(IGEOM-1+NDIM*(2-1)+I)
     &                           -ZR(IGEOM-1+NDIM*(1-1)+I))**2.D0
                  NORM14=NORM14+(ZR(IGEOM-1+NDIM*(4-1)+I)
     &                           -ZR(IGEOM-1+NDIM*(1-1)+I))**2.D0
                  NORM15=NORM15+(ZR(IGEOM-1+NDIM*(5-1)+I)
     &                           -ZR(IGEOM-1+NDIM*(1-1)+I))**2.D0
 115           CONTINUE
               NORM12=NORM12**.5D0
               NORM14=NORM14**.5D0
               NORM15=NORM15**.5D0
C       Volume elementaire
               MEAST = NORM12 * NORM14 * NORM15
           ENDIF

         ELSEIF (NDIME.EQ.2) THEN
           IF (TYPMA(1:3).EQ.'QUA') THEN
               NORM12=0.D0
               NORM14=0.D0
               DO 116 I=1,2
                  NORM12=NORM12+(ZR(IGEOM-1+NDIM*(2-1)+I)
     &                           -ZR(IGEOM-1+NDIM*(1-1)+I))**2.D0
                  NORM14=NORM14+(ZR(IGEOM-1+NDIM*(4-1)+I)
     &                           -ZR(IGEOM-1+NDIM*(1-1)+I))**2.D0
 116           CONTINUE
               NORM12=NORM12**.5D0
               NORM14=NORM14**.5D0
C       Surface elementaire
               MEAST = NORM12 * NORM14
           ELSEIF (TYPMA(1:3).EQ.'TRI') THEN
               CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,
     &                  IDFDE,JGANO)
C               CALL ASSERT(NPG.EQ.1)
               CALL DFDM2D(NNO,1,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,MEAST)
               CALL ELREF4(' ','NOEU',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,
     &                  IDFDE,JGANO)
           ENDIF
         ENDIF
C  -----------------------
C  CALCUL DE NI AUX NOEUDS
C  -----------------------
         DO 100 INO=1,NNO

          IF (NDIME.EQ.3) THEN
            CALL DFDM3D(NNO,INO,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,
     &                  DFDZ,JAC)
          ELSEIF (NDIME.EQ.2) THEN
            CALL DFDM2D(NNO,INO,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,
     &                  JAC)
          ENDIF

            IF (TYPMA(1:5).EQ.'TETRA') THEN
               ZR(INI-1+(INO-1)*3+1) = NDIM * MEAST * DFDX(INO)
               ZR(INI-1+(INO-1)*3+2) = NDIM * MEAST * DFDY(INO)
               ZR(INI-1+(INO-1)*3+3) = NDIM * MEAST * DFDZ(INO)

            ELSEIF (TYPMA(1:4).EQ.'HEXA') THEN
               ZR(INI-1+(INO-1)*3+1) = NDIM * MEAST * DFDX(INO)/4.D0
               ZR(INI-1+(INO-1)*3+2) = NDIM * MEAST * DFDY(INO)/4.D0
               ZR(INI-1+(INO-1)*3+3) = NDIM * MEAST * DFDZ(INO)/4.D0

            ELSEIF (TYPMA(1:3).EQ.'QUA') THEN
C   Rq : Pur les QUAD la derivee des FF en 0 vaut la moitie de celle au
C   Noeud considere. Il faudra penser a s'occuper des FF d'ordre 2!
               ZR(INI-1+(INO-1)*2+1) = NDIM * MEAST * DFDX(INO)/2.D0
               ZR(INI-1+(INO-1)*2+2) = NDIM * MEAST * DFDY(INO)/2.D0

            ELSEIF (TYPMA(1:3).EQ.'TRI') THEN
               ZR(INI-1+(INO-1)*2+1) = NDIM * MEAST * DFDX(INO)
               ZR(INI-1+(INO-1)*2+2) = NDIM * MEAST * DFDY(INO)

            ENDIF
 100     CONTINUE

C  VERIFICATION SIGMA(NI)=0
         IF (TYPMA(1:4).EQ.'HEXA') THEN
            DO 120 I=1,3
               SIGMNI(I)=0
               DO 130 INO=1,NNO
                  SIGMNI(I)=SIGMNI(I)+ZR(INI-1+(INO-1)*3+I)
 130           CONTINUE
               IF (ABS(SIGMNI(I)).GT.TOLENI) THEN
                  CALL U2MESK('F','ELEMENTS3_20',1,NOMAIL)
               ENDIF
 120        CONTINUE
         ENDIF

         ZR(IMEAST) = MEAST

C-----------------------------------------------------------------------
      ELSEIF (OPTION.EQ.'XFEM_SMPLX_CALC') THEN
C-----------------------------------------------------------------------

C  ------------------------
C  RECUPERATION DES ENTREES
C  ------------------------
         CALL JEVECH('PLSNO','L',ILSNO)
         CALL JEVECH('PGRLS','L',IGRLS)
         CALL JEVECH('PGRANDF','L',IGDF)
         CALL JEVECH('PNIELNO','L',INI)

         CALL ELREF4(' ','NOEU',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &               JGANO)
         CALL ASSERT(NNO.LE.8)
         CALL GETVID(' ','MODELE',1,IARG,1,NOMO,IBID)
C
C --- NOM DU MAILLAGE ATTACHE AU MODELE
C
         CALL JEVEUO(NOMO(1:8)//'.MODELE    .LGRF','L',IADRMA)
         NOMA  = ZK8(IADRMA)
         CALL JEVEUO(NOMA//'.DIME','L',ADDIM)
         NDIME=ZI(ADDIM-1+6)

C  ------------------------------------------
C  INITIALISATION DE DELTAPHI ET ALPHA(I) A 0
C  ------------------------------------------
         DELPHI=0.D0
         DO 200 INO=1,NNO
            ALPHA(INO)=0.D0
 200     CONTINUE

C  ---------------------
C  TRAITEMENT DU CAS F=0
C  ---------------------
C  CETTE PARTIE EST UTILE EN REINITIALISATION 3D, SI L'ON A DES ELEMENTS
C  AYANT AUTANT DE NOEUDS DE PART ET D'AUTRE DE L'ISOZERO EN L'ABSENCE
C  DE L'ALGORITHME DE CALCUL DIRECT DES DISTANCES

         IF (ZR(IGDF).EQ.0.D0) GOTO 850
C  --------------------------------------
C  CALCUL DU GRADIENT DE LS SUR L'ELEMENT
C  --------------------------------------
         GRADX = 0.D0
         GRADY = 0.D0
         GRADZ = 0.D0
         DO 300 INO=1,NNO
            GRADX = GRADX + ZR(IGRLS-1+(INO-1)*NDIM+1)/(NNO*4.D0)
            GRADY = GRADY + ZR(IGRLS-1+(INO-1)*NDIM+2)/(NNO*4.D0)
            IF (NDIM.EQ.3)
     &         GRADZ = GRADZ+ZR(IGRLS-1+(INO-1)*NDIM+3)/(NNO*4.D0)
 300     CONTINUE

C  CALCUL DE LA NORME DU GRADIENT
         NORMGR = (GRADX**2.D0 + GRADY**2.D0 + GRADZ**2.D0 )**0.5D0

         IF (ABS(NORMGR).LT.R8PREM()) GOTO 850

C  -----------------------
C  CALCUL DE KI AUX NOEUDS
C  -----------------------
         DO 400 INO=1,NNO

C  RECUPERATION DE LA DIRECTION NI AU NOEUD
            NX = ZR(INI-1+(INO-1)*NDIME+1)
            NY = ZR(INI-1+(INO-1)*NDIME+2)
            IF (NDIME.EQ.3) NZ = ZR(INI-1+(INO-1)*3+3)
            IF (NDIME.EQ.2) NZ = 0.D0


C  CALCUL DE KI AU NOEUD
           K(INO) = ZR(IGDF)*(GRADX*NX+GRADY*NY+GRADZ*NZ)/(NORMGR*NDIM)

 400     CONTINUE

C  ----------------------
C  CALCUL DE SIGMA(KI(-))
C  ----------------------
         SIGMAK = 0.D0
         DO 500 INO=1,NNO
            SIGMAK = SIGMAK + MIN(0.D0,K(INO))
 500     CONTINUE

         IF (ABS(SIGMAK).LT.R8PREM()) GOTO 850

C  ------------------------------------------------------
C  CALCUL DE DELTAPHI AUX NOEUDS ET DELTAPHI DE L'ELEMENT
C  ------------------------------------------------------
         DELPHI = 0.D0

         DO 600 INO=1,NNO

C  CALCUL DE SIGKFI = SIGMA( K(I)(-)*(LS(INO)-LS(I)) ) AUX NOEUDS
            SIGKFI = 0.D0
            DO 610 I=1,NNO
               SIGKFI = SIGKFI + MIN(0.D0,K(I)) *
     &                  ( ZR(ILSNO+INO-1) - ZR(ILSNO+I-1) )
 610        CONTINUE

            DPHI(INO) = MAX(0.D0,K(INO)) * SIGKFI / SIGMAK
            DELPHI = DELPHI + K(INO)*ZR(ILSNO+INO-1)

 600     CONTINUE

         IF (ABS(DELPHI).LT.R8PREM()) GOTO 850

C  --------------------------------------------------------
C  CALCUL DE SIGMA( MAX(0,DPHI(I)/DELPHI) ) SUR LES NOEUDS
C  --------------------------------------------------------
         SMXDFI = 0.D0
         DO 700 INO=1,NNO
            SMXDFI = SMXDFI + MAX(0.D0,DPHI(INO)/DELPHI)
 700     CONTINUE

         IF (ABS(SMXDFI).LT.R8PREM()) GOTO 850

C  --------------------------
C  CALCUL DE ALPHA AUX NOEUDS
C  --------------------------
         DO 800 INO=1,NNO
            ALPHA(INO) = MAX(0.D0,DPHI(INO)/DELPHI) / SMXDFI
 800     CONTINUE

 850     CONTINUE

C  --------------------
C  STOCKAGE DES SORTIES
C  --------------------
         CALL JEVECH('PDPHI','E',IDPHI)
         CALL JEVECH('PALPHA','E',IALPHA)

         ZR(IDPHI) = DELPHI

         DO 900 INO=1,NNO
            ZR(IALPHA-1+INO) = ALPHA(INO)

 900     CONTINUE

C-----------------------------------------------------------------------
      ENDIF
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
