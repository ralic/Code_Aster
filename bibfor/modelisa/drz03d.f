      SUBROUTINE DRZ03D(LISNOZ,LONLIS,CHARGZ,TYPLAZ,LISREZ,DMIN)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
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
C TOLE CRP_20
      CHARACTER*8 CHARGE
      CHARACTER*19 LISREL
      CHARACTER*24 LISNOE
      CHARACTER*(*) CHARGZ,LISNOZ,TYPLAZ,LISREZ
      REAL*8 DMIN
C -------------------------------------------------------
C     BLOCAGE DES DEPLACEMENTS RELATIFS D'UNE LISTE DE NOEUDS
C     SPECIFIEE PAR L'UTILISATEUR DANS LE CAS OU L' ON EST
C     EN 3D ET AUCUN  NOEUD NE PORTE LE DDL DRZ
C -------------------------------------------------------
C  LISNOE        - IN    - K24 - : NOM DE LA LISTE DES
C                -       -     -   NOEUDS A LIER
C -------------------------------------------------------
C  LONLIS        - IN    - I   - : LONGUEUR DE LA LISTE DES
C                -       -     -   NOEUDS A LIER
C -------------------------------------------------------
C  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
C                - JXIN  -      -
C -------------------------------------------------------
C TYPLAG         - IN    - K2  - : TYPE DES MULTIPLICATEURS DE LAGRANGE
C                                  ASSOCIES A LA RELATION :
C                              SI = '12'  LE PREMIER LAGRANGE EST AVANT
C                                         LE NOEUD PHYSIQUE
C                                         LE SECOND LAGRANGE EST APRES
C                              SI = '22'  LE PREMIER LAGRANGE EST APRES
C                                         LE NOEUD PHYSIQUE
C                                         LE SECOND LAGRANGE EST APRES
C -------------------------------------------------------
C  LISREL        - IN    - K19  - : NOM DE LA SD
C                - JXVAR -      -   LISTE DE RELATIONS
C -------------------------------------------------------
C  DMIN          - IN    - R8 - : LONGUEUR DE L APLUS PETITE ARRETE
C                                 DU MAILLAGE
C -------------------------------------------------------

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ------
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR,RPETIT
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ------

C --------- VARIABLES LOCALES ---------------------------
      PARAMETER (NMOCL=300)
      COMPLEX*16 BETAC
      CHARACTER*2 TYPLAG
      CHARACTER*4 TYPVAL,TYPCOE
      CHARACTER*8 BETAF,RESU
      CHARACTER*8 MOD,NOMG,NOMNOE,K8BID,NO1
      CHARACTER*8 NOMA,CMP,NOMCMP(NMOCL)
      CHARACTER*8 NOEUA,NOEUB,NOEUC,NOEUM
      CHARACTER*9 NOMTE
      CHARACTER*16 TYPE,OPER
      CHARACTER*19 LIGRMO
      INTEGER NTYPEL(NMOCL),DG
      INTEGER VALI(2)
      REAL*8 M1(3,3),MINV1(3,3),M2(3,12),M3(3,3)
      REAL*8 M4(3,12),M5(3,12)
      REAL*8 B(3),C(3),M(3),N(3),BN(3),CN(3)
      REAL*8 ML1(3,9),ML3(3,3),ML5(3,9)
      REAL*8 N1(3),N2(3),ABM(3)
      LOGICAL VERIF,EXISDG
      CHARACTER*1 K1BID
C --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
      CALL JEMARQ()
      CALL GETRES(RESU,TYPE,OPER)
      LISREL = LISREZ
      CHARGE = CHARGZ
      TYPLAG = TYPLAZ
      LISNOE = LISNOZ

C --- INITIALISATIONS

      BETAF = '&FOZERO'
      BETA = 0.0D0
      BETAC = (0.0D0,0.0D0)
      UN = 1.0D0
      ZERO = 0.0D0
      RPETIT = 1.D-6*DMIN

C --- MODELE ASSOCIE AU LIGREL DE CHARGE

      CALL DISMOI('F','NOM_MODELE',CHARGE(1:8),'CHARGE',IBID,MOD,IER)

C ---  LIGREL DU MODELE

      LIGRMO = MOD(1:8)//'.MODELE'

C --- MAILLAGE ASSOCIE AU MODELE

      CALL JEVEUO(LIGRMO//'.LGRF','L',JNOMA)
      NOMA = ZK8(JNOMA)

C --- TYPE DES VALEURS DES COEFFICIENTS DES RELATIONS

      TYPCOE = 'REEL'

C --- TYPE DES VALEURS AU SECOND MEMBRE DES RELATIONS

      IF (OPER(15:16).EQ.'_F') THEN
        TYPVAL = 'FONC'
      ELSE IF (OPER(15:16).EQ.'_C') THEN
        TYPVAL = 'COMP'
      ELSE IF (OPER(15:16).EQ.'  ') THEN
        TYPVAL = 'REEL'
      ELSE
        CALL ASSERT(.FALSE.)
      END IF

C --- RECUPERATION DES NOMS DES DDLS ET DES NUMEROS
C --- D'ELEMENTS DE LAGRANGE ASSOCIES

      NOMG = 'DEPL_R'
      NOMTE = 'D_DEPL_R_'

      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMG),'L',INOM)
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMG),'LONMAX',NBCMP,K1BID)
      NDDLA = NBCMP - 1
      IF (NDDLA.GT.NMOCL) THEN
        VALI (1) = NMOCL
        VALI (2) = NDDLA
        CALL U2MESG('F', 'MODELISA8_29',0,' ',2,VALI,0,0.D0)
      END IF
      DO 10 I = 1,NDDLA
        NOMCMP(I) = ZK8(INOM-1+I)
        CALL JENONU(JEXNOM('&CATA.TE.NOMTE',NOMTE//NOMCMP(I) (1:7)),
     &              NTYPEL(I))
   10 CONTINUE
      CALL DISMOI('F','NB_EC',NOMG,'GRANDEUR',NBEC,K8BID,IERD)

      IF (NBEC.GT.10) THEN
        CALL U2MESS('F','MODELISA_94')
      END IF

C --- CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION
C --- DE LISREL

C ---  MAJORANT DU NOMBRE DE TERMES DANS UNE RELATION
      NBTERM = 12
C ---  VECTEUR DU NOM DES NOEUDS
      CALL WKVECT('&&DRZ03D.LISNO','V V K8',NBTERM,JLISNO)
C ---  VECTEUR DU NOM DES DDLS
      CALL WKVECT('&&DRZ03D.LISDDL','V V K8',NBTERM,JLISDL)
C ---  VECTEUR DES COEFFICIENTS REELS
      CALL WKVECT('&&DRZ03D.COER','V V R',NBTERM,JLISCR)
C ---  VECTEUR DES COEFFICIENTS COMPLEXES
      CALL WKVECT('&&DRZ03D.COEC','V V C',NBTERM,JLISCC)
C ---  VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
      CALL WKVECT('&&DRZ03D.DIRECT','V V R',3*NBTERM,JLISDI)
C ---  VECTEUR DES DIMENSIONS DE CES DIRECTIONS
      CALL WKVECT('&&DRZ03D.DIME','V V I',NBTERM,JLISDM)

C --- RECUPERATION DU TABLEAU DES COORDONNEES

      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',JCOOR)

C --- ACQUISITION DE LA LISTE DES NOEUDS A LIER
C --- (CETTE LISTE EST NON REDONDANTE)

      CALL JEVEUO(LISNOE,'L',ILISNO)

C ---       INITIALISATIONS

      DO 30 I = 1,3
        DO 20 J = 1,12
          M2(I,J) = ZERO
          M4(I,J) = ZERO
          M5(I,J) = ZERO
   20   CONTINUE
   30 CONTINUE

      DO 50 I = 1,3
        DO 40 J = 1,3
          M1(I,J) = ZERO
          MINV1(I,J) = ZERO
          M3(I,J) = ZERO
   40   CONTINUE
   50 CONTINUE

C ---  RECHERCHE DE NOEUDS A, B, C FORMANT UN TRIANGLE
C ---  DE SURFACE NON-NULLE.
C ---  ON PREND POUR A LE PREMIER NOEUD
C ---           POUR B LE PREMIER NOEUD DISTINCT DE A
C ---           POUR C LE PREMIER NOEUD DISTINCT DE A ET B
C ---                  ET NON COLINEAIRE A AB.

      NOEUA = ZK8(ILISNO+1-1)
      CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+1-1)),INOA)
      ITRIAN = 0
      DO 70 J = 2,LONLIS

        CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+J-1)),INOB)

        B(1) = ZR(JCOOR-1+3* (INOB-1)+1) - ZR(JCOOR-1+3* (INOA-1)+1)
        B(2) = ZR(JCOOR-1+3* (INOB-1)+2) - ZR(JCOOR-1+3* (INOA-1)+2)
        B(3) = ZR(JCOOR-1+3* (INOB-1)+3) - ZR(JCOOR-1+3* (INOA-1)+3)

        J2 = J
        IF (SQRT(B(1)*B(1)+B(2)*B(2)+B(3)*B(3)).GT.RPETIT) THEN

          NOEUB = ZK8(ILISNO+J2-1)
          DO 60 K = J2 + 1,LONLIS

            CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+K-1)),INOC)

            C(1) = ZR(JCOOR-1+3* (INOC-1)+1) - ZR(JCOOR-1+3* (INOA-1)+1)
            C(2) = ZR(JCOOR-1+3* (INOC-1)+2) - ZR(JCOOR-1+3* (INOA-1)+2)
            C(3) = ZR(JCOOR-1+3* (INOC-1)+3) - ZR(JCOOR-1+3* (INOA-1)+3)

C ---    CALCUL DE N = B X C

            CALL PROVEC(B,C,N)
            IF (SQRT(N(1)*N(1)+N(2)*N(2)+N(3)*N(3)).LT.RPETIT) THEN
              GO TO 60
            ELSE
              K2 = K
              NOEUC = ZK8(ILISNO+K2-1)
              ITRIAN = 1
              GO TO 80
            END IF
   60     CONTINUE
C ---  FIN DU TEST SUR LA NORME DE B
        END IF
   70 CONTINUE

   80 CONTINUE

C ---  CAS OU L'ON A PU TOUVER 3 NOEUDS FORMANT UN TRIANGLE
C ---  DE SURFACE NON NULLE

      IF (ITRIAN.EQ.1) THEN

C ---  CALCUL DE BN = B X N

        CALL PROVEC(B,N,BN)

C ---     CALCUL DE CN = C X N

        CALL PROVEC(C,N,CN)

C ---     DEFINITION DE M1

        M1(1,1) = BN(1)
        M1(1,2) = BN(2)
        M1(1,3) = BN(3)
        M1(2,1) = CN(1)
        M1(2,2) = CN(2)
        M1(2,3) = CN(3)
        M1(3,1) = N(1)
        M1(3,2) = N(2)
        M1(3,3) = N(3)

C ---     DEFINITION DE M2

        M2(1,1) = -N(1)
        M2(1,2) = -N(2)
        M2(1,3) = -N(3)
        M2(1,4) = N(1)
        M2(1,5) = N(2)
        M2(1,6) = N(3)
        M2(2,1) = -N(1)
        M2(2,2) = -N(2)
        M2(2,3) = -N(3)
        M2(2,7) = N(1)
        M2(2,8) = N(2)
        M2(2,9) = N(3)
        M2(3,1) = -C(1)
        M2(3,2) = -C(2)
        M2(3,3) = -C(3)
        M2(3,4) = C(1)
        M2(3,5) = C(2)
        M2(3,6) = C(3)

C ---     DEFINITION DE M4

        M4(1,1) = -UN
        M4(2,2) = -UN
        M4(3,3) = -UN
        M4(1,10) = UN
        M4(2,11) = UN
        M4(3,12) = UN

C ---     INVERSION DE M1

        CALL INVMA3(M1,MINV1)

C ---     PREMIERE RELATION POUR LES NOEUDS B ET C :
C ---     (UB-UA).B = 0

        NBTERM = 6

        ZK8(JLISNO+1-1) = NOEUB
        ZK8(JLISNO+2-1) = NOEUA
        ZK8(JLISNO+3-1) = NOEUB
        ZK8(JLISNO+4-1) = NOEUA
        ZK8(JLISNO+5-1) = NOEUB
        ZK8(JLISNO+6-1) = NOEUA

        ZK8(JLISDL+1-1) = 'DX'
        ZK8(JLISDL+2-1) = 'DX'
        ZK8(JLISDL+3-1) = 'DY'
        ZK8(JLISDL+4-1) = 'DY'
        ZK8(JLISDL+5-1) = 'DZ'
        ZK8(JLISDL+6-1) = 'DZ'

        ZR(JLISCR+1-1) = B(1)
        ZR(JLISCR+2-1) = -B(1)
        ZR(JLISCR+3-1) = B(2)
        ZR(JLISCR+4-1) = -B(2)
        ZR(JLISCR+5-1) = B(3)
        ZR(JLISCR+6-1) = -B(3)

        CALL AFRELA(ZR(JLISCR),ZC(JLISCC),ZK8(JLISDL),ZK8(JLISNO),
     &              ZI(JLISDM),ZR(JLISDI),NBTERM,BETA,BETAC,BETAF,
     &              TYPCOE,TYPVAL,TYPLAG,0.D0,LISREL)

C ---     DEUXIEME RELATION POUR LES NOEUDS B ET C :
C ---     (UC-UA).C = 0

        NBTERM = 6

        ZK8(JLISNO+1-1) = NOEUC
        ZK8(JLISNO+2-1) = NOEUA
        ZK8(JLISNO+3-1) = NOEUC
        ZK8(JLISNO+4-1) = NOEUA
        ZK8(JLISNO+5-1) = NOEUC
        ZK8(JLISNO+6-1) = NOEUA

        ZR(JLISCR+1-1) = C(1)
        ZR(JLISCR+2-1) = -C(1)
        ZR(JLISCR+3-1) = C(2)
        ZR(JLISCR+4-1) = -C(2)
        ZR(JLISCR+5-1) = C(3)
        ZR(JLISCR+6-1) = -C(3)

        CALL AFRELA(ZR(JLISCR),ZC(JLISCC),ZK8(JLISDL),ZK8(JLISNO),
     &              ZI(JLISDM),ZR(JLISDI),NBTERM,BETA,BETAC,BETAF,
     &              TYPCOE,TYPVAL,TYPLAG,0.D0,LISREL)

C ---     TROISIEME RELATION POUR LES NOEUDS B ET C :
C ---     (UB-UA).C + (UC-UA).B = 0

        NBTERM = 9

        ZK8(JLISNO+1-1) = NOEUA
        ZK8(JLISNO+2-1) = NOEUB
        ZK8(JLISNO+3-1) = NOEUC
        ZK8(JLISNO+4-1) = NOEUA
        ZK8(JLISNO+5-1) = NOEUB
        ZK8(JLISNO+6-1) = NOEUC
        ZK8(JLISNO+7-1) = NOEUA
        ZK8(JLISNO+8-1) = NOEUB
        ZK8(JLISNO+9-1) = NOEUC

        ZK8(JLISDL+1-1) = 'DX'
        ZK8(JLISDL+2-1) = 'DX'
        ZK8(JLISDL+3-1) = 'DX'
        ZK8(JLISDL+4-1) = 'DY'
        ZK8(JLISDL+5-1) = 'DY'
        ZK8(JLISDL+6-1) = 'DY'
        ZK8(JLISDL+7-1) = 'DZ'
        ZK8(JLISDL+8-1) = 'DZ'
        ZK8(JLISDL+9-1) = 'DZ'

        ZR(JLISCR+1-1) = -B(1) - C(1)
        ZR(JLISCR+2-1) = C(1)
        ZR(JLISCR+3-1) = B(1)
        ZR(JLISCR+4-1) = -B(2) - C(2)
        ZR(JLISCR+5-1) = C(2)
        ZR(JLISCR+6-1) = B(2)
        ZR(JLISCR+7-1) = -B(3) - C(3)
        ZR(JLISCR+8-1) = C(3)
        ZR(JLISCR+9-1) = B(3)

        CALL AFRELA(ZR(JLISCR),ZC(JLISCC),ZK8(JLISDL),ZK8(JLISNO),
     &              ZI(JLISDM),ZR(JLISDI),NBTERM,BETA,BETAC,BETAF,
     &              TYPCOE,TYPVAL,TYPLAG,0.D0,LISREL)

        DO 150 J = 2,LONLIS

          CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+J-1)),INOEM)

          IF (INOEM.EQ.INOB .OR. INOEM.EQ.INOC) GO TO 150
          M(1) = ZR(JCOOR-1+3* (INOEM-1)+1) - ZR(JCOOR-1+3* (INOA-1)+1)
          M(2) = ZR(JCOOR-1+3* (INOEM-1)+2) - ZR(JCOOR-1+3* (INOA-1)+2)
          M(3) = ZR(JCOOR-1+3* (INOEM-1)+3) - ZR(JCOOR-1+3* (INOA-1)+3)

C ---        DEFINITION DE M3

          M3(1,2) = -M(3)
          M3(1,3) = M(2)
          M3(2,1) = M(3)
          M3(2,3) = -M(1)
          M3(3,1) = -M(2)
          M3(3,2) = M(1)

C ---        CALCUL DE M1 <-- M3.MINV1

          CALL PMAT(3,M3,MINV1,M1)

C ---        CALCUL DE M5 <-- M3.MINV1.M2

          CALL PMPPR(M1,3,3,1,M2,3,12,1,M5,3,12)

C ---        CALCUL DE M5 <-- M4 + M3.MINV1.M2

          DO 100 J1 = 1,3
            DO 90 J2 = 1,12
              M5(J1,J2) = M5(J1,J2) + M4(J1,J2)
   90       CONTINUE
  100     CONTINUE

C ---     ECRITURE DES 3 RELATIONS CORRESPONDANTES A M5.UABCM = 0

          NBTERM = 12

          DO 110 K = 1,3
            ZK8(JLISNO+K-1) = NOEUA
            ZK8(JLISNO+3+K-1) = NOEUB
            ZK8(JLISNO+6+K-1) = NOEUC
            ZK8(JLISNO+9+K-1) = ZK8(ILISNO+J-1)
  110     CONTINUE

          DO 120 K = 1,4
            ZK8(JLISDL+3* (K-1)+1-1) = 'DX'
            ZK8(JLISDL+3* (K-1)+2-1) = 'DY'
            ZK8(JLISDL+3* (K-1)+3-1) = 'DZ'
  120     CONTINUE

          DO 140 J1 = 1,3
            DO 130 J2 = 1,12
              ZR(JLISCR+J2-1) = M5(J1,J2)
  130       CONTINUE

            CALL AFRELA(ZR(JLISCR),ZC(JLISCC),ZK8(JLISDL),ZK8(JLISNO),
     &                  ZI(JLISDM),ZR(JLISDI),NBTERM,BETA,BETAC,BETAF,
     &                  TYPCOE,TYPVAL,TYPLAG,0.D0,LISREL)
  140     CONTINUE
C ---     FIN DE LA BOUCLE SUR LES NOEUDS DE LA LISTE
  150   CONTINUE

C ---   ANALYSE DU CAS OU L'ON N'A PAS PU TROUVER 3 NOEUDS DE LA
C ---   LISTE FORMANT UN TRIANGLE

      ELSE IF (ITRIAN.EQ.0) THEN

        X1 = ZR(JCOOR-1+3* (INOA-1)+1)
        Y1 = ZR(JCOOR-1+3* (INOA-1)+2)
        Z1 = ZR(JCOOR-1+3* (INOA-1)+3)

        IALIGN = 0

        DO 160 I = 2,LONLIS
          CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+I-1)),INOI)
          XI = ZR(JCOOR-1+3* (INOI-1)+1)
          YI = ZR(JCOOR-1+3* (INOI-1)+2)
          ZIJ = ZR(JCOOR-1+3* (INOI-1)+3)
          IF ((ABS(XI-X1).GT.RPETIT) .OR. (ABS(YI-Y1).GT.RPETIT) .OR.
     &        (ABS(ZIJ-Z1).GT.RPETIT)) THEN
            IALIGN = 1
            GO TO 200
          END IF
  160   CONTINUE

C ---   CAS OU TOUS LES NOEUDS DE LA LISTE ONT LES MEMES COORDONNEES

C ---     PREMIERE RELATION
C ---     DX(M) -DX(A) = 0

        NBTERM = 2
        ZK8(JLISNO+1-1) = ZK8(ILISNO+1-1)
        ZK8(JLISDL+1-1) = 'DX'
        ZR(JLISCR+1-1) = UN

        DO 170 I = 2,LONLIS
          ZK8(JLISNO+2-1) = ZK8(ILISNO+I-1)
          ZK8(JLISDL+2-1) = 'DX'
          ZR(JLISCR+2-1) = -UN

          CALL AFRELA(ZR(JLISCR),ZC(JLISCC),ZK8(JLISDL),ZK8(JLISNO),
     &                ZI(JLISDM),ZR(JLISDI),NBTERM,BETA,BETAC,BETAF,
     &                TYPCOE,TYPVAL,TYPLAG,0.D0,LISREL)
  170   CONTINUE

C ---     DEUXIEME RELATION
C ---     DY(M) -DY(A) = 0

        ZK8(JLISDL+1-1) = 'DY'

        DO 180 I = 2,LONLIS
          ZK8(JLISNO+2-1) = ZK8(ILISNO+I-1)
          ZK8(JLISDL+2-1) = 'DY'

          CALL AFRELA(ZR(JLISCR),ZC(JLISCC),ZK8(JLISDL),ZK8(JLISNO),
     &                ZI(JLISDM),ZR(JLISDI),NBTERM,BETA,BETAC,BETAF,
     &                TYPCOE,TYPVAL,TYPLAG,0.D0,LISREL)
  180   CONTINUE

C ---     TROISIEME RELATION
C ---     DZ(M) -DZ(A) = 0

        ZK8(JLISDL+1-1) = 'DZ'

        DO 190 I = 2,LONLIS
          ZK8(JLISNO+2-1) = ZK8(ILISNO+I-1)
          ZK8(JLISDL+2-1) = 'DZ'

          CALL AFRELA(ZR(JLISCR),ZC(JLISCC),ZK8(JLISDL),ZK8(JLISNO),
     &                ZI(JLISDM),ZR(JLISDI),NBTERM,BETA,BETAC,BETAF,
     &                TYPCOE,TYPVAL,TYPLAG,0.D0,LISREL)
  190   CONTINUE

  200   CONTINUE

C ---    CAS OU TOUS LES NOEUDS SONT ALIGNES

        IF (IALIGN.EQ.1) THEN

C ---           CAS OU L'ON N'A QUE 2 POINTS, LA SEULE RELATION EST :
C ---           (UB-UA).B = 0

          IF (LONLIS.EQ.2) THEN

            NBTERM = 6

            ZK8(JLISNO+1-1) = NOEUB
            ZK8(JLISNO+2-1) = NOEUA
            ZK8(JLISNO+3-1) = NOEUB
            ZK8(JLISNO+4-1) = NOEUA
            ZK8(JLISNO+5-1) = NOEUB
            ZK8(JLISNO+6-1) = NOEUA

            ZK8(JLISDL+1-1) = 'DX'
            ZK8(JLISDL+2-1) = 'DX'
            ZK8(JLISDL+3-1) = 'DY'
            ZK8(JLISDL+4-1) = 'DY'
            ZK8(JLISDL+5-1) = 'DZ'
            ZK8(JLISDL+6-1) = 'DZ'

            ZR(JLISCR+1-1) = B(1)
            ZR(JLISCR+2-1) = -B(1)
            ZR(JLISCR+3-1) = B(2)
            ZR(JLISCR+4-1) = -B(2)
            ZR(JLISCR+5-1) = B(3)
            ZR(JLISCR+6-1) = -B(3)

            CALL AFRELA(ZR(JLISCR),ZC(JLISCC),ZK8(JLISDL),ZK8(JLISNO),
     &                  ZI(JLISDM),ZR(JLISDI),NBTERM,BETA,BETAC,BETAF,
     &                  TYPCOE,TYPVAL,TYPLAG,0.D0,LISREL)
          ELSE IF (LONLIS.GT.2) THEN

C ---               INITIALISATIONS

            DO 220 I = 1,3
              DO 210 J = 1,9
                ML1(I,J) = ZERO
                ML5(I,J) = ZERO
  210         CONTINUE
  220       CONTINUE

            DO 240 I = 1,3
              DO 230 J = 1,3
                ML3(I,J) = ZERO
  230         CONTINUE
  240       CONTINUE

            DO 250 I = 2,LONLIS
              CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+I-1)),INOB)
              B(1) = ZR(JCOOR-1+3* (INOB-1)+1) -
     &               ZR(JCOOR-1+3* (INOA-1)+1)
              B(2) = ZR(JCOOR-1+3* (INOB-1)+2) -
     &               ZR(JCOOR-1+3* (INOA-1)+2)
              B(3) = ZR(JCOOR-1+3* (INOB-1)+3) -
     &               ZR(JCOOR-1+3* (INOA-1)+3)
              IF (SQRT(B(1)*B(1)+B(2)*B(2)+B(3)*B(3)).GT.RPETIT) THEN
                I2 = I
                INO2 = INOB
                NOEUB = ZK8(ILISNO+I2-1)
                GO TO 260
              END IF
  250       CONTINUE

  260       CONTINUE

C ---              DEFINITION DU VECTEUR N1
C ---              ON ESSAIE D'ABORD N1 = I X B

            N1(1) = ZERO
            N1(2) = -B(3)
            N1(3) = B(2)

            IF (SQRT(B(3)*B(3)+B(2)*B(2)).LT.RPETIT) THEN

C ---                ON FAIT UN AUTRE ESSAI AVEC N1 = J X B

              N1(1) = B(3)
              N1(2) = ZERO
              N1(3) = -B(1)

              IF (SQRT(B(3)*B(3)+B(1)*B(1)).LT.RPETIT) THEN
                CALL U2MESS('F','MODELISA4_41')
              END IF
            END IF

C ---              DEFINITION DU VECTEUR N2 = B X N1

            N2(1) = B(2)*N1(3) - B(3)*N1(2)
            N2(2) = B(3)*N1(1) - B(1)*N1(3)
            N2(3) = B(1)*N1(2) - B(2)*N1(1)

C ---              DEFINITION DE ML1

            ML1(1,2) = N2(2)*N1(1) - N1(2)*N2(1)
            ML1(1,3) = N2(3)*N1(1) - N1(3)*N2(1)
            ML1(2,1) = N2(1)*N1(2) - N1(1)*N2(2)
            ML1(2,3) = N2(3)*N1(2) - N1(3)*N2(2)
            ML1(3,1) = N2(1)*N1(3) - N1(1)*N2(3)
            ML1(3,2) = N2(2)*N1(3) - N1(2)*N2(3)
            ML1(1,5) = -ML1(1,2)
            ML1(1,6) = -ML1(1,3)
            ML1(2,4) = -ML1(2,1)
            ML1(2,6) = -ML1(2,3)
            ML1(3,4) = -ML1(3,1)
            ML1(3,5) = -ML1(3,2)

C ---              ON VERIFIE QUE LES NOEUDS DE LA LISTE SONT
C ---              EFFECTIVEMENT ALIGNES

            DO 270 I = 2,LONLIS
              CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+I-1)),INOEM)

              M(1) = ZR(JCOOR-1+3* (INOEM-1)+1) -
     &               ZR(JCOOR-1+3* (INOA-1)+1)
              M(2) = ZR(JCOOR-1+3* (INOEM-1)+2) -
     &               ZR(JCOOR-1+3* (INOA-1)+2)
              M(3) = ZR(JCOOR-1+3* (INOEM-1)+3) -
     &               ZR(JCOOR-1+3* (INOA-1)+3)


C ---                 DEFINITION DU VECTEUR ABM = AB X AM

              ABM(1) = B(2)*M(3) - B(3)*M(2)
              ABM(2) = B(3)*M(1) - B(1)*M(3)
              ABM(3) = B(1)*M(2) - B(2)*M(1)

              IF (SQRT(ABM(1)*ABM(1)+ABM(2)*ABM(2)+ABM(3)*ABM(3)).GT.
     &            RPETIT) THEN
                CALL U2MESS('F','MODELISA4_42')
              END IF
  270       CONTINUE

C ---           PREMIERE RELATION  : (UB-UA).B = 0

            NBTERM = 6

            ZK8(JLISNO+1-1) = NOEUB
            ZK8(JLISNO+2-1) = NOEUA
            ZK8(JLISNO+3-1) = NOEUB
            ZK8(JLISNO+4-1) = NOEUA
            ZK8(JLISNO+5-1) = NOEUB
            ZK8(JLISNO+6-1) = NOEUA

            ZK8(JLISDL+1-1) = 'DX'
            ZK8(JLISDL+2-1) = 'DX'
            ZK8(JLISDL+3-1) = 'DY'
            ZK8(JLISDL+4-1) = 'DY'
            ZK8(JLISDL+5-1) = 'DZ'
            ZK8(JLISDL+6-1) = 'DZ'

            ZR(JLISCR+1-1) = B(1)
            ZR(JLISCR+2-1) = -B(1)
            ZR(JLISCR+3-1) = B(2)
            ZR(JLISCR+4-1) = -B(2)
            ZR(JLISCR+5-1) = B(3)
            ZR(JLISCR+6-1) = -B(3)

            CALL AFRELA(ZR(JLISCR),ZC(JLISCC),ZK8(JLISDL),ZK8(JLISNO),
     &                  ZI(JLISDM),ZR(JLISDI),NBTERM,BETA,BETAC,BETAF,
     &                  TYPCOE,TYPVAL,TYPLAG,0.D0,LISREL)

C ---              CALCUL DU COEFFICIENT K

            COEK = (N1(1)*N1(1)+N1(2)*N1(2)+N1(3)*N1(3))*
     &             (B(1)*B(1)+B(2)*B(2)+B(3)*B(3))
            COEK1 = 1.0D0/COEK

            DO 330 I = 2,LONLIS
              CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+I-1)),INOEM)
              IF (INOEM.EQ.INO2) GO TO 330

              M(1) = ZR(JCOOR-1+3* (INOEM-1)+1) -
     &               ZR(JCOOR-1+3* (INOA-1)+1)
              M(2) = ZR(JCOOR-1+3* (INOEM-1)+2) -
     &               ZR(JCOOR-1+3* (INOA-1)+2)
              M(3) = ZR(JCOOR-1+3* (INOEM-1)+3) -
     &               ZR(JCOOR-1+3* (INOA-1)+3)

C ---                 DEFINITION DE ML3

              ML3(1,2) = -M(3)
              ML3(1,3) = M(2)
              ML3(2,1) = M(3)
              ML3(2,3) = -M(1)
              ML3(3,1) = -M(2)
              ML3(3,2) = M(1)

C ---                 CALCUL DE ML5 <-- ML3.ML1

              CALL PMPPR(ML3,3,3,1,ML1,3,9,1,ML5,3,9)

              DO 290 J1 = 1,3
                DO 280 J2 = 1,9
                  ML5(J1,J2) = COEK1*ML5(J1,J2)
  280           CONTINUE
  290         CONTINUE

C ---                 RAJOUT DE ML4

              ML5(1,1) = ML5(1,1) - UN
              ML5(2,2) = ML5(2,2) - UN
              ML5(3,3) = ML5(3,3) - UN
              ML5(1,7) = ML5(1,7) + UN
              ML5(2,8) = ML5(2,8) + UN
              ML5(3,9) = ML5(3,9) + UN


C ---     ECRITURE DES 3 RELATIONS CORRESPONDANTES A LA RELATION
C ---     MATRICIELLE : (ML4+1/K*ML3*ML1)*UABM = 0

              NBTERM = 9

              DO 300 K = 1,3

                ZK8(JLISNO+K-1) = NOEUA
                ZK8(JLISNO+3+K-1) = NOEUB
                ZK8(JLISNO+6+K-1) = ZK8(ILISNO+I-1)

                ZK8(JLISDL+3* (K-1)+1-1) = 'DX'
                ZK8(JLISDL+3* (K-1)+2-1) = 'DY'
                ZK8(JLISDL+3* (K-1)+3-1) = 'DZ'
  300         CONTINUE

              DO 320 J1 = 1,3
                DO 310 J2 = 1,9
                  ZR(JLISCR+J2-1) = ML5(J1,J2)
  310           CONTINUE

                CALL AFRELA(ZR(JLISCR),ZC(JLISCC),ZK8(JLISDL),
     &                      ZK8(JLISNO),ZI(JLISDM),ZR(JLISDI),NBTERM,
     &                      BETA,BETAC,BETAF,TYPCOE,TYPVAL,TYPLAG,0.D0,
     &                      LISREL)
  320         CONTINUE
  330       CONTINUE
C ---           FIN DU CAS LONLIS GT 2
          END IF
C ---         FIN DU CAS OU LES NOEUDS SONT ALIGNES
        END IF
C ---       FIN DU CAS OU L'ON NE PEUT PAS TROUVER 3 NOEUDS FORMANT
C ---       UN TRIANGLE
      END IF

C --- DESTRUCTION DES OBJETS DE TRAVAIL

      CALL JEDETR('&&DRZ03D.LISNO')
      CALL JEDETR('&&DRZ03D.LISDDL')
      CALL JEDETR('&&DRZ03D.COER')
      CALL JEDETR('&&DRZ03D.COEC')
      CALL JEDETR('&&DRZ03D.DIRECT')
      CALL JEDETR('&&DRZ03D.DIME')

      CALL JEDEMA()
      END
