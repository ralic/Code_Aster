      SUBROUTINE DRZ03D (LISNOZ ,LONLIS, CHARGZ, TYPLAZ, LISREZ,DMIN)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 04/11/2004   AUTEUR G8BHHXD X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C TOLE CRP_20
      CHARACTER*8  CHARGE
      CHARACTER*19 LISREL
      CHARACTER*24 LISNOE
      CHARACTER*(*) CHARGZ, LISNOZ, TYPLAZ, LISREZ
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
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR,RPETIT
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE /ZK8(1),ZK16(1),ZK24(1),ZK32(1), ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ------
C
C --------- VARIABLES LOCALES ---------------------------
      PARAMETER    (NMOCL = 300)
      COMPLEX*16    BETAC
      CHARACTER*2   TYPLAG
      CHARACTER*4   TYPVAL, TYPCOE
      CHARACTER*8   BETAF, RESU
      CHARACTER*8   MOD, NOMG, NOMNOE, K8BID, NO1
      CHARACTER*8   NOMA, CMP, NOMCMP(NMOCL)
      CHARACTER*8   NOEUA, NOEUB, NOEUC, NOEUM
      CHARACTER*9   NOMTE
      CHARACTER*16  TYPE, OPER 
      CHARACTER*19  LIGRMO
      INTEGER       NTYPEL(NMOCL), DG
      REAL*8        M1(3,3), MINV1(3,3), M2(3,12), M3(3,3)
      REAL*8        M4(3,12), M5(3,12)
      REAL*8        B(3), C(3), M(3), N(3), BN(3), CN(3)
      REAL*8        ML1(3,9), ML3(3,3), ML5(3,9)
      REAL*8        N1(3), N2(3), ABM(3)
      LOGICAL       VERIF, EXISDG
      CHARACTER*1 K1BID
C --------- FIN  DECLARATIONS  VARIABLES LOCALES --------
      CALL JEMARQ()
      CALL GETRES ( RESU, TYPE, OPER )
      LISREL = LISREZ
      CHARGE = CHARGZ
      TYPLAG = TYPLAZ
      LISNOE = LISNOZ
C
C --- INITIALISATIONS
C
      BETAF =  '&FOZERO'
      BETA  = 0.0D0
      BETAC = (0.0D0,0.0D0)
      UN    = 1.0D0
      ZERO  = 0.0D0
      RPETIT=1.D-6*DMIN
C
C --- MODELE ASSOCIE AU LIGREL DE CHARGE
C
      CALL DISMOI('F','NOM_MODELE',CHARGE(1:8),'CHARGE',IBID,MOD,IER)
C
C ---  LIGREL DU MODELE
C
      LIGRMO = MOD(1:8)//'.MODELE'
C
C --- MAILLAGE ASSOCIE AU MODELE
C
      CALL JEVEUO(LIGRMO//'.NOMA','L',JNOMA)
      NOMA = ZK8(JNOMA)
C
C --- TYPE DES VALEURS DES COEFFICIENTS DES RELATIONS
C
      TYPCOE = 'REEL'
C
C --- TYPE DES VALEURS AU SECOND MEMBRE DES RELATIONS
C
      IF(OPER(15:16).EQ.'_F') THEN
         TYPVAL = 'FONC'
      ELSE IF(OPER(15:16).EQ.'_C') THEN
         TYPVAL = 'COMP'
      ELSE IF(OPER(15:16).EQ.'  ') THEN
         TYPVAL = 'REEL'
      ELSE
         CALL ASSERT(.FALSE.)
      ENDIF
C
C --- RECUPERATION DES NOMS DES DDLS ET DES NUMEROS
C --- D'ELEMENTS DE LAGRANGE ASSOCIES
C
      NOMG = 'DEPL_R'
      NOMTE = 'D_DEPL_R_'
C
      CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMG),'L',INOM)
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMG),'LONMAX',NBCMP,K1BID)
      NDDLA = NBCMP-1
      IF (NDDLA.GT.NMOCL) THEN
        CALL UTDEBM('F','DRZ03D','NOMBRE DE CMPS SUPERIEUR AU MAX')
        CALL UTIMPI('L','NMAXCMP= ',1,NMOCL)
        CALL UTIMPI('L','NCMP   = ',1,NDDLA)
        CALL UTFINM()
      ENDIF
      DO 10 I=1,NDDLA
        NOMCMP(I)=ZK8(INOM-1+I)
        CALL JENONU(JEXNOM('&CATA.TE.NOMTE',
     &                     NOMTE//NOMCMP(I)(1:7)),NTYPEL(I))
 10   CONTINUE
      CALL DISMOI('F','NB_EC',NOMG,'GRANDEUR',NBEC,K8BID,IERD)
C
      IF (NBEC.GT.10) THEN
         CALL UTMESS('F','DRZ03D',
     &                   'LE DESCRIPTEUR_GRANDEUR DES DEPLACEMENTS'//
     &                    ' NE TIENT PAS SUR DIX ENTIERS CODES')
      END IF
C
C --- CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION
C --- DE LISREL
C
C ---  MAJORANT DU NOMBRE DE TERMES DANS UNE RELATION
      NBTERM = 12
C ---  VECTEUR DU NOM DES NOEUDS
      CALL WKVECT ('&&DRZ03D.LISNO','V V K8',NBTERM,JLISNO)
C ---  VECTEUR DU NOM DES DDLS
      CALL WKVECT ('&&DRZ03D.LISDDL','V V K8',NBTERM,JLISDL)
C ---  VECTEUR DES COEFFICIENTS REELS
      CALL WKVECT ('&&DRZ03D.COER','V V R',NBTERM,JLISCR)
C ---  VECTEUR DES COEFFICIENTS COMPLEXES
      CALL WKVECT ('&&DRZ03D.COEC','V V C',NBTERM,JLISCC)
C ---  VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
      CALL WKVECT ('&&DRZ03D.DIRECT','V V R',3*NBTERM,JLISDI)
C ---  VECTEUR DES DIMENSIONS DE CES DIRECTIONS
      CALL WKVECT ('&&DRZ03D.DIME','V V I',NBTERM,JLISDM)
C
C --- RECUPERATION DU TABLEAU DES COORDONNEES
C
      CALL JEVEUO (NOMA//'.COORDO    .VALE','L',JCOOR)
C
C --- ACQUISITION DE LA LISTE DES NOEUDS A LIER
C --- (CETTE LISTE EST NON REDONDANTE)
C
      CALL JEVEUO (LISNOE,'L',ILISNO)
C
C ---       INITIALISATIONS
C
      DO 20 I=1, 3
         DO 30 J=1, 12
            M2(I,J) = ZERO
            M4(I,J) = ZERO
            M5(I,J) = ZERO
  30     CONTINUE
  20  CONTINUE
C
      DO 40 I=1, 3
         DO 50 J=1, 3
            M1(I,J)    = ZERO
            MINV1(I,J) = ZERO
            M3(I,J)    = ZERO
  50     CONTINUE
  40  CONTINUE
C
C ---  RECHERCHE DE NOEUDS A, B, C FORMANT UN TRIANGLE
C ---  DE SURFACE NON-NULLE.
C ---  ON PREND POUR A LE PREMIER NOEUD
C ---           POUR B LE PREMIER NOEUD DISTINCT DE A
C ---           POUR C LE PREMIER NOEUD DISTINCT DE A ET B
C ---                  ET NON COLINEAIRE A AB.
C
      NOEUA = ZK8(ILISNO+1-1)
      CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+1-1)),INOA)
      ITRIAN = 0
      DO 60 J=2, LONLIS
C
         CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+J-1)),
     +               INOB)
C
         B(1) =  ZR(JCOOR-1+3*(INOB-1)+1)
     +          -ZR(JCOOR-1+3*(INOA-1)+1)
         B(2) =  ZR(JCOOR-1+3*(INOB-1)+2)
     +          -ZR(JCOOR-1+3*(INOA-1)+2)
         B(3) =  ZR(JCOOR-1+3*(INOB-1)+3)
     +          -ZR(JCOOR-1+3*(INOA-1)+3)
C
         J2 = J
         IF (SQRT(B(1)*B(1)+B(2)*B(2)+B(3)*B(3)).GT.RPETIT) THEN
C
             NOEUB = ZK8(ILISNO+J2-1)
             DO 70 K=J2+1, LONLIS
C
                CALL JENONU(JEXNOM(NOMA//'.NOMNOE',
     +                      ZK8(ILISNO+K-1)),INOC)
C
                C(1) =  ZR(JCOOR-1+3*(INOC-1)+1)
     +                 -ZR(JCOOR-1+3*(INOA-1)+1)
                C(2) =  ZR(JCOOR-1+3*(INOC-1)+2)
     +                 -ZR(JCOOR-1+3*(INOA-1)+2)
                C(3) =  ZR(JCOOR-1+3*(INOC-1)+3)
     +                 -ZR(JCOOR-1+3*(INOA-1)+3)
C
C ---    CALCUL DE N = B X C
C
                CALL PROVEC(B, C, N)
                IF (SQRT(N(1)*N(1)+N(2)*N(2)+N(3)*N(3))
     +              .LT.RPETIT) THEN
                       GOTO 70
                ELSE
                       K2 = K
                       NOEUC = ZK8(ILISNO+K2-1)
                       ITRIAN = 1
                       GOTO 80
                ENDIF
   70        CONTINUE
C ---  FIN DU TEST SUR LA NORME DE B
         ENDIF
   60 CONTINUE
C
   80 CONTINUE
C
C ---  CAS OU L'ON A PU TOUVER 3 NOEUDS FORMANT UN TRIANGLE
C ---  DE SURFACE NON NULLE
C
      IF (ITRIAN.EQ.1) THEN
C
C ---  CALCUL DE BN = B X N
C
             CALL PROVEC(B, N, BN)
C
C ---     CALCUL DE CN = C X N
C
             CALL PROVEC(C, N, CN)
C
C ---     DEFINITION DE M1
C
             M1(1,1) = BN(1)
             M1(1,2) = BN(2)
             M1(1,3) = BN(3)
             M1(2,1) = CN(1)
             M1(2,2) = CN(2)
             M1(2,3) = CN(3)
             M1(3,1) = N(1)
             M1(3,2) = N(2)
             M1(3,3) = N(3)
C
C ---     DEFINITION DE M2
C
             M2(1,1) = -N(1)
             M2(1,2) = -N(2)
             M2(1,3) = -N(3)
             M2(1,4) =  N(1)
             M2(1,5) =  N(2)
             M2(1,6) =  N(3)
             M2(2,1) = -N(1)
             M2(2,2) = -N(2)
             M2(2,3) = -N(3)
             M2(2,7) =  N(1)
             M2(2,8) =  N(2)
             M2(2,9) =  N(3)
             M2(3,1) = -C(1)
             M2(3,2) = -C(2)
             M2(3,3) = -C(3)
             M2(3,4) =  C(1)
             M2(3,5) =  C(2)
             M2(3,6) =  C(3)
C
C ---     DEFINITION DE M4
C
             M4(1,1)  = -UN
             M4(2,2)  = -UN
             M4(3,3)  = -UN
             M4(1,10) =  UN
             M4(2,11) =  UN
             M4(3,12) =  UN
C
C ---     INVERSION DE M1
C
             CALL INVMA3(M1, MINV1)
C
C ---     PREMIERE RELATION POUR LES NOEUDS B ET C :
C ---     (UB-UA).B = 0
C
             NBTERM = 6
C
             ZK8(JLISNO+1-1) = NOEUB
             ZK8(JLISNO+2-1) = NOEUA
             ZK8(JLISNO+3-1) = NOEUB
             ZK8(JLISNO+4-1) = NOEUA
             ZK8(JLISNO+5-1) = NOEUB
             ZK8(JLISNO+6-1) = NOEUA
C
             ZK8(JLISDL+1-1) = 'DX'
             ZK8(JLISDL+2-1) = 'DX'
             ZK8(JLISDL+3-1) = 'DY'
             ZK8(JLISDL+4-1) = 'DY'
             ZK8(JLISDL+5-1) = 'DZ'
             ZK8(JLISDL+6-1) = 'DZ'
C
             ZR(JLISCR+1-1) =  B(1)
             ZR(JLISCR+2-1) = -B(1)
             ZR(JLISCR+3-1) =  B(2)
             ZR(JLISCR+4-1) = -B(2)
             ZR(JLISCR+5-1) =  B(3)
             ZR(JLISCR+6-1) = -B(3)
C
             CALL AFRELA (ZR(JLISCR), ZC(JLISCC), ZK8(JLISDL),
     +                    ZK8(JLISNO), ZI(JLISDM), ZR(JLISDI),
     +                    NBTERM, BETA, BETAC, BETAF, TYPCOE,
     +                    TYPVAL, TYPLAG, LISREL)
C
C ---     DEUXIEME RELATION POUR LES NOEUDS B ET C :
C ---     (UC-UA).C = 0
C
             NBTERM = 6
C
             ZK8(JLISNO+1-1) = NOEUC
             ZK8(JLISNO+2-1) = NOEUA
             ZK8(JLISNO+3-1) = NOEUC
             ZK8(JLISNO+4-1) = NOEUA
             ZK8(JLISNO+5-1) = NOEUC
             ZK8(JLISNO+6-1) = NOEUA
C
             ZR(JLISCR+1-1) =  C(1)
             ZR(JLISCR+2-1) = -C(1)
             ZR(JLISCR+3-1) =  C(2)
             ZR(JLISCR+4-1) = -C(2)
             ZR(JLISCR+5-1) =  C(3)
             ZR(JLISCR+6-1) = -C(3)
C
             CALL AFRELA (ZR(JLISCR), ZC(JLISCC), ZK8(JLISDL),
     +                    ZK8(JLISNO), ZI(JLISDM), ZR(JLISDI),
     +                    NBTERM, BETA, BETAC, BETAF, TYPCOE,
     +                    TYPVAL, TYPLAG, LISREL)
C
C ---     TROISIEME RELATION POUR LES NOEUDS B ET C :
C ---     (UB-UA).C + (UC-UA).B = 0
C
             NBTERM = 9
C
             ZK8(JLISNO+1-1) = NOEUA
             ZK8(JLISNO+2-1) = NOEUB
             ZK8(JLISNO+3-1) = NOEUC
             ZK8(JLISNO+4-1) = NOEUA
             ZK8(JLISNO+5-1) = NOEUB
             ZK8(JLISNO+6-1) = NOEUC
             ZK8(JLISNO+7-1) = NOEUA
             ZK8(JLISNO+8-1) = NOEUB
             ZK8(JLISNO+9-1) = NOEUC
C
             ZK8(JLISDL+1-1) = 'DX'
             ZK8(JLISDL+2-1) = 'DX'
             ZK8(JLISDL+3-1) = 'DX'
             ZK8(JLISDL+4-1) = 'DY'
             ZK8(JLISDL+5-1) = 'DY'
             ZK8(JLISDL+6-1) = 'DY'
             ZK8(JLISDL+7-1) = 'DZ'
             ZK8(JLISDL+8-1) = 'DZ'
             ZK8(JLISDL+9-1) = 'DZ'
C
             ZR(JLISCR+1-1) = -B(1)-C(1)
             ZR(JLISCR+2-1) =  C(1)
             ZR(JLISCR+3-1) =  B(1)
             ZR(JLISCR+4-1) = -B(2)-C(2)
             ZR(JLISCR+5-1) =  C(2)
             ZR(JLISCR+6-1) =  B(2)
             ZR(JLISCR+7-1) = -B(3)-C(3)
             ZR(JLISCR+8-1) =  C(3)
             ZR(JLISCR+9-1) =  B(3)
C
             CALL AFRELA (ZR(JLISCR), ZC(JLISCC), ZK8(JLISDL),
     +                    ZK8(JLISNO), ZI(JLISDM), ZR(JLISDI),
     +                    NBTERM, BETA, BETAC, BETAF, TYPCOE,
     +                    TYPVAL, TYPLAG, LISREL)
C
             DO 90 J=2, LONLIS
C
                CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+J-1)),
     +                      INOEM)
C
                IF (INOEM.EQ.INOB.OR.INOEM.EQ.INOC) GOTO 90
                M(1) =  ZR(JCOOR-1+3*(INOEM-1)+1)
     +                 -ZR(JCOOR-1+3*(INOA-1)+1)
                M(2) =  ZR(JCOOR-1+3*(INOEM-1)+2)
     +                 -ZR(JCOOR-1+3*(INOA-1)+2)
                M(3) =  ZR(JCOOR-1+3*(INOEM-1)+3)
     +                 -ZR(JCOOR-1+3*(INOA-1)+3)
C
C ---        DEFINITION DE M3
C
                M3(1,2) = -M(3)
                M3(1,3) =  M(2)
                M3(2,1) =  M(3)
                M3(2,3) = -M(1)
                M3(3,1) = -M(2)
                M3(3,2) =  M(1)
C
C ---        CALCUL DE M1 <-- M3.MINV1
C
                CALL PMAT(3, M3, MINV1, M1)
C
C ---        CALCUL DE M5 <-- M3.MINV1.M2
C
                CALL PMPPR(M1,3,3,1, M2,3,12,1,M5,3,12)
C
C ---        CALCUL DE M5 <-- M4 + M3.MINV1.M2
C
                DO 100 J1=1, 3
                   DO 110 J2=1, 12
                      M5(J1,J2) = M5(J1,J2) + M4(J1,J2)
  110              CONTINUE
  100           CONTINUE
C
C ---     ECRITURE DES 3 RELATIONS CORRESPONDANTES A M5.UABCM = 0
C
                NBTERM = 12
C
                DO 120 K=1, 3
                    ZK8(JLISNO+K-1)   = NOEUA
                    ZK8(JLISNO+3+K-1) = NOEUB
                    ZK8(JLISNO+6+K-1) = NOEUC
                    ZK8(JLISNO+9+K-1) = ZK8(ILISNO+J-1)
  120           CONTINUE
C
                DO 130 K=1, 4
                   ZK8(JLISDL+3*(K-1)+1-1) = 'DX'
                   ZK8(JLISDL+3*(K-1)+2-1) = 'DY'
                   ZK8(JLISDL+3*(K-1)+3-1) = 'DZ'
  130           CONTINUE
C
                DO 140 J1=1, 3
                   DO 150 J2=1, 12
                      ZR(JLISCR+J2-1) =  M5(J1,J2)
  150              CONTINUE
C
                   CALL AFRELA (ZR(JLISCR), ZC(JLISCC), ZK8(JLISDL),
     +                          ZK8(JLISNO), ZI(JLISDM), ZR(JLISDI),
     +                          NBTERM, BETA, BETAC, BETAF, TYPCOE,
     +                          TYPVAL, TYPLAG, LISREL)
  140           CONTINUE
C ---     FIN DE LA BOUCLE SUR LES NOEUDS DE LA LISTE
   90        CONTINUE
C
C ---   ANALYSE DU CAS OU L'ON N'A PAS PU TROUVER 3 NOEUDS DE LA
C ---   LISTE FORMANT UN TRIANGLE
C
      ELSEIF (ITRIAN.EQ.0) THEN

             X1 =  ZR(JCOOR-1+3*(INOA-1)+1)
             Y1 =  ZR(JCOOR-1+3*(INOA-1)+2)
             Z1 =  ZR(JCOOR-1+3*(INOA-1)+3)
C
             IALIGN = 0
C
             DO 160 I=2, LONLIS
                   CALL JENONU(JEXNOM(NOMA//'.NOMNOE',ZK8(ILISNO+I-1)),
     +                      INOI)
                   XI  =  ZR(JCOOR-1+3*(INOI-1)+1)
                   YI  =  ZR(JCOOR-1+3*(INOI-1)+2)
                   ZIJ =  ZR(JCOOR-1+3*(INOI-1)+3)
                   IF ((ABS(XI-X1).GT.RPETIT).OR.
     +                 (ABS(YI-Y1).GT.RPETIT).OR.
     +                 (ABS(ZIJ-Z1).GT.RPETIT)) THEN
                          IALIGN = 1
                          GOTO 170
                   ENDIF
  160        CONTINUE
C
C ---   CAS OU TOUS LES NOEUDS DE LA LISTE ONT LES MEMES COORDONNEES
C
C ---     PREMIERE RELATION
C ---     DX(M) -DX(A) = 0
C
             NBTERM = 2
             ZK8(JLISNO+1-1) = ZK8(ILISNO+1-1)
             ZK8(JLISDL+1-1) = 'DX'
             ZR(JLISCR+1-1) =  UN
C
             DO 180 I=2, LONLIS
                   ZK8(JLISNO+2-1) = ZK8(ILISNO+I-1)
                   ZK8(JLISDL+2-1) = 'DX'
                   ZR(JLISCR+2-1) =  -UN
C
                   CALL AFRELA (ZR(JLISCR), ZC(JLISCC), ZK8(JLISDL),
     +                          ZK8(JLISNO), ZI(JLISDM), ZR(JLISDI),
     +                          NBTERM, BETA, BETAC, BETAF, TYPCOE,
     +                          TYPVAL, TYPLAG, LISREL)
  180        CONTINUE
C
C ---     DEUXIEME RELATION
C ---     DY(M) -DY(A) = 0
C
             ZK8(JLISDL+1-1) = 'DY'
C
             DO 190 I=2, LONLIS
                   ZK8(JLISNO+2-1) = ZK8(ILISNO+I-1)
                   ZK8(JLISDL+2-1) = 'DY'
C
                   CALL AFRELA (ZR(JLISCR), ZC(JLISCC), ZK8(JLISDL),
     +                          ZK8(JLISNO), ZI(JLISDM), ZR(JLISDI),
     +                          NBTERM, BETA, BETAC, BETAF, TYPCOE,
     +                          TYPVAL, TYPLAG, LISREL)
  190           CONTINUE
C
C ---     TROISIEME RELATION
C ---     DZ(M) -DZ(A) = 0
C
             ZK8(JLISDL+1-1) = 'DZ'
C
             DO 200 I=2, LONLIS
                   ZK8(JLISNO+2-1) = ZK8(ILISNO+I-1)
                   ZK8(JLISDL+2-1) = 'DZ'
C
                   CALL AFRELA (ZR(JLISCR), ZC(JLISCC), ZK8(JLISDL),
     +                          ZK8(JLISNO), ZI(JLISDM), ZR(JLISDI),
     +                          NBTERM, BETA, BETAC, BETAF, TYPCOE,
     +                          TYPVAL, TYPLAG, LISREL)
  200        CONTINUE
C
  170        CONTINUE
C
C ---    CAS OU TOUS LES NOEUDS SONT ALIGNES
C
             IF (IALIGN.EQ.1) THEN
C
C ---           CAS OU L'ON N'A QUE 2 POINTS, LA SEULE RELATION EST :
C ---           (UB-UA).B = 0
C
                   IF (LONLIS.EQ.2) THEN
C
                       NBTERM = 6
C
                       ZK8(JLISNO+1-1) = NOEUB
                       ZK8(JLISNO+2-1) = NOEUA
                       ZK8(JLISNO+3-1) = NOEUB
                       ZK8(JLISNO+4-1) = NOEUA
                       ZK8(JLISNO+5-1) = NOEUB
                       ZK8(JLISNO+6-1) = NOEUA
C
                       ZK8(JLISDL+1-1) = 'DX'
                       ZK8(JLISDL+2-1) = 'DX'
                       ZK8(JLISDL+3-1) = 'DY'
                       ZK8(JLISDL+4-1) = 'DY'
                       ZK8(JLISDL+5-1) = 'DZ'
                       ZK8(JLISDL+6-1) = 'DZ'
C
                       ZR(JLISCR+1-1) =  B(1)
                       ZR(JLISCR+2-1) = -B(1)
                       ZR(JLISCR+3-1) =  B(2)
                       ZR(JLISCR+4-1) = -B(2)
                       ZR(JLISCR+5-1) =  B(3)
                       ZR(JLISCR+6-1) = -B(3)
C
                       CALL AFRELA (ZR(JLISCR), ZC(JLISCC), ZK8(JLISDL),
     +                              ZK8(JLISNO), ZI(JLISDM), ZR(JLISDI),
     +                              NBTERM, BETA, BETAC, BETAF, TYPCOE,
     +                              TYPVAL, TYPLAG, LISREL)
                   ELSEIF (LONLIS.GT.2) THEN
C
C ---               INITIALISATIONS
C
                       DO 210 I=1, 3
                          DO 220 J=1, 9
                             ML1(I,J) = ZERO
                             ML5(I,J) = ZERO
  220                     CONTINUE
  210                 CONTINUE
C
                       DO 230 I=1, 3
                          DO 240 J=1, 3
                             ML3(I,J) = ZERO
  240                     CONTINUE
  230                 CONTINUE
C
                      DO 250 I=2, LONLIS
                         CALL JENONU(JEXNOM(NOMA//'.NOMNOE',
     +                               ZK8(ILISNO+I-1)),INOB)
                         B(1) =  ZR(JCOOR-1+3*(INOB-1)+1)
     +                          -ZR(JCOOR-1+3*(INOA-1)+1)
                         B(2) =  ZR(JCOOR-1+3*(INOB-1)+2)
     +                          -ZR(JCOOR-1+3*(INOA-1)+2)
                         B(3) =  ZR(JCOOR-1+3*(INOB-1)+3)
     +                          -ZR(JCOOR-1+3*(INOA-1)+3)
                         IF (SQRT(B(1)*B(1)+B(2)*B(2)+B(3)*B(3))
     +                       .GT.RPETIT) THEN
                             I2    = I
                             INO2  = INOB
                             NOEUB = ZK8(ILISNO+I2-1)
                             GOTO 260
                         ENDIF
  250                 CONTINUE
C
  260                 CONTINUE
C
C ---              DEFINITION DU VECTEUR N1
C ---              ON ESSAIE D'ABORD N1 = I X B
C
                      N1(1) =  ZERO
                      N1(2) = -B(3)
                      N1(3) =  B(2)
C
                      IF (SQRT(B(3)*B(3)+B(2)*B(2)).LT.RPETIT) THEN
C
C ---                ON FAIT UN AUTRE ESSAI AVEC N1 = J X B
C
                         N1(1) =  B(3)
                         N1(2) =  ZERO
                         N1(3) = -B(1)
C
                         IF (SQRT(B(3)*B(3)+B(1)*B(1)).LT.RPETIT) THEN
                             CALL UTMESS('F','DRZ03D',
     +                    'PROBLEME DANS LE CAS 3D OU LES NOEUDS SONT'//
     +                    ' ALIGNES, LA DISTANCE SEPARANT 2 NOEUDS'//
     +                    ' NON-IDENTIQUES DE LA LISTE EST TROP PETITE')
                         ENDIF
                      ENDIF
C
C ---              DEFINITION DU VECTEUR N2 = B X N1
C
                      N2(1) =  B(2)*N1(3) - B(3)*N1(2)
                      N2(2) =  B(3)*N1(1) - B(1)*N1(3)
                      N2(3) =  B(1)*N1(2) - B(2)*N1(1)
C
C ---              DEFINITION DE ML1
C
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
C
C ---              ON VERIFIE QUE LES NOEUDS DE LA LISTE SONT
C ---              EFFECTIVEMENT ALIGNES
C
                      DO 270 I=2, LONLIS
                         CALL JENONU(JEXNOM(NOMA//'.NOMNOE',
     +                                ZK8(ILISNO+I-1)),INOEM)
C
                         M(1) =  ZR(JCOOR-1+3*(INOEM-1)+1)
     +                          -ZR(JCOOR-1+3*(INOA-1)+1)
                         M(2) =  ZR(JCOOR-1+3*(INOEM-1)+2)
     +                           -ZR(JCOOR-1+3*(INOA-1)+2)
                         M(3) =  ZR(JCOOR-1+3*(INOEM-1)+3)
     +                          -ZR(JCOOR-1+3*(INOA-1)+3)

C
C ---                 DEFINITION DU VECTEUR ABM = AB X AM
C
                         ABM(1) =  B(2)*M(3) - B(3)*M(2)
                         ABM(2) =  B(3)*M(1) - B(1)*M(3)
                         ABM(3) =  B(1)*M(2) - B(2)*M(1)
C
                         IF (SQRT(ABM(1)*ABM(1)+ABM(2)*ABM(2)+
     +                            ABM(3)*ABM(3)).GT.RPETIT) THEN
                                    CALL UTMESS('F','DRZ03D',
     +                   'PROBLEME DANS LE CAS 3D OU LES NOEUDS SONT'//
     +                   ' ALIGNES ET OU POURTANT ON ARRIVE A TROUVER'//
     +                   ' 3 NOEUDS FORMANT UN TRIANGLE DE SURFACE'//
     +                   ' NON-NULLE')
                         ENDIF
  270                 CONTINUE
C
C ---           PREMIERE RELATION  : (UB-UA).B = 0
C
                      NBTERM = 6
C
                      ZK8(JLISNO+1-1) = NOEUB
                      ZK8(JLISNO+2-1) = NOEUA
                      ZK8(JLISNO+3-1) = NOEUB
                      ZK8(JLISNO+4-1) = NOEUA
                      ZK8(JLISNO+5-1) = NOEUB
                      ZK8(JLISNO+6-1) = NOEUA
C
                      ZK8(JLISDL+1-1) = 'DX'
                      ZK8(JLISDL+2-1) = 'DX'
                      ZK8(JLISDL+3-1) = 'DY'
                      ZK8(JLISDL+4-1) = 'DY'
                      ZK8(JLISDL+5-1) = 'DZ'
                      ZK8(JLISDL+6-1) = 'DZ'
C
                      ZR(JLISCR+1-1) =  B(1)
                      ZR(JLISCR+2-1) = -B(1)
                      ZR(JLISCR+3-1) =  B(2)
                      ZR(JLISCR+4-1) = -B(2)
                      ZR(JLISCR+5-1) =  B(3)
                      ZR(JLISCR+6-1) = -B(3)
C
                      CALL AFRELA (ZR(JLISCR), ZC(JLISCC), ZK8(JLISDL),
     +                             ZK8(JLISNO), ZI(JLISDM), ZR(JLISDI),
     +                             NBTERM, BETA, BETAC, BETAF, TYPCOE,
     +                             TYPVAL, TYPLAG, LISREL)
C
C ---              CALCUL DU COEFFICIENT K
C
                      COEK =  (N1(1)*N1(1)+N1(2)*N1(2)+N1(3)*N1(3))
     +                    *(B(1)*B(1)+B(2)*B(2)+B(3)*B(3))
                      COEK1 = 1.0D0/COEK
C
                      DO 280 I=2, LONLIS
                         CALL JENONU(JEXNOM(NOMA//'.NOMNOE',
     +                                ZK8(ILISNO+I-1)),INOEM)
                         IF (INOEM.EQ.INO2) GOTO 280
C
                         M(1) =  ZR(JCOOR-1+3*(INOEM-1)+1)
     +                          -ZR(JCOOR-1+3*(INOA-1)+1)
                         M(2) =  ZR(JCOOR-1+3*(INOEM-1)+2)
     +                           -ZR(JCOOR-1+3*(INOA-1)+2)
                         M(3) =  ZR(JCOOR-1+3*(INOEM-1)+3)
     +                          -ZR(JCOOR-1+3*(INOA-1)+3)
C
C ---                 DEFINITION DE ML3
C
                         ML3(1,2) = -M(3)
                         ML3(1,3) =  M(2)
                         ML3(2,1) =  M(3)
                         ML3(2,3) = -M(1)
                         ML3(3,1) = -M(2)
                         ML3(3,2) =  M(1)
C
C ---                 CALCUL DE ML5 <-- ML3.ML1
C
                         CALL PMPPR(ML3,3,3,1, ML1,3,9,1,ML5,3,9)
C
                         DO 290 J1=1, 3
                            DO 300 J2=1, 9
                               ML5(J1,J2) = COEK1*ML5(J1,J2)
  300                       CONTINUE
  290                    CONTINUE
C
C ---                 RAJOUT DE ML4
C
                         ML5(1,1) = ML5(1,1) - UN
                         ML5(2,2) = ML5(2,2) - UN
                         ML5(3,3) = ML5(3,3) - UN
                         ML5(1,7) = ML5(1,7) + UN
                         ML5(2,8) = ML5(2,8) + UN
                         ML5(3,9) = ML5(3,9) + UN
C
C
C ---     ECRITURE DES 3 RELATIONS CORRESPONDANTES A LA RELATION
C ---     MATRICIELLE : (ML4+1/K*ML3*ML1)*UABM = 0
C
                         NBTERM = 9
C
                         DO 310 K=1, 3
C
                           ZK8(JLISNO+K-1)   = NOEUA
                           ZK8(JLISNO+3+K-1) = NOEUB
                           ZK8(JLISNO+6+K-1) = ZK8(ILISNO+I-1)
C
                           ZK8(JLISDL+3*(K-1)+1-1) = 'DX'
                           ZK8(JLISDL+3*(K-1)+2-1) = 'DY'
                           ZK8(JLISDL+3*(K-1)+3-1) = 'DZ'
  310                    CONTINUE
C
                         DO 320 J1=1, 3
                            DO 330 J2=1, 9
                               ZR(JLISCR+J2-1) =  ML5(J1,J2)
  330                       CONTINUE
C
                            CALL AFRELA (ZR(JLISCR),ZC(JLISCC),
     +                                   ZK8(JLISDL),ZK8(JLISNO),
     +                                   ZI(JLISDM),ZR(JLISDI),
     +                                   NBTERM,BETA,BETAC,BETAF,
     +                                   TYPCOE,TYPVAL,TYPLAG,LISREL)
  320                    CONTINUE
  280                 CONTINUE
C ---           FIN DU CAS LONLIS GT 2
                   ENDIF
C ---         FIN DU CAS OU LES NOEUDS SONT ALIGNES
             ENDIF
C ---       FIN DU CAS OU L'ON NE PEUT PAS TROUVER 3 NOEUDS FORMANT
C ---       UN TRIANGLE
      ENDIF
C
C --- DESTRUCTION DES OBJETS DE TRAVAIL
C
      CALL JEDETR('&&DRZ03D.LISNO')
      CALL JEDETR('&&DRZ03D.LISDDL')
      CALL JEDETR('&&DRZ03D.COER')
      CALL JEDETR('&&DRZ03D.COEC')
      CALL JEDETR('&&DRZ03D.DIRECT')
      CALL JEDETR('&&DRZ03D.DIME')
C
      CALL JEDEMA()
      END
