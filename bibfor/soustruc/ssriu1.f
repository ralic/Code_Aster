      SUBROUTINE SSRIU1(NOMU)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 18/09/2012   AUTEUR LADIER A.LADIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'
      CHARACTER*8 NOMU
C ----------------------------------------------------------------------
C     BUT:
C       1) METTRE A JOUR .DESM(3,4,5,8,9,10)
C       2) METTRE LES DDLS INTERNES AVANT LES DLLS EXTERNES.
C          ON CHANGE LE VECTEUR D'INDIRECTION .NUEQ DU PROF_CHNO
C       3) ECRIRE LE "LONMAX" DE LA COLLECTION .LICA
C       4) CALCULER L'OBJET .CONX :
C          I VARIE DE 1 A NBNOET=NBNOE+NLAGE+NLAGL
C          (L'ORDRE EST CELUI DE LA NUMEROTAION DE LA MATRICE DE RIGI)
C          CONX(I,1) CONTIENT LE NUMERO DU LIGREL OU EST DEFINI LE
C                      I_EME NOEUD "EXTERNE"
C          CONX(I,2) CONTIENT LE NUMERO (DANS LE LIGREL)
C                      DU I_EME NOEUD "EXTERNE"
C          CONX(I,3) = -0 SI LE NOEUD EST PHYSIQUE
C                    = -1 SI LE NOEUD EST DE LAGRANGE (TYPE "AVANT).
C                    = -2 SI LE NOEUD EST DE LAGRANGE (TYPE "APRES").

C        ON SUPPOSE QU'AVANT L'APPEL A CETTE ROUTINE, L'OBJET .NUEQ
C        EST L'INDIRECTION "IDENTITE" (ZI(IANUEQ-1 +I) = I).

C        ON "DEPLACE" VERS L'AVANT (MODIFICATION DES ADRESSES VIA .NUEQ)
C        LES GROUPES DE DDLS PORTES PAR LES NOEUDS INTERNES.
C        ON SE SERT POUR CELA DE .DEEQ.

C          REGLE ADOPTEE POUR LES DDDLS/NOEUDS DE LAGRANGE:
C          - ON NE GARDE COMME DDLS INTERNES QUE :
C             - LES DDLS PHYSIQUES DES NOEUDS INTERNES.
C             - LES DDLS LAGRANGES ASSOCIES A
C               DES BLOCAGES DE NOEUDS INTERNES
C          - LES DDLS LAGRANGES ASSOCIES A DES LIAISONS SONT DONC
C            EXTERNES.

C       EXEMPLE:   NOEUDS INTERNES : 1,3
C                  NOEUDS EXTERNES : 2

C       DEEQ       NUEQ        CEUX QUI      NUEQ       "CONX"
C                  (AVANT)     REMONTENT (APRES)
C        1, 1         1           X           1           - - -
C        1, 2         2           X           2           - - -
C        0, 0         3                       7           2 1 -1
C        2,-1         4                       8           1 2 0
C        2,-2         5                       9           - - -
C        2, 1         6                      10           - - -
C        2, 2         7                      11           - - -
C        2,-1         8                      12           - - -
C        2,-2         9                      13           - - -
C        3,-1        10           X           3           - - -
C        3, 1        11           X           4           - - -
C        3, 2        12           X           5           - - -
C        3,-1        13           X           6           - - -
C        0, 0        14                      14           2 2 -2

C     IN: NOMU   : NOM DU MACR_ELEM_STAT

C     OUT: L OBJET .DESM EST MODIFIE.
C          L OBJET .NUEQ EST MODIFIE.
C          L OBJET .DEEQ EST MODIFIE.
C          L OBJET .DELG EST MODIFIE.

C ----------------------------------------------------------------------


      INTEGER I
      CHARACTER*8 KBID,NOGDSI
      CHARACTER*19 NU
      INTEGER IACONX ,IADEEQ ,IADELG ,IADESM ,IAINTR ,IALINO ,IANUEQ
      INTEGER IAPRNO ,IAWRK1 ,IAWRK2 ,IBID ,ICO ,ICOE ,ICOI
      INTEGER IED ,IEQN ,IERD ,ILI ,INL ,INO ,IRET
      INTEGER ITYLAG ,N1 ,NBNO ,NBNOE ,NBNOET ,NDDLE ,NDDLI
      INTEGER NDDLT ,NEC ,NLAGE ,NLAGI ,NLAGL ,NLILI ,NUDDL
      INTEGER NUEQ ,NULAG ,NUNO ,NUNO2 ,NUNOLD,INDIIS
C-----------------------------------------------------------------------
      CALL JEMARQ()
      NU = NOMU
      NU = NU(1:14)//'.NUME'

      CALL DISMOI('F','NOM_GD',NU(1:14),'NUME_DDL',IBID,NOGDSI,IERD)
      IF (NOGDSI.NE.'DEPL_R') CALL U2MESS('F','SOUSTRUC_70')
      CALL DISMOI('F','NU_CMP_LAGR','DEPL_R','GRANDEUR',NULAG,KBID,IED)
      CALL DISMOI('F','NB_EC',NOGDSI,'GRANDEUR',NEC,KBID,IERD)
      CALL JEVEUO(NU//'.DEEQ','E',IADEEQ)
      CALL JEVEUO(NU//'.DELG','E',IADELG)
      CALL JEVEUO(NU//'.NUEQ','E',IANUEQ)
      CALL JELIRA(NU//'.NUEQ','LONMAX',NDDLT,KBID)
      CALL JELIRA(NU//'.PRNO','NMAXOC',NLILI,KBID)

      CALL JEVEUO(NOMU//'.DESM','E',IADESM)
      CALL JEVEUO(NOMU//'.LINO','E',IALINO)
      NBNOE = ZI(IADESM-1+2)


C     -- ALLOCATION D'UN VECTEUR DE TRAVAIL QUI CONTIENDRA
C        DES "1" SUR LES DDL INTERNES.
      CALL WKVECT('&&SSRIU1.INTERNE','V V I',NDDLT,IAINTR)


C     -- BOUCLE SUR LES  DDLS, REMPLISSAGE DE .INTERNE:
C     -------------------------------------------------
      NUNOLD = 0
      ICOI = 0
      ICOE = 0
      NDDLI = 0
      NLAGL = 0
      NLAGI = 0
      NLAGE = 0

      DO 10,I = 1,NDDLT
        CALL ASSERT(ZI(IANUEQ-1+I).EQ.I)

        NUNO = ZI(IADEEQ-1+2* (I-1)+1)
        NUDDL = ZI(IADEEQ-1+2* (I-1)+2)

C        -- LES LAGRANGES DU MAILLAGE SONT TOUS DECLARES EXTERNES:
C           (ON LES CONSERVERA DONC A TOUS LES NIVEAUX)
        IF (NUDDL.EQ.NULAG) THEN
          NLAGE = NLAGE + 1
          GO TO 10
        END IF

        IF (NUNO.NE.0) THEN
          NUNO2 = INDIIS(ZI(IALINO),NUNO,1,NBNOE)
          IF (NUNO2.EQ.0) THEN
            ZI(IAINTR-1+I) = 1
            NDDLI = NDDLI + 1
          END IF

C           -- ON COMPTE LES LAGRANGES INTERNES ET EXTERNES:
          IF (NUDDL.LT.0) THEN
            IF (NUNO2.EQ.0) THEN
              NLAGI = NLAGI + 1
            ELSE
              NLAGE = NLAGE + 1
            END IF
          END IF

C           -- ON COMPTE LES NOEUDS INTERNES ET EXTERNES:
          IF ((NUDDL.GT.0) .AND. (NUNOLD.NE.NUNO)) THEN
            NUNOLD = NUNO
            IF (NUNO2.EQ.0) THEN
              ICOI = ICOI + 1
            ELSE
              ICOE = ICOE + 1
              CALL ASSERT(ICOE.LE.NBNOE)
            END IF
          END IF
        ELSE
          NLAGL = NLAGL + 1
        END IF
   10 CONTINUE

      CALL ASSERT(NBNOE.EQ.ICOE)
      IF (ICOI.EQ.0) CALL U2MESS('F','SOUSTRUC_71')
      ZI(IADESM-1+3) = ICOI
      NDDLE = NDDLT - NDDLI
      ZI(IADESM-1+4) = NDDLE
      ZI(IADESM-1+5) = NDDLI
      ZI(IADESM-1+8) = NLAGE
      ZI(IADESM-1+9) = NLAGL
      ZI(IADESM-1+10) = NLAGI

C     -- DIMENSIONNEMENT DES OBJETS DE LA COLLECTION .LICA:
C     -----------------------------------------------------
      CALL JEECRA(NOMU//'.LICA','LONMAX',2*NDDLT,KBID)


C     -- MODIFICATION DE .NUEQ:
C     -------------------------
      CALL WKVECT('&&SSRIU1.WORK2','V V I',NDDLT,IAWRK2)
C    .WORK2 CONTIENT LA RECIPROQUE DU NOUVEAU .NUEQ:
      ICO = 0
C     -- ON CLASSE LES DDLS INTERNES:
      DO 20,I = 1,NDDLT
        IF (ZI(IAINTR-1+I).EQ.1) THEN
          ICO = ICO + 1
          ZI(IANUEQ-1+I) = ICO
          ZI(IAWRK2-1+ICO) = I
        END IF
   20 CONTINUE

C     -- ON CLASSE LES DDLS EXTERNES:
      DO 30,I = 1,NDDLT
        IF (ZI(IAINTR-1+I).EQ.0) THEN
          ICO = ICO + 1
          ZI(IANUEQ-1+I) = ICO
          ZI(IAWRK2-1+ICO) = I
        END IF
   30 CONTINUE


C     -- CREATION DE .CONX:
C     ---------------------
      NBNOET = NLAGE + NLAGL + NBNOE
      CALL WKVECT(NOMU//'.CONX','G V I',3*NBNOET,IACONX)
      CALL WKVECT('&&SSRIU1.WORK1','V V I',2*NDDLT,IAWRK1)
      ICO = 0
      NUNOLD = 0

C     -- MISE A JOUR DE .CONX : NOEUDS DU MAILLAGE + TYPE_LAGRANGE :
C     ------------------------------------------------------------
C     --ON TRAVAILLE AVEC L'ANCIEN .DEEQ:
      DO 40,I = 1,NDDLT
        NUNO = ZI(IADEEQ-1+2* (I-1)+1)
        NUDDL = ZI(IADEEQ-1+2* (I-1)+2)
C        -- ITYLAG EST LE TYPE DU NOEUD DE LAGRANGE (-1 OU -2)
        ITYLAG = ZI(IADELG-1+I)
        IF (NUNO.NE.0) THEN
          NUNO2 = INDIIS(ZI(IALINO),NUNO,1,NBNOE)

C           -- TYPE LAGRANGE DES NOEUDS SUPPLEMENTAIRES:
          IF (NUDDL.LT.0) THEN
            IF (NUNO2.NE.0) THEN
              ICO = ICO + 1
              ZI(IACONX-1+3* (ICO-1)+3) = ITYLAG
              IEQN = ZI(IANUEQ-1+I)
              CALL ASSERT(IEQN.GT.NDDLI)
              ZI(IAWRK1-1+IEQN) = ICO
            END IF
          END IF

C           -- NOEUDS LAGRANGES DU MAILLAGE :
          IF (NUDDL.EQ.NULAG) THEN
            ICO = ICO + 1
            ZI(IACONX-1+3* (ICO-1)+1) = 1
            ZI(IACONX-1+3* (ICO-1)+2) = NUNO
            ZI(IACONX-1+3* (ICO-1)+3) = ITYLAG
            IEQN = ZI(IANUEQ-1+I)
            CALL ASSERT(IEQN.GT.NDDLI)
            ZI(IAWRK1-1+IEQN) = ICO
          END IF

C           -- NOEUDS PHYSIQUES DU MAILLAGE :
          IF ((NUDDL.GT.0) .AND. (NUNOLD.NE.NUNO)) THEN
            NUNOLD = NUNO
            IF (NUNO2.NE.0) THEN
              ICO = ICO + 1
              ZI(IACONX-1+3* (ICO-1)+1) = 1
              ZI(IACONX-1+3* (ICO-1)+2) = NUNO
            END IF
          END IF
        ELSE

C           -- NOEUDS LAGRANGE DES LIAISONS DDL :
          ICO = ICO + 1
          ZI(IACONX-1+3* (ICO-1)+3) = ITYLAG
          IEQN = ZI(IANUEQ-1+I)
          CALL ASSERT(IEQN.GT.NDDLI)
          ZI(IAWRK1-1+IEQN) = ICO
        END IF
   40 CONTINUE

C     -- MISE A JOUR DE .CONX : NOEUDS DE LAGRANGE :
C     ----------------------------------------------
      DO 60,ILI = 2,NLILI
        CALL JEEXIN(JEXNUM(NU//'.PRNO',ILI),IRET)
        IF (IRET.EQ.0) GO TO 60
        CALL JELIRA(JEXNUM(NU//'.PRNO',ILI),'LONMAX',N1,KBID)
        IF (N1.EQ.0) GO TO 60
        CALL JEVEUO(JEXNUM(NU//'.PRNO',ILI),'L',IAPRNO)
        NBNO = N1/ (NEC+2)
        DO 50,INO = 1,NBNO
          NUEQ = ZI(IAPRNO-1+ (INO-1)* (NEC+2)+1)
          IF (NUEQ.EQ.0) GO TO 50
          IEQN = ZI(IANUEQ-1+NUEQ)
          IF (IEQN.GT.NDDLI) THEN
            INL = ZI(IAWRK1-1+IEQN)
            ZI(IACONX-1+3* (INL-1)+1) = ILI
            ZI(IACONX-1+3* (INL-1)+2) = INO
          END IF
   50   CONTINUE
   60 CONTINUE


C     -- REMISE EN ORDRE DE .DEEQ ET .DELG POUR TENIR COMPTE
C        DE LA MODIFICATION DE .NUEQ :
C        ---------------------------------------------------
      DO 70,I = 1,NDDLT
        ZI(IAWRK1-1+I) = ZI(IADELG-1+ZI(IAWRK2-1+I))
   70 CONTINUE
      DO 80,I = 1,NDDLT
        ZI(IADELG-1+I) = ZI(IAWRK1-1+I)
   80 CONTINUE

      DO 90,I = 1,NDDLT
        ZI(IAWRK1-1+2* (I-1)+1) = ZI(IADEEQ-1+2* (ZI(IAWRK2-1+I)-1)+1)
        ZI(IAWRK1-1+2* (I-1)+2) = ZI(IADEEQ-1+2* (ZI(IAWRK2-1+I)-1)+2)
   90 CONTINUE
      DO 100,I = 1,2*NDDLT
        ZI(IADEEQ-1+I) = ZI(IAWRK1-1+I)
  100 CONTINUE


C     -- ON REMET .LINO DANS UN ORDRE COHERENT AVEC .CONX:
C        ---------------------------------------------------
      ICO = 0
      DO 110,I = 1,NBNOET

C     -- SI C'EST UN NOEUD PHYSIQUE DU MAILLAGE :
        IF ((ZI(IACONX-1+3* (I-1)+1).EQ.1) .AND.
     &      (ZI(IACONX-1+3* (I-1)+3).EQ.0)) THEN
          ICO = ICO + 1
          ZI(IALINO-1+ICO) = ZI(IACONX-1+3* (I-1)+2)
        END IF
  110 CONTINUE
C
C --- MENAGE
      CALL JEDETR('&&SSRIU1.INTERNE')
      CALL JEDETR('&&SSRIU1.WORK1')
      CALL JEDETR('&&SSRIU1.WORK2')

      CALL JEDEMA()
      END
