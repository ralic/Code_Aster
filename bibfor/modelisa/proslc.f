      SUBROUTINE PROSLC(MATREZ,LIMAT,NBMAT,BASEZ,NUMEDD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/11/2004   AUTEUR MABBAS M.ABBAS 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)

C     PROSLC  --  LE BUT DE CETTE ROUTINE EST DE CONSTRUIRE LA MATR_ASSE
C                 DE NOM MATRES QUI VA RESULTER DE LA COMBINAISON
C                 LINEAIRE DES NBMAT MATR_ASSE DE LA LISTE LISMAT
C                 DE NOMS DE MATR_ASSE. LES MATRICES SONT STOCKEES EN
C                 LIGNE DE CIEL
C
C   ARGUMENT        E/S  TYPE         ROLE
C    MATREZ         OUT    K*     NOM DE LA MATR_ASSE RESULTANT DE LA
C                                 COMBINAISON LINEAIRE DES MATR_ASSE
C                                 DE LA LISTE LISMAT.
C    LIMAT          IN    K24     LISTE DES MATR_ASSE A COMBINER
C                                 DES MATR_ASSE A COMBINER.
C    NBMAT          IN    I       ON FAIT LA COMBINAISON LINEAIRE
C                                 DES NBMAT PREMIERS MATR_ASSE DE LA
C                                 LISTE LIMAT.
C    BASEZ          IN    K*      NOM DE LA BASE SUR LAQUELLE ON
C                                 CONSTRUIT LA MATR_ASSE.
C    NUMEDD         IN    K14    NOM DU NUME_DDL SUR LEQUEL S'APPUIERA
C                                 LA MATR_ASSE MATREZ
C        SI NUMEDD  =' ', LE NOM DU NUME_DDL SERA OBTENU PAR GCNCON
C        SI NUMEDD /=' ', ON PRENDRA NUMEDD COMME NOM DE NUME_DDL

C.========================= DEBUT DES DECLARATIONS ====================
C ----- COMMUNS NORMALISES  JEVEUX
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
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C -----  ARGUMENTS
      INTEGER NBMAT
      CHARACTER*(*) MATREZ,BASEZ,NUMEDD
      CHARACTER*24 LIMAT(NBMAT)
C -----  VARIABLES LOCALES
      CHARACTER*1 K1BID,BASE
      CHARACTER*14 NUMDDL,NUMDD1,NUMDDI
      CHARACTER*19 MATRES,MAT1,MATI
      CHARACTER*19 PFCHNO
      CHARACTER*24 KHCOL,KADIA,KABLO,KIABL,KDESC,KREFA,KCONL,KVALE,
     &             KABLO1,KHCO1,KREFE,KREFI

C.========================= DEBUT DU CODE EXECUTABLE ==================

      CALL JEMARQ()

C --- INITIALISATIONS :
C     ---------------
      BASE = BASEZ
      MATRES = MATREZ

      UN = 1.0D0

C --- NOM DU NUME_DDL A CONSTRUIRE :
C     ----------------------------
      IF (NUMEDD.EQ.' ') THEN
        CALL GCNCON('_',NUMDDL(1:8))
        NUMDDL(9:14) = '.NUDDL'
      ELSE
        NUMDDL=NUMEDD
      END IF


C --- NOM DE LA PREMIERE MATR_ASSE :
C     ----------------------------
      MAT1 = LIMAT(1) (1:19)

C --- RECUPERATION DU NUME_DDL ATTACHE A LA PREMIERE MATR_ASSE  :
C     --------------------------------------------------------
      CALL DISMOI('F','NOM_NUME_DDL',MAT1,'MATR_ASSE',IBID,NUMDD1,IER)

C --- RECOPIE DU PROF_CHNO DE LA PREMIERE MATRICE SUR LA MATRICE
C --- RESULTANTE :
C     ---------
      CALL JEDUPO(NUMDD1//'.NUME.DEEQ',BASE,NUMDDL//'.NUME.DEEQ',
     &            .FALSE.)
      CALL JEDUPO(NUMDD1//'.NUME.DELG',BASE,NUMDDL//'.NUME.DELG',
     &            .FALSE.)
      CALL JEDUPO(NUMDD1//'.NUME.LILI',BASE,NUMDDL//'.NUME.LILI',
     &            .FALSE.)
      CALL JEDUPO(NUMDD1//'.NUME.LPRN',BASE,NUMDDL//'.NUME.LPRN',
     &            .FALSE.)
      CALL JEDUPO(NUMDD1//'.NUME.NUEQ',BASE,NUMDDL//'.NUME.NUEQ',
     &            .FALSE.)
      CALL JEDUPO(NUMDD1//'.NUME.PRNO',BASE,NUMDDL//'.NUME.PRNO',.TRUE.)
      CALL JEDUPO(NUMDD1//'.NUME.REFN',BASE,NUMDDL//'.NUME.REFN',
     &            .FALSE.)

C --- CREATION ET AFFECTATION DU TABLEAU .ABLO DES NUMEROS
C --- DES LIGNES DE DEBUT ET DE FIN DE BLOC :
C     ====================================
      KABLO = NUMDDL//'.SLCS.ABLO'

      KABLO1 = NUMDD1//'.SLCS.ABLO'

C --- ON INITIALISE LE .ABLO DE LA MATRICE RESULTANTE EN LUI
C --- AFFECTANT D'ABORD LE .ABLO DE LA PREMIERE MATRICE A COMBINER :
C     ------------------------------------------------------------
      CALL JEDUPO(KABLO1,BASE,KABLO,.FALSE.)
      CALL JEVEUO(KABLO,'E',IDABLO)

C --- CONCATENATION DE TOUS LES .ABLO DE LA LISTE A COMBINER :
C     ------------------------------------------------------
      DO 10 I = 2,NBMAT

C ---   NOM DE LA MATR_ASSE COURANTE :
C       ----------------------------
        MATI = LIMAT(I) (1:19)

C ---   RECUPERATION DU NUME_DDL ATTACHE A LA IEME MATR_ASSE  :
C       ----------------------------------------------------
        CALL DISMOI('F','NOM_NUME_DDL',MATI,'MATR_ASSE',IBID,NUMDDI,IER)

C ---   RECUPERATION DU .ABLO CORRESPONDANT :
C       -----------------------------------
        CALL JEVEUO(NUMDDI//'.SLCS.ABLO','L',IDABLI)

C ---   CONCATENATION :
C       -------------
        CALL COCALI(KABLO,NUMDDI//'.SLCS.ABLO','I')

   10 CONTINUE

C --- LONGUEUR DU VECTEUR .ABLO FINAL :
C     -------------------------------
      CALL JELIRA(KABLO,'LONMAX',NBLOC1,K1BID)
      CALL JEVEUO(KABLO,'E',IDABLO)

C --- TRI PAR ORDRE CROISSANT ET SUPPRESSION DES VALEURS
C --- MULTIPLES DU .ABLO :
C     ------------------
      CALL UTTRII(ZI(IDABLO),NBLOC1)

C --- NOMBRE DE BLOCS DE LA MATRICE RESULTANTE :
C     ----------------------------------------
      NBLOC = NBLOC1 - 1

C --- RECUPERATION DU NOMBRE D'EQUATIONS DE LA PREMIERE MATRICE
C --- A COMBINER (C'EST LE MEME POUR TOUTES LES MATRICES) :
C     ---------------------------------------------------
      CALL JEVEUO(NUMDD1//'.SLCS.DESC','L',IDESC1)

      NEQ = ZI(IDESC1+1-1)

C --- CREATION ET AFFECTATION DU TABLEAU .HCOL DES LONGUEURS
C --- DE LIGNES , ON PREND LA LONGUEUR MAXIMALE DES LIGNES :
C     ====================================================
      KHCOL = NUMDDL//'.SLCS.HCOL'
      CALL WKVECT(KHCOL,BASE//' V I',NEQ,IDHCOL)

      DO 30 I = 1,NBMAT

C ---   NOM DE LA MATR_ASSE COURANTE :
C       ----------------------------
        MATI = LIMAT(I) (1:19)

C ---   RECUPERATION DU NUME_DDL ATTACHE A LA IEME MATR_ASSE  :
C       ----------------------------------------------------
        CALL DISMOI('F','NOM_NUME_DDL',MATI,'MATR_ASSE',IBID,NUMDDI,IER)

C ---   RECUPERATION DU .HCOL CORRESPONDANT :
C       -----------------------------------
        CALL JEVEUO(NUMDDI//'.SLCS.HCOL','L',IDHCOI)

        DO 20 J = 1,NEQ
          ZI(IDHCOL+J-1) = MAX(ZI(IDHCOL+J-1),ZI(IDHCOI+J-1))
   20   CONTINUE
   30 CONTINUE

C --- CREATION ET AFFECTATION DU TABLEAU .ADIA DES POSITIONS
C --- DES TERMES DIAGONAUX DANS LES BLOCS :
C     ===================================
      KADIA = NUMDDL//'.SLCS.ADIA'

      CALL WKVECT(KADIA,BASE//' V I',NEQ,IDADIA)

C --- BOUCLE SUR LES BLOCS DE LA MATRICE :
C     ---------------------------------
      DO 50 IBLOC = 1,NBLOC

C ---   PREMIERE LIGNE DU BLOC :
C       ----------------------
        IL1 = ZI(IDABLO+IBLOC-1) + 1

C ---   DERNIERE LIGNE DU BLOC :
C       ----------------------
        IL2 = ZI(IDABLO+IBLOC)

        ZI(IDADIA+IL1-1) = ZI(IDHCOL+IL1-1)

C ---   BOUCLE SUR LES LIGNES DU BLOC :
C       -----------------------------
        DO 40 I = IL1 + 1,IL2
          ZI(IDADIA+I-1) = ZI(IDADIA+I-1-1) + ZI(IDHCOL+I-1)
   40   CONTINUE
   50 CONTINUE

C --- CREATION ET AFFECTATION DU TABLEAU .IABL FAISANT CORRESPONDRE
C --- A UN NUMERO DE LIGNE DONNE, LE BLOC AUQUEL IL APPARTIENT :
C     ========================================================
      KIABL = NUMDDL//'.SLCS.IABL'

      CALL WKVECT(KIABL,BASE//' V I',NEQ,IDIABL)

      IEQ = 0

C --- BOUCLE SUR LES BLOCS DE LA MATRICE :
C     ---------------------------------
      DO 70 IBLOC = 1,NBLOC

C ---   PREMIERE LIGNE DU BLOC :
C       ----------------------
        IL1 = ZI(IDABLO+IBLOC-1) + 1

C ---   DERNIERE LIGNE DU BLOC :
C       ----------------------
        IL2 = ZI(IDABLO+IBLOC)

C ---   BOUCLE SUR LES LIGNES DU BLOC :
C       -----------------------------
        DO 60 I = IL1,IL2
          IEQ = IEQ + 1
          ZI(IDIABL+IEQ-1) = IBLOC
   60   CONTINUE
   70 CONTINUE

C --- DETERMINATION DE LA PLUS GRANDE TAILLE DE BLOC DE LA
C --- MATRICE RESULTANTE :
C     ==================

      NBTERM = 0

C --- BOUCLE SUR LES BLOCS DE LA MATRICE :
C     ---------------------------------
      DO 90 IBLOC = 1,NBLOC

        NBTERB = 0

C ---   PREMIERE LIGNE DU BLOC :
C       ----------------------
        IL1 = ZI(IDABLO+IBLOC-1) + 1

C ---   DERNIERE LIGNE DU BLOC :
C       ----------------------
        IL2 = ZI(IDABLO+IBLOC)

C ---  BOUCLE SUR LES LIGNES DE LA MATRICE :
C      ----------------------------------
        DO 80 I = IL1,IL2

          NBTERB = NBTERB + ZI(IDHCOL+I-1)

   80   CONTINUE

        NBTERM = MAX(NBTERM,NBTERB)

   90 CONTINUE

      ITBLOC = NBTERM

C --- CREATION ET AFFECTATION DU TABLEAU .DESC DE DESCRIPTION
C --- DE LA MATRICE :
C     =============
      KDESC = NUMDDL//'.SLCS.DESC'

      CALL WKVECT(KDESC,BASE//' V I',6,IDDESC)

      ZI(IDDESC+1-1) = NEQ
      ZI(IDDESC+2-1) = ITBLOC
      ZI(IDDESC+3-1) = NBLOC

C --- CREATION ET AFFECTATION DE LA COLLECTION .VALE
C --- DES BLOCS DE LA MATRICE :
C     =========================
      KVALE = MATRES//'.VALE'

      CALL JECREC(KVALE,BASE//' V R','NU','DISPERSE','CONSTANT',NBLOC)

      CALL JEECRA(KVALE,'LONMAX',ITBLOC,' ')
      CALL JEECRA(KVALE,'DOCU',IBID,'MS')

C --- BOUCLE SUR LES BLOCS DE LA MATRICE :
C     ----------------------------------
      DO 100 I = 1,NBLOC

C ---   CREATION DU BLOC COURANT DE LA MATRICE :
C       --------------------------------------
        CALL JECROC(JEXNUM(KVALE,I))
        CALL JEVEUO(JEXNUM(KVALE,I),'E',IDKVAL)
        CALL JELIBE(JEXNUM(KVALE,I))

  100 CONTINUE

C --- CREATION ET AFFECTATION DU TABLEAU .REFA DES REFERENCES
C --- DE LA MATRICE :
C     =============
      KREFA = MATRES//'.REFA'
      KCONL = MATRES//'.CONL'
      CALL WKVECT(KREFA,BASE//' V K24',4,IDREFA)

      ZK24(IDREFA+2-1) = NUMDDL//'.NUME'
      ZK24(IDREFA+3-1) = NUMDDL//'.SLCS'

C --- BOUCLE SUR LES MATRICES A COMBINER :
C     ----------------------------------
      DO 110 I = 1,NBMAT

C ---   NOM DE LA MATR_ASSE COURANTE :
C       ----------------------------
        MATI = LIMAT(I) (1:19)
        KREFI = MATI//'.REFA'
        CALL JEVEUO(KREFI,'L',IDREFI)
        IF (ZK24(IDREFI+1-1).NE.' ') THEN
          ZK24(IDREFA+1-1) = ZK24(IDREFI+1-1)
          GO TO 120
        END IF
  110 CONTINUE
  120 CONTINUE

C --- ON AFFECTE L'ETAT 'ASSEMBLE' A LA MATRICE :
C     -----------------------------------------
      CALL JEECRA(KREFA,'DOCU',IBID,'ASSE')

C --- CREATION ET AFFECTATION DE L'OBJET .REFE REFERENCANT LE
C --- NUME_DDL DE LA MATRICE :
C     ======================
      KREFE = NUMDDL//'.SLCS.REFE'
      CALL WKVECT(KREFE,BASE//' V K24',1,IDREFE)

      ZK24(IDREFE+1-1) (1:14) = NUMDDL

C --- ON AFFECTE LE STOCKAGE EN LIGNE DE CIEL A LA MATRICE :
C     ----------------------------------------------------
      CALL JEECRA(KREFE,'DOCU',IBID,'SLCS')

C --- CREATION ET AFFECTATION DU VECTEUR .CONL DE CONDITIONNEMENT
C --- DES INCONNUES DE LAGRANGE :
C     =========================
      CALL WKVECT(KCONL,BASE//' V R',NEQ,IDCONL)

      DO 130 IEQ = 1,NEQ
        ZR(IDCONL+IEQ-1) = UN
  130 CONTINUE

      CALL JEDEMA()

      END
