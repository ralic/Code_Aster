       SUBROUTINE ALGOCL(DEFICO,RESOCO,MATASS,LMAT,LDSCON,NOMA,CINE,
     &                   DEPTOT,ITERAT,LREAC,RESU,LICCVG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/01/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C ======================================================================
      IMPLICIT NONE
C
      LOGICAL      LREAC(4)
      INTEGER      LMAT
      INTEGER      LDSCON
      INTEGER      ITERAT
      CHARACTER*8  NOMA
      CHARACTER*19 MATASS
      CHARACTER*24 DEFICO
      CHARACTER*24 RESOCO
      CHARACTER*24 CINE
      CHARACTER*24 RESU
      CHARACTER*24 DEPTOT
      INTEGER      LICCVG(5)
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : CFALGO
C ----------------------------------------------------------------------
C
C ALGORITHME DE CONTACT
C
C ALGO. POUR CONTACT    : DUALISATION (LAGRANGIEN)
C ALGO. POUR FROTTEMENT : SANS
C
C RESOLUTION DE : C.DU + ACT.MU  = F
C                 AC(U+DU)      <= E (POUR LES LIAISONS ACTIVES)
C
C AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
C
C     AC = MATRICE DE CONTACT
C
C      C = ( K  BT ) MATRICE DE RIGIDITE INCLUANT LES LAGRANGE
C          ( B  0  )
C
C      U = ( DEPL )
C          ( LAM  )
C
C      F = ( DL  ) DANS LA PHASE DE PREDICTION
C          ( DUD )
C
C      F = ( L - QT.SIG - BT.LAM  ) AU COURS D'UNE ITERATION DE NEWTON
C          (           0          )
C
C IN  DEFICO  : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO  : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C                'E':  RESOCO(1:14)//'.APJEU'
C                'E':  RESOCO(1:14)//'.LIAC'
C                'E':  RESOCO(1:14)//'.LIOT'
C                'E':  RESOCO(1:14)//'.MU'
C                'E':  RESOCO(1:14)//'.DEL0'
C                'E':  RESOCO(1:14)//'.DELT'
C                'E':  RESOCO(1:14)//'.COCO'
C                'E':  RESOCO(1:14)//'.CM1A'
C IN  MATASS  : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
C IN  LMAT    : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  LDSCON  : DESCRIPTEUR DE LA MATRICE -A.C-1.AT
C IN  NOMA    : NOM DU MAILLAGE
C IN  CINE    : CHAM_NO CINEMATIQUE
C IN  DEPTOT  : DEPLACEMENT TOTAL OBTENU A L'ISSUE DE L'ITERATION
C               DE NEWTON PRECEDENTE
C IN  ITERAT  : ITERATION COURANTE DE NEWTON
C IN  LREAC   : ETAT DU CONTACT
C              (1) = TRUE  SI REACTUALISATION A FAIRE
C              (2) = TRUE  SI ATTENTE POINT FIXE CONTACT
C              (3) = TRUE  SI METHODE CONTINUE
C              (4) = TRUE  SI MODELISATION DU CONTACT
C VAR RESU    : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS DEPTOT
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C OUT LICCVG  : CODES RETOURS D'ERREUR
C                       (1) PILOTAGE
C                       (2) LOI DE COMPORTEMENT
C                       (3) CONTACT/FROTTEMENT: NOMBRE MAXI D'ITERATIONS
C                       (4) CONTACT/FROTTEMENT: MATRICE SINGULIERE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
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
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      CFDISI
      COMPLEX*16   CBID
      CHARACTER*24 APPARI,COCO,APPOIN,DELTA,CONTNO,CONTMA
      INTEGER      JAPPAR,JCOCO,JAPPTR,JDELTA,JNOCO,JMACO
      CHARACTER*24 NOZOCO,APCOEF,APDDL,APJEU,APMEMO
      INTEGER      JZOCO,JAPCOE,JAPDDL,JAPJEU,JAPMEM
      CHARACTER*24 MACONT,K24BID
      CHARACTER*19 DELT0,LIAC,LIOT,MU
      INTEGER      JDELT0,JLIAC,JLIOT,JMU
      CHARACTER*19 SOLVEU
      INTEGER      JSLVK,JRESU,JDEPP
      INTEGER      IFM,NIV
      INTEGER      IBID,IZONE,II,JJ,KK,IER,LLF,LLF1,LLF2
      INTEGER      NESCL,NESMAX,NBLIAI,NBLIAC,NEQMAX
      INTEGER      NEQ,NDIM,NBDDL,NBLCIN
      INTEGER      NDECI,NPVNEG,ISINGU
      INTEGER      AJLIAI,SPLIAI,INDIC,INDFAC,SPAVAN,ILIAC,LLMIN
      INTEGER      JDECAL,POSIT,BTOTAL
      CHARACTER*1  TYPEAJ
      CHARACTER*2  TYPEC0
      LOGICAL      TROUAC,DELPOS,LELPIV
      REAL*8       XJVMAX,AJEU,VAL,AADELT,RHO,RHORHO,X1,R8BID
      REAL*8       R8MAEM,R8PREM
      INTEGER      ITER,ITEMAX,ITEMUL,ISTO
      LOGICAL      CFEXCL
C
C ----------------------------------------------------------------------
C
C ======================================================================
C             INITIALISATIONS DES OBJETS ET DES ADRESSES
C ======================================================================
C U      : DEPTOT + RESU+
C RESU   : INCREMENT DEPUIS DEPTOT (ACTUALISE AU COURS DES ITERATIONS
C          DE CONTRAINTES ACTIVES : RESU+ = RESU- + RHO.DELTA)
C          C'EST DU/K OU DU/K+1.
C DELTA  : INCREMENT DONNE PAR CHAQUE ITERATION DE CONTRAINTES ACTIVES.
C          C'EST D/K+1.
C DELT0  : INCREMENT DE DEPLACEMENT DEPUIS LA DERNIERE ITERATION DE
C          NEWTON SANS TRAITER LE CONTACT. C'EST C-1.F.
C ======================================================================
      CALL INFNIV(IFM,NIV)
      IF(NIV.GE.2) THEN
        WRITE (IFM,*) '<CONTACT> <> ALGO_CONTACT   : DUALISATION'
        WRITE (IFM,*) '<CONTACT> <> ALGO_FROTTEMENT: SANS'
      ENDIF
      CALL JEMARQ()
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CONTMA = DEFICO(1:16)//'.MAILCO'
      NOZOCO = DEFICO(1:16)//'.NOZOCO'
      APPARI = RESOCO(1:14)//'.APPARI'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APJEU  = RESOCO(1:14)//'.APJEU'
      APMEMO = RESOCO(1:14)//'.APMEMO'
      APDDL  = RESOCO(1:14)//'.APDDL'
      LIAC   = RESOCO(1:14)//'.LIAC'
      LIOT   = RESOCO(1:14)//'.LIOT'
      MU     = RESOCO(1:14)//'.MU'
      DELT0  = RESOCO(1:14)//'.DEL0'
      DELTA  = RESOCO(1:14)//'.DELT'
      COCO   = RESOCO(1:14)//'.COCO'
      MACONT = ZK24(ZI(LDSCON+1))
      SOLVEU = '&&OP0070.SOLVEUR'
C ======================================================================
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(NOZOCO,'L',JZOCO)
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APDDL,'L',JAPDDL)
      CALL JEVEUO(APJEU,'E',JAPJEU)
      CALL JEVEUO(APMEMO,'L',JAPMEM)
      CALL JEVEUO(LIAC,'E',JLIAC)
      CALL JEVEUO(LIOT,'E',JLIOT)
      CALL JEVEUO(MU,'E',JMU)
      CALL JEVEUO(DELT0,'E',JDELT0)
      CALL JEVEUO(DELTA,'E',JDELTA)
      CALL JEVEUO(RESU(1:19)//'.VALE','E',JRESU)
      CALL JEVEUO(DEPTOT(1:19)//'.VALE','E',JDEPP)
      CALL JEVEUO(COCO,'E',JCOCO)
      CALL JEVEUO(SOLVEU//'.SLVK','L',JSLVK)
      CALL JEECRA(MACONT(1:19)//'.REFA','DOCU',IBID,'ASSE')
C ======================================================================
C --- INITIALISATION DE VARIABLES
C --- NESCL  : NOMBRE DE NOEUDS ESCLAVES SUSCEPTIBLES D'ETRE EN CONTACT
C --- NBLIAI : NOMBRE DE LIAISONS DE CONTACT
C --- NEQ    : NOMBRE D'EQUATIONS DU MODELE
C --- ITEMUL : NOMBRE PAR LEQUEL IL FAUT MULTIPLIER LE NOMBRE DE
C              LIAISONS DE CONTACT POUR OBTENIR LE NOMBRE MAXI
C              D'ITERATIONS DANS L'ALGO ITEMAX=ITEMUL*NBLIAI
C --- ISTO   : ACTION STOP_SINGULIER='OUI' OU 'NON'
C --- ITEMAX : NOMBRE D'ITERATIONS DE CONTACT DANS L'ALGO
C --- NESMAX : NOMBRE MAXI DE NOEUDS ESCLAVES
C              SERT AU DECALAGE DANS LES ROUTINES DE FROTTEMENT 3D
C              (VAUT DONC ZERO SI SANS FROTTEMENT OU FROTTEMENT 2D)
C --- INDFAC : INDICE DE DEBUT DE LA FACTORISATION
C --- INDIC  : 0  INITIALISATION,
C             +1 ON A RAJOUTE UNE LIAISON
C             -1 ON A ENLEVE UNE LIAISON
C --- SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
C --- AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON CORRECTE DU CALCUL
C              DE LA MATRICE DE CONTACT ACM1AT
C --- LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
C              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
C               DIRECTIONS SIMULTANEES (EN 3D)
C                          -- ZERO --
C --- LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               PREMIERE DIRECTION (EN 3D)
C                          -- ZERO --
C --- LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               SECONDE DIRECTION (EN 3D)
C                          -- ZERO --
C ======================================================================
      NESCL  = ZI(JAPPAR)
      NBLIAI = NESCL
      NEQ    = ZI(LMAT+2)
      IZONE  = 1
      ITEMUL = CFDISI(DEFICO,'ITER_MULT_MAXI',IZONE)
      ITEMAX = ITEMUL*NBLIAI
      ISTO   = CFDISI(DEFICO,'STOP_SINGULIER',IZONE)
      NESMAX = 0
      SPLIAI = 0
      AJLIAI = 0
      INDIC  = 0
      INDFAC = 1
      LLF    = 0
      LLF1   = 0
      LLF2   = 0
      TYPEAJ = 'A'
      TYPEC0 = 'C0'
C ======================================================================
C --- SOUVENIRS DE L'ETAT DE CONTACT -> ON NE PEUT PAS S'EN SERVIR
C --- AU DEBUT CAR SI ON A REAPPARIE LES LIAISONS SONT DIFFERENTES
C ======================================================================
      CALL CFDISD(JCOCO,
     &            NDIM,NBLIAC,IBID,IBID,IBID)


C ======================================================================
C                             INITIALISATIONS
C ======================================================================
C ======================================================================
C --- RECOPIE DANS DELT0 DU CHAMP DE DEPLACEMENTS OBTENU SANS
C --- TRAITER LE CONTACT (LE DDEPLA DONNE PAR STAT_NON_LINE)
C --- CREATION DE DELTA0 = C-1B
C ======================================================================
      DO 10 II = 1,NEQ
        ZR(JDELT0-1+II) = ZR(JRESU-1+II)
        ZR(JRESU -1+II) = 0.0D0
   10 CONTINUE
      XJVMAX = 0.0D0
C ======================================================================
C --- DETECTION DES COUPLES DE NOEUDS INTERPENETRES
C --- ON CALCULE LE NOUVEAU JEU : AJEU+ = AJEU/I/N - A.DDEPLA
C --- (IL EST NEGATIF LORSQU'IL Y A INTERPENETRATION -> LIAISON ACTIVE)
C ======================================================================
      IF ( NIV .EQ. 2 ) THEN
        WRITE(IFM,*)'<CONTACT> <> LIAISONS INITIALES '
      ENDIF
      IF (ITERAT.EQ.0) THEN
         NBLIAC   = 0
         ZI(JLIOT+4*NBLIAI) = 0
         DO 30 II = 1,NBLIAI
            IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
              AJEU = ZR(JAPJEU+II-1)
              IF (AJEU.LT.0.0D0) THEN
               POSIT  = NBLIAC + 1
               CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &                     RESOCO,TYPEAJ,POSIT,II,TYPEC0)

               IF (NIV.GE.2) THEN
                CALL CFIMP2(IFM,NOMA,II,TYPEC0,TYPEAJ,'ALG',AJEU,
     &                      JAPPAR,JNOCO,JMACO)
               ENDIF
              ENDIF
            ENDIF
   30   CONTINUE
      ELSE
C ======================================================================
C --- TRAITEMENT DU CONTACT APRES REACTUALISATION GEOMETRIQUE
C ======================================================================
        IF (LREAC(1)) THEN
          BTOTAL = NBLIAC
          DO 50 II = 1,NBLIAI
            IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
              AJEU = ZR(JAPJEU+II-1)
              IF (AJEU.LT.0.0D0) THEN
                TROUAC = .FALSE.
                DO 40,JJ = 1,BTOTAL
                  IF (ZI(JLIAC-1+JJ).EQ.II) THEN
                    TROUAC = .TRUE.
                  END IF
   40           CONTINUE
                IF (.NOT.TROUAC) THEN
                  POSIT  = NBLIAC + 1
                  CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &                       RESOCO,TYPEAJ,POSIT,II,TYPEC0)
                  IF (NIV.GE.2) THEN
                    CALL CFIMP2(IFM,NOMA,II,TYPEC0,TYPEAJ,'ALG',AJEU,
     &                    JAPPAR,JNOCO,JMACO)
                   ENDIF
                ENDIF
              ENDIF
            ENDIF
   50     CONTINUE
        END IF
      END IF

      NBLCIN = NBLIAC
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1000) NBLIAI
        WRITE(IFM,1005) NBLIAC
        WRITE(IFM,1001) ITEMAX
      ENDIF

C ======================================================================
C                    REPRISE DE LA BOUCLE PRINCIPALE
C ======================================================================
      ITER = 0

 60   CONTINUE

      ITER = ITER + 1
C
C --- A-T-ON DEPASSE LE NOMBRE D'ITERATIONS DE CONTACT AUTORISE ?
C
      IF (ITER.GT.ITEMAX+1) THEN
        LICCVG(3) = 1
        GO TO 999
      END IF
C
C ======================================================================
C ---
C --- RESOLUTION MATRICIELLE POUR DES LIAISONS ACTIVES
C ---
C ======================================================================
C
      IF (NBLIAC.NE.0) THEN
C --- PAR LDLT OU MULT_FRONT
C
         SPAVAN = SPLIAI
C
C --- CALCUL DE -A.C-1.AT COLONNE PAR COLONNE (A PARTIR DE INDFAC)
C
         CALL CFACAT(NDIM,INDIC,NBLIAC,AJLIAI,SPLIAI,0,0,0,
     &               INDFAC,NESMAX,DEFICO,RESOCO,LMAT,CINE,NBLIAI,
     &               XJVMAX)
C ======================================================================
C ---
C --- ELIMINATION DES PIVOTS NULS
C ---
C ======================================================================
         CALL ELPIV1(XJVMAX, INDIC, NBLIAC, AJLIAI, SPLIAI, SPAVAN,
     &               NOMA, DEFICO, RESOCO)
C
C --- ON A SUPPRIME UNE LIAISON
C
         IF (INDIC .EQ. -1) THEN
            GOTO 60
         ENDIF
C ======================================================================
C ---
C --- FACTORISATION LDLT DE -A.C-1.AT
C ---
C ======================================================================
C --- ATTENTION : SI ON RAJOUTE DES LIAISONS ON NE FACTORISE QUE
C --- LA PARTIE RAJOUTEE (LE RESTE EST ENCORE VALABLE, CF. PROPRIETES
C --- MAGIQUES DES FACTORISATIONS).
C --- SI ON ENLEVE LA DERNIERE LIAISON (IDEBUT > NBLIAC),PAS BESOIN DE
C --- REFACTORISER : L'INSTRUCTION ZI(LDSCON+2) = NBLIAC ECRITE PLUS
C --- LOIN FERA QUE RLDLGG PRENDRA LA BONNE TAILLE DE MATRICE, QUI
C --- EST DEJA FACTORISEE (SI ON REFACTORISAIT A PARTIR DE 1, ON
C --- FACTORISERAIT LA FACTORISEE, CE QUI EST GENANT, CAR FACTORISATION
C --- EN PLACE)
C ======================================================================
        CALL JEECRA(MACONT(1:19)//'.REFA','DOCU',IBID,'ASSE')
C ======================================================================
        IF (INDFAC.LE.NBLIAC) THEN
           IF(NIV.GE.2) THEN
             WRITE(IFM,*)'<CONTACT> <> FACTORISATION MATRICE CONTACT '
           ENDIF
           CALL TLDLGG(2,LDSCON,INDFAC,NBLIAC,0,NDECI,ISINGU,NPVNEG,IER)
           INDFAC = NBLIAC + 1
C
C --- LA MATRICE DE CONTACT EST-ELLE SINGULIERE ?
C
           IF (IER.GT.ISTO) THEN
              LICCVG(4) = 1
              GOTO 999
           END IF
        END IF
C ======================================================================
C --- SECOND MEMBRE : ON MET JEU(DEPTOT) - A.DELT0 DANS MU
C ======================================================================
        DO 100 II = 1,NBLIAI
          ZR(JMU-1+II) = 0.D0
  100   CONTINUE
C ======================================================================
C --- APPEL DE LA ROUTINE DE CALCUL DU SECOND MEMBRE -------------------
C ======================================================================
        CALL CFADU(RESOCO, K24BID, NEQ, NDIM, NBLIAC, 0, 0, 0, NESMAX)
C ======================================================================
C --- RESOLUTION POUR OBTENIR MU : -A.C-1.AT.MU = JEU(DEPTOT) - A.DELT0
C --- ON TRUANDE LA SD MATR_ASSE POUR NE RESOUDRE LE SYSTEME QUE
C --- DE 1 A NBLIAC
C ======================================================================
        NEQMAX       = ZI(LDSCON+2)
        ZI(LDSCON+2) = NBLIAC
        CALL RLDLGG(LDSCON,ZR(JMU),CBID,1)
        ZI(LDSCON+2) = NEQMAX
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU
C ======================================================================
        DO 120 II = 1,NEQ
           ZR(JDELTA-1+II) = ZR(JDELT0-1+II) - ZR(JRESU-1+II)
  120   CONTINUE
C ======================================================================
C --- MISE A JOUR DU VECTEUR DEPLACEMENT <DU> CORRIGE PAR LE CONTACT
C ======================================================================
        CALL CFMAJU(RESOCO, NEQ, NDIM, NBLIAI, NBLIAC, 0, 0, 0)
      ELSE
        DO 101 II = 1,NEQ
           ZR(JDELTA-1+II) = ZR(JDELT0-1+II)
 101    CONTINUE
      END IF
C
C ======================================================================
C ---
C --- CALCUL DE RHO = MIN ( (E(DEPTOT) - A.RESU)II / (A.DELTA)II) )
C --- SUR LES LIAISONS NON ACTIVES DE NUMERO II
C ---
C ======================================================================
C
      RHO    = R8MAEM()
      DELPOS = .FALSE.
C
      IF (NBLIAC.EQ.NBLIAI) THEN
C ======================================================================
C -- SI TOUTES LES LIAISONS SONT ACTIVES : RHO = 1
C ======================================================================
         RHO = 1.D0
      ELSE IF (NBLIAC.LT.NBLIAI) THEN
C ======================================================================
C -- S'IL Y A DES LIAISONS NON ACTIVES : CALCUL DE RHO
C ======================================================================
        DO 180 II = 1,NBLIAI
          TROUAC = .FALSE.
C ======================================================================
C -- LA LIAISON II EST-ELLE ACTIVE ? (-> TROUAC)
C ======================================================================
          DO 170 ILIAC = 1,NBLIAC
            IF (ZI(JLIAC-1+ILIAC).EQ.II) TROUAC = .TRUE.
  170     CONTINUE
C ======================================================================
C -- CALCUL DE A.DELTA SI LA LIAISON II N'EST PAS ACTIVE
C ======================================================================
          IF (.NOT.TROUAC) THEN
            JDECAL = ZI(JAPPTR+II-1)
            NBDDL  = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                  ZR(JDELTA),AADELT)
C ======================================================================
C -- SI A.DELTA EST POSITIF POUR II : CALCUL DE E(DEPTOT) - A.RESU
C -- RHO = MIN ( ( E(DEPTOT) - A.RESU )II / (A.DELTA)II )
C -- ON STOCKE DANS LLMIN LE NUMERO DE LA LIAISON REALISANT LE
C -- MINIMUM (CE SERA LA LIAISON LA PLUS VIOLEE)
C ======================================================================
            IF (AADELT.GT.R8PREM()) THEN
C ======================================================================
C -- ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
C ======================================================================
               CALL CFELPV(II,TYPEC0,RESOCO,NBLIAI,LELPIV)
               IF (LELPIV) THEN
                 GOTO 180
               ENDIF
C ======================================================================
C -- FIN ELIMINATION LIAISON A PIVOT NUL
C ======================================================================
               DELPOS = .TRUE.
               CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                     ZI(JAPDDL+JDECAL),ZR(JRESU),VAL)
               IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
                 AJEU = ZR(JAPJEU+II-1) - VAL
                 AJEU = AJEU/AADELT
                 IF (AJEU.LT.RHO) THEN
                   RHO = AJEU
                   LLMIN = II
                 ENDIF
               ENDIF
             ENDIF
          END IF
 180   CONTINUE
C ======================================================================
C -- SI TOUS LES (A.DELTA)II SONT NEGATIFS : RHO = 1
C ======================================================================
        IF (.NOT.DELPOS) THEN
          RHO = 1.0D0
        END IF
      END IF
C ======================================================================
C ---
C --- TESTS SUR RHO ET ACTUALISATION DE RESU
C ---
C ======================================================================
      X1 = 1.D0
      RHORHO = MIN(RHO,X1)
C ======================================================================
C -- SI RHO < 1 (AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE) :
C -- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE (LLMIN)
C ======================================================================
      IF (RHORHO.LT.1.0D0) THEN
         POSIT = NBLIAC + 1
         CALL CFTABL(INDIC, NBLIAC, AJLIAI, SPLIAI, LLF,LLF1,LLF2,
     &               RESOCO, TYPEAJ, POSIT, LLMIN, TYPEC0)
         IF (NIV.GE.2) THEN
            CALL CFIMP2(IFM,NOMA,LLMIN,TYPEC0,TYPEAJ,'ALG',
     &                  ZR(JAPJEU-1+LLMIN),JAPPAR,JNOCO,JMACO)
         END IF
         DO 190 KK = 1,NEQ
            ZR(JRESU-1+KK) = ZR(JRESU-1+KK) + RHORHO*ZR(JDELTA-1+KK)
  190    CONTINUE
         GO TO 60
      END IF

C ======================================================================
C                            ON A CONVERGE
C ======================================================================

  191 CONTINUE

C ======================================================================
C --- ON ENLEVE TOUTES LES LIAISONS DE CONTACT POUR LESQUELLES
C --- LA PRESSION EST NEGATIVE
C ======================================================================
      CALL CFNEG(RESOCO,DEFICO,NOMA,NDIM,
     &           INDIC,NBLIAI,NBLIAC,AJLIAI,
     &           SPLIAI,LLF,LLF1,LLF2)
C ======================================================================
C --- RECUPERATION DU DEPLACEMENT FINAL
C ======================================================================
      DO 230 II = 1,NEQ
        ZR(JRESU-1+II) = ZR(JRESU-1+II) + ZR(JDELTA-1+II)
  230 CONTINUE
C ======================================================================
C --- CALCUL DES FORCES DE CONTACT (AT.MU)
C ======================================================================
      CALL CFATMU(NEQ,NESMAX,NDIM,NBLIAC,0,0,0,0,RESOCO)
C ======================================================================
C --- STOCKAGE DE L'ETAT DE CONTACT DEFINITIF
C ======================================================================
C --- VALEUR DES VARIABLES DE CONVERGENCE
      LICCVG(3) = 0
      LICCVG(4) = 0
C --- ATTENTE POINT FIXE
      IF ( NBLIAC.NE.NBLCIN ) THEN
        LREAC(2) = .TRUE.
      ENDIF
C --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
      ZI(JCOCO+1) = INDIC
      ZI(JCOCO+2) = NBLIAC
      ZI(JCOCO+3) = AJLIAI
      ZI(JCOCO+4) = SPLIAI
C ======================================================================
C --- CALCUL DU JEU FINAL
C ======================================================================
      CALL CFJEFI(NEQ,NBLIAI,
     &            JAPPTR,JAPCOE,JAPDDL,JRESU,JAPJEU,0,0)
C ======================================================================
C --- AFFICHAGE FINAL
C ======================================================================
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1002) ITER
        WRITE(IFM,1003) NBLIAC
        WRITE(IFM,*)'<CONTACT> <> LIAISONS FINALES '
        CALL CFIMP1(DEFICO,RESOCO,NOMA,NBLIAI,IFM)
      END IF
C
  999 CONTINUE
C ======================================================================
C --- SAUVEGARDE DES INFOS DE DIAGNOSTIC (NOMBRE D'ITERATIONS)
C ======================================================================
      CALL CFITER(RESOCO,'E','ITER',ITER,R8BID)
C
      CALL JEDEMA()
C
 1000 FORMAT (' <CONTACT> <> NOMBRE DE LIAISONS POSSIBLES: ',I6)
 1001 FORMAT (' <CONTACT> <> DEBUT DES ITERATIONS (MAX: ',I6,')')
 1002 FORMAT (' <CONTACT> <> FIN DES ITERATIONS (NBR: ',I6,')')
 1003 FORMAT (' <CONTACT> <> NOMBRE DE LIAISONS CONTACT FINALES:',
     &       I6,')')
 1005 FORMAT (' <CONTACT> <> NOMBRE DE LIAISONS CONTACT INITIALES:',
     &       I6,')')
C ======================================================================
      END
