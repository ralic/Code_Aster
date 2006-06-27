      SUBROUTINE FROPGD (DEFICO,RESOCO,LMAT,LDSCON,NOMA,CINE,
     &           RESU,DEPTOT,ITERAT,LREAC,CONV,DEPDEL,LICCVG)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/06/2006   AUTEUR MABBAS M.ABBAS 
C TOLE CRP_20
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
      IMPLICIT     NONE
      LOGICAL      LREAC(4)
      INTEGER      LMAT
      INTEGER      LDSCON
      INTEGER      ITERAT
      REAL*8       CONV(*)
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO
      CHARACTER*24 RESOCO
      CHARACTER*24 CINE
      CHARACTER*24 RESU
      CHARACTER*24 DEPTOT
      CHARACTER*24 DEPDEL
      INTEGER      LICCVG(5)
C ======================================================================
C ROUTINE APPELEE PAR : CFALGO
C ======================================================================
C
C ALGO DE CONTACT/FROTTEMENT
C
C ALGO. POUR CONTACT    : DUALISATION (LAGRANGIEN)
C ALGO. POUR FROTTEMENT : PENALISATION
C
C
C RESO. DE : C.DU + ACT.MUC     = F - KG AGT.AG (E-U)
C                 AC. (U+DU)   <= E  (= POUR LES LIAISONS ACTIVES)
C
C AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
C
C     AC = MATRICE DE CONTACT
C
C    AG  = MATRICE DE FROTTEMENT POUR LES NOEUDS GLISSANTS
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
C IN  LMAT    : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  LDSCON  : DESCRIPTEUR DE LA MATRICE -A.C-1.AT
C IN  NOMA    : NOM DU MAILLAGE
C IN  CINE    : CHAM_NO CINEMATIQUE
C IN  DEPTOT  : DEPLACEMENT TOTAL OBTENU A L'ISSUE DE L'ITERATION
C               DE NEWTON PRECEDENTE
C IN  ITERAT : ITERATION DE NEWTON
C IN  LREAC  : ETAT DU CONTACT
C              (1) = TRUE  SI REACTUALISATION A FAIRE
C              (2) = TRUE  SI ATTENTE POINT FIXE CONTACT
C              (3) = TRUE  SI METHODE CONTINUE
C              (4) = TRUE  SI MODELISATION DU CONTACT
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C VAR RESU   : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS DEPTOT
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C IN  CONV   : INFORMATIONS SUR LA CONVERGENCE DU CALCUL
C                     1 - RESI_DUAL_ABSO      (LAGRANGIEN AUGMENTE)
C                     2 - RESI_PRIM_ABSO      (LAGRANGIEN AUGMENTE)
C                     3 - NOMBRE D'ITERATIONS DUAL (LAGRANGIEN AUGMENTE)
C                     4 - NUMERO ITERATION BFGS (LAGRANGIEN AUGMENTE)
C                    10 - NOMBRE D'ITERATIONS (RECHERCHE LINEAIRE)
C                    11 - RHO                 (RECHERCHE LINEAIRE)
C                    20 - RESI_GLOB_RELA
C                    21 - RESI_GLOB_MAXI
C OUT LICCVG : CODES RETOURS D'ERREUR
C                    (1) PILOTAGE
C                    (2) LOI DE COMPORTEMENT
C                    (3) CONTACT/FROTTEMENT: NOMBRE MAXI D'ITERATIONS
C                    (4) CONTACT/FROTTEMENT: MATRICE SINGULIERE
C
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------
C
      CHARACTER*32       JEXNUM
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
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
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      ZCONV
      PARAMETER    (ZCONV=4)
      LOGICAL      TROUAC,DELPOS,LELPIV,CFEXCL
      INTEGER      IFM,NIV
      INTEGER      IBID,IER,II,JJ,KK,JJC,LL,IZONE
      INTEGER      NDECI,ISINGU,NPVNEG,ISTO
      INTEGER      NEQ,NESCL,NBLIAC,NBLIAI,NDIM
      INTEGER      NBDDL,NEQMAX,NBLCIN,NESMAX,NBLIG
      INTEGER      ILIAC,LLIAC,JDECAL
      INTEGER      INDFAC,POSIT,SPAVAN,INDIC,NUMIN
      INTEGER      BTOTAL,ITEMUL
      REAL*8       R8MAEM,R8PREM,AJEU,RHO,RHORHO,AADELT,AJEUFY,XF
      REAL*8       X1,VAL,XXMIN,XXMAX,XX
      REAL*8       XTOL,AJEUFX,XK,XMU,R8BID
      REAL*8       ALPHA,BETA,RESIGR,XJVMAX
      INTEGER      ITER,ITEMAX
      INTEGER      AJLIAI,SPLIAI,LLF,LLF1,LLF2
      COMPLEX*16   CBID
      CHARACTER*1  TYPEAJ
      CHARACTER*2  TYPEC0
      CHARACTER*14 NUMEDD
      CHARACTER*19 MAT,MAF2,MAFROT
      INTEGER      JRESU,JDEPDE,JDEPP
      CHARACTER*19 AFMU,MAF1,CM2A,CM3A
      INTEGER      JAFMU,LMAF1,JCM2A,JCM3A
      CHARACTER*19 LIAC,MU,ATMU,DELT0,DELTA,COCO,LIOT
      INTEGER      JLIAC,JMU,JATMU,JDELT0,JDELTA,JCOCO,JLIOT
      CHARACTER*24 APJEFX,APJEFY,APMEMO
      INTEGER      JAPJFY,JAPJFX,JAPMEM
      CHARACTER*24 NDIMCO,CONTNO,CONTMA,APCOFR,FROTE,PENAL,COMAFO
      INTEGER      JDIM,JNOCO,JMACO,JAPCOF,IFRO,IPENA,ICOMA
      CHARACTER*24 CONVCO,APPARI,APPOIN,APCOEF,APJEU,APDDL
      INTEGER      JCONV,JAPPAR,JAPPTR,JAPCOE,JAPJEU,JAPDDL
C
C ======================================================================
C
C ======================================================================
C             INITIALISATIONS DES OBJETS ET DES ADRESSES
C ======================================================================
C U      : DEPTOT + RESU+
C DEPTOT : DEPLACEMENT TOTAL OBTENU A L'ISSUE DE L'ITERATION DE NEWTON
C          PRECEDENTE. C'EST U/I/N.
C RESU   : INCREMENT DEPUIS DEPTOT (ACTUALISE AU COURS DES ITERATIONS
C          DE CONTRAINTES ACTIVES : RESU+ = RESU- + RHO.DELTA)
C          C'EST DU/K OU DU/K+1.
C DELTA  : INCREMENT DONNE PAR CHAQUE ITERATION DE CONTRAINTES ACTIVES.
C          C'EST D/K+1.
C DELT0  : INCREMENT DE DEPLACEMENT DEPUIS LA DERNIERE ITERATION DE
C          NEWTON SANS TRAITER LE CONTACT. C'EST C-1.F.
C ======================================================================
      CALL INFNIV (IFM,NIV)
      IF (NIV.GE.2) THEN
           WRITE (IFM,*) '<CONTACT> <> ALGO_CONTACT   : DUALISATION'
           WRITE (IFM,*) '<CONTACT> <> ALGO_FROTTEMENT: PENALISATION'
      ENDIF
      CALL JEMARQ ()
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
      APPARI = RESOCO(1:14)//'.APPARI'
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CONVCO = DEFICO(1:16)//'.CONVCO'
      COCO   = RESOCO(1:14)//'.COCO'
      CONTMA = DEFICO(1:16)//'.MAILCO'
      APPARI = RESOCO(1:14)//'.APPARI'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APCOFR = RESOCO(1:14)//'.APCOFR'
      APJEU  = RESOCO(1:14)//'.APJEU'
      APJEFX = RESOCO(1:14)//'.APJEFX'
      APJEFY = RESOCO(1:14)//'.APJEFY'
      APMEMO = RESOCO(1:14)//'.APMEMO'
      APDDL  = RESOCO(1:14)//'.APDDL'
      LIAC   = RESOCO(1:14)//'.LIAC'
      LIOT   = RESOCO(1:14)//'.LIOT'
      MU     = RESOCO(1:14)//'.MU'
      ATMU   = RESOCO(1:14)//'.ATMU'
      AFMU   = RESOCO(1:14)//'.AFMU'
      DELT0  = RESOCO(1:14)//'.DEL0'
      DELTA  = RESOCO(1:14)//'.DELT'
      CM2A   = RESOCO(1:14)//'.CM2A'
      CM3A   = RESOCO(1:14)//'.CM3A'
      MAFROT = RESOCO(1:8)//'.MAFR'
      MAF1   = '&&FROPGD.MAF1'
      MAF2   = '&&FROPGD.MAF2'
      FROTE  = DEFICO(1:16)//'.FROTE'
      PENAL  = DEFICO(1:16)//'.PENAL'
      COMAFO = DEFICO(1:16)//'.COMAFO'
      NDIMCO = DEFICO(1:16)//'.NDIMCO'
      MAT    = ZK24(ZI(LMAT+1))
C ======================================================================
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(COCO,'E',JCOCO)
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(CONVCO,'L',JCONV)
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APCOFR,'L',JAPCOF)
      CALL JEVEUO(APJEU,'E',JAPJEU)
      CALL JEVEUO(APJEFX,'E',JAPJFX)
      CALL JEVEUO(APJEFY,'E',JAPJFY)
      CALL JEVEUO(APMEMO,'L',JAPMEM)
      CALL JEVEUO(APDDL,'L',JAPDDL)
      CALL JEVEUO(LIAC,'E',JLIAC)
      CALL JEVEUO(LIOT,'E',JLIOT)
      CALL JEVEUO(MU,'E',JMU)
      CALL JEVEUO(ATMU,'E',JATMU)
      CALL JEVEUO(AFMU,'E',JAFMU)
      CALL JEVEUO(DELT0,'E',JDELT0)
      CALL JEVEUO(DELTA,'E',JDELTA)
      CALL JEVEUO(RESU(1:19)//'.VALE','E',JRESU)
      CALL JEVEUO(DEPTOT(1:19)//'.VALE','L',JDEPP)
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      CALL JEVEUO(FROTE,'L',IFRO)
      CALL JEVEUO(PENAL,'L',IPENA)
      CALL JEVEUO(COMAFO,'L',ICOMA)
      CALL JEVEUO(NDIMCO,'L',JDIM)
      CALL DISMOI('F','NOM_NUME_DDL',MAT,'MATR_ASSE',IBID,NUMEDD,IER)
C ======================================================================
C --- INITIALISATION DE VARIABLES
C --- NESCL  : NOMBRE DE NOEUDS ESCLAVES SUSCEPTIBLES D'ETRE EN CONTACT
C --- NBLIAI : NOMBRE DE LIAISONS DE CONTACT
C --- NESMAX : NOMBRE MAXI DE NOEUDS ESCLAVES
C              SERT AU DECALAGE DANS LES ROUTINES DE FROTTEMENT 3D
C              (VAUT DONC ZERO SI SANS FROTTEMENT OU FROTTEMENT 2D)
C --- ITEMUL : NOMBRE PAR LEQUEL IL FAUT MULTIPLIER LE NOMBRE DE
C              LIAISONS DE CONTACT POUR OBTENIR LE NOMBRE MAXI
C              D'ITERATIONS DANS L'ALGO ITEMAX=ITEMUL*NBLIAI
C --- ISTO   : ACTION STOP_SINGULIER='OUI' OU 'NON'
C --- ITEMAX : NOMBRE D'ITERATIONS DE CONTACT DANS L'ALGO
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
C --- LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               PREMIERE DIRECTION (EN 3D)
C --- LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               SECONDE DIRECTION (EN 3D)
C --- XJVMAX : VALEUR MAXI DU PIVOT DE LA MATRICE DE CONTACT
C ======================================================================
C ======================================================================
      NESCL  = ZI(JAPPAR)
      NESMAX = ZI(JDIM+8)
      NBLIAI = NESCL
      RESIGR = CONV(20)
      NEQ    = ZI(LMAT+2)
      IZONE  = 1
      ITEMUL = ZI(JCONV+ZCONV*(IZONE-1)+2)
      ITEMAX = ITEMUL*NBLIAI
      ISTO   = ZI(JCONV+ZCONV*(IZONE-1))
      INDIC  = 0
      INDFAC = 1
      SPLIAI = 0
      AJLIAI = 0
      TYPEAJ = 'A'
      TYPEC0 = 'C0'
C --- TOLERANCE UTILISEE POUR LE FROTTEMENT
      XTOL   = 1.D-08
      XJVMAX = 0.0D0
C ======================================================================
C --- SOUVENIRS DE L'ETAT DE CONTACT -> ON NE PEUT PAS S'EN SERVIR
C --- AU DEBUT CAR SI ON A REAPPARIE LES LIAISONS SONT DIFFERENTES
C ======================================================================
      CALL CFDISD(JCOCO,
     &            NDIM,NBLIAC,LLF,LLF1,LLF2)
      LLF    = 0
      LLF1   = 0
      LLF2   = 0
C ======================================================================
C --- CREATION DE DELTA0 = C-1B
C ======================================================================
      DO 10 II = 1,NEQ
        ZR(JDELT0-1+II) = ZR(JRESU-1+II)
        ZR(JRESU-1+II) = 0.0D0
        ZR(JATMU-1+II) = 0.0D0
   10 CONTINUE

C ======================================================================
C --- DETECTION DES COUPLES DE NOEUDS INTERPENETRES
C --- ON CALCULE LE NOUVEAU JEU : AJEU+ = AJEU/I/N - A.DDEPLA
C --- (IL EST NEGATIF LORSQU'IL Y A INTERPENETRATION -> LIAISON ACTIVE)
C ======================================================================
      IF ( NIV .EQ. 2 ) THEN
        WRITE(IFM,*)'<CONTACT> <> LIAISONS INITIALES '
      ENDIF
      IF (ITERAT.EQ.0) THEN
        ZI(JLIOT+4*NBLIAI) = 0
        ZI(JLIOT+4*NBLIAI+1) = 0
        ZI(JLIOT+4*NBLIAI+2) = 0
        ZI(JLIOT+4*NBLIAI+3) = 0
        NBLIAC = 0
        DO 30 II = 1,NBLIAI
          ZR(JMU-1+3*NBLIAI+II) = 0.D0
          ZR(JMU-1+2*NBLIAI+II) = 0.D0
          ZR(JMU-1+NBLIAI+II) = 0.D0
          ZR(JMU-1+II) = 0.D0
          JDECAL = ZI(JAPPTR+II-1)
          NBDDL = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
          CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                ZR(JDELTA),VAL)
          IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
            AJEU = ZR(JAPJEU+II-1)-VAL
            IF (AJEU.LT.0.0D0) THEN
              POSIT  = NBLIAC + 1
              CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &                    RESOCO,TYPEAJ,POSIT,II,TYPEC0)
              IF (NIV.GE.2) THEN
                CALL CFIMP2(IFM,NOMA,II,TYPEC0,TYPEAJ,'ALG',
     &                      AJEU,JAPPAR,JNOCO,JMACO)
              END IF
            END IF
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
              IF (AJEU.LE.R8PREM()) THEN
                TROUAC = .FALSE.
                DO 40,JJ = 1, BTOTAL
                  IF (ZI(JLIAC-1+JJ).EQ.II) THEN
                    TROUAC = .TRUE.
                  END IF
   40           CONTINUE
                IF (.NOT.TROUAC) THEN
                   POSIT = NBLIAC + 1
                   CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                         RESOCO,TYPEAJ,POSIT,II,TYPEC0)
                   IF (NIV.GE.2) THEN
                     CALL CFIMP2(IFM,NOMA,II,TYPEC0,TYPEAJ,'ALG',
     &                      AJEU,JAPPAR,JNOCO,JMACO)
                   END IF
                END IF
              END IF
            ENDIF
   50     CONTINUE
        END IF
      END IF
C ======================================================================
C --- FIN DE LA PRISE EN COMPTE DU FROTTEMENT A L'INITIATION
C ======================================================================
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

   60 CONTINUE

      ITER = ITER + 1
C
C --- A-T-ON DEPASSE LE NOMBRE D'ITERATIONS DE CONTACT AUTORISE ?
C
      IF (ITER.GT.ITEMAX+1) THEN
        LICCVG(3) = 1
        GOTO 999
      END IF
C
C ======================================================================
C ---
C --- RESOLUTION MATRICIELLE POUR DES LIAISONS ACTIVES
C ---
C ======================================================================
C
      IF (NBLIAC.NE.0) THEN
C
C --- CALCUL DE -A.C-1.AT COLONNE PAR COLONNE (A PARTIR DE INDFAC)
C
         SPAVAN = SPLIAI
         CALL CFACAT(NDIM, INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1,
     +    LLF2, INDFAC, NESMAX,DEFICO, RESOCO, LMAT, CINE,NBLIAI,XJVMAX)
C ======================================================================
C ---
C --- ELIMINATION DES PIVOTS NULS
C ---
C ======================================================================
         CALL ELPIV1(XJVMAX, INDIC, NBLIAC, AJLIAI, SPLIAI, SPAVAN,
     +               NOMA, DEFICO, RESOCO)
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
C --- SI ON ENLEVE LA DERNIERE LIAISON,PAS BESOIN DE
C --- REFACTORISER : L'INSTRUCTION ZI(LDSCON+2) = NBLIAC ECRITE PLUS
C --- LOIN FERA QUE RLDLGG PRENDRA LA BONNE TAILLE DE MATRICE, QUI
C --- EST DEJA FACTORISEE (SI ON REFACTORISAIT A PARTIR DE 1, ON
C --- FACTORISERAIT LA FACTORISEE, CE QUI EST GENANT, CAR FACTORISATION
C --- EN PLACE)
C ======================================================================
C ======================================================================
         IF (INDFAC.LE.NBLIAC) THEN
           IF(NIV.GE.2) THEN
             WRITE(IFM,*)'<CONTACT> <> FACTORISATION MATRICE CONTACT '
           ENDIF
           CALL TLDLG3 ('LDLT',' ',2,LDSCON,INDFAC,NBLIAC,0,NDECI,
     &                   ISINGU,NPVNEG,IER)
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
        DO 120 II = 1,NDIM*NBLIAI
          ZR(JMU-1+II) = 0.D0
  120   CONTINUE
C ======================================================================
C --- APPEL DE LA ROUTINE DE CALCUL DU SECOND MEMBRE -------------------
C ======================================================================
        CALL CFADU(RESOCO,DEPDEL,NEQ,NDIM,NBLIAC,LLF,LLF1,LLF2,NESMAX)
C ======================================================================
C --- RESOLUTION POUR OBTENIR MU : -A.C-1.AT.MU = JEU(DEPTOT) - A.DELT0
C --- ON TRUANDE LA SD MATR_ASSE POUR NE RESOUDRE LE SYSTEME QUE
C --- DE 1 A NBLIAC
C ======================================================================
        NEQMAX = ZI(LDSCON+2)
        ZI(LDSCON+2) = NBLIAC
        CALL RLDLG3('LDLT',LDSCON,ZR(JMU),CBID,1)
        ZI(LDSCON+2) = NEQMAX
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU
C ======================================================================
        DO 140 II = 1,NEQ
          ZR(JDELTA-1+II) = ZR(JDELT0-1+II) - ZR(JRESU-1+II)
  140   CONTINUE
        CALL CFMAJU(RESOCO, NEQ, NDIM, NBLIAI, NBLIAC, LLF, LLF1, LLF2)
      ELSE
        DO 170 II = 1,NEQ
          ZR(JDELTA-1+II) = ZR(JDELT0-1+II)
  170   CONTINUE
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
        DO 200 II = 1,NBLIAI
          TROUAC = .FALSE.
C ======================================================================
C - LA LIAISON II EST-ELLE ACTIVE ? (-> TROUAC)
C ======================================================================
          DO 180 ILIAC = 1,NBLIAC
            IF (ZI(JLIAC-1+ILIAC).EQ.II) TROUAC = .TRUE.
  180     CONTINUE
C ======================================================================
C - CALCUL DE A.DELTA SI LA LIAISON II N'EST PAS ACTIVE
C ======================================================================
          IF (.NOT.TROUAC) THEN
            JDECAL = ZI(JAPPTR+II-1)
            NBDDL = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                  ZR(JDELTA),AADELT)
C ======================================================================
C - SI A.DELTA EST POSITIF POUR II : CALCUL DE E(DEPTOT) - A.RESU
C - RHO = MIN ( ( E(DEPTOT) - A.RESU )II / (A.DELTA)II )
C - ON STOCKE DANS NUMIN LE NUMERO DE LA LIAISON REALISANT LE
C - MINIMUM (CE SERA LA LIAISON LA PLUS VIOLEE)
C ======================================================================
            IF (AADELT.GT.R8PREM()) THEN
C ======================================================================
C -  ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
C ======================================================================
               CALL CFELPV(II, TYPEC0, RESOCO, NBLIAI, LELPIV)
               IF (LELPIV) THEN
                 GOTO 200
               ENDIF
C ======================================================================
C -  FIN ELIMINATION LIAISON A PIVOT NUL
C ======================================================================
              DELPOS = .TRUE.
              CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                    ZR(JRESU),VAL)
              IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
                AJEU = ZR(JAPJEU+II-1) - VAL
                AJEU = AJEU/AADELT
                IF (AJEU.LT.RHO) THEN
                  RHO = AJEU
                  NUMIN = II
                ENDIF
              ENDIF
            END IF
          END IF

  200   CONTINUE
C ======================================================================
C - SI TOUS LES (A.DELTA)II SONT NEGATIFS : RHO = 1
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
      X1     = 1.D0
      RHORHO = MIN(RHO,X1)
C ======================================================================
C -- SI RHO < 1 (AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE) :
C -- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE
C ======================================================================
      IF (RHO.LT.1.0D0) THEN
         POSIT = NBLIAC + 1
         CALL CFTABL(INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1, LLF2,
     &               RESOCO, TYPEAJ, POSIT, NUMIN, TYPEC0)
         IF (NIV.GE.2) THEN
            CALL CFIMP2(IFM,NOMA,NUMIN,TYPEC0,TYPEAJ,'ALG',
     &                  ZR(JAPJEU-1+NUMIN),JAPPAR,JNOCO,JMACO)
         END IF
         DO 210 KK = 1,NEQ
           ZR(JDELTA-1+KK) = RHORHO*ZR(JDELTA-1+KK)
  210    CONTINUE
         GOTO 60
      END IF

C ======================================================================
C                            ON A CONVERGE
C ======================================================================

C ======================================================================
C --- ON ENLEVE TOUTES LES LIAISONS DE CONTACT POUR LESQUELLES
C --- LA PRESSION EST NEGATIVE
C ======================================================================
      IF (NBLIAC.NE.0) THEN
         CALL CFNEG(RESOCO,DEFICO,NOMA,NDIM,
     &              INDIC,NBLIAI,NBLIAC,AJLIAI,SPLIAI,
     &              LLF,LLF1,LLF2)
      ENDIF
C ======================================================================
C --- RECUPERATION DU DEPLACEMENT FINAL
C ======================================================================
  230 CONTINUE
      DO 240 II = 1,NEQ
        ZR(JRESU-1+II) = ZR(JRESU-1+II) + ZR(JDELTA-1+II)
        ZR(JDELTA-1+II) = ZR(JDEPDE-1+II) + ZR(JDELTA-1+II)
  240 CONTINUE
C ======================================================================
C --- CALCUL DES FORCES DE CONTACT (AT.MU)
C ======================================================================
      CALL CFATMU(NEQ,NESMAX,NDIM,NBLIAC,1,LLF,LLF1,LLF2,RESOCO)
C ======================================================================
C ---            TRAITEMENT DU FROTTEMENT
C ======================================================================
C
C --- DETERMINATION DU PLUS GRAND ET DU PLUS PETIT GLISSEMENT TANGENT
C
      XXMAX = 0.D0
      XXMIN = R8MAEM()
C
      DO 260 II = 1,NBLIAC
C
C --- CALCUL DES GLISSEMENTS. ILS SONT PLACES DANS JAPJFX ET JAPJFY
C
        AJEUFX = 0.D0
        AJEUFY = 0.D0
        LLIAC = ZI(JLIAC-1+II)
        JDECAL = ZI(JAPPTR+LLIAC-1)
        NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
        CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),
     &              ZR(JDELTA),VAL)
        AJEUFX = ZR(JAPJFX-1+LLIAC) - VAL
        IF (NDIM.EQ.3) THEN
          CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
          AJEUFY = ZR(JAPJFY-1+LLIAC) - VAL
        END IF
        XX = SQRT(AJEUFX**2+AJEUFY**2)
        XXMAX = MAX(XXMAX,XX)
        XXMIN = MIN(XXMIN,XX)
  260 CONTINUE
      XXMIN = MAX(XXMIN,R8PREM())
C ======================================================================
C --- ON REMPLIT ZR(JMU-1+3*NBLIAI+LLIAC) PAR LA RACINE DU
C --- RAPPORT ENTRE KPG ET NORME DE MUG. IL DOIT RESTER
C --- INFERIEUR OU EGAL A RACINE DE E_T. SI MUG EST TROP PETIT
C --- ON INTRODUIT UNE VALEUR QUI CONSERVE LE CONDITIONNEMENT
C --- DE LA MATRICE DE FROTTEMENT.
C ======================================================================
      DO 270 II = 1,NBLIAC
        AJEUFX = 0.D0
        AJEUFY = 0.D0
C --- ON RECUPERE DES GLISSEMENTS DE LA LIAISON
        LLIAC = ZI(JLIAC-1+II)
        JDECAL = ZI(JAPPTR+LLIAC-1)
        NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
        CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),
     &              ZR(JDELTA),VAL)
        AJEUFX = ZR(JAPJFX-1+LLIAC) - VAL
        IF (NDIM.EQ.3) THEN
          CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
          AJEUFY = ZR(JAPJFY-1+LLIAC) - VAL
        END IF
        XK = ZR(IFRO-1+LLIAC)
C --- ON DETERMINE XK = MU*MUC
        XK = XK*ZR(JMU-1+II)
C --- XF = SQRT( E_T )
C --- XX = NORME DU GLISSEMENT TANGENT
        XF = SQRT(ZR(IPENA-1+2*LLIAC))
        XX = SQRT(AJEUFX**2+AJEUFY**2)
C --- SI XX < XXMAX*XTOL (DEPLACEMENT NEGLIGEABLE)
        IF (XX.LE.XXMAX*XTOL) XX = XXMIN
        ZR(JMU-1+3*NBLIAI+LLIAC) = SQRT(XK/XX)
        IF (ZR(JMU-1+3*NBLIAI+LLIAC).GE.XF) THEN
          ZR(JMU-1+3*NBLIAI+LLIAC) = XF
        END IF
  270 CONTINUE
C ======================================================================
C ---   CONSTRUCTION DE LA MATRICE TANGENTE DE FROTTEMENT
C --- KT = K + KF - THETA*KFR
C --- K   : MATRICE DE RIGIDITE DU PROBLEME MECANIQUE SANS FROTTEMENT
C --- KF  : PREMIERE MATRICE TANGENTE DE FROTTEMENT (TERME POSITIF)
C --- KFR : DEUXIEME MATRICE DE FROTTEMENT (TERME NEGATIF)
C --- KT  : NOUVELLE MATRICE TANGENTE DU PROBLEME
C ======================================================================

      DO 300 II = 1,NBLIAI*(NDIM-1)
        TROUAC = .TRUE.
C --- NUMERO DE LA LIAISON
        IF (II.LE.NBLIAI) THEN
          LLIAC = II
        ELSE
          LLIAC = II - NBLIAI
        END IF
C --- CALCUL DE CM2A (PARTIE SYMETRIQUE DE KF) POUR LES LIAISONS
C --- ACTIVES AVEC INTIALISATION A ZERO DE LA COLONNE CORRESPONDANT
C --- A LA LIAISON
        DO 280 JJ = 1,NBLIAC
          IF (ZI(JLIAC-1+JJ).EQ.LLIAC) TROUAC = .FALSE.
  280   CONTINUE
        CALL JEVEUO(JEXNUM(CM2A,II),'E',JCM2A)
        DO 290 KK = 1,NEQ
          ZR(JCM2A-1+KK) = 0.0D0
  290   CONTINUE
        IF (.NOT.TROUAC) THEN
          JDECAL = ZI(JAPPTR+LLIAC-1)
          NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
          XMU = ZR(JMU-1+3*NBLIAI+LLIAC)
          IF (II.GT.NBLIAI) THEN
            CALL CALATM(NEQ,NBDDL,XMU,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                  ZI(JAPDDL+JDECAL),ZR(JCM2A))
          ELSE
            CALL CALATM(NEQ,NBDDL,XMU,ZR(JAPCOF+JDECAL),
     &                  ZI(JAPDDL+JDECAL),ZR(JCM2A))
          END IF
        END IF
        CALL JELIBE(JEXNUM(CM2A,II))
  300 CONTINUE
C ======================================================================
C --- CALCUL DE KF STOCKE DANS MAF1 = CM2AT*CM2A
C ======================================================================
      NBLIG = NBLIAI*NDIM
      CALL ATASMO(CM2A,NUMEDD,MAF1,'V',NBLIG)
C ======================================================================
C --- CREATION DU VECTEUR DE CISAILLEMENT (PARTIE SYMETRIQUE DE KFR)
C ---   KF*(ZR(JDEPDE-1+NUM1) + ZR(JDELTA-1+NUM1))
C --- CE VECTEUR EST REAFFECTE DANS ZR(JAFMU)
C ======================================================================
      CALL MTDSCR(MAF1)
      CALL JEVEUO(MAF1//'.&INT','E',LMAF1)
      CALL MRMULT('ZERO',LMAF1,ZR(JDELTA),'R',ZR(JAFMU),1)
C ======================================================================
C - CALCUL DE KFR STOCKE DANS CM3A SUR L ENSEMBLE DES LIAISONS
C ======================================================================
      DO 330 II = 1,NBLIAI
        TROUAC = .TRUE.
        LLIAC = II
C
C --- ON NOTE JJC LA PLACE DE NOTRE LIAISON SI ELLE EST ACTIVE
C
        DO 310 JJ = 1,NBLIAC
          IF (ZI(JLIAC-1+JJ).EQ.II) THEN
            TROUAC = .FALSE.
            JJC = JJ
          END IF
  310   CONTINUE
C --- INITIALISATION DE CM3A
        CALL JEVEUO(JEXNUM(CM3A,II),'E',JCM3A)
        DO 320 LL = 1,NEQ
          ZR(JCM3A-1+LL) = 0.0D0
  320   CONTINUE
C --- ON EFFECTUE LE CALCUL DE CM3A SUR LES LIAISONS ACTIVES
        IF (.NOT.TROUAC) THEN
          AJEUFX = 0.D0
          AJEUFY = 0.D0
          JDECAL = ZI(JAPPTR+LLIAC-1)
          NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
          CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),
     &                ZR(JDELTA),VAL)
          AJEUFX = ZR(JAPJFX-1+LLIAC) - VAL
          IF (NDIM.EQ.3) THEN
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                  ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
            AJEUFY = ZR(JAPJFY-1+LLIAC) - VAL
          END IF

          XK = ZR(IFRO-1+LLIAC)
          XK = XK*ZR(JMU-1+JJC)
C
C --- DETERMINATION DE BETA QUI RENTRE DANS LE CALCUL
C --- DU DENOMINATEUR DE KFR
C
          XF = SQRT(ZR(IPENA-1+2*LLIAC))
          XX = SQRT(AJEUFX**2+AJEUFY**2)
          IF (XK.EQ.0.D0) THEN
            BETA = 0.D0
          ELSE
            IF (XX.LE.XXMAX*XTOL) XX = XXMIN
              ALPHA = SQRT(XK/XX)
            BETA = SQRT(1.D0/ (XK*XX))
            IF (ALPHA.GT.XF) BETA = 0.D0
          END IF
C
C --- ON MULTIPIE BETA PAR COEF_MATR_FROT QUI VAUT 1
C --- SI LE RESIDU EST PLUS PETIT QUE 1E-3
C
          IF (RESIGR.GE.1.0D-03) THEN
            XMU = SQRT(ZR(ICOMA-1+LLIAC))
            BETA = BETA*XMU
          END IF
C --- ON EFFECTUE CM3A = AFMU * BETA
          CALL CALAPR(NEQ,NBDDL,BETA,ZR(JAFMU),ZI(JAPDDL+JDECAL),
     &                ZR(JCM3A))
        END IF
        CALL JELIBE(JEXNUM(CM3A,II))
  330 CONTINUE
C ======================================================================
C --- CREATION DE LA MATRICE DE FROTTEMENT - SECONDE PARTIE (MAF2)
C ======================================================================
      NBLIG = NBLIAI
      CALL ATASMO(CM3A,NUMEDD,MAF2,'V',NBLIG)
C ======================================================================
C --- CALCUL DE LA MATRICE TANGENTE AVEC FROTTEMENT
C ======================================================================
      CALL CFFROT(MAT,MAF1,MAF2,MAFROT)
C ======================================================================
C --- STOCKAGE DE L'ETAT DE CONTACT DEFINITIF
C ======================================================================
C --- ATTENTE POINT FIXE
      IF ( NBLIAC.NE.NBLCIN ) THEN
        LREAC(2) = .TRUE.
      ENDIF
      ZI(JCOCO+2) = NBLIAC
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
C ======================================================================
  999 CONTINUE
C ======================================================================
C --- SAUVEGARDE DES INFOS DE DIAGNOSTIC (NOMBRE D'ITERATIONS)
C ======================================================================
      CALL CFITER(RESOCO,'E','ITER',ITER,R8BID)
C ======================================================================
 1000 FORMAT (' <CONTACT> <> NBRE DE LIAISONS POSSIBLES: ',I6)
 1001 FORMAT (' <CONTACT> <> DEBUT DES ITERATIONS (MAX: ',I6,')')
 1002 FORMAT (' <CONTACT> <> FIN DES ITERATIONS (NBR: ',I6,')')
 1003 FORMAT (' <CONTACT> <> NBRE DE LIAISONS CONTACT FINALES:',
     &       I6,')')
 1005 FORMAT (' <CONTACT> <> NBRE DE LIAISONS CONTACT INITIALES:',
     &       I6,')')
      CALL JEDEMA()
C ======================================================================
      END
