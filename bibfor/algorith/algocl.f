       SUBROUTINE ALGOCL(DEFICO,RESOCO,LMAT  ,LDSCON,NOMA  ,
     &                   RESU  ,CTCCVG,CTCFIX)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/10/2010   AUTEUR DESOZA T.DESOZA 
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
C
      IMPLICIT NONE
      LOGICAL      CTCFIX
      INTEGER      LMAT
      INTEGER      LDSCON
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*19 RESU
      INTEGER      CTCCVG(2)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
C
C ALGO. POUR CONTACT    : DUALISATION (LAGRANGIEN)
C ALGO. POUR FROTTEMENT : SANS
C
C ----------------------------------------------------------------------
C
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
C IN  LMAT    : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  LDSCON  : DESCRIPTEUR DE LA MATRICE -A.C-1.AT
C IN  NOMA    : NOM DU MAILLAGE
C OUT CTCFIX : .TRUE.  SI ATTENTE POINT FIXE CONTACT
C VAR RESU    : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS L'ITERATION
C              DE NEWTON PRECEDENTE
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C OUT CTCCVG : CODES RETOURS D'ERREUR DU COTNACT
C                (1) NOMBRE MAXI D'ITERATIONS
C                (2) MATRICE SINGULIERE
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
      INTEGER      CFDISI,CFDISD
      COMPLEX*16   CBID
      CHARACTER*24 APPOIN,DELTA
      INTEGER      JAPPTR,JDELTA
      CHARACTER*24 APCOEF,APDDL,APJEU
      INTEGER      JAPCOE,JAPDDL,JAPJEU
      CHARACTER*24 MACONT,K24BLA
      CHARACTER*19 DELT0,LIAC,LIOT,MU
      INTEGER      JDELT0,JLIAC,JLIOT,JMU
      INTEGER      JRESU
      INTEGER      IFM,NIV
      INTEGER      IBID,ILIAI,KK,IER,LLF,LLF1,LLF2
      INTEGER      NBLIAI,NBLIAC,NEQMAX
      INTEGER      NEQ,NDIM,NBDDL,NBLCIN,NESMAX
      INTEGER      NDECI,NPVNEG,ISINGU
      INTEGER      AJLIAI,SPLIAI,INDIC,INDFAC,SPAVAN,ILIAC,LLMIN
      INTEGER      JDECAL,POSIT
      CHARACTER*1  TYPEAJ
      CHARACTER*2  TYPEC0
      LOGICAL      TROUAC,DELPOS,LELPIV
      REAL*8       XJVMAX,AJEU,VAL,AADELT,RHO,RHORHO,X1,R8BID
      REAL*8       R8MAEM,R8PREM
      INTEGER      ITER,ITEMAX,ITEMUL,ISTO
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
      CALL INFDBG('CONTACT',IFM,NIV) 
      IF(NIV.GE.2) THEN
        WRITE (IFM,*) '<CONTACT> <> ALGO_CONTACT   : DUALISATION'
        WRITE (IFM,*) '<CONTACT> <> ALGO_FROTTEMENT: SANS'
      ENDIF
      CALL JEMARQ()
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
      APPOIN = RESOCO(1:14)//'.APPOIN'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APJEU  = RESOCO(1:14)//'.APJEU'
      APDDL  = RESOCO(1:14)//'.APDDL'
      LIAC   = RESOCO(1:14)//'.LIAC'
      LIOT   = RESOCO(1:14)//'.LIOT'
      MU     = RESOCO(1:14)//'.MU'
      DELT0  = RESOCO(1:14)//'.DEL0'
      DELTA  = RESOCO(1:14)//'.DELT'
      MACONT = ZK24(ZI(LDSCON+1))
C ======================================================================
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APDDL,'L',JAPDDL)
      CALL JEVEUO(APJEU,'E',JAPJEU)
      CALL JEVEUO(LIAC,'E',JLIAC)
      CALL JEVEUO(LIOT,'E',JLIOT)
      CALL JEVEUO(MU,'E',JMU)
      CALL JEVEUO(DELT0,'E',JDELT0)
      CALL JEVEUO(DELTA,'E',JDELTA)
      CALL JEVEUO(RESU(1:19)//'.VALE','E',JRESU)
      CALL JEECRA(MACONT(1:19)//'.REFA','DOCU',IBID,'ASSE')
C ======================================================================
C --- INITIALISATION DE VARIABLES
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
      NBLIAI = CFDISD(RESOCO,'NBLIAI')
      NEQ    = CFDISD(RESOCO,'NEQ'   )
      NDIM   = CFDISD(RESOCO,'NDIM'  )
      ITEMUL = CFDISI(DEFICO,'ITER_CONT_MULT')
      ITEMAX = ITEMUL*NBLIAI
      ISTO   = CFDISI(DEFICO,'STOP_SINGULIER')
      NESMAX = CFDISD(RESOCO,'NESMAX')
      NBLIAC = CFDISD(RESOCO,'NBLIAC')      
      LLF    = CFDISD(RESOCO,'LLF'   )
      LLF1   = CFDISD(RESOCO,'LLF1'  )
      LLF2   = CFDISD(RESOCO,'LLF2'  )
      AJLIAI = CFDISD(RESOCO,'AJLIAI')
      SPLIAI = CFDISD(RESOCO,'SPLIAI') 
      IF (NBLIAC.GT.0) THEN
        INDIC  = 1
      ELSE 
        INDIC  = 0  
      ENDIF
      INDFAC = 1
      TYPEAJ = 'A'
      TYPEC0 = 'C0'
      K24BLA = ' '
      ITER   = 0
      XJVMAX = 0.D0
      NBLCIN = NBLIAC
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1001) ITEMAX
      ENDIF
C
C ======================================================================
C                    REPRISE DE LA BOUCLE PRINCIPALE
C ======================================================================
      

 40   CONTINUE

      ITER = ITER + 1
C
C --- A-T-ON DEPASSE LE NOMBRE D'ITERATIONS DE CONTACT AUTORISE ?
C
      IF (ITER.GT.ITEMAX+1) THEN
        CTCCVG(1) = 1
        GO TO 999
      END IF
C
C ======================================================================
C ---
C --- RESOLUTION MATRICIELLE POUR DES LIAISONS ACTIVES
C ---
C ======================================================================
C

C ======================================================================
C --- SI PAS DE LIAISON ACTIVE, ON REMPLIT DELTA ET ON VA
C --- DIRECTEMENT AU CALCUL DE RHO
C ======================================================================

      IF (NBLIAC.EQ.0) THEN
        DO 101 KK = 1,NEQ
          ZR(JDELTA+KK-1) = ZR(JDELT0+KK-1)
  101   CONTINUE
      END IF

C ======================================================================
C --- S'IL Y A DES LIAISONS ACTIVES, ON CALCULE MU ET DELTA
C ======================================================================

C
C --- DETERMINATION DE LA 1ERE LIAISON AYANT CHANGE D'ETAT (IN/ACTIF)
C --- (ON NE RECONSTRUIRA -A.C-1.AT QU'A PARTIR DE CETTE LIAISON)
C
      IF (NBLIAC.NE.0) THEN
C --- PAR LDLT OU MULT_FRONT
C
        SPAVAN = SPLIAI
C
C --- CALCUL DE -A.C-1.AT COLONNE PAR COLONNE (A PARTIR DE INDFAC)
C
        CALL CFACAT(NDIM  ,INDIC ,NBLIAC,AJLIAI,SPLIAI,
     &              LLF   ,LLF1  ,LLF2  ,INDFAC,NESMAX,
     &              DEFICO,RESOCO,LMAT  ,NBLIAI,XJVMAX)
C ======================================================================
C ---
C --- ELIMINATION DES PIVOTS NULS
C ---
C ======================================================================
        CALL ELPIV1(XJVMAX,INDIC ,NBLIAC,AJLIAI,SPLIAI,
     &              SPAVAN,NOMA  ,DEFICO,RESOCO)
C
C --- ON A SUPPRIME UNE LIAISON
C
         IF (INDIC .EQ. -1) THEN
            GOTO 40
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
C ======================================================================
        IF (INDFAC.LE.NBLIAC) THEN
           IF(NIV.GE.2) THEN
             WRITE(IFM,*)'<CONTACT> <> FACTORISATION MATRICE CONTACT '
           ENDIF

           CALL TLDLGG(2     ,LDSCON,INDFAC,NBLIAC,0     ,
     &                 NDECI ,ISINGU,NPVNEG,IER   )
           INDFAC = NBLIAC + 1
C
C --- LA MATRICE DE CONTACT EST-ELLE SINGULIERE ?
C
           IF (IER.GT.ISTO) THEN
             CTCCVG(2) = 1
             GOTO 999
           END IF
        END IF
C ======================================================================
C --- SECOND MEMBRE : ON MET JEU(DEPTOT) - A.DELT0 DANS MU
C ======================================================================
        CALL CFADU(RESOCO,K24BLA,NEQ   ,NDIM  ,NBLIAC,
     &             LLF   ,LLF1  ,LLF2  ,NESMAX)
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
        DO 120 ILIAI = 1,NEQ
           ZR(JDELTA-1+ILIAI) = ZR(JDELT0-1+ILIAI) - ZR(JRESU-1+ILIAI)
  120   CONTINUE
C ======================================================================
C --- MISE A JOUR DU VECTEUR DEPLACEMENT <DU> CORRIGE PAR LE CONTACT
C ======================================================================
        CALL CFMAJU(RESOCO,NEQ   ,NDIM  ,NBLIAI,NBLIAC,
     &              LLF   ,LLF1  ,LLF2  )
      END IF
C
C ======================================================================
C ---
C --- CALCUL DE RHO = MIN ( (E(DEPTOT) - A.RESU)ILIAI / (A.DELTA)ILIAI))
C --- SUR LES LIAISONS NON ACTIVES DE NUMERO ILIAI
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
        DO 180 ILIAI = 1,NBLIAI
          TROUAC = .FALSE.
C ======================================================================
C -- LA LIAISON ILIAI EST-ELLE ACTIVE ? (-> TROUAC)
C ======================================================================
          DO 170 ILIAC = 1,NBLIAC
            IF (ZI(JLIAC-1+ILIAC).EQ.ILIAI) TROUAC = .TRUE.
  170     CONTINUE
C ======================================================================
C -- CALCUL DE A.DELTA SI LA LIAISON ILIAI N'EST PAS ACTIVE
C ======================================================================
          IF (.NOT.TROUAC) THEN
            JDECAL = ZI(JAPPTR+ILIAI-1)
            NBDDL  = ZI(JAPPTR+ILIAI) - ZI(JAPPTR+ILIAI-1)
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                  ZR(JDELTA),AADELT)
C ======================================================================
C -- SI A.DELTA EST POSITIF POUR ILIAI : CALCUL DE E(DEPTOT) - A.RESU
C -- RHO = MIN ( ( E(DEPTOT) - A.RESU )ILIAI / (A.DELTA)ILIAI )
C -- ON STOCKE DANS LLMIN LE NUMERO DE LA LIAISON REALISANT LE
C -- MINIMUM (CE SERA LA LIAISON LA PLUS VIOLEE)
C ======================================================================
            IF (AADELT.GT.R8PREM()) THEN
C ======================================================================
C -- ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
C ======================================================================
               CALL CFELPV(ILIAI,TYPEC0,RESOCO,NBLIAI,LELPIV)
               IF (LELPIV) THEN
                 GOTO 180
               ENDIF
C ======================================================================
C -- FIN ELIMINATION LIAISON A PIVOT NUL
C ======================================================================
               DELPOS = .TRUE.
               CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                     ZI(JAPDDL+JDECAL),ZR(JRESU),VAL)
               
               AJEU = ZR(JAPJEU+ILIAI-1) - VAL
               AJEU = AJEU/AADELT
               IF (AJEU.LT.RHO) THEN
                 RHO   = AJEU
                 LLMIN = ILIAI
               ENDIF
             ENDIF
          END IF
 180   CONTINUE
C ======================================================================
C -- SI TOUS LES (A.DELTA)ILIAI SONT NEGATIFS : RHO = 1
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
      
      DO 190 KK = 1,NEQ
        ZR(JRESU-1+KK) = ZR(JRESU-1+KK) + RHORHO*ZR(JDELTA-1+KK)
  190 CONTINUE      
C ======================================================================
C -- SI RHO < 1 (AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE) :
C -- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE (LLMIN)
C ======================================================================
      IF (RHORHO.LT.1.D0) THEN
        POSIT = NBLIAC + 1
        CALL CFTABL(INDIC ,NBLIAC,AJLIAI,SPLIAI,LLF   ,
     &              LLF1  ,LLF2  ,RESOCO,TYPEAJ,POSIT ,
     &              LLMIN ,TYPEC0)
        IF (NIV.GE.2) THEN
          CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,LLMIN  ,
     &                TYPEC0,TYPEAJ,'ALG' ,ZR(JAPJEU+LLMIN-1)) 
        END IF

        GO TO 40
      END IF

C ======================================================================
C                            ON A CONVERGE
C ======================================================================

C ======================================================================
C --- ON ENLEVE TOUTES LES LIAISONS DE CONTACT POUR LESQUELLES
C --- LA PRESSION EST NEGATIVE
C ======================================================================
      INDIC = 0
      CALL CFNEG(RESOCO,DEFICO,NOMA   ,NDIM  ,
     &           INDIC ,NBLIAI,NBLIAC ,AJLIAI,
     &           SPLIAI,LLF   ,LLF1   ,LLF2  )
C 
C --- CALCUL DES FORCES DE CONTACT (AT.MU)
C 
      CALL CFATMU(NEQ   ,NESMAX,NDIM  ,NBLIAC,0     ,
     &            LLF   ,LLF1  ,LLF2  ,RESOCO)
C      
C --- VALEUR DES VARIABLES DE CONVERGENCE
C
      CTCCVG(1) = 0
      CTCCVG(2) = 0
C
C --- ATTENTE POINT FIXE
C
      IF ((NBLIAC.NE.NBLCIN).AND.(INDIC.NE.0)) THEN
        CTCFIX = .TRUE.
      ENDIF
C      
C --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
C
      CALL CFECRD(RESOCO,'NBLIAC',NBLIAC)
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1002) ITER
      END IF
C
  999 CONTINUE
C 
C --- SAUVEGARDE DES INFOS DE DIAGNOSTIC 
C 
      CALL CFITER(RESOCO,'E','CONT',ITER  ,R8BID)
      CALL CFITER(RESOCO,'E','LIAC',NBLIAC,R8BID)
C
      CALL JEDEMA()
C
 1001 FORMAT (' <CONTACT> <> DEBUT DES ITERATIONS (MAX: ',I6,')')
 1002 FORMAT (' <CONTACT> <> FIN DES ITERATIONS (NBR: ',I6,')')
C ======================================================================
      END
