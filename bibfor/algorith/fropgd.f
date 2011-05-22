      SUBROUTINE FROPGD(DEFICO,RESOCO,LMAT  ,LDSCON,NOMA  ,
     &                  RESU  ,RESIGR,DEPDEL,CTCCVG,CTCFIX)
C   
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/05/2011   AUTEUR ABBAS M.ABBAS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C
      IMPLICIT     NONE
      LOGICAL      CTCFIX
      INTEGER      LMAT      
      INTEGER      LDSCON
      REAL*8       RESIGR
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*19 RESU,DEPDEL
      INTEGER      CTCCVG
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
C
C ALGO. POUR CONTACT    : DUALISATION (LAGRANGIEN)
C ALGO. POUR FROTTEMENT : PENALISATION
C
C ----------------------------------------------------------------------
C
C      
C RESOLUTION DE : C.DU + ACT.MUC     = F - KG AGT.AG (E-U)
C                      AC. (U+DU)   <= E  (= POUR LES LIAISONS ACTIVES)
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
C OUT CTCFIX : .TRUE.  SI ATTENTE POINT FIXE CONTACT
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C VAR RESU   : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS L'ITERATION
C              DE NEWTON PRECEDENTE
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C IN  RESIGR : RESI_GLOB_RELA
C OUT CTCCVG : CODE RETOUR CONTACT DISCRET
C                0 - OK
C                1 - NOMBRE MAXI D'ITERATIONS
C                2 - MATRICE SINGULIERE
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
      INTEGER      CFDISI,CFDISD
      LOGICAL      TROUAC,DELPOS,LELPIV
      INTEGER      IFM,NIV
      INTEGER      NDLMAX,NMULT
      PARAMETER   (NDLMAX = 30)      
      INTEGER      IBID,IER,ILIAI,JJ,KK,JJC,LL
      INTEGER      NDECI,ISINGU,NPVNEG,ISTO,ILIDEB,ILIFIN
      INTEGER      NEQ,NBLIAC,NBLIAI,NDIM
      INTEGER      NBDDL,NEQMAX,NBLCIN,NESMAX
      INTEGER      ILIAC,LLIAC,JDECAL
      INTEGER      INDFAC,POSIT,SPAVAN,INDIC,NUMIN
      INTEGER      ITEMUL
      REAL*8       R8MAEM,R8PREM,AJEU,RHO,RHORHO,AADELT,AJEUFY,XF
      REAL*8       X1,VAL,XXMIN,XXMAX,XX
      REAL*8       XTOL,AJEUFX,XK,XMU,R8BID
      REAL*8       ALPHA,BETA,XJVMAX,COEFTE,COEFPT
      INTEGER      ITER,ITEMAX
      INTEGER      AJLIAI,SPLIAI,LLF,LLF1,LLF2
      COMPLEX*16   CBID
      CHARACTER*1  TYPEAJ
      CHARACTER*2  TYPEC0
      CHARACTER*14 NUMEDD,NUMEF1,NUMEF2,NUFROT
      CHARACTER*19 MAT,MAF2,MAFROT
      INTEGER      JRESU,JDEPDE
      CHARACTER*19 AFMU,MAF1,FRO1,FRO2
      INTEGER      JAFMU,LMAF1,JFRO1,JFRO2
      CHARACTER*19 LIAC,MU,ATMU,DELT0,DELTA
      INTEGER      JLIAC,JMU,JATMU,JDELT0,JDELTA
      CHARACTER*24 APJEU,APJEFX,APJEFY
      INTEGER      JAPJEU,JAPJFY,JAPJFX
      CHARACTER*24 APCOFR
      INTEGER      JAPCOF
      CHARACTER*24 APPOIN,APCOEF,APDDL
      INTEGER      JAPPTR,JAPCOE,JAPDDL
      CHARACTER*24 TACFIN
      INTEGER      JTACF
      INTEGER      CFMMVD,ZTACF       
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
      CALL INFDBG('CONTACT',IFM,NIV)
      IF (NIV.GE.2) THEN
           WRITE (IFM,*) '<CONTACT> <> ALGO_CONTACT   : DUALISATION'
           WRITE (IFM,*) '<CONTACT> <> ALGO_FROTTEMENT: PENALISATION'
      ENDIF
      CALL JEMARQ ()
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
      APPOIN = RESOCO(1:14)//'.APPOIN'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APCOFR = RESOCO(1:14)//'.APCOFR'
      APJEU  = RESOCO(1:14)//'.APJEU'
      APJEFX = RESOCO(1:14)//'.APJEFX'
      APJEFY = RESOCO(1:14)//'.APJEFY'
      APDDL  = RESOCO(1:14)//'.APDDL'
      LIAC   = RESOCO(1:14)//'.LIAC'
      MU     = RESOCO(1:14)//'.MU'
      ATMU   = RESOCO(1:14)//'.ATMU'
      AFMU   = RESOCO(1:14)//'.AFMU'
      DELT0  = RESOCO(1:14)//'.DEL0'
      DELTA  = RESOCO(1:14)//'.DELT'
      FRO1   = RESOCO(1:14)//'.FRO1'
      FRO2   = RESOCO(1:14)//'.FRO2'
      MAFROT = RESOCO(1:14)//'.MAFR'
      TACFIN = RESOCO(1:14)//'.TACFIN'  
      MAF1   = '&&FROPGD.MAF1'
      MAF2   = '&&FROPGD.MAF2'
      MAT    = ZK24(ZI(LMAT+1))(1:19)
C ======================================================================
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APCOFR,'L',JAPCOF)
      CALL JEVEUO(APJEU,'L',JAPJEU)
      CALL JEVEUO(APJEFX,'E',JAPJFX)
      CALL JEVEUO(APJEFY,'E',JAPJFY)
      CALL JEVEUO(APDDL,'L',JAPDDL)
      CALL JEVEUO(LIAC,'E',JLIAC)
      CALL JEVEUO(MU,'E',JMU)
      CALL JEVEUO(ATMU,'E',JATMU)
      CALL JEVEUO(AFMU,'E',JAFMU)
      CALL JEVEUO(DELT0,'E',JDELT0)
      CALL JEVEUO(DELTA,'E',JDELTA)
      CALL JEVEUO(RESU(1:19)//'.VALE','E',JRESU)
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      CALL JEVEUO(TACFIN,'L',JTACF )
      ZTACF  = CFMMVD('ZTACF')          
      CALL DISMOI('F','NOM_NUME_DDL',MAT,'MATR_ASSE',IBID,NUMEDD,IER)
C ======================================================================
C --- INITIALISATION DE VARIABLES
C --- NBLIAI : NOMBRE DE LIAISONS DE CONTACT
C --- NESMAX : NOMBRE MAXI DE NOEUDS ESCLAVES
C              SERT AU DECALAGE DANS LES ROUTINES DE FROTTEMENT 3D
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
C --- TOLERANCE UTILISEE POUR LE FROTTEMENT
      XTOL   = 1.D-08
      ITER   = 0
      XJVMAX = 0.D0 
      NBLCIN = NBLIAC
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1001) ITEMAX
      ENDIF

C ======================================================================
C                    REPRISE DE LA BOUCLE PRINCIPALE
C ======================================================================

   60 CONTINUE

      ITER = ITER + 1
C
C --- A-T-ON DEPASSE LE NOMBRE D'ITERATIONS DE CONTACT AUTORISE ?
C
      IF (ITER.GT.ITEMAX+1) THEN
        CTCCVG = 1
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
         CALL CFACAT(NDIM  ,INDIC ,NBLIAC,AJLIAI,SPLIAI,
     &               LLF   ,LLF1  ,LLF2  ,INDFAC,NESMAX,
     &               DEFICO,RESOCO,LMAT  ,NBLIAI,XJVMAX)
C
C --- ELIMINATION DES PIVOTS NULS
C
         CALL ELPIV1(XJVMAX,INDIC ,NBLIAC,AJLIAI,SPLIAI,
     &               SPAVAN,NOMA  ,DEFICO,RESOCO)
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
C 
        IF (INDFAC.LE.NBLIAC) THEN
          IF(NIV.GE.2) THEN
            WRITE(IFM,*)'<CONTACT> <> FACTORISATION MATRICE CONTACT '
          ENDIF
          ILIDEB = INDFAC
          ILIFIN = NBLIAC
          CALL TLDLGG(2     ,LDSCON,ILIDEB,ILIFIN,0     ,
     &                NDECI ,ISINGU,NPVNEG,IER   )
          INDFAC = ILIFIN + 1
C
C --- LA MATRICE DE CONTACT EST-ELLE SINGULIERE ?
C
           IF (IER.GT.ISTO) THEN
             CTCCVG = 2
             GOTO 999
           END IF
         END IF
C ======================================================================
C --- SECOND MEMBRE : ON MET JEU(DEPTOT) - A.DELT0 DANS MU
C ======================================================================
         DO 120 ILIAI = 1,NDIM*NBLIAI
           ZR(JMU-1+ILIAI) = 0.D0
  120    CONTINUE
C ======================================================================
C --- APPEL DE LA ROUTINE DE CALCUL DU SECOND MEMBRE -------------------
C ======================================================================
         CALL CFADU (RESOCO,DEPDEL,NEQ   ,NDIM  ,NBLIAC,
     &               LLF   ,LLF1  ,LLF2  ,NESMAX)
C ======================================================================
C --- RESOLUTION POUR OBTENIR MU : -A.C-1.AT.MU = JEU(DEPTOT) - A.DELT0
C --- ON TRUANDE LA SD MATR_ASSE POUR NE RESOUDRE LE SYSTEME QUE
C --- DE 1 A ILIFIN
C ======================================================================
         NEQMAX       = ZI(LDSCON+2)
         ZI(LDSCON+2) = ILIFIN
         CALL RLDLGG(LDSCON,ZR(JMU),CBID,1)
         ZI(LDSCON+2) = NEQMAX
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU
C ======================================================================
         DO 140 ILIAI = 1,NEQ
           ZR(JDELTA-1+ILIAI) = ZR(JDELT0-1+ILIAI) - ZR(JRESU-1+ILIAI)
  140    CONTINUE
         CALL CFMAJU(RESOCO,NEQ   ,NDIM  ,NBLIAI,NBLIAC,
     &               LLF   ,LLF1  ,LLF2  )
      ELSE
        DO 170 ILIAI = 1,NEQ
          ZR(JDELTA-1+ILIAI) = ZR(JDELT0-1+ILIAI)
  170   CONTINUE

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
        DO 200 ILIAI = 1,NBLIAI
          TROUAC = .FALSE.
C ======================================================================
C - LA LIAISON ILIAI EST-ELLE ACTIVE ? (-> TROUAC)
C ======================================================================
          DO 180 ILIAC = 1,NBLIAC
            IF (ZI(JLIAC-1+ILIAC).EQ.ILIAI) TROUAC = .TRUE.
  180     CONTINUE
C ======================================================================
C - CALCUL DE A.DELTA SI LA LIAISON ILIAI N'EST PAS ACTIVE
C ======================================================================
          IF (.NOT.TROUAC) THEN
            JDECAL = ZI(JAPPTR+ILIAI-1)
            NBDDL = ZI(JAPPTR+ILIAI) - ZI(JAPPTR+ILIAI-1)
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                  ZR(JDELTA),AADELT)
C ======================================================================
C - SI A.DELTA EST POSITIF POUR ILIAI : CALCUL DE E(DEPTOT) - A.RESU
C - RHO = MIN ( ( E(DEPTOT) - A.RESU )ILIAI / (A.DELTA)ILIAI )
C - ON STOCKE DANS NUMIN LE NUMERO DE LA LIAISON REALISANT LE
C - MINIMUM (CE SERA LA LIAISON LA PLUS VIOLEE)
C ======================================================================
            IF (AADELT.GT.R8PREM()) THEN
C ======================================================================
C -  ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
C ======================================================================
               CALL CFELPV(ILIAI, TYPEC0, RESOCO, NBLIAI, LELPIV)
               IF (LELPIV) THEN
                 GOTO 200
               ENDIF
C ======================================================================
C -  FIN ELIMINATION LIAISON A PIVOT NUL
C ======================================================================
              DELPOS = .TRUE.
              CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                    ZR(JRESU),VAL)
              AJEU = ZR(JAPJEU+ILIAI-1) - VAL
              AJEU = AJEU/AADELT
              IF (AJEU.LT.RHO) THEN
                RHO = AJEU
                NUMIN = ILIAI
              ENDIF
            END IF
          END IF

  200   CONTINUE
C ======================================================================
C - SI TOUS LES (A.DELTA)ILIAI SONT NEGATIFS : RHO = 1
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
         CALL CFTABL(INDIC ,NBLIAC,AJLIAI,SPLIAI,LLF   , 
     &               LLF1  ,LLF2  ,RESOCO,TYPEAJ,POSIT , 
     &               NUMIN, TYPEC0)
         IF (NIV.GE.2) THEN
           CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,NUMIN ,
     &                 TYPEC0,TYPEAJ,'ALG' ,AADELT)
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
      INDIC = 0
      IF (NBLIAC.NE.0) THEN
        CALL CFNEG (RESOCO,DEFICO,NOMA  ,NDIM  ,INDIC ,
     &              NBLIAI,NBLIAC,AJLIAI,SPLIAI,LLF   ,
     &              LLF1  ,LLF2)
      ENDIF
C ======================================================================
C --- RECUPERATION DU DEPLACEMENT FINAL
C ======================================================================
      DO 240 ILIAI = 1,NEQ
        ZR(JRESU-1+ILIAI) = ZR(JRESU-1+ILIAI) + ZR(JDELTA-1+ILIAI)
        ZR(JDELTA-1+ILIAI) = ZR(JDEPDE-1+ILIAI) + ZR(JDELTA-1+ILIAI)
  240 CONTINUE
C ======================================================================
C --- INITIALISATION DE ATMU ET DE AFMU
C ======================================================================
      DO 110 ILIAI = 1, NEQ
         ZR(JATMU+ILIAI-1) = 0.0D0
         ZR(JAFMU+ILIAI-1) = 0.0D0
 110  CONTINUE
      IF (NBLIAC.EQ.0) GOTO 999
C ======================================================================
C --- CALCUL DES FORCES DE CONTACT (AT.MU)
C ======================================================================
      CALL CFATMU(NEQ   ,NESMAX,NDIM  ,NBLIAC,1     ,
     &            LLF   ,LLF1  ,LLF2  ,RESOCO)           
C ======================================================================
C ---            TRAITEMENT DU FROTTEMENT
C ======================================================================
C
C --- DETERMINATION DU PLUS GRAND ET DU PLUS PETIT GLISSEMENT TANGENT
C
      XXMAX = 0.D0
      XXMIN = R8MAEM()
C
      DO 260 ILIAI = 1,NBLIAC
C
C --- CALCUL DES GLISSEMENTS. ILS SONT PLACES DANS JAPJFX ET JAPJFY
C
        AJEUFX = 0.D0
        AJEUFY = 0.D0
        LLIAC = ZI(JLIAC-1+ILIAI)
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
      DO 270 ILIAI = 1,NBLIAC
        AJEUFX = 0.D0
        AJEUFY = 0.D0
C --- ON RECUPERE DES GLISSEMENTS DE LA LIAISON
        LLIAC = ZI(JLIAC-1+ILIAI)
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
        XK = ZR(JTACF+ZTACF*(LLIAC-1)+0)
C --- ON DETERMINE XK = MU*MUC
        XK = XK*ZR(JMU-1+ILIAI)
C --- XF = SQRT( E_T )
C --- XX = NORME DU GLISSEMENT TANGENT
        COEFPT = ZR(JTACF+ZTACF*(LLIAC-1)+2)
        XF = SQRT(COEFPT)
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

      DO 300 ILIAI = 1,NBLIAI*(NDIM-1)
        TROUAC = .TRUE.
C --- NUMERO DE LA LIAISON
        IF (ILIAI.LE.NBLIAI) THEN
          LLIAC = ILIAI
        ELSE
          LLIAC = ILIAI - NBLIAI
        END IF
C --- CALCUL DE FRO1 (PARTIE SYMETRIQUE DE KF) POUR LES LIAISONS
C --- ACTIVES AVEC INTIALISATION A ZERO DE LA COLONNE CORRESPONDANT
C --- A LA LIAISON
        DO 280 JJ = 1,NBLIAC
          IF (ZI(JLIAC-1+JJ).EQ.LLIAC) TROUAC = .FALSE.
  280   CONTINUE
        CALL JEVEUO(JEXNUM(FRO1,ILIAI),'E',JFRO1)
        DO 290 KK = 1,NDLMAX
          ZR(JFRO1-1+KK) = 0.0D0
  290   CONTINUE
        IF (.NOT.TROUAC) THEN
          JDECAL = ZI(JAPPTR+LLIAC-1)
          NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
          XMU = ZR(JMU-1+3*NBLIAI+LLIAC)
          IF (ILIAI.GT.NBLIAI) THEN
            CALL DAXPY(NBDDL,XMU,
     &                 ZR(JAPCOF+JDECAL+30*NESMAX),1,ZR(JFRO1),1)
          ELSE
            CALL DAXPY(NBDDL,XMU,ZR(JAPCOF+JDECAL),1,ZR(JFRO1),1)
          END IF
        END IF
        CALL JELIBE(JEXNUM(FRO1,ILIAI))
         
  300 CONTINUE
C ======================================================================
C --- CALCUL DE KF STOCKE DANS MAF1 = FRO1T*FRO1
C ======================================================================
      NMULT = NDIM - 1
      NUMEF1 = '&&FROPGD.NUF1'
      CALL ATASMO(NEQ   ,FRO1  ,ZI(JAPDDL),ZI(JAPPTR),NUMEDD,MAF1  ,'V',
     &            NBLIAI,NMULT ,NUMEF1)
C ======================================================================
C --- CREATION DU VECTEUR DE CISAILLEMENT (PARTIE SYMETRIQUE DE KFR)
C ---   KF*(ZR(JDEPDE-1+NUM1) + ZR(JDELTA-1+NUM1))
C --- CE VECTEUR EST REAFFECTE DANS ZR(JAFMU)
C ======================================================================
      CALL MTDSCR(MAF1)
      CALL JEVEUO(MAF1//'.&INT','E',LMAF1)
      CALL MRMULT('ZERO',LMAF1,ZR(JDELTA),'R',ZR(JAFMU),1)      
C ======================================================================
C - CALCUL DE KFR STOCKE DANS FRO2 SUR L ENSEMBLE DES LIAISONS
C ======================================================================
      DO 330 ILIAI = 1,NBLIAI
        TROUAC = .TRUE.
        LLIAC = ILIAI
C
C --- ON NOTE JJC LA PLACE DE NOTRE LIAISON SI ELLE EST ACTIVE
C
        DO 310 JJ = 1,NBLIAC
          IF (ZI(JLIAC-1+JJ).EQ.ILIAI) THEN
            TROUAC = .FALSE.
            JJC = JJ
          END IF
  310   CONTINUE
C --- INITIALISATION DE FRO2
        CALL JEVEUO(JEXNUM(FRO2,ILIAI),'E',JFRO2)
        DO 320 LL = 1,NDLMAX
          ZR(JFRO2-1+LL) = 0.0D0
  320   CONTINUE
C --- ON EFFECTUE LE CALCUL DE FRO2 SUR LES LIAISONS ACTIVES
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

          XK = ZR(JTACF+ZTACF*(LLIAC-1)+0)
          XK = XK*ZR(JMU-1+JJC)
C
C --- DETERMINATION DE BETA QUI RENTRE DANS LE CALCUL
C --- DU DENOMINATEUR DE KFR
C
          COEFPT = ZR(JTACF+ZTACF*(LLIAC-1)+2)
          XF     = SQRT(COEFPT)
          XX     = SQRT(AJEUFX**2+AJEUFY**2)
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
            COEFTE = ZR(JTACF+ZTACF*(LLIAC-1)+3)
            XMU    = SQRT(COEFTE)
            BETA   = BETA*XMU
          END IF
C --- ON EFFECTUE FRO2 = AFMU * BETA
          CALL CALAPR(NBDDL,BETA,ZR(JAFMU),ZI(JAPDDL+JDECAL),ZR(JFRO2))
        END IF
        CALL JELIBE(JEXNUM(FRO2,ILIAI))
  330 CONTINUE
C ======================================================================
C --- CREATION DE LA MATRICE DE FROTTEMENT - SECONDE PARTIE (MAF2)
C ======================================================================
      NMULT = 1
      NUMEF2 = '&&FROPGD.NUF2'
      CALL ATASMO(NEQ   ,FRO2  ,ZI(JAPDDL),ZI(JAPPTR),NUMEDD,MAF2  ,'V',
     &            NBLIAI,NMULT ,NUMEF2)
C ======================================================================
C --- CALCUL DE LA MATRICE TANGENTE AVEC FROTTEMENT
C ======================================================================
      NUFROT = '&&FROPGD.NUFR'
      CALL CFFROT(MAF1,'-',MAF2,MAFROT,NUFROT)
C
C --- ATTENTE POINT FIXE
C
      IF ((NBLIAC.NE.NBLCIN).AND.(INDIC.NE.0)) THEN
        CTCFIX = .TRUE.
      ENDIF
C
  999 CONTINUE
C      
C --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
C      
      CALL CFECRD(RESOCO,'NBLIAC',NBLIAC) 
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1002) ITER
      END IF
C 
C --- SAUVEGARDE DES INFOS DE DIAGNOSTIC 
C 
      CALL CFITER(RESOCO,'E','CONT',ITER  ,R8BID)
      CALL CFITER(RESOCO,'E','LIAC',NBLIAC,R8BID)
C ======================================================================
 1001 FORMAT (' <CONTACT> <> DEBUT DES ITERATIONS (MAX: ',I6,')')
 1002 FORMAT (' <CONTACT> <> FIN DES ITERATIONS (NBR: ',I6,')')
      CALL JEDEMA()
C ======================================================================
      END
