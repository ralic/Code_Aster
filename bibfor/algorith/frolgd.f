      SUBROUTINE FROLGD(DEFICO,RESOCO,LMAT  ,LDSCON,NOMA  ,
     &                  RESU  ,RESIGR,REAGEO,REAPRE,DEPDEL,
     &                  CTCCVG)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2008   AUTEUR DESOZA T.DESOZA 
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
      IMPLICIT     NONE
      INTEGER      LMAT,LDSCON
      LOGICAL      REAGEO,REAPRE
      REAL*8       RESIGR
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*19 RESU,DEPDEL
      INTEGER      CTCCVG(2)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
C
C ALGO. POUR CONTACT    : DUALISATION (LAGRANGIEN)
C ALGO. POUR FROTTEMENT : DUALISATION (LAGRANGIEN 3D)
C
C ----------------------------------------------------------------------
C
C
C RESOLUTION DE : C.DU + ACT.MUC + ASGT.MUSG + AGT.MUG = F
C                 AC. (U+DU)      <= E  (= POUR LES LIAISONS ACTIVES)
C                 ASG.(U+DU)       = E' (POUR LES NOEUDS ADHERENTS)
C
C AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
C
C     AC = MATRICE DE CONTACT
C
C    ASG = MATRICE DE FROTTEMENT POUR LES NOEUDS ADHERENTS
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
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  LDSCON : DESCRIPTEUR DE LA MATRICE -A.C-1.AT
C IN  NOMA   : NOM DU MAILLAGE
C IN  REAGEO : VAUT .TRUE. SI REACTUALISATION GEOMETRIQUE
C IN  REAPRE : VAUT .TRUE. SI PREMIERE BOUCLE GEOMETRIQUE ET PREMIERE
C               ITERATION (PREDICTION)
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C VAR RESU   : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS L'ITERATION
C              DE NEWTON PRECEDENTE
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C IN  RESIGR : RESI_GLOB_RELA
C OUT CTCCVG : CODES RETOURS D'ERREUR DU COTNACT
C                (1) NOMBRE MAXI D'ITERATIONS
C                (2) MATRICE SINGULIERE
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
      INTEGER      CFDISI
      LOGICAL      TROUAC,DELPOS,GLISS1,GLISS2,LELPIV,LELPI1,LELPI2
      LOGICAL      CFEXCL
      INTEGER      JJC,JDEPDE,LLF,JDIM,NESMAX,LFMIN
      INTEGER      IBID,IER,IFM,NIV,NDECI,ISINGU,NPVNEG,LFMIN2
      INTEGER      ILIAI,JJ,KK,LL,IZONE,ILIAC,NUMIN
      INTEGER      JRESU,JMU,JATMU
      INTEGER      JDELT0,JDELTA,JLIAC,JCOCO
      INTEGER      NEQ,NESCL,NBLIAC,NBLIAI,NBDDL,NDIM,NEQMAX
      INTEGER      LLIAC,LLJAC,JDECAL,INDIC
      INTEGER      JAPPAR,JAPPTR,JAPCOE,JAPJEU,JAPDDL
      INTEGER      NBLCIN,LLF1,LLF2,LLKAC,AJLIAI,SPLIAI
      INTEGER      JLIOT
      INTEGER      ICOMA,IFRO
      INTEGER      JAPCOF,JAFMU,LMAF1,JCM2A1,JCM2A2,JCM3A
      INTEGER      LFMIN1,JGLI1,JGLI2,JADHR
      INTEGER      ITER,ITEMAX,ITEMUL,NBLFIN,ISTO,INCR
      INTEGER      INDFAC,POSIT,BTOTAL,JVECC,COMPT0,JZOCO
      REAL*8       R8MAEM,R8PREM,AJEU,RHO,RHORHO,AADELT,AJEUFY
      REAL*8       X1,VAL,XX
      REAL*8       AJEUFX,XK,XMU,R8BID
      REAL*8       ALPHA,BETA,XMUL,XVAL,VDIAGM,XJVMAX
      COMPLEX*16   CBID
      CHARACTER*1  TYPEAJ
      CHARACTER*2  TYPEC0, TYPEF0, TYPEF1, TYPEF2
      CHARACTER*14 NUMEDD
      CHARACTER*16 NMGLI1,NMGLI2, NMADHR
      CHARACTER*19 AFMU,MAT,CM2A,CM3A,MAF1,MAF2,MAFROT
      CHARACTER*19 LIAC,MU,ATMU,DELT0,DELTA,COCO,LIOT,CONVEC
      CHARACTER*24 APPARI,APPOIN,APCOEF,APJEU,APDDL
      CHARACTER*24 NDIMCO,APCOFR,FROTE,COMAFO,NOZOCO
      CHARACTER*24 APMEMO
      INTEGER      JAPMEM
C
C ======================================================================
C
C ======================================================================
C --- INITIALISATIONS DES OBJETS ET DES ADRESSES
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
         WRITE (IFM,*) '<CONTACT> <> ALGO_FROTTEMENT: DUALISATION (3D)'
      ENDIF
      CALL JEMARQ ()
C 
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C 
      AFMU     = RESOCO(1:14)//'.AFMU'
      APCOEF   = RESOCO(1:14)//'.APCOEF'
      APCOFR   = RESOCO(1:14)//'.APCOFR'
      APDDL    = RESOCO(1:14)//'.APDDL'
      APJEU    = RESOCO(1:14)//'.APJEU'
      APMEMO   = RESOCO(1:14)//'.APMEMO'
      APPARI   = RESOCO(1:14)//'.APPARI'
      APPOIN   = RESOCO(1:14)//'.APPOIN'
      ATMU     = RESOCO(1:14)//'.ATMU'
      CM2A     = RESOCO(1:14)//'.CM2A'
      CM3A     = RESOCO(1:14)//'.CM3A'
      COCO     = RESOCO(1:14)//'.COCO'
      COMAFO   = DEFICO(1:16)//'.COMAFO'
      CONVEC   = RESOCO(1:14)//'.CONVEC'
      DELT0    = RESOCO(1:14)//'.DEL0'
      DELTA    = RESOCO(1:14)//'.DELT'
      FROTE    = DEFICO(1:16)//'.FROTE'
      LIAC     = RESOCO(1:14)//'.LIAC'
      LIOT     = RESOCO(1:14)//'.LIOT'
      MAFROT   = RESOCO(1:8)//'.MAFR'
      MU       = RESOCO(1:14)//'.MU'
      NDIMCO   = DEFICO(1:16)//'.NDIMCO'
      NOZOCO   = DEFICO(1:16)//'.NOZOCO'
      MAF1     = '&&FROLGD.MAF1'
      MAF2     = '&&FROLGD.MAF2'
      MAT      = ZK24(ZI(LMAT+1))
      CALL JEVEUO(NOZOCO,'L',JZOCO)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APCOFR,'L',JAPCOF)
      CALL JEVEUO(APJEU, 'E',JAPJEU)
      CALL JEVEUO(APMEMO,'L',JAPMEM)
      CALL JEVEUO(APDDL, 'L',JAPDDL)
      CALL JEVEUO(LIAC,  'E',JLIAC)
      CALL JEVEUO(LIOT,  'E',JLIOT)
      CALL JEVEUO(CONVEC,'L',JVECC)
      CALL JEVEUO(MU,    'E',JMU)
      CALL JEVEUO(ATMU,  'E',JATMU)
      CALL JEVEUO(AFMU , 'E',JAFMU )
      CALL JEVEUO(DELT0, 'E',JDELT0)
      CALL JEVEUO(DELTA, 'E',JDELTA)
      CALL JEVEUO(RESU(1:19)//'.VALE'  ,'E',JRESU)
      CALL JEVEUO(DEPDEL(1:19)//'.VALE', 'L', JDEPDE)
      CALL JEVEUO(FROTE,'L',IFRO)
      CALL JEVEUO(COMAFO,'L',ICOMA)
      CALL JEVEUO(NDIMCO,'L',JDIM)
      CALL JEVEUO(COCO,'E',JCOCO)
      CALL DISMOI('F','NOM_NUME_DDL',MAT,'MATR_ASSE',IBID,NUMEDD,IER)
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
C --- XJVMAX : VALEUR MAXI DU PIVOT DE LA MATRICE DE CONTACT
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
C --- NUMIN  : NUMERO DE LA LIAISON LA PLUS "VIOLEE"
C --- KKMIN  : NUMERO DE LA LIAISON LA PLUS "DECOLLEE"
C --- LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
C              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
C               DIRECTIONS SIMULTANEES (EN 3D)
C --- LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               PREMIERE DIRECTION (EN 3D)
C --- LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               SECONDE DIRECTION (EN 3D)
C ======================================================================
      NESCL  = ZI(JAPPAR)
      NESMAX = ZI(JDIM+8)
      NBLIAI = NESCL
      NEQ    = ZI(LMAT+2)
      IZONE  = 1
      ITEMUL = CFDISI(DEFICO,'ITER_MULT_MAXI',IZONE)
      ITEMAX = ITEMUL*NBLIAI
      ISTO   = CFDISI(DEFICO,'STOP_SINGULIER',IZONE)
      TYPEAJ = 'A'
      TYPEC0 = 'C0'
      TYPEF0 = 'F0'
      TYPEF1 = 'F1'
      TYPEF2 = 'F2'
      XJVMAX = 0.0D0
      SPLIAI = 0
      AJLIAI = 0
      INDIC  = 0
      INDFAC = 1
C ======================================================================
C --- SOUVENIRS DE L'ETAT DE CONTACT -> ON NE PEUT PAS S'EN SERVIR
C --- AU DEBUT CAR SI ON A REAPPARIE LES LIAISONS SONT DIFFERENTES
C ======================================================================
      CALL CFDISD(JCOCO,
     &            NDIM,NBLIAC,LLF,LLF1,LLF2)

C ======================================================================
C --- CREATION DE DELTA0 = C-1B
C ======================================================================
      DO 1 ILIAI = 1, NEQ
         ZR(JDELT0-1+ILIAI) = ZR(JRESU-1+ILIAI)
         ZR(JRESU -1+ILIAI) = 0.0D0
 1    CONTINUE
C ======================================================================
C --- DETECTION DES COUPLES DE NOEUDS INTERPENETRES
C --- ON CALCULE LE NOUVEAU JEU : AJEU+ = AJEU/I/N - A.DDEPLA
C ======================================================================
      IF ( NIV .EQ. 2 ) THEN
        WRITE(IFM,*)'<CONTACT> <> LIAISONS INITIALES '
      ENDIF
      IF (REAPRE) THEN
         NBLIAC   = 0
         LLF      = 0
         LLF1     = 0
         LLF2     = 0
C ======================================================================
C --- INITIALISATION POUR L'ELIMINATION DES PIVOTS NULS
C ======================================================================
         CALL CFDIAG( LMAT, VDIAGM )
         ZR(JMU+6*NBLIAI-1)    = VDIAGM ** 0.25D0
         ZI(JLIOT+4*NBLIAI  )  = 0
         ZI(JLIOT+4*NBLIAI+1)  = 0
         ZI(JLIOT+4*NBLIAI+2)  = 0
         ZI(JLIOT+4*NBLIAI+3)  = 0
         DO 4 ILIAI = 1,NBLIAI
            ZR(JMU-1+2*NBLIAI+ILIAI) = 0.D0
            ZR(JMU-1+  NBLIAI+ILIAI) = 0.D0
            ZR(JMU-1+         ILIAI) = 0.D0
            IF (.NOT.CFEXCL(JAPPAR,JAPMEM,ILIAI)) THEN
              AJEU = ZR(JAPJEU+ILIAI-1)
              IF (AJEU.LE.R8PREM()) THEN
C --- LA LIAISON ILIAI EST ACTIVE - NOUVELLE LIAISON ACTIVE: POSIT
                POSIT  = NBLIAC + LLF + LLF1 + LLF2 + 1
                CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &                      RESOCO,TYPEAJ,POSIT,ILIAI,TYPEC0)
              ENDIF
            ENDIF
 4       CONTINUE
C ======================================================================
C --- TOUTES LES LIAISONS DE CONTACT SONT CONSIDEREES ADHERENTES
C ======================================================================
         BTOTAL = NBLIAC + LLF + LLF1 + LLF2

         DO 7 ILIAI = 1, BTOTAL
            LLIAC  = ZI(JLIAC-1+ILIAI)
            POSIT  = NBLIAC + LLF + LLF1 + LLF2 + 1
            CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &                  RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF0)
            IF (NIV.GE.2) THEN
              CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,LLIAC ,
     &                    TYPEF0,TYPEAJ,'ALG' ,ZR(JAPJEU+LLIAC-1))
            END IF
            ZR(JMU-1+3*NBLIAI+LLIAC) = 0.0D0
 7       CONTINUE
      ELSE
C ======================================================================
C --- TRAITEMENT DU CONTACT APRES REACTUALISATION GEOMETRIQUE
C ======================================================================
         IF(REAGEO) THEN
            BTOTAL  = NBLIAC + LLF + LLF1 + LLF2
            DO 5 ILIAI = 1,NBLIAI
               IF (.NOT.CFEXCL(JAPPAR,JAPMEM,ILIAI)) THEN
                 AJEU = ZR(JAPJEU+ILIAI-1)
                 IF (AJEU.LE.R8PREM()) THEN
                   TROUAC = .FALSE.
                   DO 940 JJ = 1, BTOTAL
                     IF (ZI(JLIAC-1+JJ).EQ.ILIAI) THEN
                        TROUAC = .TRUE.
                     ENDIF
 940               CONTINUE
C --- LA LIAISON ILIAI N'ETAIT PAS DEJA ACTIVE -> ON L'ACTIVE
                   IF (.NOT.TROUAC) THEN
                     POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1
                     CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,
     &                          LLF2,RESOCO,TYPEAJ,POSIT,ILIAI,TYPEC0)
                     IF (NIV.GE.2) THEN
                       CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,ILIAI  ,
     &                             TYPEC0,TYPEAJ,'ALG' ,AJEU)
                     END IF
                   ENDIF
                 ENDIF
               ENDIF
 5          CONTINUE
         ENDIF
      ENDIF
C ======================================================================
C --- FIN DE LA PRISE EN COMPTE DU FROTTEMENT A L'INITIATION
C ======================================================================
      TROUAC = .TRUE.
      NBLCIN = NBLIAC
      NBLFIN = LLF
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1000) NBLIAI
        WRITE(IFM,1005) NBLCIN
        WRITE(IFM,1007) NBLFIN
        WRITE(IFM,1001) ITEMAX
      ENDIF
C
      XMUL = ZR(JMU+6*NBLIAI-1)
C
C ======================================================================
C                    REPRISE DE LA BOUCLE PRINCIPALE
C ======================================================================
C
      ITER = 0

 100  CONTINUE

C ======================================================================
C --- MISE A JOUR DE L'INCREMENT DE DEPLACEMENT A CALCULER
C ======================================================================
      DO 122 ILIAI = 1,NEQ
         ZR(JDELTA-1+ILIAI) = ZR(JDELT0-1+ILIAI) - ZR(JRESU-1+ILIAI)
 122  CONTINUE

 300  CONTINUE

      ITER = ITER + 1
C
C --- A-T-ON DEPASSE LE NOMBRE D'ITERATIONS DE CONTACT AUTORISE ?
C
      IF (ITER.GT.ITEMAX+1) THEN
        CTCCVG(1) = 1
        GOTO 9990
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
         CALL CFACAT(NDIM  ,INDIC ,NBLIAC,AJLIAI,SPLIAI,
     &               LLF   ,LLF1  ,LLF2  ,INDFAC,NESMAX,
     &               DEFICO,RESOCO,LMAT  ,NBLIAI,XJVMAX)
C
C --- DETECTION DES PIVOTS NULS
C
         CALL ELPIV2(XJVMAX,NDIM  ,INDIC ,NBLIAC,AJLIAI,
     &               SPLIAI,LLF   ,LLF1  ,LLF2  ,NOMA  ,
     &               DEFICO,RESOCO)
C
C --- ON A SUPPRIME UNE LIAISON
C
         IF (INDIC .EQ. -1) THEN
            GOTO 300
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
         IF (INDFAC.LE.(NBLIAC+(NDIM-1)*LLF+LLF1+LLF2)) THEN
           IF(NIV.GE.2) THEN
             WRITE(IFM,*)'<CONTACT> <> FACTORISATION MATRICE CONTACT '
           ENDIF
           CALL TLDLGG (2,LDSCON,INDFAC,NBLIAC+(NDIM-1)*LLF+LLF1+LLF2,
     &                  0,NDECI,ISINGU,NPVNEG,IER)
           INDFAC = NBLIAC + (NDIM-1)*LLF + LLF1 + LLF2 + 1
C
C --- LA MATRICE DE CONTACT EST-ELLE SINGULIERE ?
C
           IF (IER.GT.ISTO) THEN
             CTCCVG(2) = 1
             GOTO 9990
           END IF
         END IF
C ======================================================================
C --- SECOND MEMBRE : ON MET JEU(DEPTOT) - A.DELT0 DANS MU
C ======================================================================
         DO 25 ILIAI = 1,NDIM*NBLIAI
            ZR(JMU-1+ILIAI)= 0.D0
 25      CONTINUE
C ======================================================================
C --- APPEL DE LA ROUTINE DE CALCUL DU SECOND MEMBRE
C ======================================================================
         CALL CFADU(RESOCO,DEPDEL,NEQ,NDIM,NBLIAC,LLF,LLF1,LLF2,NESMAX)
C ======================================================================
C --- RESOLUTION POUR OBTENIR MU : -A.C-1.AT.MU = JEU(DEPTOT) - A.DELT0
C --- ON TRUANDE LA SD MATR_ASSE POUR NE RESOUDRE LE SYSTEME QUE
C --- DE 1 A NBLIAC
C ======================================================================
         NEQMAX       = ZI(LDSCON+2)
         ZI(LDSCON+2) = NBLIAC+(NDIM-1)*LLF+LLF1+LLF2
         CALL RLDLGG (LDSCON,ZR(JMU),CBID,1)
         ZI(LDSCON+2) = NEQMAX
C ======================================================================
C --- ON REORDONNE LE VECTEUR MU
C ======================================================================
         CALL CFMAJM(RESOCO,NDIM,NBLIAC,LLF,LLF1,LLF2)
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU
C ======================================================================
         CALL CFMAJU(RESOCO,NEQ,NDIM,NBLIAI,NBLIAC,LLF,LLF1,LLF2)
      ENDIF
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
        BTOTAL = NBLIAC + LLF + LLF1 + LLF2
        DO 70 ILIAI = 1, NBLIAI
C ======================================================================
C - LA LIAISON ILIAI EST-ELLE ACTIVE ?
C ======================================================================
            DO 700 ILIAC = 1, BTOTAL
               IF (ZI(JLIAC-1+ILIAC).EQ.ILIAI) GOTO 70
 700        CONTINUE
C ======================================================================
C - CALCUL DE A.DELTA SI LA LIAISON ILIAI N'EST PAS ACTIVE
C ======================================================================
            JDECAL = ZI(JAPPTR+ILIAI-1)
            NBDDL  = ZI(JAPPTR+ILIAI) - ZI(JAPPTR+ILIAI-1)
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                   ZI(JAPDDL+JDECAL),ZR(JDELTA),AADELT)
C ======================================================================
C - SI A.DELTA EST POSITIF POUR ILIAI : CALCUL DE E(DEPTOT) - A.RESU
C - RHO = MIN ( ( E(DEPTOT) - A.RESU )ILIAI / (A.DELTA)ILIAI )
C ======================================================================
            IF (AADELT.GT.R8PREM()) THEN
C ======================================================================
C -  ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
C ======================================================================
               CALL CFELPV(ILIAI,TYPEC0,RESOCO,NBLIAI,LELPIV)
               IF (LELPIV) THEN
                 GOTO 70
               ENDIF
C - ON STOCKE DANS NUMIN LE NUMERO DE LA LIAISON REALISANT LE
C - MINIMUM (CE SERA LA LIAISON LA PLUS VIOLEE)
               DELPOS = .TRUE.
               CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                     ZI(JAPDDL+JDECAL),ZR(JRESU),VAL)
               IF (.NOT.CFEXCL(JAPPAR,JAPMEM,ILIAI)) THEN
                 AJEU = ZR(JAPJEU+ILIAI-1) - VAL
                 AJEU = AJEU/AADELT
                 IF (AJEU.LT.RHO) THEN
                   RHO   = AJEU
                   NUMIN = ILIAI
                 ENDIF
               ENDIF
            ENDIF
 70      CONTINUE
C ======================================================================
C - SI TOUS LES (A.DELTA)ILIAI SONT NEGATIFS : RHO = 1
C ======================================================================
         IF(.NOT.DELPOS) THEN
            RHO = 1.0D0
         ENDIF
      ENDIF
C ======================================================================
C ---
C --- TESTS SUR RHO ET ACTUALISATION DE RESU
C ---
C ======================================================================
      X1     = 1.D0
      RHORHO = MIN(RHO,X1)
C ======================================================================
C --- SI RHO < 1 (AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE) :
C --- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE (NUMIN)
C ======================================================================
      IF (RHORHO.LT.1.0D0) THEN
         POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1
         CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &               RESOCO,TYPEAJ,POSIT,NUMIN,TYPEC0)
         IF (NIV.GE.2) THEN
           CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,NUMIN ,
     &                 TYPEC0,TYPEAJ,'ALG' ,ZR(JAPJEU-1+NUMIN))
         END IF
         IF (REAPRE) THEN

C ======================================================================
C -  ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
C ======================================================================
            POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1
            CALL CFELPV(NUMIN,TYPEF0,RESOCO,NBLIAI,LELPIV)
            IF (.NOT.LELPIV) THEN
               CALL CFELPV(NUMIN, TYPEF1, RESOCO, NBLIAI, LELPI1)
               IF (LELPI1) THEN
                  CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &                        RESOCO,TYPEAJ,POSIT,NUMIN,TYPEF2)
                  IF (NIV.GE.2) THEN
                    CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,NUMIN ,
     &                          TYPEF2,TYPEAJ,'ALG' ,ZR(JAPJEU-1+NUMIN))
                  END IF
               ELSE
                  CALL CFELPV(NUMIN, TYPEF2, RESOCO, NBLIAI, LELPI2)
                  IF (LELPI2) THEN
                     CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,
     &                 LLF1,LLF2,RESOCO,TYPEAJ,POSIT,NUMIN,TYPEF1)
                     IF (NIV.GE.2) THEN
                       CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,NUMIN ,
     &                          TYPEF1,TYPEAJ,'ALG' ,ZR(JAPJEU-1+NUMIN))
                     END IF
                  ELSE
                     CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,
     &                 LLF1,LLF2,RESOCO,TYPEAJ,POSIT,NUMIN,TYPEF0)
                     IF (NIV.GE.2) THEN
                       CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,NUMIN ,
     &                          TYPEF0,TYPEAJ,'ALG' ,ZR(JAPJEU-1+NUMIN))
                     END IF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
C ======================================================================
C --- LA LIAISON EST SUPPOSEE GLISSANTE
C ======================================================================
         ZR(JMU-1+3*NBLIAI+NUMIN) = 0.D0
C ======================================================================
C --- RECUPERATION DU DEPLACEMENT FINAL
C ======================================================================
         DO 84 KK = 1, NEQ
            ZR(JRESU-1+KK) = ZR(JRESU-1+KK) + RHORHO*ZR(JDELTA-1+KK)
 84      CONTINUE

         GOTO 100
      ENDIF

C ======================================================================
C --- SI RHO = 1 , ON A CONVERGE
C ======================================================================

C ======================================================================
C --- RECUPERATION DU DEPLACEMENT FINAL
C ======================================================================
      DO 150 ILIAI = 1,NEQ
          ZR(JRESU -1+ILIAI) = ZR(JRESU-1+ILIAI) + ZR(JDELTA-1+ILIAI)
          ZR(JDELTA-1+ILIAI) = ZR(JRESU-1+ILIAI) + ZR(JDEPDE-1+ILIAI)
 150  CONTINUE
C ======================================================================
C --- ON ENLEVE TOUTES LES LIAISONS DE CONTACT POUR LESQUELLES
C --- LA PRESSION EST NEGATIVE
C ======================================================================
      IF (NBLIAC.NE.0) THEN
         CALL CFNEG (RESOCO,DEFICO,NOMA  ,NDIM  ,INDIC ,
     &               NBLIAI,NBLIAC,AJLIAI,SPLIAI,LLF   ,
     &               LLF1  ,LLF2  )
      ENDIF  
C ======================================================================
C --- LES LIAISONS CONSIDEREES ADHERENTES LE SONT-ELLES VRAIMENT ?
C ======================================================================
      IF ( (LLF+LLF1+LLF2) .NE. 0 ) THEN
         CALL CFADH (RESOCO,DEFICO,NOMA  ,NDIM  ,INDIC ,
     &               NBLIAC,NBLIAI,AJLIAI,SPLIAI,LLF   ,
     &               LLF1  ,LLF2  )
      ENDIF
C ======================================================================
C --- LES LIAISONS CONSIDEREES GLISSANTES LE SONT-ELLES VRAIMENT ? -----
C ======================================================================
      LFMIN  = 0
      LFMIN1 = 0
      LFMIN2 = 0
      COMPT0 = 0

      IF ( (LLF+LLF1+LLF2).LT.NBLIAC ) THEN
         NMGLI1 = '&&FROLGD.GLI1'
         NMGLI2 = '&&FROLGD.GLI2'
         NMADHR = '&&FROLGD.ADHR'
         CALL WKVECT ( NMGLI1, 'V V I', NBLIAC, JGLI1 )
         CALL WKVECT ( NMGLI2, 'V V I', NBLIAC, JGLI2 )
         CALL WKVECT ( NMADHR, 'V V I', NBLIAC, JADHR )
         BTOTAL = NBLIAC + LLF + LLF1 + LLF2
         DO 156 ILIAI = 1, BTOTAL
            IF (ZK8(JVECC-1+ILIAI).EQ.TYPEC0) THEN
               COMPT0 = COMPT0 + 1
               AJEUFX = 0.0D0
               AJEUFY = 0.0D0
               LLIAC  = ZI(JLIAC-1+ILIAI)
               JDECAL = ZI(JAPPTR+LLIAC-1)
               NBDDL  = ZI(JAPPTR+LLIAC  ) - ZI(JAPPTR+LLIAC-1)
C ======================================================================
C --- CALCUL DU JEU SUIVANT LA PREMIERE DIRECTION ----------------------
C ======================================================================
               CALL CFELPV(LLIAC, TYPEF1, RESOCO, NBLIAI, GLISS1)
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
               IF (.NOT.GLISS1) THEN
                  CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                         ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
                  AJEUFX = VAL
               ENDIF
C ======================================================================
C --- CALCUL DU JEU SUIVANT LA SECONDE DIRECTION -----------------------
C ======================================================================
               CALL CFELPV(LLIAC, TYPEF2, RESOCO, NBLIAI, GLISS2)
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
               IF (.NOT.GLISS2) THEN
                  CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                         ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
                  AJEUFY = VAL
               ENDIF
C ======================================================================
C --- CALCUL DE KGP ----------------------------------------------------
C ======================================================================
               XK = ZR(IFRO-1+LLIAC)
               IF ( ZR(JMU-1+COMPT0).GT.0.0D0 ) THEN
                  XK = XK*ZR(JMU-1+COMPT0)
               ELSE
                  XK = 0.D0
               ENDIF
               XX = SQRT( AJEUFX**2 + AJEUFY**2 )
               IF ((XX.GT.R8PREM()).AND.(XX .LT. (XK/XMUL**2))) THEN
C ======================================================================
C --- LA LIAISON EST CONSIDEREE ADHERENTE ET EST TRAITEE PAR -----------
C --- MULTIPLICATEUR DE LAGRANGE ---------------------------------------
C ======================================================================
                  TROUAC = .FALSE.
                  DO 157 KK = ILIAI + 1, BTOTAL
                     LLKAC = ZI(JLIAC-1+KK)
                     IF (LLKAC.EQ.LLIAC) THEN
C ======================================================================
C --- LA LIAISON EST DEJA ADHERENTE ------------------------------------
C ======================================================================
                        TROUAC = .TRUE.
                     ENDIF
 157              CONTINUE
                  IF (TROUAC) GOTO 156
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LES DEUX DIRECTIONS -----------
C ======================================================================
                  CALL CFELPV(LLIAC, TYPEF0, RESOCO, NBLIAI, TROUAC)
                  IF (.NOT.TROUAC) THEN
                     CALL CFELPV(LLIAC,TYPEF1,RESOCO,NBLIAI,GLISS1)
                     IF (GLISS1) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
                         LFMIN2 = LFMIN2 + 1
                         ZI(JGLI2-1+LFMIN2) = LLIAC
                         GOTO 156
                      ENDIF
                      CALL CFELPV(LLIAC,TYPEF2,RESOCO,NBLIAI,GLISS2)
                      IF (GLISS2) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
                           LFMIN1 = LFMIN1 + 1
                           ZI(JGLI1-1+LFMIN1) = LLIAC
                           GOTO 156
                      ENDIF
                      LFMIN = LFMIN + 1
                      ZI(JADHR-1+LFMIN) = LLIAC
                  ENDIF
               ENDIF
            ENDIF
 156     CONTINUE
         IF ((LFMIN.NE.0).OR.(LFMIN1.NE.0).OR.(LFMIN2.NE.0))  THEN
C ======================================================================
C --- S'IL EXISTE AU MOINS UNE LIAISON ADHERENTE SUPPLEMENTAIRE, -------
C --- ON NE PREND PAS EN COMPTE LES INCREMENTS DE DEPLACEMENTS CALCULES-
C --- ET ON RECOMMENCE LES CALCULS (NOTAMMENT POUR MU_SG) --------------
C ======================================================================
            DO 197 LL=1,LFMIN
               LLIAC = ZI(JADHR-1+LL)
               POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1
               CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                              RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF0)
               IF (NIV.GE.2) THEN
                 CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,LLIAC ,
     &                       TYPEF0,TYPEAJ,'ADH',0.D0)
               ENDIF
 197        CONTINUE
            DO 198 LL=1,LFMIN1
               LLIAC = ZI(JGLI1-1+LL)
               POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1
               CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                              RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF1)
               IF (NIV.GE.2) THEN
                 CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,LLIAC ,
     &                       TYPEF1,TYPEAJ,'ADH',0.D0)
               ENDIF
 198        CONTINUE
            DO 199 LL=1,LFMIN2
               LLIAC = ZI(JGLI2-1+LL)
               POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1
               CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                              RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF2)
               IF (NIV.GE.2) THEN
                 CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,LLIAC ,
     &                       TYPEF2,TYPEAJ,'ADH',0.D0)
               ENDIF
 199        CONTINUE
            CALL JEDETR(NMGLI1)
            CALL JEDETR(NMGLI2)
            CALL JEDETR(NMADHR)
            XVAL = XMUL**2
            IF (XVAL.LT.(1.0D0/R8PREM())) THEN
               XMUL = XMUL*SQRT(10.D0)
            ENDIF
            ZR(JMU+6*NBLIAI-1) = XMUL
            GOTO 100
         ENDIF
         CALL JEDETR(NMGLI1)
         CALL JEDETR(NMGLI2)
         CALL JEDETR(NMADHR)
      ENDIF
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
      CALL CFATMU(NEQ,NESMAX,NDIM,NBLIAC,1,LLF,LLF1,LLF2,RESOCO)
C
C ======================================================================
C --- AFFECTATION DE A LIGNE PAR LIGNE (A PARTIR DE IDEBUT)
C ======================================================================
      DO 200 ILIAI = 1, NBLIAI
         CALL JEVEUO ( JEXNUM(CM2A,ILIAI       ), 'E', JCM2A1 )
         CALL JEVEUO ( JEXNUM(CM2A,ILIAI+NBLIAI), 'E', JCM2A2 )
         TROUAC = .TRUE.
C ======================================================================
C --- MISE A ZERO DE LA COLONNE
C ======================================================================
         LLIAC = ILIAI
         COMPT0 = 0
         DO 203 JJ = 1, NBLIAC + LLF + LLF1 + LLF2
            LLJAC = ZI(JLIAC-1+JJ)
            IF (ZK8(JVECC-1+JJ).EQ.TYPEC0) THEN
               COMPT0 = COMPT0 + 1
            ENDIF
            IF (LLJAC.EQ.LLIAC) THEN
               TROUAC = .NOT.TROUAC
               JJC = COMPT0
            ENDIF
 203     CONTINUE
         IF (TROUAC) THEN
            DO 2192 LL = 1, NEQ
               ZR(JCM2A1-1+LL) = 0.0D0
               ZR(JCM2A2-1+LL) = 0.0D0
 2192       CONTINUE
            CALL JELIBE(JEXNUM(CM2A,ILIAI       ))
            CALL JELIBE(JEXNUM(CM2A,ILIAI+NBLIAI))
            GOTO 200
         ENDIF
         CALL CFELPV(LLIAC, TYPEF0, RESOCO, NBLIAI, TROUAC)
         IF ( .NOT.TROUAC ) THEN
            AJEUFX = 0.0D0
            AJEUFY = 0.0D0
            XK     = ZR(IFRO-1+LLIAC) * ZR(JMU-1+JJC)
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL  = ZI(JAPPTR+LLIAC)-ZI(JAPPTR+LLIAC-1)

C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION
C ======================================================================

            CALL CFELPV(LLIAC, TYPEF1, RESOCO, NBLIAI, GLISS1)
            IF (.NOT.GLISS1) THEN
               DO 209 LL = 1, NEQ
                  ZR(JCM2A1-1+LL) = 0.0D0
 209           CONTINUE
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                 ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
               AJEUFX = VAL
            ENDIF

C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA SECONDE DIRECTION
C ======================================================================

            CALL CFELPV(LLIAC, TYPEF2, RESOCO, NBLIAI, GLISS2)
            IF (.NOT.GLISS2) THEN
               DO 219 LL = 1, NEQ
                  ZR(JCM2A2-1+LL) = 0.0D0
 219           CONTINUE
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                 ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
               AJEUFY = VAL
            ENDIF
C ======================================================================
            XX  = SQRT (AJEUFX**2 + AJEUFY**2)
            IF (XX.LE.R8PREM()) THEN
               XMU = XMUL
            ELSE
               XMU = SQRT ( XK / XX )
            ENDIF
            IF (.NOT.GLISS1) THEN
               CALL CALATM (NEQ,NBDDL,XMU,ZR(JAPCOF+JDECAL),
     &                                     ZI(JAPDDL+JDECAL),ZR(JCM2A1))
            ENDIF
            IF (.NOT.GLISS2) THEN
               CALL CALATM (NEQ,NBDDL,XMU,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                     ZI(JAPDDL+JDECAL),ZR(JCM2A2))
            ENDIF
         ELSE
            DO 2091 LL = 1, NEQ
               ZR(JCM2A1-1+LL) = 0.0D0
               ZR(JCM2A2-1+LL) = 0.0D0
 2091       CONTINUE
         ENDIF
         CALL JELIBE(JEXNUM(CM2A,ILIAI       ))
         CALL JELIBE(JEXNUM(CM2A,ILIAI+NBLIAI))
 200  CONTINUE
C ======================================================================
C --- CREATION DE LA MATRICE ATA
C ======================================================================

      CALL ATASMO(CM2A,NUMEDD,MAF1,'V',NBLIAI*(NDIM-1))
C ======================================================================
C --- CREATION DU VECTEUR DE CISAILLEMENT
C --- MATF*((ZR(JDEPDE-1+NUM1)+ZR(JDELT0-1+NUM1))
C --- CE VECTEUR EST REAFFECTE DANS ZR(JAFMU)
C ======================================================================
      CALL MTDSCR( MAF1 )
      CALL JEVEUO( MAF1//'.&INT', 'E', LMAF1 )
      CALL MRMULT('ZERO', LMAF1, ZR(JDELTA), 'R', ZR(JAFMU), 1 )
C ======================================================================
C --- CALCUL DE LA MATRICE TANGENTE DU SYSTEME
C ======================================================================
      DO 1300 ILIAI = 1, NBLIAI
         TROUAC = .TRUE.
         LLIAC = ILIAI
         COMPT0 = 0
         DO 301 JJ = 1, NBLIAC + LLF + LLF1 + LLF2
            IF (ZK8(JVECC-1+JJ).EQ.TYPEC0) THEN
               COMPT0 = COMPT0 + 1
            ENDIF
            LLJAC = ZI(JLIAC-1+JJ)
            IF (LLJAC.EQ.LLIAC) THEN
               TROUAC = .NOT.TROUAC
               JJC = COMPT0
            ENDIF
 301     CONTINUE
         CALL JEVEUO(JEXNUM(CM3A,ILIAI),'E',JCM3A)
         DO 303 LL = 1, NEQ
            ZR(JCM3A-1+LL) = 0.0D0
 303     CONTINUE
         IF (TROUAC) THEN
            CALL JELIBE(JEXNUM(CM3A,ILIAI))
            GOTO 1300
         ENDIF
         CALL CFELPV(LLIAC, TYPEF0, RESOCO, NBLIAI, TROUAC)
         IF ( .NOT. TROUAC ) THEN
C ======================================================================
C --- POUR LES LIAISONS GLISSANTES
C ======================================================================
            AJEUFX = 0.D0
            AJEUFY = 0.D0
            XK = ZR(IFRO-1+LLIAC) * ZR(JMU-1+JJC)
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION
C ======================================================================
            CALL CFELPV(LLIAC, TYPEF1, RESOCO, NBLIAI, GLISS1)
C ======================================================================
            IF (.NOT.GLISS1) THEN
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                 ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
               AJEUFX = VAL
            ENDIF

C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA SECONDE DIRECTION
C ======================================================================

            CALL CFELPV(LLIAC, TYPEF2, RESOCO, NBLIAI, GLISS2)
C ======================================================================
            IF (.NOT.GLISS2) THEN
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                 ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
               AJEUFY = VAL
            ENDIF
C ======================================================================
            XX  = SQRT( AJEUFX**2 + AJEUFY**2 )
            IF ( XK .EQ. 0.D0 ) THEN
               BETA = 0.D0
            ELSE
               IF ( XX .LE. R8PREM() ) THEN
                  BETA  = 0.D0
               ELSE
                  ALPHA = XK / XX
                  BETA  = SQRT(1.D0/(XK*XX))
                  IF ( ALPHA .GT. (XMUL**2) )  BETA = 0.D0
               ENDIF
            ENDIF
            IF ( RESIGR .GE. 1.0D-03 ) THEN
               XMU  = SQRT( ZR(ICOMA-1+LLIAC) )
               BETA = BETA * XMU
            ENDIF
            CALL CALAPR(NEQ,NBDDL,BETA,ZR(JAFMU),
     &                  ZI(JAPDDL+JDECAL),ZR(JCM3A))
         ENDIF
         CALL JELIBE(JEXNUM(CM3A,ILIAI))
 1300 CONTINUE
C ======================================================================
C --- CREATION DE LA MATRICE DE FROTTEMENT - SECONDE PARTIE (MAF2)
C ======================================================================
      CALL ATASMO(CM3A,NUMEDD,MAF2,'V',NBLIAI)
C ======================================================================
C --- CALCUL DE LA MATRICE TANGENTE AVEC FROTTEMENT
C ======================================================================
      CALL CFFROT(MAF1,MAF2,MAFROT)
C ======================================================================
 999  CONTINUE
C ======================================================================
C --- STOCKAGE DE L'ETAT DE CONTACT DEFINITIF
C ======================================================================
C --- VALEUR DES VARIABLES DE CONVERGENCE
      CTCCVG(1) = 0
      CTCCVG(2) = 0
C --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
      ZI(JCOCO+1) = INDIC
      ZI(JCOCO+2) = NBLIAC
      ZI(JCOCO+3) = AJLIAI
      ZI(JCOCO+4) = SPLIAI
      ZI(JCOCO+5) = LLF
      ZI(JCOCO+6) = LLF1
      ZI(JCOCO+7) = LLF2
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
        WRITE(IFM,1006) LLF
        WRITE(IFM,2006) LLF1
        WRITE(IFM,3006) LLF2
        WRITE(IFM,*)'<CONTACT> <> LIAISONS FINALES '
        CALL CFIMP1(DEFICO,RESOCO,NOMA,NBLIAI,IFM)
      END IF
C ======================================================================
 9990 CONTINUE
C 
C --- SAUVEGARDE DES INFOS DE DIAGNOSTIC 
C
      CALL CFITER(RESOCO,'E','CONT',ITER  ,R8BID)
      CALL CFITER(RESOCO,'E','LIAC',NBLIAC,R8BID)
      INCR  = LLF+LLF1+LLF2
      CALL CFITER(RESOCO,'E','LIAF',INCR  ,R8BID)
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
 1000 FORMAT (' <CONTACT> <> NBRE DE LIAISONS POSSIBLES: ',I6)
 1001 FORMAT (' <CONTACT> <> DEBUT DES ITERATIONS (MAX: ',I6,')')
 1002 FORMAT (' <CONTACT> <> FIN DES ITERATIONS (NBR: ',I6,')')
 1003 FORMAT (' <CONTACT> <> NBRE DE LIAISONS CONTACT FINALES:',
     &       I6,')')
 1005 FORMAT (' <CONTACT> <> NBRE DE LIAISONS CONTACT INITIALES:',
     &       I6,')')
 1006 FORMAT (' <CONTACT> <> NBRE DE LIAISONS ADH. FINALES (1 ET 2):'
     &       ,I6,')')
 1007 FORMAT (' <CONTACT> <> NBRE DE LIAISONS ADH. INITIALES (1 ET 2):'
     &       ,I6,')')
 2006 FORMAT (' <CONTACT> <> NBRE DE LIAISONS ADH. FINALES (1 UNIQ.):'
     &       ,I6,')')
 3006 FORMAT (' <CONTACT> <> NBRE DE LIAISONS ADH. FINALES (2 UNIQ.):'
     &       ,I6,')')
      END
