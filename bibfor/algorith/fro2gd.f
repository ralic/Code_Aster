      SUBROUTINE FRO2GD (DEFICO,RESOCO,LMAT,LDSCON,NOMA,
     &                   CINE,RESU,DEPTOT,ITERAT,LREAC,DEPDEL,ISTO)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/10/2002   AUTEUR CIBHHBC R.FERNANDES 
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
      IMPLICIT          NONE
      LOGICAL           LREAC(4)
      INTEGER           LMAT,LDSCON,ITERAT,ISTO
      CHARACTER*8       NOMA
      CHARACTER*24      DEFICO,RESOCO,CINE,RESU,DEPTOT,DEPDEL
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : NMCONT
C ----------------------------------------------------------------------
C
C METHODE LAGRANGIEN POUR LE CONTACT-FROTTEMENT EN 2D
C  => MULT. DE LAGRANGE POUR LE CONTACT ET LE FROTTEMENT
C
C RESOLUTION DE : C.DU + AcT.Ac.MUc + AsgT.Asg.MUsg + AgT.Ag.MUg = F
C                 Ac. (U+DU)      <= E  (= POUR LES LIAISONS ACTIVES)
C                 Asg.(U+DU)       = E' (POUR LES NOEUDS ADHERENTS)
C
C AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
C
C     Ac = MATRICE DE CONTACT
C
C    Asg = MATRICE DE FROTTEMENT POUR LES NOEUDS ADHERENTS
C
C    Ag  = MATRICE DE FROTTEMENT POUR LES NOEUDS GLISSANTS
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
C IN  ISTO    : 0 ON ARRETE LE CALCUL SI ON PERD PLUS DE 8 DECIMALES
C               1 ON N'ARRETE PAS LE CALCUL
C VAR RESU    : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS DEPTOT
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C ======================================================================
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------
C ======================================================================
      CHARACTER*32       JEXNUM , JEXNOM
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
C ======================================================================
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C ======================================================================
      LOGICAL      TROUAC,DELPOS
      INTEGER      IBID,IER,IFM,NIV,NDECI,ISINGU,NPVNEG,JAPJFX
      INTEGER      ICONTA,II,JJ,KK,IDEBUT,ILIAC,KCOUNT,NUMIN,KKMIN
      INTEGER      JRESU,JDEPP,JMU,JCMU,JATMU,POSMA,NDIM,NEQMAX
      INTEGER      JDELT0,JDELTA,JLIAC,JVALE,JCOCO,JCM1A,JRCINE,JVA
      INTEGER      NEQ,NESCL,NBLIAC,NBLIAI,NBLIAP,INDIC
      INTEGER      LLIAC,LLJAC,POS1,POS2,NUM1,NUM2,JDECAL,NBDDL
      INTEGER      JAPPAR,JAPPTR,JAPCOE,JAPJEU,JAPDDL,JNOCO,JMACO
      INTEGER      NBLCIN,NBLFIN,JLIOT,IOTE
      INTEGER      LLF,JAPCOF,IFROT,LFMIN,JAFMU,PIVOT,ZERO
      INTEGER      LLKAC,IIKAC,ICOUNT,LCOUNT,LIND,LLFN,IFRO,JDEPDE
      REAL*8       R8MAEM,R8PREM,AJEU,RHO,RHORHO,AADELT
      REAL*8       X1,VAL,VAL1,VAL2,ZMU,XFORC,XJVMAX,XJVMIN
      REAL*8       AJEUFX,XPDT,XK,XCOMP,XQUOT,XCOS,XSIN,XXX
      COMPLEX*16   CBID
      CHARACTER*8  NOM1,NOM2
      CHARACTER*14 CHAIN
      CHARACTER*19 LIAC,LIOT,MU,ATMU,DELT0,DELTA,CM1A,MATR,COCO,AFMU
      CHARACTER*24 APPARI,APPOIN,APCOEF,APJEU,APDDL,COEFMU
      CHARACTER*24 CONTNO,CONTMA,APCOFR,FROTE,MACONT,APJEFX
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
      CALL JEMARQ ()
C ======================================================================
C --- LE CONTACT DOIT-IL ETRE MODELISE ?
C ======================================================================
      APPARI = RESOCO(1:14)//'.APPARI'
      CALL JEEXIN (APPARI,ICONTA)
      IF (ICONTA.EQ.0) GO TO 999
      CALL JEVEUO (APPARI,'L',JAPPAR)
      NESCL = ZI(JAPPAR)
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
C LIAC   : LISTE DES INDICES DES LIAISONS ACTIVES
C MU     : MULTIPLICATEURS DE LAGRANGE DU CONTACT (DOIVENT ETRE > 0)
C COEFMU : COEFFICIENT PAR LEQUEL IL FAUT MULTIPLIER MU AVANT DE
C          TESTER SON SIGNE (-1 SI CONDITION EN PRESSION OU TEMPERATURE)
C ATMU   : FORCES DE CONTACT
C CM1A   : C-1.AT AVEC C MATRICE DE RIGIDITE TANGENTE,
C          ET A MATRICE DE CONTACT (AT SA TRANSPOSEE)
C ======================================================================
      CONTNO   = DEFICO(1:16)//'.NOEUCO'
      CONTMA   = DEFICO(1:16)//'.MAILCO'
      APPARI   = RESOCO(1:14)//'.APPARI'
      APPOIN   = RESOCO(1:14)//'.APPOIN'
      APCOEF   = RESOCO(1:14)//'.APCOEF'
      APCOFR   = RESOCO(1:14)//'.APCOFR'
      APJEU    = RESOCO(1:14)//'.APJEU'
      APJEFX   = RESOCO(1:14)//'.APJEFX'
      APDDL    = RESOCO(1:14)//'.APDDL'
      LIAC     = RESOCO(1:14)//'.LIAC'
      LIOT     = RESOCO(1:14)//'.LIOT'
      MU       = RESOCO(1:14)//'.MU'
      COEFMU   = RESOCO(1:14)//'.COEFMU'
      ATMU     = RESOCO(1:14)//'.ATMU'
      AFMU     = RESOCO(1:14)//'.AFMU'
      DELT0    = RESOCO(1:14)//'.DEL0'
      DELTA    = RESOCO(1:14)//'.DELT'
      CM1A     = RESOCO(1:14)//'.CM1A'
      MATR     = RESOCO(1:14)//'.MATR'
      FROTE    = DEFICO(1:16)//'.FROTE'
C
      CALL JEVEUO (CONTNO,'L',JNOCO )
      CALL JEVEUO (CONTMA,'L',JMACO )
      CALL JEVEUO (APPARI,'L',JAPPAR)
      CALL JEVEUO (APPOIN,'L',JAPPTR)
      CALL JEVEUO (APCOEF,'L',JAPCOE)
      CALL JEVEUO (APCOFR,'L',JAPCOF)
      CALL JEVEUO (APJEU, 'E',JAPJEU)
      CALL JEVEUO (APJEFX,'E',JAPJFX)
      CALL JEVEUO (APDDL, 'L',JAPDDL)
      CALL JEVEUO (LIAC,  'E',JLIAC )
      CALL JEVEUO (LIOT,  'E',JLIOT )
      CALL JEVEUO (MU,    'E',JMU   )
      CALL JEVEUO (COEFMU,'L',JCMU  )
      CALL JEVEUO (ATMU,  'E',JATMU )
      CALL JEVEUO (AFMU , 'E',JAFMU )
      CALL JEVEUO (DELT0, 'E',JDELT0)
      CALL JEVEUO (DELTA, 'E',JDELTA)
      CALL JEVEUO (FROTE, 'L',IFRO  )
      CALL JEVEUO (RESU(1:19)//'.VALE'  ,'E',JRESU)
      CALL JEVEUO (DEPTOT(1:19)//'.VALE','E',JDEPP)
      CALL JEVEUO (DEPDEL(1:19)//'.VALE', 'L', JDEPDE)
C ======================================================================
      NBLIAI = NESCL
C ======================================================================
      MACONT = ZK24(ZI(LDSCON+1))
      CALL JEECRA (MACONT(1:19)//'.REFA','DOCU',IBID,'ASSE')
      NEQ = ZI(LMAT+2)
      CALL JEVEUO (JEXNUM(MATR//'.VALE',1),'E',JVALE)
C ======================================================================
C --- SOUVENIRS DE L'ETAT DE CONTACT -> ON NE PEUT PAS S'EN SERVIR
C --- AU DEBUT CAR SI ON A REAPPARIE LES LIAISONS SONT DIFFERENTES
C ======================================================================
C --- NBLIAC : NOMBRE DE LIAISONS ACTIVES
C --- LLF    : NOMBRE DE LIAISONS ADHERENTES
C ======================================================================
      COCO   = RESOCO(1:14)//'.COCO'
      CALL     JEVEUO (COCO,'E',JCOCO)
      NDIM   = ZI(JCOCO)
      NBLIAC = ZI(JCOCO+1)
      LLF    = ZI(JCOCO+6)
      KKMIN  = 0
C ======================================================================
C                             INITIALISATIONS
C ======================================================================
C --- CREATION DE DELTA0 = C-1B
C ======================================================================
      DO 1 II = 1, NEQ
         ZR(JDELT0-1+II) = ZR(JRESU-1+II)
         ZR(JRESU -1+II) = 0.0D0
 1    CONTINUE
C ======================================================================
C --- CALCUL DE -A.DEPTOT ET RANGEMENT DANS APJEU
C --- (UNIQUEMENT POUR LES CL SANS APPARIEMENT,
C --- C'EST-A-DIRE POUR P, T, OU U RIGIDE : LORSQUE POSMA = 0)
C ======================================================================
      DO 2 II = 1,NBLIAI
         POSMA = ZI(JAPPAR+3*(II-1)+2)
         IF (POSMA.EQ.0) THEN
            JDECAL = ZI(JAPPTR+II-1)
            NBDDL  = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &           ZI(JAPDDL+JDECAL),ZR(JDEPP),VAL)
            ZR(JAPJEU+II-1) = ZR(JAPJEU+II-1) - VAL
         END IF
 2    CONTINUE
C ======================================================================
C --- DETECTION DES COUPLES DE NOEUDS INTERPENETRES A L'INITIALISATION -
C --- (IL EST NEGATIF LORSQU'IL Y A INTERPENETRATION -> LIAISON ACTIVE)-
C ======================================================================
      IF(ITERAT.EQ.0) THEN
         NBLIAC = 0
         ZI(JLIOT+4*NBLIAI)   = 0
         ZI(JLIOT+4*NBLIAI+1) = 0
         ZI(JLIOT+4*NBLIAI+2) = 0
         ZI(JLIOT+4*NBLIAI+3) = 0
         DO 4 II = 1,NBLIAI
            ZR(JMU-1+3*NBLIAI+II) = 0.D0
            ZR(JMU-1+  NBLIAI+II) = 0.D0
            ZR(JMU-1+         II) = 0.D0
            ZR(JAPJFX-1+II)=0.D0
            AJEU = ZR(JAPJEU+II-1)
            IF ( AJEU.LT.0.0D0 ) THEN
               INDIC = 0
               NBLIAC = NBLIAC + 1
               ZI(JLIAC-1+NBLIAC) = II
               IF (NIV.GE.2) THEN
                  POS1 = ZI(JAPPAR+3*(II-1)+1)
                  NUM1 = ZI(JNOCO+POS1-1)
                  CALL JENUNO (JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
                  POS2 = ZI(JAPPAR+3*(II-1)+2)
                  IF (POS2.GT.0) THEN
                     CHAIN = ' A  LA MAILLE '
                     NUM2 = ZI(JMACO+POS2-1)
                     CALL JENUNO (JEXNUM(NOMA//'.NOMMAI',NUM2),NOM2)
                  ELSE IF (POS2.LT.0) THEN
                     CHAIN = ' AU NOEUD     '
                     NUM2 = ZI(JNOCO+ABS(POS2)-1)
                     CALL JENUNO (JEXNUM(NOMA//'.NOMNOE',NUM2),NOM2)
                  ELSE IF (POS2.EQ.0) THEN
                     CHAIN = ' '
                     NOM2  = ' '
                  END IF
                  WRITE (IFM,1000) 'LE NOEUD ',NOM1,
     &                 ' EST ASSOCIE ',CHAIN,NOM2
               ENDIF
            END IF
 4       CONTINUE
C ======================================================================
C --- TOUTES LES LIAISONS DE CONTACT SONT CONSIDEREES ADHERENTES -------
C ======================================================================
         LLF = 0
         DO 7 II = 1, NBLIAC
            LLIAC = ZI(JLIAC-1+II)
            LLF   = LLF + 1
            ZI(JLIAC-1+NBLIAC+LLF)   = LLIAC
            ZR(JMU-1+3*NBLIAI+LLIAC) = 0.0D0
 7       CONTINUE
C ======================================================================
      ELSE
C ======================================================================
C --- TRAITEMENT DU CONTACT APRES REACTUALISATION GEOMETRIQUE ----------
C ======================================================================
         IF(LREAC(1)) THEN
            DO 5 II = 1,NBLIAI
               AJEU = ZR(JAPJEU+II-1)
               IF (AJEU.LT.0.0D0) THEN
                  TROUAC = .FALSE.
                  DO 940, JJ=1,NBLIAC
                     IF(ZI(JLIAC-1+JJ).EQ.II) THEN
                        TROUAC = .TRUE.
                     ENDIF
 940              CONTINUE
                  IF(.NOT.TROUAC) THEN
                     NBLIAC = NBLIAC + 1
                     DO 211 JJ=1,LLF
                        ZI(JLIAC+NBLIAC+LLF-JJ) =
     &                       ZI(JLIAC-1+NBLIAC+LLF-JJ)
 211                 CONTINUE
                     ZI(JLIAC-1+NBLIAC) = II
                  ENDIF
               ENDIF
 5          CONTINUE 
         END IF
      ENDIF
C ======================================================================
C --- FIN DE LA PRISE EN COMPTE DU FROTTEMENT A L'INITIATION
C ======================================================================
      NBLCIN = NBLIAC
      NBLFIN = LLF
      IF ( NIV .EQ. 2 ) WRITE(IFM,*)'NBLIACI',NBLCIN,'LLFI',NBLFIN
 100  CONTINUE
C ======================================================================
C --- MISE A JOUR DE L'INCREMENT DE DEPLACEMENT A CALCULER -------------
C ======================================================================
      DO 122 II = 1,NEQ
         ZR(JDELTA-1+II) = ZR(JDELT0-1+II) - ZR(JRESU-1+II)
 122  CONTINUE
C ======================================================================
C --- CALCUL DE A.C-1.AT S'IL EXISTE AU MOINS UNE LIAISON DE CONTACT ---
C ======================================================================
      IF (NBLIAC.NE.0) THEN
         IDEBUT = 1
C ======================================================================
C --- CALCUL DE -A.C-1.AT COLONNE PAR COLONNE (A PARTIR DE IDEBUT)
C ======================================================================
         XJVMAX = 0.D0
         XJVMIN = 1.D0/R8PREM()
         DO 20 ILIAC = IDEBUT,NBLIAC+LLF
            LLIAC = ZI(JLIAC+ILIAC-1)
C ======================================================================
C --- CALCUL DE CHAQUE COLONNE DE AT (UNE PAR LIAISON ACTIVE)
C ======================================================================
            IF(ILIAC.GT.NBLIAC) THEN
               CALL JEVEUO(JEXNUM(CM1A,LLIAC+NBLIAI),'E',JCM1A)
            ELSE
               CALL JEVEUO(JEXNUM(CM1A,LLIAC),'E',JCM1A)
            ENDIF
            DO 202 KK = 1, NEQ
               ZR(JCM1A-1+KK) = 0.0D0
 202        CONTINUE
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
            IF(ILIAC.GT.NBLIAC) THEN
               CALL CALATM (NEQ,NBDDL,1.D0,ZR(JAPCOF+JDECAL),
     &              ZI(JAPDDL+JDECAL),ZR(JCM1A))
            ELSE
               CALL CALATM (NEQ,NBDDL,1.D0,ZR(JAPCOE+JDECAL),
     &              ZI(JAPDDL+JDECAL),ZR(JCM1A))
            ENDIF
C ======================================================================
C --- CALCUL DE C-1.AT (EN TENANT COMPTE DES CHARGES CINEMATIQUES)
C ======================================================================
            CALL JEVEUO (CINE(1:19)//'.VALE','E',JRCINE)
            CALL NMRLDL (LMAT,ZR(JRCINE),ZR(JCM1A))
C ======================================================================
C - CALCUL DE -A.C-1.AT (REDUITE AUX LIAISONS ACTIVES)
C - (STOCKAGE DE LA MOITIE PAR SYMETRIE)
C ======================================================================
            DO 23 JJ = 1, ILIAC
               LLJAC = ZI(JLIAC-1+JJ)
               JVA = JVALE-1+(ILIAC-1)*ILIAC/2+JJ
               ZR(JVA) = 0.0D0
               JDECAL = ZI(JAPPTR+LLJAC-1)
               NBDDL = ZI(JAPPTR+LLJAC) - ZI(JAPPTR+LLJAC-1)
               IF(JJ.GT.NBLIAC) THEN
                  CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                 ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ELSE
                  CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                 ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ENDIF
               ZR(JVA) = ZR(JVA) - VAL
               IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
               IF(ABS(ZR(JVA)).LT.XJVMIN) XJVMIN = ABS(ZR(JVA)) 
 23         CONTINUE
C     
            IF(ILIAC.GT.NBLIAC) THEN
               CALL JELIBE(JEXNUM(CM1A,LLIAC+NBLIAI))
            ELSE
               CALL JELIBE(JEXNUM(CM1A,LLIAC))
            ENDIF
C
 20      CONTINUE
C ======================================================================
C --- ELIMINATION DES PIVOTS NULS
C ======================================================================
        ZERO = 0
        CALL ELPIV2(XJVMAX, MATR, NOMA, DEFICO, RESOCO, IDEBUT, LLF,
     &                      ZERO,ZERO,NBLIAC, NDIM, KKMIN, PIVOT, INDIC)
        IF (INDIC .EQ. -1 .AND. PIVOT .EQ. 1) GOTO 100
C ======================================================================
C --- FACTORISATION LDLT DE -A.C-1.AT
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
         CALL JEECRA (MACONT(1:19)//'.REFA','DOCU',IBID,'ASSE')
         IF (IDEBUT.LE.NBLIAC+LLF) THEN
            CALL TLDLGG (2,LDSCON,IDEBUT,NBLIAC+LLF,0,NDECI,ISINGU,
     &           NPVNEG,IER)

C--- LA MATRICE DE CONTACT EST-ELLE SINGULIERE?
            IF (IER.GT.ISTO) THEN
               CALL UTMESS ('F','FRO2GD','ARRET SUR MATRICE SINGULIERE')
            ENDIF
         END IF
C ======================================================================
C --- SECOND MEMBRE : ON MET JEU(DEPTOT) - A.DELT0 DANS MU
C ======================================================================
         DO 25 II = 1,2*NBLIAI
            ZR(JMU-1+II)= 0.D0
 25      CONTINUE
         DO 30 ILIAC = 1, NBLIAC+LLF
            LLIAC = ZI(JLIAC-1+ILIAC)
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
            IF(ILIAC.GT.NBLIAC) THEN
C ======================================================================
C --- CALCUL DE MU_SG DEPUIS LE DEBUT DU PAS DE TEMPS ------------------
C ======================================================================
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &              ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &              ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
               VAL = VAL1 + VAL2
            ELSE
C ======================================================================
C --- CALCUL DE MU_C ---------------------------------------------------
C ======================================================================
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &              ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL)
               ZR(JMU+ILIAC-1) = ZR(JAPJEU+LLIAC-1)
            ENDIF
            ZR(JMU+ILIAC-1) = ZR(JMU+ILIAC-1) - VAL
 30      CONTINUE
C ======================================================================
C --- RESOLUTION POUR OBTENIR MU : -A.C-1.AT.MU = JEU(DEPTOT) - A.DELT0
C ======================================================================
C ON TRUANDE LA SD MATR_ASSE POUR NE RESOUDRE LE SYSTEME QUE
C DE 1 A NBLIAC :
C ======================================================================
         NEQMAX= ZI(LDSCON+2)
         ZI(LDSCON+2) = NBLIAC+LLF
         CALL RLDLGG (LDSCON,ZR(JMU),CBID,1)
         ZI(LDSCON+2) = NEQMAX
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU
C ======================================================================
         DO 41 ILIAC = 1,NBLIAC+LLF
            LLIAC = ZI(JLIAC-1+ILIAC)
            IF (ILIAC.GT.NBLIAC) THEN
               CALL JEVEUO (JEXNUM(CM1A,LLIAC+NBLIAI),'L',JCM1A)
               DO 42 KK = 1,NEQ
                  ZR(JDELTA-1+KK) = ZR(JDELTA-1+KK) -
     &                 ZR(JCM1A-1+KK)*ZR(JMU-1+ILIAC)
 42            CONTINUE
               CALL JELIBE (JEXNUM(CM1A,LLIAC+NBLIAI))
            ELSE
               CALL JEVEUO (JEXNUM(CM1A,LLIAC),'L',JCM1A)
               DO 43 KK = 1,NEQ
                  ZR(JDELTA-1+KK) = ZR(JDELTA-1+KK) -
     &                 ZR(JCM1A-1+KK)*ZR(JMU-1+ILIAC)
 43            CONTINUE
               CALL JELIBE (JEXNUM(CM1A,LLIAC))
            ENDIF
 41      CONTINUE
C ======================================================================
C --- LES LIAISONS CONSIDEREES GLISSANTES LE SONT-ELLES VRAIMENT ? -----
C ======================================================================
         LFMIN = 0
         IF (LLF.LT.NBLIAC) THEN
            DO 710 II = 1, NBLIAC
               LLIAC = ZI(JLIAC-1+II)
               XK    = ZR(IFRO-1+LLIAC)
               TROUAC = .FALSE.
               DO 72 JJ = 1, LLF
                  IF(ZI(JLIAC-1+NBLIAC+JJ).EQ.LLIAC) TROUAC = .TRUE.
 72            CONTINUE
               IF (.NOT.TROUAC) THEN
                  JDECAL = ZI(JAPPTR+LLIAC-1)
                  NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
                  CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                 ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
                  AJEUFX = ZR(JAPJFX-1+LLIAC) + VAL
                  CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                 ZI(JAPDDL+JDECAL),ZR(JRESU),VAL)
                  AJEUFX = AJEUFX + VAL
                  XPDT = ZR(JMU+3*NBLIAI-1+LLIAC)*AJEUFX
                  IF (XPDT.LT.0.D0) THEN
C ======================================================================
C --- LA LIAISON EST EN FAITE ADHERENTE --------------------------------
C ======================================================================
                     LFMIN = LFMIN + 1
                     ZI(JLIAC-1+NBLIAC+LLF+LFMIN)= LLIAC
                     ZR(JMU-1+3*NBLIAI+LLIAC)=0.D0
                  ELSE IF (XPDT.EQ.0.D0) THEN
C ======================================================================
C --- LA LIAISON EST BIEN GLISSANTE ------------------------------------
C --- ON MET A JOUR MU_G EN FONCTION DE L'INCREMENT DE DEPLACEMENT -----
C ======================================================================
                    CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                 ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL)
                    AJEUFX = ZR(JAPJFX-1+LLIAC) + VAL
                    XCOMP = ABS( AJEUFX )
                    XCOS  = AJEUFX / XCOMP
                    ZR(JMU+3*NBLIAI-1+LLIAC) = XK*XCOS
                  ENDIF
               ENDIF
 710        CONTINUE
C ======================================================================
            IF (LFMIN.NE.0)  THEN
C ======================================================================
C --- S'IL EXISTE AU MOINS UNE LIAISON ADHERENTE SUPPLEMENTAIRE, -------
C --- ON NE PREND PAS EN COMPTE LES INCREMENTS DE DEPLACEMENTS CALCULES-
C --- ET ON RECOMMENCE LES CALCULS (NOTAMMENT POUR MU_SG) --------------
C ======================================================================
               LLF   = LLF+LFMIN
               GO TO 100
            ENDIF
         ENDIF
C ======================================================================
C --- LES LIAISONS CONSIDEREES ADHERENTES LE SONT-ELLES VRAIMENT ? -----
C ======================================================================
         KCOUNT = 0
         IF ( LLF .NE. 0 ) THEN
            DO 78 JJ = NBLIAC+1,NBLIAC+LLF
               LLJAC = ZI(JLIAC-1+JJ)
               XK    = ZR(IFRO-1+LLJAC)
               XCOMP = ABS( ZR(JMU-1+JJ) )
               DO 79 KK = 1,NBLIAC
                  LLKAC = ZI(JLIAC-1+KK)
                  IF ( LLKAC .EQ. LLJAC ) THEN
                     XQUOT = 0.0D0
                     IF ( ZR(JMU-1+KK) .GT. 0.0D0 ) THEN
                        XQUOT = XCOMP/ZR(JMU-1+KK)
                     ENDIF
                     IF ( ABS(XQUOT) .GE. XK ) THEN
                        KCOUNT = KCOUNT + 1
                        XCOS   = ZR(JMU-1+JJ)/XCOMP
                        ZR(JMU+3*NBLIAI-1+LLJAC) = XCOS * XK
                     ELSE
C ======================================================================
C --- DECALAGE DES INDICES DES LIAISONS EN CONTACT ---------------------
C ======================================================================
                        IF (JJ-NBLIAC-KCOUNT.GT.0) THEN
                           ZI(JLIAC-1-KCOUNT+JJ)=LLJAC
                           ZR(JMU  -1-KCOUNT+JJ)=ZR(JMU-1+JJ)
                        ENDIF
                        ZR(JMU+3*NBLIAI-1+LLJAC)=0.0D0
                     ENDIF
                  ENDIF
 79            CONTINUE
 78         CONTINUE
         ENDIF
        LLF = LLF - KCOUNT
      ENDIF
C ======================================================================
C --- CALCUL DE RHO ET MISE A JOUR DE L'ENSEMBLE DES LIAISONS ACTIVES --
C --- ET TEST DE CV ----------------------------------------------------
C ======================================================================
C --- CALCUL DE RHO = MIN ( (E(DEPTOT) - A.RESU)II / (A.DELTA)II) ) ----
C --- SUR LES LIAISONS NON ACTIVES DE NUMERO II ------------------------
C ======================================================================
      RHO    = R8MAEM()
      DELPOS = .FALSE.
C ======================================================================
C -- SI TOUTES LES LIAISONS SONT ACTIVES : RHO = 1 ---------------------
C ======================================================================
      IF (NBLIAC.EQ.NBLIAI) THEN
C ======================================================================
         RHO = 1.D0
C ======================================================================
C -- S'IL Y A DES LIAISONS NON ACTIVES : CALCUL DE RHO -----------------
C ======================================================================
      ELSE IF (NBLIAC.LT.NBLIAI) THEN
C ======================================================================
         DO 70 II = 1, NBLIAI
C ======================================================================
            TROUAC = .FALSE.
C ======================================================================
C - LA LIAISON II EST-ELLE ACTIVE ? (-> TROUAC) ------------------------
C ======================================================================
            DO 700 ILIAC = 1, NBLIAC
               IF (ZI(JLIAC-1+ILIAC).EQ.II) TROUAC = .TRUE.
 700        CONTINUE
C ======================================================================
C - CALCUL DE A.DELTA SI LA LIAISON II N'EST PAS ACTIVE
C ======================================================================
            IF (.NOT.TROUAC) THEN
               JDECAL = ZI(JAPPTR+II-1)
               NBDDL = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                              ZI(JAPDDL+JDECAL),ZR(JDELTA),AADELT)
C ======================================================================
C - SI A.DELTA EST POSITIF POUR II : CALCUL DE E(DEPTOT) - A.RESU
C - RHO = MIN ( ( E(DEPTOT) - A.RESU )II / (A.DELTA)II )
C - ON STOCKE DANS LLMIN LE NUMERO DE LA LIAISON REALISANT LE
C - MINIMUM (CE SERA LA LIAISON LA PLUS VIOLEE)
C ======================================================================
               IF (AADELT.GT.R8PREM()) THEN
C ======================================================================
C -  ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
C ======================================================================
                  DO  71 IOTE= 1,ZI(JLIOT+4*NBLIAI)
                     IF (ZI(JLIOT-1+IOTE).EQ.II) GOTO 70
 71               CONTINUE
C ======================================================================
C -  FIN ELIMINATION LIAISON A PIVOT NUL
C ======================================================================
                  DELPOS = .TRUE.
                  CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JRESU),VAL)
                  AJEU = ZR(JAPJEU+II-1) - VAL
                  AJEU = AJEU/AADELT
                  IF (AJEU.LT.RHO) THEN
                     RHO = AJEU
                     NUMIN = II
                  ENDIF
               ENDIF
            ENDIF
 70      CONTINUE
C ======================================================================
C - SI TOUS LES (A.DELTA)II SONT NEGATIFS : RHO = 1
C ======================================================================
         IF(.NOT.DELPOS) THEN
            RHO = 1.0D0
         ENDIF
      ENDIF
C ======================================================================
C --- TESTS SUR RHO ET ACTUALISATION DE RESU
C ======================================================================
      X1 = 1.D0
      RHORHO = MIN(RHO,X1)
C ======================================================================
C --- SI RHO < 1 (AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE) :
C --- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE (LLMIN)
C ======================================================================
      IF (RHORHO.LT.1.0D0) THEN
         NBLIAC = NBLIAC + 1
         INDIC = 1
         DO 124 II = 0,LLF-1
            ZI(JLIAC-1+NBLIAC+LLF-II) = ZI(JLIAC-1+NBLIAC+LLF-II-1)
 124     CONTINUE
         ZI(JLIAC-1+NBLIAC) = NUMIN
C ======================================================================
C --- LA LIAISON EST SUPPOSEE GLISSANTE
C ======================================================================
         ZR(JMU-1+3*NBLIAI+NUMIN) = 0.D0
         DO 84 KK = 1, NEQ
            ZR(JRESU-1+KK) = ZR(JRESU-1+KK) + RHORHO*ZR(JDELTA-1+KK)
 84      CONTINUE
         IF (NIV.GE.2) THEN
            POS1 = ZI(JAPPAR+3*(NUMIN-1)+1)
            NUM1 = ZI(JNOCO+ABS(POS1)-1)
            CALL JENUNO (JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
            POS2 = ZI(JAPPAR+3*(NUMIN-1)+2)
            IF (POS2.GT.0) THEN
               CHAIN = ' A  LA MAILLE '
               NUM2 = ZI(JMACO+POS2-1)
               CALL JENUNO (JEXNUM(NOMA//'.NOMMAI',NUM2),NOM2)
            ELSE IF (POS2.LT.0) THEN
               CHAIN = ' AU NOEUD     '
               NUM2 = ZI(JNOCO+ABS(POS2)-1)
               CALL JENUNO (JEXNUM(NOMA//'.NOMNOE',NUM2),NOM2)
            ELSE IF (POS2.EQ.0) THEN
               CHAIN = ' '
               NOM2  = ' '
            END IF
            WRITE (IFM,1000) 'LE NOEUD ',NOM1,' EST ASSOCIE  ',
     &           CHAIN,NOM2
         ENDIF
         GO TO 100
      ENDIF
C ======================================================================
C --- INITIALISATION DE ATMU ET DE AFMU --------------------------------
C ======================================================================
      DO 110 II = 1, NEQ
         ZR(JATMU+II-1) = 0.0D0
         ZR(JAFMU+II-1) = 0.0D0
 110  CONTINUE
C ======================================================================
      IF (NBLIAC.EQ.0) GO TO 9999
C ======================================================================
C --- ON ENLEVE TOUTES LES LIAISONS DE FROTTEMENT POUR LESQUELLES ------
C --- LA PRESSION EST NEGATIVE -----------------------------------------
C ======================================================================
      LCOUNT = 0
      DO 80 JJ = 1, LLF
         LLJAC = ZI(JLIAC-1+NBLIAC+JJ)
         DO 90 II = 1,NBLIAC
            LLIAC = ZI(JLIAC-1+II)
            IF ( LLJAC .EQ. LLIAC ) THEN
               IF ( ZR(JMU-1+II) .LT. 0.D0 ) THEN
                  LCOUNT = LCOUNT + 1
               ELSE
                  IF ( (JJ-LCOUNT.GT.0) ) THEN
                     ZI(JLIAC-1+NBLIAC+JJ-LCOUNT) = 
     +                                             ZI(JLIAC-1+NBLIAC+JJ)
                     ZR(JMU  -1+NBLIAC+JJ-LCOUNT) = 
     +                                             ZR(JMU  -1+NBLIAC+JJ)
                  ENDIF
               ENDIF
               GO TO 80
            ENDIF
 90      CONTINUE
 80   CONTINUE
C ======================================================================
C --- NOMBRE FINAL DE LIAISONS ADHERENTES ------------------------------
C ======================================================================
      LLF = LLF - LCOUNT
C ======================================================================
C --- ON ENLEVE TOUTES LES LIAISONS DE CONTACT POUR LESQUELLES ---------
C --- LA PRESSION EST NEGATIVE -----------------------------------------
C ======================================================================
      KCOUNT = 0
      DO 130 II = 1,NBLIAC
         LLIAC = ZI(JLIAC-1+II)
         IF ( ZR(JMU-1+II).LT.0.0D0 ) THEN
            KCOUNT = KCOUNT + 1
            ZR(JMU+3*NBLIAI-1+LLIAC) = 0.0D0
         ELSE
            IF(II-KCOUNT.GT.0) THEN
               ZI(JLIAC-1+II-KCOUNT) = ZI(JLIAC-1+II)
               ZR(JMU-1+II-KCOUNT) = ZR(JMU-1+II)
            ENDIF
         ENDIF
 130  CONTINUE
C ======================================================================
C --- AJUSTEMENT DES LIAISONS DE FROTTEMENTS ---------------------------
C ======================================================================
         DO 88 II = 1,LLF
            ZI(JLIAC-1+II+NBLIAC-KCOUNT) = ZI(JLIAC-1+II+NBLIAC)
            ZR(JMU  -1+II+NBLIAC-KCOUNT) = ZR(JMU  -1+II+NBLIAC)
 88      CONTINUE
C ======================================================================
C --- NOMBRE FINAL DE LIAISONS DE CONTACT ------------------------------
C ======================================================================
      NBLIAC = NBLIAC - KCOUNT
C ======================================================================
C --- CALCUL DE AT.MU --------------------------------------------------
C ======================================================================
         DO 123 ILIAC = 1, NBLIAC+LLF
            LLIAC  = ZI(JLIAC+ILIAC-1)
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
            IF ( ILIAC.LE.NBLIAC ) THEN
               CALL CALATM (NEQ,NBDDL,ZR(JMU+ILIAC-1),
     +                   ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),ZR(JATMU))
            ELSE
               CALL CALATM(NEQ,NBDDL,ZR(JMU+ILIAC-1),
     +                   ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),ZR(JATMU))
            ENDIF
 123     CONTINUE
C ======================================================================
C --- CALCUL DE AFMU ---------------------------------------------------
C ======================================================================
      DO 140 II = 1, NBLIAI
         XFORC =  0.0D0
         DO 160 KK = 1,NBLIAC
            LLKAC = ZI(JLIAC-1+KK)
            IF ( LLKAC.EQ.II ) THEN
               XFORC = ZR(JMU-1+KK)
               GO TO 170
            ENDIF
 160     CONTINUE
 170     CONTINUE
         JDECAL = ZI(JAPPTR+II-1)
         NBDDL  = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
         ZMU    = XFORC * ZR(JMU-1+3*NBLIAI+II)
         CALL CALATM(NEQ,NBDDL,ZMU,ZR(JAPCOF+JDECAL),
     +                                      ZI(JAPDDL+JDECAL),ZR(JAFMU))
 140  CONTINUE
C ======================================================================
 9999 CONTINUE
C ======================================================================
C --- RECUPERATION DU DEPLACEMENT FINAL --------------------------------
C ======================================================================
      DO 150 II = 1,NEQ
          ZR(JRESU-1+II) = ZR(JRESU-1+II) + ZR(JDELTA-1+II)
 150  CONTINUE
C ======================================================================
C --- STOCKAGE DE L'ETAT DE CONTACT DEFINITIF --------------------------
C ======================================================================
      IF ( NIV .EQ. 2 ) WRITE(IFM,*)'NBLIACF',NBLIAC,'LLFF',LLF
C      IF ( NBLIAC.NE.NBLCIN ) PRIMCV(2) = 100
C      IF ( LLF   .NE.NBLFIN ) PRIMCV(2) = 100
      ZI(JCOCO+1) = NBLIAC
      ZI(JCOCO+6) = LLF
C ======================================================================
C --- CALCUL DES JEUX FINAUX -------------------------------------------
C ======================================================================
      DO 400 II=1,NBLIAI
         JDECAL = ZI(JAPPTR+II-1)
         NBDDL  = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
C ======================================================================
         CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     +                                  ZI(JAPDDL+JDECAL),ZR(JRESU),VAL)
         ZR(JAPJEU-1+II) = ZR(JAPJEU-1+II)-VAL
C ======================================================================
         CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     +                                  ZI(JAPDDL+JDECAL),ZR(JRESU),VAL)
         ZR(JAPJFX-1+II) = ZR(JAPJFX-1+II)+VAL
C ======================================================================
 400  CONTINUE
C ======================================================================
      IF (NIV.GE.2) THEN
         DO 200 II = 1,NBLIAI
            WRITE (IFM,1010) '<FRO2GD> JEU FINAL LIAISON ',II,' : ',
     +                                                   ZR(JAPJEU+II-1)
 200     CONTINUE
      END IF
C ======================================================================
 999  CONTINUE
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
 1000 FORMAT ('<CONTACT_2> ',A9,A8,A14,A14,A8)
 1010 FORMAT ('<CONTACT_3> ',A27,I5,A3,E10.3)
C ======================================================================
      END
