       SUBROUTINE ALGOCL(DEFICO,RESOCO,LMAT,LDSCON,NOMA,CINE,RESU,
     &                   DEPTOT,ITERAT,LREAC,ISTO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/09/2002   AUTEUR PABHHHH N.TARDIEU 
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
C ======================================================================
      IMPLICIT NONE
      LOGICAL LREAC(4)
      INTEGER LMAT,LDSCON,ITERAT,ISTO
      CHARACTER*8 NOMA
      CHARACTER*24 DEFICO,RESOCO,CINE,RESU,DEPTOT
C ======================================================================
C ROUTINE APPELEE PAR : NMCONT
C ======================================================================
C RESOLUTION DU TRAITEMENT DU CONTACT PAR LA METHODE LAGRANGIENNE
C
C RESOLUTION DE : C.DU + AT.MU  = F
C                 A(U+DU)      <= E (= POUR LES LIAISONS ACTIVES)
C AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
C
C      C = ( K  BT ) MATRICE DE RIGIDITE INCLUANT LES LAGRANGE
C          ( B  0  )
C ======================================================================
C      U = ( DEPL )
C          ( LAM  )
C ======================================================================
C      F = ( DL  ) DANS LA PHASE DE PREDICTION
C          ( DUD )
C ======================================================================
C      F = ( L - QT.SIG - BT.LAM  ) AU COURS D'UNE ITERATION DE NEWTON
C          (           0          )
C ======================================================================
C IN  DEFICO  : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO  : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  LMAT    : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  LDSCON  : DESCRIPTEUR DE LA MATRICE -A.C-1.AT
C IN  NOMA    : NOM DU MAILLAGE
C IN  CINE    : CHAM_NO CINEMATIQUE
C IN  DEPTOT  : DEPLACEMENT TOTAL OBTENU A L'ISSUE DE L'ITERATION
C               DE NEWTON PRECEDENTE
C IN  ITERAT  : ITERATION COURANTE DE NEWTON
C IN  LREAC   : INDICATEUR DE REACTUALISATION GEOMETRIQUE
C IN  ISTO    : 0 ON ARRETE LE CALCUL SI ON PERD PLUS DE 8 DECIMALES
C               1 ON N'ARRETE PAS LE CALCUL
C VAR RESU    : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS DEPTOT
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C ======================================================================
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------
C ======================================================================
      CHARACTER*32 JEXNUM,JEXNOM
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
C ======================================================================
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C ======================================================================
      LOGICAL TROUAC,DELPOS,GCPC
      INTEGER JCHSEC,JCHSOL,JSLVK,IREAC,KKMIN
      INTEGER IBID,IER,IFM,NIV,NDECI,ISINGU,NPVNEG,NBLCIN
      INTEGER ICONTA,II,JJ,KK,IDEBUT,ILIAC,KCOUNT,NUMIN
      INTEGER JRESU,JDEPP,JMU,JCMU,JATMU,POSMA,NDIM,NEQMAX
      INTEGER JDELT0,JDELTA,JLIAC,JVALE,JCOCO,JCM1A,JRCINE,JVA,JLIOT
      INTEGER NEQ,NESCL,NBLIAC,NBLIAI,NBLIAP,INDIC,LLMIN,IOTE
      INTEGER LLIAC,LLJAC,POS1,POS2,NUM1,NUM2,JDECAL,NBDDL,PIVOT
      INTEGER JAPPAR,JAPPTR,JAPCOE,JAPJEU,JAPDDL,JNOCO,JMACO,JVALC
      REAL*8 R8MAEM,R8PREM,AJEU,RHO,RHORHO,AADELT
      REAL*8 X1,VAL,XJVMAX,XJVMIN
      COMPLEX*16 CBID
      CHARACTER*8 NOM1,NOM2
      CHARACTER*14 CHAIN
      CHARACTER*19 LIAC,MU,ATMU,DELT0,DELTA,CM1A,MATR,COCO,LIOT
      CHARACTER*19 MATASS,MATAS1,MATPRE,CHASEC,CHASOL,SOLVEU
      CHARACTER*24 APPARI,APPOIN,APCOEF,APJEU,APDDL,COEFMU
      CHARACTER*24 CONTNO,CONTMA,MACONT
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
      CALL JEMARQ()
C ======================================================================
C --- LE CONTACT DOIT-IL ETRE MODELISE ?
C ======================================================================
      APPARI = RESOCO(1:14)//'.APPARI'
      CALL JEEXIN(APPARI,ICONTA)
      IF (ICONTA.EQ.0) GO TO 260
      CALL JEVEUO(APPARI,'L',JAPPAR)
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
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CONTMA = DEFICO(1:16)//'.MAILCO'
      APPARI = RESOCO(1:14)//'.APPARI'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APJEU  = RESOCO(1:14)//'.APJEU'
      APDDL  = RESOCO(1:14)//'.APDDL'
      LIAC   = RESOCO(1:14)//'.LIAC'
      LIOT   = RESOCO(1:14)//'.LIOT'
      MU     = RESOCO(1:14)//'.MU'
      COEFMU = RESOCO(1:14)//'.COEFMU'
      ATMU   = RESOCO(1:14)//'.ATMU'
      DELT0  = RESOCO(1:14)//'.DEL0'
      DELTA  = RESOCO(1:14)//'.DELT'
      CM1A   = RESOCO(1:14)//'.CM1A'
      MATR   = RESOCO(1:14)//'.MATR'
C      COMPOR = '&&NMDORC.COMPOR'
C ======================================================================
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APJEU,'E',JAPJEU)
      CALL JEVEUO(APDDL,'L',JAPDDL)
      CALL JEVEUO(LIAC,'E',JLIAC)
      CALL JEVEUO(LIOT,'E',JLIOT)
      CALL JEVEUO(MU,'E',JMU)
      CALL JEVEUO(COEFMU,'L',JCMU)
      CALL JEVEUO(ATMU,'E',JATMU)
      CALL JEVEUO(DELT0,'E',JDELT0)
      CALL JEVEUO(DELTA,'E',JDELTA)
      CALL JEVEUO(RESU(1:19)//'.VALE','E',JRESU)
      CALL JEVEUO(DEPTOT(1:19)//'.VALE','E',JDEPP)
C ======================================================================
      NBLIAI = NESCL
      MACONT = ZK24(ZI(LDSCON+1))
      CALL JEECRA(MACONT(1:19)//'.REFA','DOCU',IBID,'ASSE')
      NEQ = ZI(LMAT+2)
      CALL JEVEUO(JEXNUM(MATR//'.VALE',1),'E',JVALE)
C ======================================================================
C --- SOUVENIRS DE L'ETAT DE CONTACT -> ON NE PEUT PAS S'EN SERVIR
C --- AU DEBUT CAR SI ON A REAPPARIE LES LIAISONS SONT DIFFERENTES
C ======================================================================
C NBLIAC : NOMBRE DE LIAISONS ACTIVES
C ======================================================================
      COCO = RESOCO(1:14)//'.COCO'
      CALL JEVEUO(COCO,'E',JCOCO)
      NBLIAC = ZI(JCOCO+1)
      KKMIN = 0
C ======================================================================
C                             INITIALISATIONS
C ======================================================================
C     SI SOLVEUR GCPC, ON ALLOUE 2 CHAM_NO UTILES POUR APPELER RESOUD :
C     -----------------------------------------------------------------
      SOLVEU = '&&OP0070.SOLVEUR'
      CALL JEVEUO(SOLVEU//'.SLVK','L',JSLVK)
      GCPC = (ZK24(JSLVK-1+1).EQ.'GCPC')
      IF (GCPC) THEN
        MATAS1 = ZK24(ZI(LMAT+1))
        MATASS = '&&MATASS'
        IF (MATAS1.NE.MATASS) CALL UTMESS('F','ALGOCL','STOP')
        MATPRE = '&&NMMATR.MAPREC'
        CHASOL = '&&ALGOCL.CHASOL'
        CHASEC = '&&ALGOCL.CHASEC'
        CALL COPISD('CHAMP_GD','V',DEPTOT,CHASOL)
        CALL COPISD('CHAMP_GD','V',DEPTOT,CHASEC)
        CALL JEVEUO(CHASOL//'.VALE','E',JCHSOL)
        CALL JEVEUO(CHASEC//'.VALE','L',JCHSEC)
      END IF
C ======================================================================
C --- CREATION DE DELTA0 = C-1B
C ======================================================================
      DO 10 II = 1,NEQ
        ZR(JDELT0-1+II) = ZR(JRESU-1+II)
        ZR(JRESU -1+II) = 0.0D0
        ZR(JATMU -1+II) = 0.0D0
   10 CONTINUE
C ======================================================================
C --- CALCUL DE -A.DEPTOT ET RANGEMENT DANS APJEU
C --- (UNIQUEMENT POUR LES CL SANS APPARIEMENT,
C --- C'EST-A-DIRE POUR P, T, OU U RIGIDE : LORSQUE POSMA = 0)
C ======================================================================
      DO 20 II = 1,NBLIAI
        POSMA = ZI(JAPPAR+3* (II-1)+2)
        IF (POSMA.EQ.0) THEN
          JDECAL = ZI(JAPPTR+II-1)
          NBDDL = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
          CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                ZR(JDEPP),VAL)
          ZR(JAPJEU+II-1) = ZR(JAPJEU+II-1) - VAL
        END IF
   20 CONTINUE
C ======================================================================
C --- DETECTION DES COUPLES DE NOEUDS INTERPENETRES
C --- ON CALCULE LE NOUVEAU JEU : AJEU+ = AJEU/I/N - A.DDEPLA
C --- (IL EST NEGATIF LORSQU'IL Y A INTERPENETRATION -> LIAISON ACTIVE)
C ======================================================================
      IF (ITERAT.EQ.0) THEN
        NBLIAC = 0
        ZI(JLIOT+4*NBLIAI) = 0
        DO 30 II = 1,NBLIAI
          JDECAL = ZI(JAPPTR+II-1)
          NBDDL = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
          CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                ZR(JDELT0),VAL)
          AJEU = ZR(JAPJEU+II-1)
          IF (AJEU.LT.0.0D0) THEN
            NBLIAC = NBLIAC + 1
            ZI(JLIAC-1+NBLIAC) = II
            IF (NIV.GE.2) THEN
              POS1 = ZI(JAPPAR+3* (II-1)+1)
              NUM1 = ZI(JNOCO+POS1-1)
              CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
              POS2 = ZI(JAPPAR+3* (II-1)+2)
              IF (POS2.GT.0) THEN
                CHAIN = ' A  LA MAILLE '
                NUM2 = ZI(JMACO+POS2-1)
                CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUM2),NOM2)
              ELSE IF (POS2.LT.0) THEN
                CHAIN = ' AU NOEUD     '
                NUM2 = ZI(JNOCO+ABS(POS2)-1)
                CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM2),NOM2)
              ELSE IF (POS2.EQ.0) THEN
                CHAIN = ' '
                NOM2 = ' '
              END IF
              WRITE (IFM,1000) 'LE NOEUD ',NOM1,' EST ASSOCIE ',CHAIN,
     &          NOM2
            END IF
          END IF
   30   CONTINUE
      ELSE
        IF (LREAC(1)) THEN
          DO 50 II = 1,NBLIAI
            AJEU = ZR(JAPJEU+II-1)
            IF (AJEU.LT.0.D0) THEN
              TROUAC = .FALSE.
              DO 40,JJ = 1,NBLIAC
                IF (ZI(JLIAC-1+JJ).EQ.II) THEN
                  TROUAC = .TRUE.
                END IF
   40         CONTINUE
              IF (.NOT.TROUAC) THEN
                NBLIAC = NBLIAC + 1
                ZI(JLIAC-1+NBLIAC) = II
              END IF
            END IF
   50     CONTINUE
        END IF

      END IF
C ======================================================================
C SI PAS DE LIAISON ACTIVE,
C ON REMPLIT DELTA ET ON VA
C DIRECTEMENT AU CALCUL DE RHO
C ======================================================================
      IDEBUT = 1
      INDIC  = 1
      NBLCIN = NBLIAC
      IF ( NIV .EQ. 2 ) WRITE(IFM,*)'NBLIACI',NBLCIN
      GOTO 2
 60   CONTINUE
      IDEBUT = NBLIAC
 2    CONTINUE
C ======================================================================
C RESOLUTION MATRICIELLE POUR DES LIAISONS ACTIVES
C ======================================================================
      IF (NBLIAC.NE.0) THEN
C ======================================================================
C MISE A ZERO DES COMPTEURS
C ======================================================================
        XJVMAX = 0.D0
        XJVMIN = 1.D0/R8PREM()
C ======================================================================
C RECUPERATION DU COMPORTEMENT
C ======================================================================
C        CALL JEVEUO(COMPOR//'.VALE','L',JVALC)
        IF (INDIC .NE. -1) THEN
C        IF (INDIC .NE. -1 .OR. ZK16(JVALC-1+1) .NE. 'ELAS' 
C     &       .OR. LREAC) THEN            
C ======================================================================
C CALCUL DE C-1.AT SI NECESSAIRE
C ======================================================================
           DO 55 ILIAC = IDEBUT,NBLIAC
              LLIAC = ZI(JLIAC+ILIAC-1)
C ======================================================================
C -  CALCUL DE CHAQUE COLONNE DE AT (UNE PAR LIAISON ACTIVE)
C ======================================================================
              CALL JEVEUO(JEXNUM(CM1A,LLIAC),'E',JCM1A)         
              DO 70 KK = 1,NEQ
                ZR(JCM1A-1+KK) = 0.0D0
 70           CONTINUE
              JDECAL = ZI(JAPPTR+LLIAC-1)
              NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
              CALL CALATM(NEQ,NBDDL,1.D0,ZR(JAPCOE+JDECAL),
     &                        ZI(JAPDDL+JDECAL),ZR(JCM1A))
C ======================================================================
C - CALCUL DE C-1.AT (EN TENANT COMPTE DES CHARGES CINEMATIQUES)
C ======================================================================
              IF (GCPC) THEN
                 CALL JACOPO(NEQ,'R',JCM1A,JCHSEC)
                 CALL RESOUD(MATASS,MATPRE,CHASEC,SOLVEU,CINE,'V',
     &                  CHASOL,'&&ALGOCL_CRIT')
                 CALL JEVEUO(CHASOL//'.VALE','L',JCHSOL)
                 CALL JACOPO(NEQ,'R',JCHSOL,JCM1A)
              ELSE
                 CALL JEVEUO(CINE(1:19)//'.VALE','E',JRCINE)
                 CALL NMRLDL(LMAT,ZR(JRCINE),ZR(JCM1A))
              END IF
              CALL JELIBE(JEXNUM(CM1A,LLIAC))
 55        CONTINUE
        ELSE
           IDEBUT = 1
        ENDIF
C ======================================================================
C - CALCUL DE -A.C-1.AT (REDUITE AUX LIAISONS ACTIVES)
C - (STOCKAGE DE LA MOITIE PAR SYMETRIE)
C ======================================================================
        DO 80 ILIAC = IDEBUT,NBLIAC
           LLIAC = ZI(JLIAC+ILIAC-1)
           CALL JEVEUO(JEXNUM(CM1A,LLIAC),'L',JCM1A)
           DO 90 JJ = 1,ILIAC
              LLJAC = ZI(JLIAC-1+JJ)
              JVA = JVALE - 1 + (ILIAC-1)*ILIAC/2 + JJ
              ZR(JVA) = 0.0D0
              JDECAL = ZI(JAPPTR+LLJAC-1)
              NBDDL = ZI(JAPPTR+LLJAC) - ZI(JAPPTR+LLJAC-1)
              CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                   ZR(JCM1A),VAL)
              ZR(JVA) = ZR(JVA) - VAL
              IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
              IF(ABS(ZR(JVA)).LT.XJVMIN) XJVMIN = ABS(ZR(JVA))
 90        CONTINUE
           CALL JELIBE(JEXNUM(CM1A,LLIAC))
 80     CONTINUE
C ======================================================================
C --- ELIMINATION DES PIVOTS NULS
C ======================================================================
        CALL ELPIV1(XJVMAX,MATR,NOMA,DEFICO,RESOCO,IDEBUT,NBLIAC,KKMIN,
     &             PIVOT,INDIC)
        IF (INDIC .EQ. -1 .AND. PIVOT .EQ. 1) GOTO 60
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
        CALL JEECRA(MACONT(1:19)//'.REFA','DOCU',IBID,'ASSE')
        IF (IDEBUT.LE.NBLIAC) THEN
           CALL TLDLGG(2,LDSCON,IDEBUT,NBLIAC,0,NDECI,ISINGU,NPVNEG,IER)

C--- LA MATRICE DE CONTACT EST-ELLE SINGULIERE?
           IF (IER.GT.ISTO) THEN
              CALL UTMESS('F','ALGOCL','ARRET SUR MATRICE SINGULIERE')
           END IF
        END IF
C ======================================================================
C --- SECOND MEMBRE : ON MET JEU(DEPTOT) - A.DELT0 DANS MU
C ======================================================================
        DO 100 II = 1,NBLIAI
          ZR(JMU-1+II) = 0.D0
  100   CONTINUE
        DO 110 ILIAC = 1,NBLIAC
          LLIAC = ZI(JLIAC-1+ILIAC)
          JDECAL = ZI(JAPPTR+LLIAC-1)
          NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
          CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                ZR(JDELT0),VAL)
          ZR(JMU+ILIAC-1) = ZR(JAPJEU+LLIAC-1) - VAL
  110   CONTINUE
C ======================================================================
C RESOLUTION POUR OBTENIR MU : -A.C-1.AT.MU = JEU(DEPTOT) - A.DELT0
C ON TRUANDE LA SD MATR_ASSE POUR NE RESOUDRE LE SYSTEME QUE
C DE 1 A NBLIAC :
C ======================================================================
        NEQMAX = ZI(LDSCON+2)
        ZI(LDSCON+2) = NBLIAC
        CALL RLDLGG(LDSCON,ZR(JMU),CBID,1)
        ZI(LDSCON+2) = NEQMAX
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU
C ======================================================================
        DO 120 II = 1,NEQ
           ZR(JDELTA-1+II) = ZR(JDELT0-1+II) - ZR(JRESU-1+II)
  120   CONTINUE
        DO 140 ILIAC = 1,NBLIAC
          LLIAC = ZI(JLIAC-1+ILIAC)
          CALL JEVEUO(JEXNUM(CM1A,LLIAC),'L',JCM1A)
          DO 130 KK = 1,NEQ
            ZR(JDELTA-1+KK) = ZR(JDELTA-1+KK) -
     &                        ZR(JCM1A-1+KK)*ZR(JMU-1+ILIAC)
  130     CONTINUE
          CALL JELIBE(JEXNUM(CM1A,LLIAC))
  140   CONTINUE
      ELSE
        DO 101 II = 1,NEQ
           ZR(JDELTA-1+II) = ZR(JDELT0-1+II)
 101    CONTINUE
      END IF
C ======================================================================
C CALCUL DE RHO ET MISE A JOUR
C DE L'ENSEMBLE DES LIAISONS
C ACTIVES + TEST DE CV
C ======================================================================
C --- CALCUL DE RHO = MIN ( (E(DEPTOT) - A.RESU)II / (A.DELTA)II) )
C --- SUR LES LIAISONS NON ACTIVES DE NUMERO II
C ======================================================================
      RHO    = R8MAEM()
      DELPOS = .FALSE.
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
C - LA LIAISON II EST-ELLE ACTIVE ? (-> TROUAC)
C ======================================================================
          DO 170 ILIAC = 1,NBLIAC
            IF (ZI(JLIAC-1+ILIAC).EQ.II) TROUAC = .TRUE.
  170     CONTINUE
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
C - ON STOCKE DANS LLMIN LE NUMERO DE LA LIAISON REALISANT LE
C - MINIMUM (CE SERA LA LIAISON LA PLUS VIOLEE)
C ======================================================================
            IF (AADELT.GT.R8PREM()) THEN
C ======================================================================
C -  ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
C ======================================================================
               DO 181 IOTE= 1,ZI(JLIOT+4*NBLIAI)
                  IF (ZI(JLIOT-1+IOTE).EQ.II) GOTO 180
 181           CONTINUE
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
               END IF
             END IF
          END IF
 180   CONTINUE
C ======================================================================
C - SI TOUS LES (A.DELTA)II SONT NEGATIFS : RHO = 1
C ======================================================================
        IF (.NOT.DELPOS) THEN
          RHO = 1.0D0
        END IF
      END IF
C ======================================================================
C --- TESTS SUR RHO ET ACTUALISATION DE RESU
C ======================================================================
      X1 = 1.D0
      RHORHO = MIN(RHO,X1)
C ======================================================================
C -- SI RHO < 1 (AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE) :
C -- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE (LLMIN)
C ======================================================================
      IF (RHORHO.LT.1.0D0) THEN
         NBLIAC = NBLIAC + 1
         INDIC = 1
         ZI(JLIAC-1+NBLIAC) = NUMIN
         DO 190 KK = 1,NEQ
            ZR(JRESU-1+KK) = ZR(JRESU-1+KK) + RHORHO*ZR(JDELTA-1+KK)
  190    CONTINUE
         IF (NIV.GE.2) THEN
            POS1 = ZI(JAPPAR+3* (NUMIN-1)+1)
            NUM1 = ZI(JNOCO+ABS(POS1)-1)
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM1),NOM1)
            POS2 = ZI(JAPPAR+3* (NUMIN-1)+2)
            IF (POS2.GT.0) THEN
               CHAIN = ' A  LA MAILLE '
               NUM2 = ZI(JMACO+POS2-1)
               CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUM2),NOM2)
            ELSE IF (POS2.LT.0) THEN
               CHAIN = ' AU NOEUD     '
               NUM2 = ZI(JNOCO+ABS(POS2)-1)
               CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUM2),NOM2)
            ELSE IF (POS2.EQ.0) THEN
               CHAIN = ' '
               NOM2 = ' '
            END IF
            WRITE (IFM,1000) 'LE NOEUD ',NOM1,' EST ASSOCIE  ',
     +                                                       CHAIN,NOM2
        END IF
        GO TO 60
      END IF
C ======================================================================
C --------- ON ENLEVE TOUTES LES LIAISONS POUR LESQUELLES
C --------- LA PRESSION EST NEGATIVE
C ======================================================================
      KCOUNT = 0
      DO 200 II = 1,NBLIAC
        IF (ZR(JMU-1+II).LT.0.D00) THEN
          KCOUNT = KCOUNT + 1
        ELSE
          IF (II-KCOUNT.GT.0) THEN
            ZI(JLIAC-1+II-KCOUNT) = ZI(JLIAC-1+II)           
            ZR(JMU-1+II-KCOUNT) = ZR(JMU-1+II)
          END IF
        END IF
  200 CONTINUE
C ======================================================================
C ----- NOMBRE FINAL DE LIAISONS DE CONTACT ET D'ADHERENCE
C ======================================================================
      NBLIAC = NBLIAC - KCOUNT
C ======================================================================
C ----- RECUPERATION DU DEPLACEMENT FINAL
C ======================================================================
      DO 230 II = 1,NEQ
        ZR(JRESU-1+II) = ZR(JRESU-1+II) + ZR(JDELTA-1+II)
  230 CONTINUE
C ======================================================================
C --- CALCUL DE AT.MU
C ======================================================================
        DO 160 ILIAC = 1,NBLIAC
          LLIAC = ZI(JLIAC+ILIAC-1)  
          JDECAL = ZI(JAPPTR+LLIAC-1)
          NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
          CALL CALATM(NEQ,NBDDL,ZR(JMU+ILIAC-1),ZR(JAPCOE+JDECAL),
     &                                      ZI(JAPDDL+JDECAL),ZR(JATMU))
  160   CONTINUE
C ======================================================================
C --- STOCKAGE DE L'ETAT DE CONTACT DEFINITIF
C ======================================================================
      IF ( NIV .EQ. 2 ) WRITE(IFM,*)'NBLIACF',NBLIAC
      IF ( NBLIAC.NE.NBLCIN ) LREAC(2) = .TRUE.
      ZI(JCOCO+1) = NBLIAC
C ======================================================================
C --- CALCUL DU JEU FINAL
C ======================================================================
      DO 240 II = 1,NBLIAI
        JDECAL = ZI(JAPPTR+II-1)
        NBDDL = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
        CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &              ZR(JRESU),VAL)
        ZR(JAPJEU-1+II) = ZR(JAPJEU-1+II) - VAL
  240 CONTINUE
      IF (NIV.GE.2) THEN
        DO 250 II = 1,NBLIAI
          WRITE (IFM,1010) '<ALGOCL> JEU FINAL LIAISON ',II,' : ',
     &      ZR(JAPJEU+II-1)
  250   CONTINUE
      END IF
  260 CONTINUE
C ======================================================================
C --- DESTRUCTION DES VECTEURS INUTILS
C ======================================================================
      CALL DETRSD('CHAMP_GD','&&ALGOCL.CHASOL')
      CALL DETRSD('CHAMP_GD','&&ALGOCL.CHASEC')
      CALL JEDEMA()
C ======================================================================
 1000 FORMAT ('<CONTACT_2> ',A9,A8,A14,A14,A8)
 1010 FORMAT ('<CONTACT_3> ',A27,I5,A3,E10.3)
C ======================================================================
      END
