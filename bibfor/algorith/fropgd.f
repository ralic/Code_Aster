      SUBROUTINE FROPGD (DEFICO,RESOCO,LMAT,LDSCON,NOMA,CINE,
     &           RESU,DEPTOT,ITERAT,LREAC,CONV,DEPDEL,ISTO)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2003   AUTEUR CIBHHPD D.NUNEZ 
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
      REAL*8            CONV(*)
      CHARACTER*8       NOMA
      CHARACTER*24      DEFICO,RESOCO,CINE,RESU,DEPTOT,DEPDEL
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : NMCONT
C ----------------------------------------------------------------------
C
C METHODE PENALISATION POUR LE CONTACT-FROTTEMENT
C  => PENALISATION DU FROTTEMENT UNIQUEMENT POUR LES NOEUDS GLISSANTS
C
C RESO. DE : C.DU + AcT.MUc     = F - kg AgT.Ag (E-U)
C                 Ac. (U+DU)   <= E  (= POUR LES LIAISONS ACTIVES)
C
C AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
C
C     Ac = MATRICE DE CONTACT
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
C VAR RESU    : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS DEPTOT
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------
C
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
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C

      LOGICAL TROUAC,DELPOS
      INTEGER JJC,LL,JDEPDE
      INTEGER IBID,IER,IFM,NIV,NDECI,ISINGU,NPVNEG,JAPJFX
      INTEGER ICONTA,II,JJ,KK,ILIAC,KCOUNT,NUMIN,JAPJFY
      INTEGER JRESU,JDEPP,JMU,JCMU,JATMU,POSMA,NDIM,NEQMAX
      INTEGER JDELT0,JDELTA,JLIAC,JVALE,JCOCO,JRCINE,JVA,JLIOT
      INTEGER NEQ,NESCL,NBLIAC,NBLIAI,INDIC,KKMIN
      INTEGER LLIAC,LLJAC,POS1,POS2,NUM1,NUM2,JDECAL,NBDDL,IPENA
      INTEGER JAPPAR,JAPPTR,JAPCOE,JAPJEU,JAPDDL,JNOCO,JMACO
      INTEGER NBLCIN,PIVOT,INDFAC,POSIT,SPAVAN
      INTEGER JDIM,NESMAX,BTOTAL,IOTE
      REAL*8 R8MAEM,R8PREM,AJEU,RHO,RHORHO,AADELT,AJEUFY,XF
      REAL*8 X1,VAL,XXMIN,XXMAX,XX
      REAL*8 XTOL,AJEUFX,XK,XMU
      REAL*8 ALPHA,BETA,RESIGR,XJVMAX,XJVMIN
      INTEGER ICOMA
      INTEGER JAPCOF,JAFMU,LMAF1,JCM2A,JCM3A
      INTEGER IFRO,AJLIAI,SPLIAI,LLF,LLF1,LLF2
      COMPLEX*16 CBID
      CHARACTER*1 TYPEAJ
      CHARACTER*2 TYPEC0
      CHARACTER*8 NOM1,NOM2
      CHARACTER*14 CHAIN,NUMEDD
      CHARACTER*19 AFMU,MAT,CM2A,CM3A,MAF1,MAF2,MAFROT
      CHARACTER*19 LIAC,MU,ATMU,DELT0,DELTA,MATR,COCO,LIOT
      CHARACTER*24 MACONT,APJEFX,APJEFY
      CHARACTER*24 APPARI,APPOIN,APCOEF,APJEU,APDDL,COEFMU
      CHARACTER*24 NDIMCO,CONTNO,CONTMA,APCOFR,FROTE,PENAL,COMAFO

C ----------------------------------------------------------------------

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

      CALL INFNIV(IFM,NIV)
      CALL JEMARQ()

C --- LE CONTACT DOIT-IL ETRE MODELISE ?

      APPARI = RESOCO(1:14)//'.APPARI'
      CALL JEEXIN(APPARI,ICONTA)
      IF (ICONTA.EQ.0) GO TO 360
      CALL JEVEUO(APPARI,'L',JAPPAR)
      NESCL = ZI(JAPPAR)

C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT

C LIAC   : LISTE DES INDICES DES LIAISONS ACTIVES
C MU     : MULTIPLICATEURS DE LAGRANGE DU CONTACT (DOIVENT ETRE > 0)
C COEFMU : COEFFICIENT PAR LEQUEL IL FAUT MULTIPLIER MU AVANT DE
C          TESTER SON SIGNE (-1 SI CONDITION EN PRESSION OU TEMPERATURE)
C ATMU   : FORCES DE CONTACT
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CONTMA = DEFICO(1:16)//'.MAILCO'
      APPARI = RESOCO(1:14)//'.APPARI'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APCOFR = RESOCO(1:14)//'.APCOFR'
      APJEU = RESOCO(1:14)//'.APJEU'
      APJEFX = RESOCO(1:14)//'.APJEFX'
      APJEFY = RESOCO(1:14)//'.APJEFY'
      APDDL = RESOCO(1:14)//'.APDDL'
      LIAC = RESOCO(1:14)//'.LIAC'
      LIOT = RESOCO(1:14)//'.LIOT'
      MU = RESOCO(1:14)//'.MU'
      COEFMU = RESOCO(1:14)//'.COEFMU'
      ATMU = RESOCO(1:14)//'.ATMU'
      AFMU = RESOCO(1:14)//'.AFMU'
      DELT0 = RESOCO(1:14)//'.DEL0'
      DELTA = RESOCO(1:14)//'.DELT'
      CM2A = RESOCO(1:14)//'.CM2A'
      CM3A = RESOCO(1:14)//'.CM3A'
      MATR = RESOCO(1:14)//'.MATR'
      MAFROT = RESOCO(1:8)//'.MAFR'
      MAF1 = '&&FROPGD.MAF1'
      MAF2 = '&&FROPGD.MAF2'
      FROTE = DEFICO(1:16)//'.FROTE'
      PENAL = DEFICO(1:16)//'.PENAL'
      COMAFO = DEFICO(1:16)//'.COMAFO'
      NDIMCO = DEFICO(1:16)//'.NDIMCO'

      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APCOFR,'L',JAPCOF)
      CALL JEVEUO(APJEU,'E',JAPJEU)
      CALL JEVEUO(APJEFX,'E',JAPJFX)
      CALL JEVEUO(APJEFY,'E',JAPJFY)
      CALL JEVEUO(APDDL,'L',JAPDDL)
      CALL JEVEUO(LIAC,'E',JLIAC)
      CALL JEVEUO(LIOT,'E',JLIOT)
      CALL JEVEUO(MU,'E',JMU)
      CALL JEVEUO(COEFMU,'L',JCMU)
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
C ======================================================================
      NESMAX = ZI(JDIM+8)
      NBLIAI = NESCL
      RESIGR = CONV(20)

      MACONT = ZK24(ZI(LDSCON+1))
      CALL JEECRA(MACONT(1:19)//'.REFA','DOCU',IBID,'ASSE')
      NEQ = ZI(LMAT+2)
      MAT = ZK24(ZI(LMAT+1))
      CALL DISMOI('F','NOM_NUME_DDL',MAT,'MATR_ASSE',IBID,NUMEDD,IER)

      CALL JEVEUO(JEXNUM(MATR//'.VALE',1),'E',JVALE)
C ======================================================================
C --- SOUVENIRS DE L'ETAT DE CONTACT -> ON NE PEUT PAS S'EN SERVIR
C --- AU DEBUT CAR SI ON A REAPPARIE LES LIAISONS SONT DIFFERENTES
C
C NBLIAC : NOMBRE DE LIAISONS ACTIVES
C ======================================================================
      COCO = RESOCO(1:14)//'.COCO'
      CALL JEVEUO(COCO,'E',JCOCO)
      NDIM   = ZI(JCOCO  ) 
      INDIC  = ZI(JCOCO+1) 
      NBLIAC = ZI(JCOCO+2) 
      AJLIAI = ZI(JCOCO+3) 
      SPLIAI = ZI(JCOCO+4)
      LLF    = 0
      LLF1   = 0
      LLF2   = 0
C ======================================================================
C --- INITIALISATION DE VARIABLES --------------------------------------
C ======================================================================
      TYPEAJ = 'A' 
      TYPEC0 = 'C0' 
C  TOLERANCE UTILISEE POUR LE FROTTEMENT
      XTOL = 1.D-08
C ======================================================================
C                             INITIALISATIONS
C ======================================================================

C --- CREATION DE DELTA0 = C-1B
C ======================================================================
      DO 10 II = 1,NEQ
        ZR(JDELT0-1+II) = ZR(JRESU-1+II)
        ZR(JRESU-1+II) = 0.0D0
        ZR(JATMU-1+II) = 0.0D0
   10 CONTINUE
      XJVMAX = 0.0D0
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
        INDFAC   = 1 
        INDIC    = 0
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
          AJEU = ZR(JAPJEU+II-1)
          IF (AJEU.LT.0.0D0) THEN
            POSIT  = NBLIAC + 1
            CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2, 
     +                                    RESOCO,TYPEAJ,POSIT,II,TYPEC0)
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
        SPLIAI = 0
        AJLIAI = 0
        INDIC  = 0
        INDFAC = 1
C --- ACTUALISATION DES JEUX
        IF (LREAC(1)) THEN
          BTOTAL = NBLIAC
          DO 50 II = 1,NBLIAI
            AJEU = ZR(JAPJEU+II-1)
            IF (AJEU.LE.R8PREM()) THEN
              TROUAC = .FALSE.
              DO 40,JJ = 1, BTOTAL
                IF (ZI(JLIAC-1+JJ).EQ.II) THEN
                  TROUAC = .TRUE.
                END IF
   40         CONTINUE
              IF (.NOT.TROUAC) THEN
                 POSIT = NBLIAC + 1
                 CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2, 
     +                                    RESOCO,TYPEAJ,POSIT,II,TYPEC0)
              END IF
            END IF
   50     CONTINUE
        END IF
      END IF
C ======================================================================
C SI PAS DE LIAISON ACTIVE ON REMPLIT DELTA ET ON VA DIRECTEMENT AU
C CALCUL DE RHO
C ======================================================================
      NBLCIN = NBLIAC
      IF (NIV.EQ.2) WRITE (IFM,*) 'NBLIACI',NBLCIN
      GO TO 70
   60 CONTINUE
   70 CONTINUE
C ======================================================================
C RESOLUTION MATRICIELLE POUR LES LIAISONS ACTIVES
C ======================================================================
      IF (NBLIAC.NE.0) THEN
C ======================================================================
C --- CALCUL DE -A.C-1.AT COLONNE PAR COLONNE
C ======================================================================
         SPAVAN = SPLIAI
         CALL CFACAT(NDIM, INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1,
     +    LLF2, INDFAC, NESMAX,DEFICO, RESOCO, LMAT, CINE,NBLIAI,XJVMAX)
C ======================================================================
C --- ELIMINATION DES PIVOTS NULS
C ======================================================================
         CALL ELPIV1(XJVMAX, INDIC, NBLIAC, AJLIAI, SPLIAI, SPAVAN,
     +                                            NOMA, DEFICO, RESOCO)

         IF (INDIC.EQ.-1) GOTO 60
C ======================================================================
C --- FACTORISATION LDLT DE -A.C-1.AT
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
         CALL JEECRA (MACONT(1:19)//'.REFA','DOCU',IBID,'ASSE')
C ======================================================================
         IF (INDFAC.LE.NBLIAC) THEN
            CALL TLDLGG (2,LDSCON,INDFAC,NBLIAC,0,NDECI,ISINGU,
     &                                                       NPVNEG,IER)
            INDFAC = NBLIAC + 1
C--- LA MATRICE DE CONTACT EST-ELLE SINGULIERE?
            IF (IER.GT.ISTO) THEN
               CALL UTMESS ('F','FROPGD','ARRET SUR MATRICE SINGULIERE')
            ENDIF
         END IF
C ======================================================================
C --- SECOND MEMBRE : ON MET JEU(DEPTOT) - A.DELT0 DANS MU
C ======================================================================
C --- INITIALISATION DU SECOND MEMBRE ----------------------------------
C ======================================================================
        DO 120 II = 1,NDIM*NBLIAI
          ZR(JMU-1+II) = 0.D0
  120   CONTINUE
C ======================================================================
C --- APPEL DE LA ROUTINE DE CALCUL DU SECOND MEMBRE -------------------
C ======================================================================
        CALL CFADU(RESOCO, DEPDEL, NEQ, NDIM, NBLIAI, NBLIAC, LLF, 
     +                                               LLF1, LLF2, NESMAX)
C ======================================================================
C --- RESOLUTION POUR OBTENIR MU : -A.C-1.AT.MU = JEU(DEPTOT) - A.DELT0
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
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
        DO 140 II = 1,NEQ
          ZR(JDELTA-1+II) = ZR(JDELT0-1+II) - ZR(JRESU-1+II)
  140   CONTINUE
C ======================================================================
C --- CALCUL -----------------------------------------------------------
C ======================================================================
         CALL CFMAJU(RESOCO, NEQ, NDIM, NBLIAI, NBLIAC, LLF, LLF1, LLF2)
      ELSE
        DO 170 II = 1,NEQ
          ZR(JDELTA-1+II) = ZR(JDELT0-1+II)
  170   CONTINUE
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
C -- SI TOUTES LES LIAISONS SONT ACTIVES : RHO = 1
      IF (NBLIAC.EQ.NBLIAI) THEN
        RHO = 1.D0
C -- S'IL Y A DES LIAISONS NON ACTIVES : CALCUL DE RHO
      ELSE IF (NBLIAC.LT.NBLIAI) THEN
        DO 200 II = 1,NBLIAI
          TROUAC = .FALSE.
C - LA LIAISON II EST-ELLE ACTIVE ? (-> TROUAC)
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
C -  ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
              DO 190 IOTE = 1,ZI(JLIOT+4*NBLIAI)
                IF (ZI(JLIOT-1+IOTE).EQ.II) GO TO 200
  190         CONTINUE
C ======================================================================
C -  FIN ELIMINATION LIAISON A PIVOT NUL
C ======================================================================
              DELPOS = .TRUE.
              CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &                    ZR(JRESU),VAL)
              AJEU = ZR(JAPJEU+II-1) - VAL
              AJEU = AJEU/AADELT
              IF (AJEU.LT.RHO) THEN
                RHO = AJEU
                NUMIN = II
              END IF
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
C --- TESTS SUR RHO ET ACTUALISATION DE RESU
C ======================================================================
      X1 = 1.D0
      RHORHO = MIN(RHO,X1)
C ======================================================================
C -- SI RHO < 1 (AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE) :
C -- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE
C ======================================================================
      IF (RHO.LT.1.0D0) THEN
         POSIT = NBLIAC + 1 
         CALL CFTABL( INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1, LLF2, 
     +                             RESOCO, TYPEAJ, POSIT, NUMIN, TYPEC0)

        DO 210 KK = 1,NEQ
          ZR(JDELTA-1+KK) = RHORHO*ZR(JDELTA-1+KK)
  210   CONTINUE

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
          WRITE (IFM,1000) 'LE NOEUD ',NOM1,' EST ASSOCIE  ',CHAIN,NOM2
        END IF
        GO TO 60
      END IF

C ======================================================================
C ON ENLEVE TOUTES LES LIAISONS POUR LESQUELLES
C LA PRESSION EST NEGATIVE
C ======================================================================
      IF (NBLIAC.NE.0) THEN
         CALL CFNEG(RESOCO, NDIM, INDIC, NBLIAI, NBLIAC,AJLIAI,SPLIAI,
     +                                                  LLF, LLF1, LLF2)
      ENDIF
C ======================================================================
C RECUPERATION DU DEPLACEMENT FINAL
C ======================================================================
  230 CONTINUE
      DO 240 II = 1,NEQ
        ZR(JRESU-1+II) = ZR(JRESU-1+II) + ZR(JDELTA-1+II)
        ZR(JDELTA-1+II) = ZR(JDEPDE-1+II) + ZR(JDELTA-1+II)
  240 CONTINUE
C ======================================================================
C --- CALCUL DE AT.MU --------------------------------------------------
C ======================================================================
      CALL CFATMU(NEQ , NESMAX, NDIM, NBLIAC, LLF, LLF1, LLF2, RESOCO) 
C ==========================================================
C                TRAITEMENT DU FROTTEMENT
C ==========================================================

C - DETERMINATION DU PLUS GRAND ET DU PLUS PETIT GLISSEMENT
C - TANGENT
      XXMAX = 0.D0
      XXMIN = R8MAEM()

      DO 260 II = 1,NBLIAC
C - CALCUL DES GLISSEMENTS
C - ILS SONT PLACES DANS JAPJFX ET JAPJFY
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
C ========================================================
C ON REMPLIT ZR(JMU-1+3*NBLIAI+LLIAC) PAR LA RACINE DU 
C RAPPORT ENTRE KPG ET NORME DE MUg. IL DOIT RESTER 
C INFERIEUR OU EGAL A RACINE DE E_T. SI MUg EST TROP PETIT
C ON INTRODUIT UNE VALEUR QUI CONSERVE LE CONDITIONNEMENT
C DE LA MATRICE DE FROTTEMENT.
C ========================================================
      DO 270 II = 1,NBLIAC
        AJEUFX = 0.D0
        AJEUFY = 0.D0
C - ON RECUPERE DES GLISSEMENTS DE LA LIAISON
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
C - ON DETERMINE XK = MU*MUc
        XK = XK*ZR(JMU-1+II)
C - XF = SQRT( E_T )
C - XX = NORME DU GLISSEMENT TANGENT
        XF = SQRT(ZR(IPENA-1+2*LLIAC))
        XX = SQRT(AJEUFX**2+AJEUFY**2)
C - SI XX < XXMAX*XTOL (DEPLACEMENT NEGLIGEABLE)
        IF (XX.LE.XXMAX*XTOL) XX = XXMIN
        ZR(JMU-1+3*NBLIAI+LLIAC) = SQRT(XK/XX)
        IF (ZR(JMU-1+3*NBLIAI+LLIAC).GE.XF) THEN
          ZR(JMU-1+3*NBLIAI+LLIAC) = XF
        END IF
  270 CONTINUE

C ==============================================================
C       CONSTRUCTION DE LA MATRICE TANGENTE DE FROTTEMENT
C  KT = K + KF - THETA*KFR
C  K   : MATRICE DE RIGIDITE DU PROBLEME MECANIQUE SANS FROTTEMENT
C  KF  : PREMIERE MATRICE TANGENTE DE FROTTEMENT (TERME POSITIF)
C  KFR : DEUXIEME MATRICE DE FROTTEMENT (TERME NEGATIF)
C  KT  : NOUVELLE MATRICE TANGENTE DU PROBLEME
C ==============================================================

      DO 300 II = 1,NBLIAI* (NDIM-1)
        TROUAC = .TRUE.
C - NUMERO DE LA LIAISON
        IF (II.LE.NBLIAI) THEN
          LLIAC = II
        ELSE
          LLIAC = II - NBLIAI
        END IF
C - CALCUL DE CM2A (PARTIE SYMETRIQUE DE KF) POUR LES LIAISONS
C - ACTIVES AVEC INTIALISATION A ZERO DE LA COLONNE CORRESPONDANT
C - A LA LIAISON
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

C - CALCUL DE KF STOCKE DANS MAF1 = CM2AT*CM2A
      CALL ATA000(CM2A,NUMEDD,400.D0,MAF1,'V',RESOCO,NESCL)

C - CREATION DU VECTEUR DE CISAILLEMENT (PARTIE SYMETRIQUE
C - DE KFR)
C -   KF*(ZR(JDEPDE-1+NUM1) + ZR(JDELTA-1+NUM1))
C - CE VECTEUR EST REAFFECTE DANS ZR(JAFMU)

      CALL MTDSCR(MAF1)
      CALL JEVEUO(MAF1//'.&INT','E',LMAF1)
      CALL MRMULT('ZERO',LMAF1,ZR(JDELTA),'R',ZR(JAFMU),1)

C - CALCUL DE KFR STOCKE DANS CM3A SUR L ENSEMBLE DES LIAISONS
      DO 330 II = 1,NBLIAI
        TROUAC = .TRUE.
        LLIAC = II
C - ON NOTE JJC LA PLACE DE NOTRE LIAISON SI ELLE EST ACTIVE
        DO 310 JJ = 1,NBLIAC
          IF (ZI(JLIAC-1+JJ).EQ.II) THEN
            TROUAC = .FALSE.
            JJC = JJ
          END IF
  310   CONTINUE
C - INITIALISATION DE CM3A
        CALL JEVEUO(JEXNUM(CM3A,II),'E',JCM3A)
        DO 320 LL = 1,NEQ
          ZR(JCM3A-1+LL) = 0.0D0
  320   CONTINUE
C - ON EFFECTUE LE CALCUL DE CM3A SUR LES LIAISONS ACTIVES
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
C - DETERMINATION DE BETA QUI RENTRE DANS LE CALCUL
C - DU DENOMINATEUR DE KFR 
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
C - ON MULTIPIE BETA PAR COEF_MATR_FROT QUI VAUT 1
C - SI LE RESIDU EST PLUS PETIT QUE 1E-3
          IF (RESIGR.GE.1.0D-03) THEN
            XMU = SQRT(ZR(ICOMA-1+LLIAC))
            BETA = BETA*XMU
          END IF

C - ON EFFECTUE CM3A = AFMU * BETA
          CALL CALAPR(NEQ,NBDDL,BETA,ZR(JAFMU),ZI(JAPDDL+JDECAL),
     &                ZR(JCM3A))
        END IF
        CALL JELIBE(JEXNUM(CM3A,II))
  330 CONTINUE
C ======================================================================
C - ON CALCUL NOTRE NOUVELLE MATRICE DE FROTTEMENT
C ======================================================================
      CALL FROT05(CM3A,NUMEDD,MAT,MAF1,MAF2,MAFROT,RESOCO,NBLIAI)
C ======================================================================
C --- STOCKAGE DE L'ETAT DE CONTACT DEFINITIF
C ======================================================================
      IF (NIV.EQ.2) WRITE (IFM,*) 'NBLIACF',NBLIAC
      IF (NBLIAC.NE.NBLCIN) LREAC(2) = .TRUE.
      ZI(JCOCO+2) = NBLIAC
C ======================================================================
C --- CALCUL DU JEU FINAL
C ======================================================================
      DO 340 II = 1,NBLIAI
        JDECAL = ZI(JAPPTR+II-1)
        NBDDL = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
        CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &              ZR(JRESU),VAL)
        ZR(JAPJEU-1+II) = ZR(JAPJEU-1+II) - VAL
  340 CONTINUE

      IF (NIV.GE.2) THEN
        DO 350 II = 1,NBLIAI
          WRITE (IFM,1010) '<FROPGD> JEU FINAL LIAISON ',II,' : ',
     &      ZR(JAPJEU+II-1)
  350   CONTINUE
      END IF
  360 CONTINUE
C ======================================================================
C --- DESTRUCTION DES VECTEURS INUTILES
C ======================================================================
      CALL JEDEMA()
C ======================================================================
 1000 FORMAT ('<CONTACT_2> ',A9,A8,A14,A14,A8)
 1010 FORMAT ('<CONTACT_3> ',A27,I5,A3,E10.3)
C ======================================================================
      END
