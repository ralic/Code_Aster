      SUBROUTINE FROLGD (DEFICO,RESOCO,LMAT,LDSCON,NOMA,CINE,
     &           RESU,DEPTOT,ITERAT,LREAC,CONV,DEPDEL,ISTO)
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
      IMPLICIT      NONE
      LOGICAL       LREAC(4)
      INTEGER       LMAT,LDSCON,ITERAT,ISTO
      REAL*8        CONV(*)
      CHARACTER*8   NOMA
      CHARACTER*24  DEFICO,RESOCO,CINE,RESU,DEPTOT,DEPDEL
C ======================================================================
C ROUTINE APPELEE PAR : NMCONT
C ======================================================================
C
C METHODE LAGRANGIEN POUR LE CONTACT-FROTTEMENT EN 3D
C  => MULT. DE LAGRANGE POUR LE CONTACT ET LE FROTTEMENT
C
C RESOLUTION DE : C.DU + AcT.MUc + AsgT.MUsg + AgT.MUg = F
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
C
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
      LOGICAL       TROUAC,DELPOS,GLISS1,GLISS2
      INTEGER       JJC,JDEPDE,LLF,ICONT,IIKAC,LLFN,JDIM,NESMAX,LFMIN
      INTEGER       IBID,IER,IFM,NIV,NDECI,ISINGU,NPVNEG,LCOUNT,LFMIN2
      INTEGER       ICONTA,II,JJ,KK,IDEBUT,ILIAC,KCONT,NUMIN,IRET,KK2
      INTEGER       JRESU,JDEPP,JMU,JCMU,JATMU,POSMA,NDIM,NEQMAX,LFMIN0
      INTEGER       JDELT0,JDELTA,JLIAC,JVALE,JCOCO,JRCINE,JVA,KCOUNT
      INTEGER       NEQ,NESCL,NBLIAC,NBLIAI,INDIC,KKMIN,LLMIN
      INTEGER       LLIAC,LLJAC,POS1,POS2,NUM1,NUM2,JDECAL,NBDDL
      INTEGER       JAPPAR,JAPPTR,JAPCOE,JAPJEU,JAPDDL,JNOCO,JMACO
      INTEGER       NBLCIN,NLFIN,LLF1,LLF2,LLKAC,LCONT
      INTEGER       IOTE,JLIOT,LCONT0,LCONT1,LCONT2,I,J,LL,NN,IDEBUC
      INTEGER       ICOMA,ICONT1,ICONT2,KCONT1,KCONT2,LIND,IFRO,PIVOT
      INTEGER       JAPCOF,JAFMU,LMAF1,JCM1A,JCM2A,JCM2A1,JCM2A2,JCM3A
      INTEGER       LLF1IN,LLF2IN,JTEMP,LFMIN1,JGLI1,JGLI2,JADHR
      REAL*8        R8MAEM,R8PREM,AJEU,RHO,RHORHO,AADELT,AJEUFY,XF,VAL3
      REAL*8        X1,VAL,VAL1,VAL2,ZMU,XFORC,XXMIN,XXMAX,XX,XTOL1
      REAL*8        XTOL,AJEUFX,XPDT,XK,XCOMP,XQUOT,XCOS,XSIN,XXX,XMU
      REAL*8        ALPHA,BETA,RESIGR,XMUL,XVAL,VDIAGM,XJVMAX,XJVMIN
      COMPLEX*16    CBID
      CHARACTER*8   NOM1,NOM2
      CHARACTER*14  CHAIN,NUMEDD
      CHARACTER*16  NMGLI1,NMGLI2, NMADHR
      CHARACTER*19  AFMU,MAT,CM1A,CM2A,CM3A,MAF1,MAF2,MAFROT
      CHARACTER*19  LIAC,MU,ATMU,DELT0,DELTA,MATR,COCO,LIOT
      CHARACTER*24  MACONT,APJEFX,APJEFY
      CHARACTER*24  APPARI,APPOIN,APCOEF,APJEU,APDDL,COEFMU
      CHARACTER*24  NDIMCO,CONTNO,CONTMA,APCOFR,FROTE,COMAFO
C ======================================================================
C --- INITIALISATIONS DES OBJETS ET DES ADRESSES -----------------------
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
      IF (ICONTA.EQ.0) GO TO 99999
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
      CM2A     = RESOCO(1:14)//'.CM2A'
      CM3A     = RESOCO(1:14)//'.CM3A'
      MATR     = RESOCO(1:14)//'.MATR'
      MAFROT   = RESOCO(1:8)//'.MAFR'
      MAF1     = '&&FROLGD.MAF1'
      MAF2     = '&&FROLGD.MAF2'
      FROTE    = DEFICO(1:16)//'.FROTE'
      COMAFO   = DEFICO(1:16)//'.COMAFO'
      NDIMCO = DEFICO(1:16)//'.NDIMCO'
C ======================================================================
      CALL JEVEUO (CONTNO,'L',JNOCO)
      CALL JEVEUO (CONTMA,'L',JMACO)
      CALL JEVEUO (APPARI,'L',JAPPAR)
      CALL JEVEUO (APPOIN,'L',JAPPTR)
      CALL JEVEUO (APCOEF,'L',JAPCOE)
      CALL JEVEUO (APCOFR,'L',JAPCOF)
      CALL JEVEUO (APJEU, 'E',JAPJEU)
      CALL JEVEUO (APDDL, 'L',JAPDDL)
      CALL JEVEUO (LIAC,  'E',JLIAC)
      CALL JEVEUO (LIOT,  'E',JLIOT)
      CALL JEVEUO (MU,    'E',JMU)
      CALL JEVEUO (COEFMU,'L',JCMU)
      CALL JEVEUO (ATMU,  'E',JATMU)
      CALL JEVEUO (AFMU , 'E', JAFMU )
      CALL JEVEUO (DELT0, 'E',JDELT0)
      CALL JEVEUO (DELTA, 'E',JDELTA)
      CALL JEVEUO (RESU(1:19)//'.VALE'  ,'E',JRESU)
      CALL JEVEUO (DEPTOT(1:19)//'.VALE','E',JDEPP)
      CALL JEVEUO (DEPDEL(1:19)//'.VALE', 'L', JDEPDE)
      CALL JEVEUO (FROTE,'L',IFRO)
      CALL JEVEUO (COMAFO,'L',ICOMA)
      CALL JEVEUO (NDIMCO,'L',JDIM)
C ======================================================================
      NESMAX = ZI(JDIM+8)
      NBLIAI = NESCL
      RESIGR = CONV(20)
      MACONT = ZK24(ZI(LDSCON+1))
      CALL JEECRA (MACONT(1:19)//'.REFA','DOCU',IBID,'ASSE')
      NEQ = ZI(LMAT+2)
      MAT = ZK24(ZI(LMAT+1))
      CALL DISMOI ('F','NOM_NUME_DDL',MAT,'MATR_ASSE',IBID,NUMEDD,IER)
      CALL JEVEUO (JEXNUM(MATR//'.VALE',1),'E',JVALE)
C ======================================================================
C --- SOUVENIRS DE L'ETAT DE CONTACT -> ON NE PEUT PAS S'EN SERVIR
C --- AU DEBUT CAR SI ON A REAPPARIE LES LIAISONS SONT DIFFERENTES
C ======================================================================
C NBLIAC : NOMBRE DE LIAISONS ACTIVES
C LLMIN  : NUMERO DE LA LIAISON LA PLUS "VIOLEE"
C KKMIN  : NUMERO DE LA LIAISON LA PLUS "DECOLLEE"
C INDIC  : 0  INITIALISATION,
C          +1 ON A RAJOUTE UNE LIAISON (LLMIN),
C          -1 ON A ENLEVE UNE LIAISON  (KKMIN)
C ======================================================================
      COCO   = RESOCO(1:14)//'.COCO'
      CALL JEVEUO (COCO,'E',JCOCO)
      NDIM   = ZI(JCOCO)
      NBLIAC = ZI(JCOCO+1)
      LLMIN  = ZI(JCOCO+2)
      KKMIN  = ZI(JCOCO+3)
      INDIC  = ZI(JCOCO+4)
      LLF    = ZI(JCOCO+6)
      LLF1   = ZI(JCOCO+7)
      LLF2   = ZI(JCOCO+8)
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
      DO 1 II = 1, NEQ
         ZR(JDELT0-1+II) = ZR(JRESU-1+II)
         ZR(JRESU -1+II) = 0.0D0
 1    CONTINUE
C ======================================================================
C --- DETECTION DES COUPLES DE NOEUDS INTERPENETRES --------------------
C --- ON CALCULE LE NOUVEAU JEU : AJEU+ = AJEU/I/N - A.DDEPLA ----------
C --- (IL EST NEGATIF LORSQU'IL Y A INTERPENETRATION -> LIAISON ACTIVE)-
C ======================================================================
      IF (ITERAT.EQ.0) THEN
         NBLIAC = 0
         LLF    = 0
         LLF1   = 0
         LLF2   = 0
C ======================================================================
C --- INITIALISATION POUR LA PREMIERE ITERATION ------------------------
C ======================================================================
         IF ( INDIC .EQ. 0 ) THEN
            CALL FROT07 ( LMAT, VDIAGM )
            ZR(JMU+6*NBLIAI-1) = VDIAGM ** 0.25D0
         ENDIF
C ======================================================================
C --- INITIALISATION POUR L'ELIMINATION DES PIVOTS NULS ----------------
C ======================================================================
         ZI(JLIOT+4*NBLIAI  )  = 0
         ZI(JLIOT+4*NBLIAI+1)  = 0
         ZI(JLIOT+4*NBLIAI+2)  = 0
         ZI(JLIOT+4*NBLIAI+3)  = 0
         DO 4 II = 1,NBLIAI
C ======================================================================
C --- INITIALISATION DES DIFFERENTS MU (AU NIVEAU DES LIAISONS DE ------
C --- CONTACT, ADHERENTES ET GLISSANTES) -------------------------------
C ======================================================================
            ZR(JMU-1+2*NBLIAI+II) = 0.D0
            ZR(JMU-1+  NBLIAI+II) = 0.D0
            ZR(JMU-1+         II) = 0.D0 
            AJEU = ZR(JAPJEU+II-1)
            IF (AJEU.LT.0.0D0) THEN
               INDIC = 0
               NBLIAC = NBLIAC + 1
               ZI(JLIAC-1+NBLIAC) = II
               IF (NIV.GE.2) THEN
C ======================================================================
C --- IMPRESSION DES RESULTATS -----------------------------------------
C ======================================================================
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
         DO 7 II = 1, NBLIAC
            LLIAC = ZI(JLIAC-1+II)
            LLF = LLF + 1
            ZI(JLIAC-1+NBLIAC+LLF)= LLIAC
 7       CONTINUE
C ======================================================================
      ELSE
C ======================================================================
C --- TRAITEMENT DU CONTACT APRES REACTUALISATION GEOMETRIQUE ----------
C ======================================================================
         IF(LREAC(1)) THEN
            DO 5 II = 1,NBLIAI
               AJEU = ZR(JAPJEU+II-1)
               IF (AJEU.LE.R8PREM()) THEN
                  TROUAC = .FALSE.
                  DO 940, JJ=1,NBLIAC
                     IF(ZI(JLIAC-1+JJ).EQ.II) THEN
                        TROUAC = .TRUE.
                     ENDIF
 940              CONTINUE
                  IF(.NOT.TROUAC) THEN
                     NBLIAC = NBLIAC + 1
                     DO 211 JJ=0,LLF+LLF1+LLF2-1
                        ZI(JLIAC-1+NBLIAC+LLF+LLF1+LLF2-JJ) =
     &                             ZI(JLIAC-1+NBLIAC+LLF+LLF1+LLF2-JJ-1)
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
      TROUAC = .TRUE.
      NBLCIN = NBLIAC
      NLFIN  = LLF
      LFMIN0 = 0
      IF ( NIV .EQ. 2 ) WRITE(IFM,*)'NBLIACI',NBLCIN,'LLFI',NLFIN
      XMUL = ZR(JMU+6*NBLIAI-1)
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
C ======================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
         LLMIN=0
         KKMIN=0
         IDEBUT = 1
         XJVMAX = 0.D0
         XJVMIN = 1.D0/R8PREM()
         IDEBUC = 1
C ======================================================================
C --- CALCUL DE CHAQUE COLONNE DE AT (UNE PAR LIAISON ACTIVE) ----------
C ======================================================================
         DO 20 ILIAC = IDEBUT,NBLIAC+(NDIM-1)*LLF+LLF1+LLF2
C ======================================================================
C --- CAS D'UNE LIAISON ADHERENTE --------------------------------------
C ======================================================================
            IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF+LLF1)) THEN
               LLIAC = ZI(JLIAC+ILIAC-1-LLF)
               CALL JEVEUO(JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI),'E',JCM1A)
            ELSE IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF)) THEN
               LLIAC = ZI(JLIAC+ILIAC-1-LLF)
               CALL JEVEUO(JEXNUM(CM1A,LLIAC+NBLIAI),'E',JCM1A)
            ELSE IF (ILIAC.GT.(NBLIAC+LLF)) THEN
               LLIAC = ZI(JLIAC+ILIAC-1-LLF)
               CALL JEVEUO(JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI),'E',JCM1A)
            ELSE IF (ILIAC.GT.NBLIAC) THEN
               LLIAC = ZI(JLIAC+ILIAC-1)
               CALL JEVEUO(JEXNUM(CM1A,LLIAC+NBLIAI),'E',JCM1A)
            ELSE
C ======================================================================
C --- CAS D'UNE LIAISON DE CONTACT -------------------------------------
C ======================================================================
               LLIAC = ZI(JLIAC+ILIAC-1)
               CALL JEVEUO(JEXNUM(CM1A,LLIAC),'E',JCM1A)
            ENDIF
C ======================================================================
C --- INITIALISATION DU VECTEUR CM1A -----------------------------------
C ======================================================================
            DO 206 KK = 1, NEQ
               ZR(JCM1A-1+KK) = 0.0D0
 206        CONTINUE
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
C ======================================================================
C --- CAS D'UNE LIAISON ADHERENTE --------------------------------------
C ======================================================================
            IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF+LLF1)) THEN
               CALL CALATM (NEQ,NBDDL,1.D0,ZR(JAPCOF+JDECAL+30*NESMAX),
     +                                     ZI(JAPDDL+JDECAL),ZR(JCM1A))
            ELSE IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF)) THEN
               CALL CALATM (NEQ,NBDDL,1.D0,ZR(JAPCOF+JDECAL),
     +                                     ZI(JAPDDL+JDECAL),ZR(JCM1A))
            ELSE IF (ILIAC.GT.(NBLIAC+LLF)) THEN
               CALL CALATM (NEQ,NBDDL,1.D0,ZR(JAPCOF+JDECAL+30*NESMAX),
     +                                     ZI(JAPDDL+JDECAL),ZR(JCM1A))
            ELSE IF (ILIAC.GT.NBLIAC) THEN
               CALL CALATM (NEQ,NBDDL,1.D0,ZR(JAPCOF+JDECAL),
     +                                     ZI(JAPDDL+JDECAL),ZR(JCM1A))
            ELSE
C ======================================================================
C --- CAS D'UNE LIAISON DE CONTACT -------------------------------------
C ======================================================================
               CALL CALATM (NEQ,NBDDL,1.D0,ZR(JAPCOE+JDECAL),
     +                                     ZI(JAPDDL+JDECAL),ZR(JCM1A))
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
               IF (JJ.GT.(NBLIAC+(NDIM-1)*LLF+LLF1)) THEN
                  LLJAC = ZI(JLIAC-1+JJ-LLF)
               ELSE IF (JJ.GT.(NBLIAC+(NDIM-1)*LLF)) THEN
                  LLJAC = ZI(JLIAC-1+JJ-LLF)
               ELSE IF (JJ.GT.(NBLIAC+LLF)) THEN
                  LLJAC = ZI(JLIAC-1+JJ-LLF)
               ELSE IF (JJ.GT.NBLIAC) THEN
                  LLJAC = ZI(JLIAC-1+JJ)
               ELSE
                  LLJAC = ZI(JLIAC-1+JJ)
               ENDIF
               JVA = JVALE-1+(ILIAC-1)*ILIAC/2+JJ
               ZR(JVA) = 0.0D0
               JDECAL = ZI(JAPPTR+LLJAC-1)
               NBDDL = ZI(JAPPTR+LLJAC) - ZI(JAPPTR+LLJAC-1)
C ======================================================================
C --- CAS D'UNE LIAISON ADHERENTE --------------------------------------
C ======================================================================
               IF (JJ.GT.(NBLIAC+(NDIM-1)*LLF+LLF1)) THEN
                  CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     +                                ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ELSE IF (JJ.GT.(NBLIAC+(NDIM-1)*LLF)) THEN
                  CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     +                                ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ELSE IF (JJ.GT.(NBLIAC+LLF)) THEN
                  CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     +                                ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ELSE IF (JJ.GT.NBLIAC) THEN
                  CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     +                                ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ELSE
C ======================================================================
C --- CAS D'UNE LIAISON DE CONTACT -------------------------------------
C ======================================================================
                  CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     +                                ZI(JAPDDL+JDECAL),ZR(JCM1A),VAL)
               ENDIF
               ZR(JVA) = ZR(JVA) - VAL
               IF(ABS(ZR(JVA)).GT.XJVMAX) XJVMAX = ABS(ZR(JVA))
               IF(ABS(ZR(JVA)).LT.XJVMIN) XJVMIN = ABS(ZR(JVA))
 23         CONTINUE
C     
            IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF+LLF1)) THEN
               CALL JELIBE(JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI))
            ELSE IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF)) THEN
               CALL JELIBE(JEXNUM(CM1A,LLIAC+NBLIAI))
            ELSE IF (ILIAC.GT.(NBLIAC+LLF)) THEN
               CALL JELIBE(JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI))
            ELSE IF (ILIAC.GT.NBLIAC) THEN
               CALL JELIBE(JEXNUM(CM1A,LLIAC+NBLIAI))
            ELSE
               CALL JELIBE(JEXNUM(CM1A,LLIAC))
            ENDIF
C
 20      CONTINUE
C ======================================================================
C --- ELIMINATION DES PIVOTS NULS --------------------------------------
C ======================================================================
         CALL ELPIV2(XJVMAX, MATR, NOMA, DEFICO, RESOCO, IDEBUC, LLF,
     +                    LLF1,LLF2,NBLIAC, NDIM, KKMIN, PIVOT, INDIC)
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
C ======================================================================
         IF (IDEBUT.LE.NBLIAC+(NDIM-1)*LLF+LLF1+LLF2) THEN
           CALL TLDLGG (2,LDSCON,IDEBUT,NBLIAC+(NDIM-1)*LLF+LLF1+LLF2,
     &     0,NDECI,ISINGU,NPVNEG,IER)
C ======================================================================
C--- LA MATRICE DE CONTACT EST-ELLE SINGULIERE?
C ======================================================================
            IF (IER.GT.ISTO) THEN
               CALL UTMESS ('F','FROLGD','ARRET SUR MATRICE SINGULIERE')
            ENDIF
         END IF
C ======================================================================
C --- SECOND MEMBRE : ON MET JEU(DEPTOT) - A.DELT0 DANS MU
C ======================================================================
         DO 25 II = 1,NDIM*NBLIAI
            ZR(JMU-1+II)= 0.D0
 25      CONTINUE
         DO 30 ILIAC = 1, NBLIAC+(NDIM-1)*LLF+LLF1+LLF2
            IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF+LLF1)) THEN
               LLIAC = ZI(JLIAC-1+ILIAC-LLF)
            ELSE IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF)) THEN
               LLIAC = ZI(JLIAC-1+ILIAC-LLF)
            ELSE IF (ILIAC.GT.(NBLIAC+LLF)) THEN
               LLIAC = ZI(JLIAC-1+ILIAC-LLF)
            ELSE IF (ILIAC.GT.NBLIAC) THEN
               LLIAC = ZI(JLIAC-1+ILIAC)
            ELSE
               LLIAC = ZI(JLIAC-1+ILIAC)
            ENDIF
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
C ======================================================================
C --- CALCUL DE MU_SG DEPUIS LE DEBUT DU PAS DE TEMPS ------------------
C ======================================================================
            IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF+LLF1)) THEN
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     +                                ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     +                                ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
               VAL = VAL1 + VAL2
            ELSE IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF)) THEN
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     +                                ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     +                                ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
               VAL = VAL1 + VAL2
            ELSE IF (ILIAC.GT.(NBLIAC+LLF)) THEN
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     +                                ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     +                                ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
               VAL = VAL1 + VAL2
            ELSE IF (ILIAC.GT.NBLIAC) THEN
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     +                                ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     +                                ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
               VAL = VAL1 + VAL2
            ELSE
C ======================================================================
C --- CALCUL DE MU_C ---------------------------------------------------
C ======================================================================
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                                 ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL)
               ZR(JMU+ILIAC-1) = ZR(JAPJEU+LLIAC-1)
            ENDIF
            ZR(JMU+ILIAC-1) = ZR(JMU+ILIAC-1) - VAL
 30      CONTINUE
C ======================================================================
C --- RESOLUTION POUR OBTENIR MU : -A.C-1.AT.MU = JEU(DEPTOT) - A.DELT0
C ======================================================================
C ON TRUANDE LA SD MATR_ASSE POUR NE RESOUDRE LE SYSTEME QUE -----------
C DE 1 A NBLIAC : ------------------------------------------------------
C ======================================================================
         NEQMAX= ZI(LDSCON+2)
         ZI(LDSCON+2) = NBLIAC+(NDIM-1)*LLF+LLF1+LLF2
         CALL RLDLGG (LDSCON,ZR(JMU),CBID,1)
         ZI(LDSCON+2) = NEQMAX
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU ------------------------------
C ======================================================================
         DO 41 ILIAC = 1,NBLIAC+(NDIM-1)*LLF+LLF1+LLF2
            IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF+LLF1)) THEN
              LLIAC = ZI(JLIAC-1+ILIAC-LLF)
              CALL JEVEUO (JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI),'L',JCM1A)
              DO 46 KK=1,NEQ
                 ZR(JDELTA-1+KK) = ZR(JDELTA-1+KK) -
     +                                    ZR(JCM1A-1+KK)*ZR(JMU-1+ILIAC)
 46           CONTINUE
              CALL JELIBE (JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI))
            ELSE IF (ILIAC.GT.(NBLIAC+(NDIM-1)*LLF)) THEN
              LLIAC=ZI(JLIAC-1+ILIAC-LLF)
              CALL JEVEUO(JEXNUM(CM1A,LLIAC+NBLIAI),'L',JCM1A)
              DO 45 KK=1,NEQ
                 ZR(JDELTA-1+KK) = ZR(JDELTA-1+KK) -
     &                                    ZR(JCM1A-1+KK)*ZR(JMU-1+ILIAC)
 45           CONTINUE
              CALL JELIBE (JEXNUM(CM1A,LLIAC+NBLIAI))
            ELSE IF (ILIAC.GT.(NBLIAC+LLF)) THEN
              LLIAC = ZI(JLIAC-1+ILIAC-LLF)
              CALL JEVEUO (JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI),'L',JCM1A)
              DO 42 KK = 1,NEQ
                 ZR(JDELTA-1+KK) = ZR(JDELTA-1+KK) -
     &                                    ZR(JCM1A-1+KK)*ZR(JMU-1+ILIAC)
 42           CONTINUE
              CALL JELIBE (JEXNUM(CM1A,LLIAC+(NDIM-1)*NBLIAI))
            ELSE IF (ILIAC.GT.NBLIAC) THEN
              LLIAC = ZI(JLIAC-1+ILIAC)
              CALL JEVEUO (JEXNUM(CM1A,LLIAC+NBLIAI),'L',JCM1A)
              DO 43 KK = 1,NEQ
                 ZR(JDELTA-1+KK) = ZR(JDELTA-1+KK) -
     &                                   ZR(JCM1A-1+KK)*ZR(JMU-1+ILIAC)
 43           CONTINUE
              CALL JELIBE (JEXNUM(CM1A,LLIAC+NBLIAI))
            ELSE
              LLIAC = ZI(JLIAC-1+ILIAC)
              CALL JEVEUO (JEXNUM(CM1A,LLIAC),'L',JCM1A)
              DO 44 KK = 1,NEQ
                 ZR(JDELTA-1+KK) = ZR(JDELTA-1+KK) -
     &                                   ZR(JCM1A-1+KK)*ZR(JMU-1+ILIAC)
 44           CONTINUE
              CALL JELIBE (JEXNUM(CM1A,LLIAC))
            ENDIF
 41      CONTINUE
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
         DO 124 II = 0,LLF+LLF1+LLF2-1
            ZI(JLIAC-1+NBLIAC+LLF+LLF1+LLF2-II) =
     &                             ZI(JLIAC-1+NBLIAC+LLF+LLF1+LLF2-II-1)
 124     CONTINUE
         ZI(JLIAC-1+NBLIAC) = NUMIN
         IF (ITERAT.EQ.0) THEN
            LLF = LLF + 1
            DO 126 II = 0,LLF1+LLF2-1
               ZI(JLIAC-1+NBLIAC+LLF+LLF1+LLF2-II) =
     &                             ZI(JLIAC-1+NBLIAC+LLF+LLF1+LLF2-II-1)
 126        CONTINUE
            ZI(JLIAC-1+NBLIAC+LLF) = NUMIN
         ENDIF
C ======================================================================
C --- LA LIAISON EST SUPPOSEE GLISSANTE
C ======================================================================
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
C --- RECUPERATION DU DEPLACEMENT FINAL --------------------------------
C ======================================================================
      DO 150 II = 1,NEQ
          ZR(JRESU -1+II) = ZR(JRESU-1+II) + ZR(JDELTA-1+II)
          ZR(JDELTA-1+II) = ZR(JRESU-1+II) + ZR(JDEPDE-1+II)
 150  CONTINUE
C ======================================================================
C --- ON ENLEVE TOUTES LES LIAISONS DE FROTTEMENT POUR LESQUELLES ------
C --- LA PRESSION EST NEGATIVE -----------------------------------------
C ======================================================================
C --- CAS LLF ----------------------------------------------------------
C ======================================================================
         LCONT0 = 0
         DO 85 JJ = 1, LLF
            LLJAC = ZI(JLIAC-1+NBLIAC+JJ)
            DO 86 II = 1,NBLIAC
               LLIAC = ZI(JLIAC-1+II)
               IF ( LLJAC .EQ. LLIAC ) THEN
                  IF ( ZR(JMU-1+II) .LT. 0.0D0 ) THEN
                     LCONT0 = LCONT0 + 1
                  ELSE
                     IF ( (JJ-LCONT0.GT.0) ) THEN
                        ZI(JLIAC-1+NBLIAC+JJ-LCONT0) = 
     +                                             ZI(JLIAC-1+NBLIAC+JJ)
                        ZR(JMU  -1+NBLIAC+JJ-LCONT0) = 
     +                                             ZR(JMU  -1+NBLIAC+JJ)
                        ZR(JMU  -1+NBLIAC+LLF+JJ-LCONT0) = 
     +                                         ZR(JMU  -1+NBLIAC+LLF+JJ)
                     ENDIF
                  ENDIF
               ENDIF
 86         CONTINUE
 85      CONTINUE
         DO 112 JJ = 1, LLF-LCONT0
            ZR(JMU-1+NBLIAC+LLF-LCONT0+JJ) = ZR(JMU-1+NBLIAC+LLF+JJ)
 112     CONTINUE
C ======================================================================
C --- CAS LLF1 ---------------------------------------------------------
C ======================================================================
         LCONT1 = 0
         DO 87 JJ = 1, LLF1
            LLJAC = ZI(JLIAC-1+NBLIAC+LLF+JJ)
            DO 88 II = 1,NBLIAC
               LLIAC = ZI(JLIAC-1+II)
               IF ( LLJAC .EQ. LLIAC ) THEN
                  IF ( ZR(JMU-1+II) .LT. 0.0D0 ) THEN
                     LCONT1 = LCONT1 + 1
                  ELSE
                     IF ( (JJ-LCONT1.GT.0) ) THEN
                        ZI(JLIAC-1+NBLIAC+LLF-LCONT0+JJ-LCONT1) = 
     +                                         ZI(JLIAC-1+NBLIAC+LLF+JJ)
                        ZR(JMU  -1+NBLIAC+2*LLF-2*LCONT0+JJ-LCONT1) = 
     +                                       ZR(JMU  -1+NBLIAC+2*LLF+JJ)
                     ENDIF
                  ENDIF
               ENDIF
 88         CONTINUE
 87      CONTINUE
C ======================================================================
C --- CAS LLF2 ---------------------------------------------------------
C ======================================================================
         LCONT2 = 0
         DO 89 JJ = 1, LLF2
            LLJAC = ZI(JLIAC-1+NBLIAC+LLF+LLF1+JJ)
            DO 94 II = 1,NBLIAC
               LLIAC = ZI(JLIAC-1+II)
               IF ( LLJAC .EQ. LLIAC ) THEN
                  IF ( ZR(JMU-1+II) .LT. 0.0D0 ) THEN
                     LCONT2 = LCONT2 + 1
                  ELSE
                     IF ( (JJ-LCONT2.GT.0) ) THEN
                 ZI(JLIAC-1+NBLIAC+LLF-LCONT0+LLF1-LCONT1+JJ-LCONT2) = 
     +                                   ZI(JLIAC-1+NBLIAC+LLF+LLF1+JJ)
               ZR(JMU-1+NBLIAC+2*LLF-2*LCONT0+LLF1-LCONT1+JJ-LCONT2) = 
     +                                   ZR(JMU-1+NBLIAC+2*LLF+LLF1+JJ)
                     ENDIF
                  ENDIF
               ENDIF
 94         CONTINUE
 89      CONTINUE
C ======================================================================
C --- NOMBRE FINAL DE LIAISONS ADHERENTES ------------------------------
C ======================================================================
      LLF  = LLF  - LCONT0
      LLF1 = LLF1 - LCONT1
      LLF2 = LLF2 - LCONT2
C ======================================================================
C --- ON ENLEVE TOUTES LES LIAISONS DE CONTACT POUR LESQUELLES ---------
C --- LA PRESSION EST NEGATIVE -----------------------------------------
C ======================================================================
      KCOUNT = 0
      DO 130 II = 1,NBLIAC
         LLIAC = ZI(JLIAC-1+II)
         IF(ZR(JMU-1+II).LT.0.0D0) THEN
            KCOUNT = KCOUNT + 1
         ELSE
            IF(II-KCOUNT.GT.0) THEN
               ZI(JLIAC-1+II-KCOUNT) = ZI(JLIAC-1+II)
               ZR(JMU  -1+II-KCOUNT) = ZR(JMU  -1+II)
            ENDIF
         ENDIF
 130  CONTINUE
C ======================================================================
C --- AJUSTEMENT DES LIAISONS DE FROTTEMENTS ---------------------------
C ======================================================================
      DO 132 II = 1,LLF+LLF1+LLF2
         ZI(JLIAC-1+II+NBLIAC-KCOUNT) = ZI(JLIAC-1+II+NBLIAC)
 132  CONTINUE
      DO 134 II = 1,2*LLF+LLF1+LLF2
         ZR(JMU  -1+II+NBLIAC-KCOUNT) = ZR(JMU  -1+II+NBLIAC)
 134  CONTINUE
      NBLIAC = NBLIAC - KCOUNT
C ======================================================================
C --- LES LIAISONS CONSIDEREES GLISSANTES LE SONT-ELLES VRAIMENT ? -----
C ======================================================================
         KCONT  = 0
         KCONT1 = 0
         KCONT2 = 0
         IF ( LLF .NE. 0 ) THEN
            DO 78 JJ = NBLIAC+1,NBLIAC+LLF
               LLJAC = ZI(JLIAC-1+JJ)
               XK    = ZR(IFRO-1+LLJAC)
               XCOMP = SQRT( ZR(JMU-1+JJ)**2 +
     &                                       ZR(JMU-1+JJ+LLF)**2 )
               DO 79 KK = 1,NBLIAC
                  LLKAC = ZI(JLIAC-1+KK)
                  IF ( LLKAC .EQ. LLJAC ) THEN
                     XQUOT = 0.0D0
                     IF ( ZR(JMU-1+KK) .GT. 0.0D0 ) THEN
                        XQUOT = XCOMP/ZR(JMU-1+KK)
                     ENDIF
                     IF ( ABS(XQUOT) .GE. XK ) THEN
                        KCONT = KCONT + 1
                     ELSE
C ======================================================================
C --- DECALAGE DES INDICES DES LIAISONS EN CONTACT ---------------------
C ======================================================================
                        IF (JJ-NBLIAC-KCONT.GT.0) THEN
                           ZI(JLIAC-1-KCONT+JJ)     = LLJAC
                           ZR(JMU  -1-KCONT+JJ)     = ZR(JMU-1+JJ)
                           ZR(JMU  -1-KCONT+JJ+LLF) = ZR(JMU-1+JJ+LLF)
                        ENDIF
                     ENDIF
                  ENDIF
 79            CONTINUE
 78         CONTINUE
         ENDIF
         IF ( LLF1 .NE. 0 ) THEN
            DO 98 JJ = NBLIAC+LLF+1,NBLIAC+LLF+LLF1
               LLJAC = ZI(JLIAC-1+JJ)
               XK    = ZR(IFRO-1+LLJAC)
               XCOMP = ABS( ZR(JMU-1+LLF+JJ) )
               DO 99 KK = 1,NBLIAC
                  LLKAC = ZI(JLIAC-1+KK)
                  IF ( LLKAC .EQ. LLJAC ) THEN
                     XQUOT = 0.D00
                     IF ( ZR(JMU-1+KK) .GT. 0.0D0 ) THEN
                        XQUOT = XCOMP/ZR(JMU-1+KK)
                     ENDIF
                     IF ( ABS(XQUOT) .GE. XK ) THEN
                        KCONT1 = KCONT1 + 1
                     ELSE
C ======================================================================
C --- DECALAGE DES INDICES DES LIAISONS EN CONTACT ---------------------
C ======================================================================
                        IF (JJ-NBLIAC-LLF-KCONT1.GT.0) THEN
                           ZI(JLIAC-1-KCONT1+JJ)     = LLJAC
                           ZR(JMU  -1-KCONT1+LLF+JJ) = ZR(JMU-1+LLF+JJ)
                        ENDIF
                     ENDIF
                  ENDIF
 99            CONTINUE
 98         CONTINUE
         ENDIF
C ======================================================================
C --- LES LIAISONS CONSIDEREES ADHERENTES (SUIVANT LA 2NDE DIRECTION) --
C --- LE SONT-ELLES VRAIMENT ? -----------------------------------------
C ======================================================================
         IF ( LLF2 .NE. 0 ) THEN
            DO 118 JJ = NBLIAC+LLF+LLF1+1, NBLIAC+LLF+LLF1+LLF2
               LLJAC = ZI(JLIAC-1+JJ)
               XK    = ZR(IFRO-1+LLJAC)
               XCOMP = ABS( ZR(JMU-1+LLF+JJ) )
               DO 119 KK = 1,NBLIAC
                  LLKAC = ZI(JLIAC-1+KK)
                  IF ( LLKAC .EQ. LLJAC ) THEN
                     XQUOT = 0.D00
                     IF ( ZR(JMU-1+KK) .GT. 0.0D0 ) THEN
                        XQUOT = XCOMP/ZR(JMU-1+KK)
                     ENDIF
                     IF ( ABS(XQUOT) .GE. XK ) THEN
                        KCONT2 = KCONT2 + 1
                     ELSE
C ======================================================================
C --- DECALAGE DES INDICES DES LIAISONS EN CONTACT ---------------------
C ======================================================================
                        IF (JJ-NBLIAC-LLF-LLF1-KCONT2.GT.0) THEN
                           ZI(JLIAC-1-KCONT2+JJ)     = LLJAC
                           ZR(JMU  -1-KCONT2+LLF+JJ) = ZR(JMU-1+LLF+JJ)
                        ENDIF
                     ENDIF
                  ENDIF
 119            CONTINUE
 118         CONTINUE
         ENDIF
C ======================================================================
C --- STOCKAGE DES NOUVELLES LIAISONS ACTIVES (PAS DE GLISSEMENT -------
C --- POUR CES LIAISONS ) ----------------------------------------------
C ======================================================================
        DO 64 II = 1,LLF - KCONT
            ZR(JMU-1+NBLIAC+LLF-KCONT+II) = ZR(JMU-1+NBLIAC+LLF+II)
 64     CONTINUE
         DO 90 II = 1,LLF1 - KCONT1
            ZI(JLIAC-1+NBLIAC+LLF-KCONT+II)   =
     &                                         ZI(JLIAC-1+NBLIAC+LLF+II)
            ZR(JMU-1+NBLIAC+2*LLF-2*KCONT+II) =
     &                                         ZR(JMU-1+NBLIAC+2*LLF+II)
 90      CONTINUE
         DO 92 II = 1, LLF2 - KCONT2
            ZI(JLIAC-1+NBLIAC+LLF-KCONT+LLF1-KCONT1+II)   =
     &                                    ZI(JLIAC-1+NBLIAC+LLF+LLF1+II)
            ZR(JMU-1+NBLIAC+2*LLF-2*KCONT+LLF1-KCONT1+II) =
     &                                    ZR(JMU-1+NBLIAC+2*LLF+LLF1+II)
 92      CONTINUE
         LLF  = LLF  - KCONT
         LLF1 = LLF1 - KCONT1
         LLF2 = LLF2 - KCONT2
C ======================================================================
C --- LES LIAISONS CONSIDEREES GLISSANTES LE SONT-ELLES VRAIMENT ? -----
C ======================================================================
      LFMIN  = 0
      LFMIN1 = 0
      LFMIN2 = 0
      IF ((LLF+LLF1+LLF2).LT.NBLIAC) THEN
         NMGLI1 = '&&FROLGD.GLI1'
         NMGLI2 = '&&FROLGD.GLI2'
         NMADHR = '&&FROLGD.ADHR'
         CALL WKVECT ( NMGLI1, 'V V I', NBLIAC, JGLI1 )
         CALL WKVECT ( NMGLI2, 'V V I', NBLIAC, JGLI2 )
         CALL WKVECT ( NMADHR, 'V V I', NBLIAC, JADHR )
         DO 156 II = 1,NBLIAC
            AJEUFX = 0.0D0
            AJEUFY = 0.0D0
            LLIAC  = ZI(JLIAC-1+II)
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
C ======================================================================
C --- CALCUL DU JEU SUIVANT LA PREMIERE DIRECTION ----------------------
C ======================================================================
            GLISS1 = .FALSE.
            DO 1782 KK = 1,ZI(JLIOT+4*NBLIAI+2)
               IF (ZI(JLIOT-1+2*NBLIAI+KK).EQ.LLIAC) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
                  GLISS1 = .TRUE.
               ENDIF
 1782        CONTINUE
            IF (.NOT.GLISS1) THEN
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
               AJEUFX = VAL
            ENDIF
C ======================================================================
C --- CALCUL DU JEU SUIVANT LA SECONDE DIRECTION -----------------------
C ======================================================================
            GLISS2 = .FALSE.
            DO 1784 KK = 1,ZI(JLIOT+4*NBLIAI+3)
               IF (ZI(JLIOT-1+3*NBLIAI+KK).EQ.LLIAC) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
                  GLISS2 = .TRUE.
               ENDIF
 1784        CONTINUE
            IF (.NOT.GLISS2) THEN
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
               AJEUFY = VAL
            ENDIF
C ======================================================================
C --- CALCUL DE KGP ----------------------------------------------------
C ======================================================================
            XK = ZR(IFRO-1+LLIAC)
            IF ( ZR(JMU-1+II) .GT. 0.0D0 ) THEN
               XK = XK*ZR(JMU-1+II)
            ELSE
               XK = 0.D0
            ENDIF
            XX = SQRT( AJEUFX**2 + AJEUFY**2 )
            IF ( (XX.GT.R8PREM()).AND.(XX .LT. (XK/XMUL**2)) ) THEN
C ======================================================================
C --- LA LIAISON EST CONSIDEREE ADHERENTE ET EST TRAITEE PAR -----------
C --- MULTIPLICATEUR DE LAGRANGE ---------------------------------------
C ======================================================================
               TROUAC = .FALSE.
               DO 157 KK = 1,LLF+LLF1+LLF2
                  IF (ZI(JLIAC-1+NBLIAC+KK).EQ.LLIAC) THEN
C ======================================================================
C --- LA LIAISON EST DEJA ADHERENTE ------------------------------------
C ======================================================================
                     TROUAC = .TRUE.
                  ENDIF
 157           CONTINUE
               DO 160 KK = 1,ZI(JLIOT+4*NBLIAI+1)
                  IF (ZI(JLIOT-1+NBLIAI+KK).EQ.LLIAC) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LES DEUX DIRECTIONS -----------
C ======================================================================
                     TROUAC = .TRUE.
                  ENDIF
 160           CONTINUE
               IF (.NOT.TROUAC) THEN
                  DO 176 KK = 1,ZI(JLIOT+4*NBLIAI+2)
                     IF (ZI(JLIOT-1+2*NBLIAI+KK).EQ.LLIAC) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
                        LFMIN2 = LFMIN2 + 1
                        ZI(JGLI2-1+LFMIN2) = LLIAC
                        GO TO 156
                     ENDIF
 176              CONTINUE
                  DO 177 KK = 1,ZI(JLIOT+4*NBLIAI+3)
                     IF (ZI(JLIOT-1+3*NBLIAI+KK).EQ.LLIAC) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
                        LFMIN1 = LFMIN1 + 1
                        ZI(JGLI1-1+LFMIN1) = LLIAC
                        GO TO 156
                     ENDIF
 177              CONTINUE
                  LFMIN = LFMIN + 1
                  ZI(JADHR-1+LFMIN) = LLIAC
               ENDIF
            ENDIF
 156     CONTINUE
         IF ((LFMIN.NE.0).OR.(LFMIN1.NE.0).OR.(LFMIN2.NE.0))  THEN
C ======================================================================
C --- S'IL EXISTE AU MOINS UNE LIAISON ADHERENTE SUPPLEMENTAIRE, -------
C --- ON NE PREND PAS EN COMPTE LES INCREMENTS DE DEPLACEMENTS CALCULES-
C --- ET ON RECOMMENCE LES CALCULS (NOTAMMENT POUR MU_SG) --------------
C ======================================================================
            DO 197 LL=1,LFMIN2
               ZI(JLIAC-1+NBLIAC+LLF+LLF1+LLF2+LL) = ZI(JGLI2-1+LL)
 197        CONTINUE
            DO 198 LL=0,LLF2+LFMIN2-1
               ZI(JLIAC-1+NBLIAC+LLF+LFMIN+LLF1+LFMIN1+LLF2+LFMIN2-LL)=
     &                       ZI(JLIAC-1+NBLIAC+LLF+LLF1+LLF2+LFMIN2-LL)
 198        CONTINUE
            LLF2 = LLF2 + LFMIN2
            DO 199 LL=1,LFMIN1
               ZI(JLIAC-1+NBLIAC+LLF+LLF1+LL) = ZI(JGLI1-1+LL)
 199        CONTINUE
            DO 2090 LL=0,LLF1+LFMIN1+LLF2-1
               ZI(JLIAC-1+NBLIAC+LLF+LFMIN+LLF1+LFMIN1+LLF2-LL)=
     &                       ZI(JLIAC-1+NBLIAC+LLF+LLF1+LFMIN1+LLF2-LL)
 2090        CONTINUE
            LLF1 = LLF1 + LFMIN1
            DO 201 LL=1,LFMIN
               ZI(JLIAC-1+NBLIAC+LLF+LL) = ZI(JADHR-1+LL)
 201        CONTINUE
            LLF    = LLF  + LFMIN
            LFMIN0 = 1
            CALL JEDETR(NMGLI1)
            CALL JEDETR(NMGLI2)
            CALL JEDETR(NMADHR)
            XVAL = XMUL**2
            IF(LFMIN0.NE.0) THEN
               IF(XVAL.LT.(1.0D0/R8PREM())) XMUL = XMUL*SQRT(10.D0)
            ENDIF
            ZR(JMU+6*NBLIAI-1) = XMUL
            GO TO 100
         ENDIF
         CALL JEDETR(NMGLI1)
         CALL JEDETR(NMGLI2)
         CALL JEDETR(NMADHR)
        ENDIF
C ======================================================================
C --- INITIALISATION DE ATMU ET DE AFMU --------------------------------
C ======================================================================
      DO 110 II = 1, NEQ
         ZR(JATMU+II-1) = 0.0D0
         ZR(JAFMU+II-1) = 0.0D0
 110  CONTINUE
C ======================================================================
C ======================================================================
C --- CALCUL DE AT.MU --------------------------------------------------
C ======================================================================
         DO 123 ILIAC = 1, NBLIAC+LLF+LLF1+LLF2
            LLIAC  = ZI(JLIAC+ILIAC-1)
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
            IF ( ILIAC.LE.NBLIAC ) THEN
               CALL CALATM (NEQ,NBDDL,ZR(JMU+ILIAC-1),
     +                   ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),ZR(JATMU))
            ELSE IF ( ILIAC.LE.(NBLIAC+LLF) ) THEN
               CALL CALATM(NEQ,NBDDL,ZR(JMU+ILIAC-1),
     +                   ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),ZR(JATMU))
               CALL CALATM(NEQ,NBDDL,ZR(JMU+ILIAC+LLF-1),
     +         ZR(JAPCOF+JDECAL+30*NESMAX),ZI(JAPDDL+JDECAL),ZR(JATMU))
            ELSE IF ( ILIAC.LE.(NBLIAC+LLF+LLF1) ) THEN
               CALL CALATM(NEQ,NBDDL,ZR(JMU+ILIAC+LLF-1),
     +         ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),ZR(JATMU))
            ELSE
               CALL CALATM(NEQ,NBDDL,ZR(JMU+ILIAC+LLF-1),
     +         ZR(JAPCOF+JDECAL+30*NESMAX),ZI(JAPDDL+JDECAL),ZR(JATMU))
            ENDIF
 123     CONTINUE
C ======================================================================
 9999 CONTINUE
C ======================================================================
C --- AFFECTATION DE A LIGNE PAR LIGNE (A PARTIR DE IDEBUT) ------------
C ======================================================================
      DO 200 II = 1, NBLIAI
         TROUAC = .TRUE.
C ======================================================================
C --- MISE A ZERO DE LA COLONNE ----------------------------------------
C ======================================================================
         LLIAC = II
         DO 203 JJ = 1,NBLIAC
            IF ( ZI(JLIAC-1+JJ) .EQ. LLIAC ) THEN
               TROUAC = .FALSE.
               JJC = JJ
            ENDIF
 203     CONTINUE
         DO 202 JJ = 1,LLF+LLF1+LLF2
            IF(ZI(JLIAC-1+NBLIAC+JJ).EQ.LLIAC) TROUAC=.TRUE.
 202     CONTINUE
         DO 168 KK = 1,ZI(JLIOT+4*NBLIAI+1)
            IF (ZI(JLIOT-1+NBLIAI+KK).EQ.LLIAC) THEN
               TROUAC = .TRUE.
            ENDIF
 168     CONTINUE
         IF ( .NOT. TROUAC ) THEN
            AJEUFX = 0.0D0
            AJEUFY = 0.0D0
            XK     = ZR(IFRO-1+LLIAC) * ZR(JMU-1+JJC)
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL  = ZI(JAPPTR+LLIAC)-ZI(JAPPTR+LLIAC-1)
C ======================================================================
            GLISS1 = .FALSE.
            DO 172 KK = 1,ZI(JLIOT+4*NBLIAI+2)
               IF (ZI(JLIOT-1+2*NBLIAI+KK).EQ.LLIAC) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
                  GLISS1 = .TRUE.
               ENDIF
 172        CONTINUE
C ======================================================================
            IF (.NOT.GLISS1) THEN
               CALL JEVEUO ( JEXNUM(CM2A,II), 'E', JCM2A1 )
               DO 209 LL = 1, NEQ
                  ZR(JCM2A1-1+LL) = 0.0D0
 209           CONTINUE
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                 ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
               AJEUFX = VAL
            ENDIF
C ======================================================================
            GLISS2 = .FALSE.
            DO 171 KK = 1,ZI(JLIOT+4*NBLIAI+3)
               IF (ZI(JLIOT-1+3*NBLIAI+KK).EQ.LLIAC) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA SECONDE DIRECTION ----------
C ======================================================================
                  GLISS2 = .TRUE.
               ENDIF
 171        CONTINUE
C ======================================================================
            IF (.NOT.GLISS2) THEN
               CALL JEVEUO ( JEXNUM(CM2A,II+NBLIAI), 'E', JCM2A2 )
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
               CALL JELIBE(JEXNUM(CM2A,II       ))
            ENDIF
            IF (.NOT.GLISS2) THEN
               CALL CALATM (NEQ,NBDDL,XMU,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                     ZI(JAPDDL+JDECAL),ZR(JCM2A2))
               CALL JELIBE(JEXNUM(CM2A,II+NBLIAI))
            ENDIF
         ELSE
            CALL JEVEUO ( JEXNUM(CM2A,II       ), 'E', JCM2A1 )
            CALL JEVEUO ( JEXNUM(CM2A,II+NBLIAI), 'E', JCM2A2 )
            DO 2091 LL = 1, NEQ
               ZR(JCM2A1-1+LL) = 0.0D0
               ZR(JCM2A2-1+LL) = 0.0D0
 2091       CONTINUE
            CALL JELIBE(JEXNUM(CM2A,II       ))
            CALL JELIBE(JEXNUM(CM2A,II+NBLIAI))
         ENDIF
 200  CONTINUE
C ======================================================================
C --- CREATION DE LA MATRICE ATA ---------------------------------------
C ======================================================================
      CALL ATA000 (CM2A,NUMEDD,400.D0,MAF1,'V',RESOCO,NBLIAI*(NDIM-1))
C ======================================================================
C --- CREATION DU VECTEUR DE CISAILLEMENT ------------------------------
C --- MATF*((ZR(JDEPDE-1+NUM1)+ZR(JDELT0-1+NUM1)) ----------------------
C --- CE VECTEUR EST REAFFECTE DANS ZR(JAFMU) --------------------------
C ======================================================================
      CALL MTDSCR ( MAF1 )
      CALL JEVEUO ( MAF1//'.&INT', 'E', LMAF1 )
      CALL MRMULT ('ZERO', LMAF1, ZR(JDELTA), 'R', ZR(JAFMU), 1 )
C ======================================================================
C --- CALCUL DE LA MATRICE TANGENTE DU SYSTEME -------------------------
C ======================================================================
      DO 300 II = 1, NBLIAI
         TROUAC = .TRUE.
         LLIAC = II
         DO 301 JJ = 1,NBLIAC
         IF ( ZI(JLIAC-1+JJ) .EQ. LLIAC ) THEN
           TROUAC=.FALSE.
           JJC = JJ
         ENDIF
 301     CONTINUE 
         DO 302 JJ = 1,LLF+LLF1+LLF2
           IF(ZI(JLIAC-1+JJ+NBLIAC).EQ.LLIAC) TROUAC=.TRUE.
 302     CONTINUE
         DO 175 KK = 1,ZI(JLIOT+4*NBLIAI+1)
            IF (ZI(JLIOT-1+NBLIAI+KK).EQ.LLIAC) THEN
               TROUAC = .TRUE.
            ENDIF
 175     CONTINUE
         CALL JEVEUO(JEXNUM(CM3A,II),'E',JCM3A)
         DO 303 LL = 1, NEQ
            ZR(JCM3A-1+LL) = 0.0D0
 303     CONTINUE
         IF ( .NOT. TROUAC ) THEN
C ======================================================================
C --- POUR LES LIAISONS GLISSANTES -------------------------------------
C ======================================================================
            AJEUFX = 0.D0
            AJEUFY = 0.D0
            XK = ZR(IFRO-1+LLIAC) * ZR(JMU-1+JJC)
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
C ======================================================================
            GLISS1 = .FALSE.
            DO 173 KK = 1,ZI(JLIOT+4*NBLIAI+2)
               IF (ZI(JLIOT-1+2*NBLIAI+KK).EQ.LLIAC) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
                  GLISS1 = .TRUE.
               ENDIF
 173        CONTINUE
C ======================================================================
            IF (.NOT.GLISS1) THEN
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                                 ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
               AJEUFX = VAL
            ENDIF
C ======================================================================
            GLISS2 = .FALSE.
            DO 174 KK = 1,ZI(JLIOT+4*NBLIAI+3)
               IF (ZI(JLIOT-1+3*NBLIAI+KK).EQ.LLIAC) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA SECONDE DIRECTION ----------
C ======================================================================
                  GLISS2 = .TRUE.
               ENDIF
 174        CONTINUE
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
     &                                      ZI(JAPDDL+JDECAL),ZR(JCM3A))
         ENDIF
         CALL JELIBE(JEXNUM(CM3A,II))
 300  CONTINUE
C ======================================================================
      CALL FROT05 (CM3A,NUMEDD,MAT,MAF1,MAF2,MAFROT,RESOCO,NBLIAI)
C ======================================================================
 999  CONTINUE
C ======================================================================
C --- STOCKAGE DE L'ETAT DE CONTACT DEFINITIF --------------------------
C ======================================================================
      IF ( NIV .EQ. 2 ) WRITE(IFM,*)'NBLIACF',NBLIAC,'LLF',LLF,'LLF1',
     &                LLF1,'LLF2',LLF2
C ======================================================================
C      IF ( NBLIAC .NE. NBLCIN ) LREAC(2) = .TRUE.
C      IF ( LLF    .NE. NLFIN  ) LREAC(2) = .TRUE.
C      IF ( LLF1   .NE. NLFIN1 ) LREAC(2) = .TRUE.
C      IF ( LLF2   .NE. NLFIN2 ) LREAC(2) = .TRUE.
C ======================================================================
      ZI(JCOCO+1) = NBLIAC
      ZI(JCOCO+2) = LLMIN
      ZI(JCOCO+3) = KKMIN
      ZI(JCOCO+4) = INDIC
      ZI(JCOCO+6) = LLF
      ZI(JCOCO+7) = LLF1
      ZI(JCOCO+8) = LLF2
C ======================================================================
      DO 400 II=1,NBLIAI
         JDECAL = ZI(JAPPTR+II-1)
         NBDDL  = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
         CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &        ZI(JAPDDL+JDECAL),ZR(JRESU),VAL)
         ZR(JAPJEU-1+II) = ZR(JAPJEU-1+II)-VAL
 400  CONTINUE
C ======================================================================
      IF (NIV.GE.2) THEN
         DO 500 II = 1,NBLIAI
            WRITE (IFM,1010) '<FROLGD> JEU FINAL LIAISON ',II,' : ',
     &           ZR(JAPJEU+II-1)
 500     CONTINUE
      END IF
C ======================================================================
99999 CONTINUE
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
 1000 FORMAT ('<CONTACT_2> ',A9,A8,A14,A14,A8)
 1010 FORMAT ('<CONTACT_3> ',A27,I5,A3,E10.3)
C ======================================================================
      END
