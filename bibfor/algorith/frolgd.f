      SUBROUTINE FROLGD (DEFICO,RESOCO,LMAT,LDSCON,NOMA,CINE, 
     &                        RESU,DEPTOT,ITERAT,LREAC,CONV,DEPDEL,ISTO)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION 
C MODIF ALGORITH  DATE 29/06/2004   AUTEUR MABBAS M.ABBAS 
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
      LOGICAL       TROUAC,DELPOS,GLISS1,GLISS2,LELPIV,LELPI1,LELPI2
      INTEGER       JJC,JDEPDE,LLF,ICONT,IIKAC,LLFN,JDIM,NESMAX,LFMIN 
      INTEGER       IBID,IER,IFM,NIV,NDECI,ISINGU,NPVNEG,LCOUNT,LFMIN2 
      INTEGER       ICONTA,II,JJ,KK,IDEBUT,ILIAC,KCONT,NUMIN,IRET,KK2 
      INTEGER       JRESU,JDEPP,JMU,JCMU,JATMU,POSMA,NDIM,NEQMAX 
      INTEGER       JDELT0,JDELTA,JLIAC,JCOCO,JRCINE,JVA,KCOUNT 
      INTEGER       NEQ,NESCL,NBLIAC,NBLIAI,INDIC,KKMIN,LLMIN,SOM
      INTEGER       LLIAC,LLJAC,POS1,POS2,NUM1,NUM2,JDECAL,NBDDL 
      INTEGER       JAPPAR,JAPPTR,JAPCOE,JAPJEU,JAPDDL,JNOCO,JMACO 
      INTEGER       NBLCIN,NLFIN,LLF1,LLF2,LLKAC,LCONT,AJLIAI,SPLIAI 
      INTEGER       IOTE,JLIOT,LCONT0,LCONT1,LCONT2,I,J,LL,NN,IDEBUC 
      INTEGER       ICOMA,ICONT1,ICONT2,KCONT1,KCONT2,LIND,IFRO,PIVOT 
      INTEGER       JAPCOF,JAFMU,LMAF1,JCM2A,JCM2A1,JCM2A2,JCM3A 
      INTEGER       LLF1IN,LLF2IN,JTEMP,LFMIN1,JGLI1,JGLI2,JADHR 
      INTEGER       INDFAC, POSIT, BTOTAL, JVECC, COMPT0
      REAL*8        R8MAEM,R8PREM,AJEU,RHO,RHORHO,AADELT,AJEUFY,XF,VAL3 
      REAL*8        X1,VAL,VAL1,VAL2,ZMU,XFORC,XXMIN,XXMAX,XX,XTOL1 
      REAL*8        XTOL,AJEUFX,XPDT,XK,XCOMP,XQUOT,XCOS,XSIN,XXX,XMU 
      REAL*8        ALPHA,BETA,RESIGR,XMUL,XVAL,VDIAGM,XJVMAX,XJVMIN 
      COMPLEX*16    CBID 
      CHARACTER*1   TYPEAJ 
      CHARACTER*2   TYPEC0, TYPEF0, TYPEF1, TYPEF2, TYPE 
      CHARACTER*8   NOM1,NOM2 
      CHARACTER*14  CHAIN,NUMEDD 
      CHARACTER*16  NMGLI1,NMGLI2, NMADHR 
      CHARACTER*19  AFMU,MAT,CM2A,CM3A,MAF1,MAF2,MAFROT 
      CHARACTER*19  LIAC,MU,ATMU,DELT0,DELTA,COCO,LIOT 
      CHARACTER*19  CONVEC
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
      IF (ICONTA.EQ.0) GO TO 9990 
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
C ======================================================================
      CONTNO   = DEFICO(1:16)//'.NOEUCO' 
      CONTMA   = DEFICO(1:16)//'.MAILCO' 
      APPOIN   = RESOCO(1:14)//'.APPOIN' 
      APCOEF   = RESOCO(1:14)//'.APCOEF' 
      APCOFR   = RESOCO(1:14)//'.APCOFR' 
      APJEU    = RESOCO(1:14)//'.APJEU' 
      APDDL    = RESOCO(1:14)//'.APDDL' 
      LIAC     = RESOCO(1:14)//'.LIAC' 
      LIOT     = RESOCO(1:14)//'.LIOT'   
      CONVEC   = RESOCO(1:14)//'.CONVEC'  
      MU       = RESOCO(1:14)//'.MU' 
      COEFMU   = RESOCO(1:14)//'.COEFMU' 
      ATMU     = RESOCO(1:14)//'.ATMU' 
      AFMU     = RESOCO(1:14)//'.AFMU' 
      DELT0    = RESOCO(1:14)//'.DEL0' 
      DELTA    = RESOCO(1:14)//'.DELT' 
      CM2A     = RESOCO(1:14)//'.CM2A' 
      CM3A     = RESOCO(1:14)//'.CM3A' 
      MAFROT   = RESOCO(1:8)//'.MAFR' 
      MAF1     = '&&FROLGD.MAF1' 
      MAF2     = '&&FROLGD.MAF2' 
      FROTE    = DEFICO(1:16)//'.FROTE' 
      COMAFO   = DEFICO(1:16)//'.COMAFO' 
      NDIMCO   = DEFICO(1:16)//'.NDIMCO' 
C ======================================================================
      CALL JEVEUO (CONTNO,'L',JNOCO) 
      CALL JEVEUO (CONTMA,'L',JMACO) 
      CALL JEVEUO (APPOIN,'L',JAPPTR) 
      CALL JEVEUO (APCOEF,'L',JAPCOE) 
      CALL JEVEUO (APCOFR,'L',JAPCOF) 
      CALL JEVEUO (APJEU, 'E',JAPJEU) 
      CALL JEVEUO (APDDL, 'L',JAPDDL) 
      CALL JEVEUO (LIAC,  'E',JLIAC) 
      CALL JEVEUO (LIOT,  'E',JLIOT) 
      CALL JEVEUO (CONVEC,'L',JVECC)
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
      CALL CFDISD(JCOCO,
     &            NDIM,INDIC,NBLIAC,AJLIAI,SPLIAI,
     &            LLF,LLF1,LLF2)

C ======================================================================
C --- INITIALISATION DE VARIABLES --------------------------------------
C ======================================================================
      TYPEAJ = 'A' 
      TYPEC0 = 'C0' 
      TYPEF0 = 'F0' 
      TYPEF1 = 'F1' 
      TYPEF2 = 'F2'
C ======================================================================
C --- CREATION DE DELTA0 = C-1B 
C ======================================================================
      DO 1 II = 1, NEQ 
         ZR(JDELT0-1+II) = ZR(JRESU-1+II) 
         ZR(JRESU -1+II) = 0.0D0
 1    CONTINUE
      XJVMAX = 0.0D0
C ======================================================================
C --- DETECTION DES COUPLES DE NOEUDS INTERPENETRES --------------------
C --- ON CALCULE LE NOUVEAU JEU : AJEU+ = AJEU/I/N - A.DDEPLA ----------
C --- (IL EST NEGATIF LORSQU'IL Y A INTERPENETRATION -> LIAISON ACTIVE)-
C ======================================================================
      IF (ITERAT.EQ.0) THEN 
         NBLIAC   = 0 
         AJLIAI   = 0
         SPLIAI   = 0
         LLF      = 0 
         LLF1     = 0 
         LLF2     = 0 
         INDFAC   = 1 
         INDIC    = 0
C ======================================================================
C --- INITIALISATION POUR L'ELIMINATION DES PIVOTS NULS ----------------
C ======================================================================
         CALL FROT07 ( LMAT, VDIAGM ) 
         ZR(JMU+6*NBLIAI-1) = VDIAGM ** 0.25D0 
         ZI(JLIOT+4*NBLIAI  )  = 0 
         ZI(JLIOT+4*NBLIAI+1)  = 0 
         ZI(JLIOT+4*NBLIAI+2)  = 0 
         ZI(JLIOT+4*NBLIAI+3)  = 0 
C ======================================================================
C --- INITIALISATION DES DIFFERENTS MU (AU NIVEAU DES LIAISONS DE ------
C --- CONTACT, ADHERENTES ET GLISSANTES) -------------------------------
C ======================================================================
         DO 4 II = 1,NBLIAI 
            ZR(JMU-1+2*NBLIAI+II) = 0.D0 
            ZR(JMU-1+  NBLIAI+II) = 0.D0 
            ZR(JMU-1+         II) = 0.D0  
            AJEU = ZR(JAPJEU+II-1) 
            IF (AJEU.LT.0.0D0) THEN 
               POSIT  = NBLIAC + LLF + LLF1 + LLF2 + 1 
               CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2, 
     +                                    RESOCO,TYPEAJ,POSIT,II,TYPEC0)
C ======================================================================
C --- IMPRESSION DES RESULTATS -----------------------------------------
C ======================================================================
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
         BTOTAL = NBLIAC + LLF + LLF1 + LLF2 
         DO 7 II = 1, BTOTAL 
            LLIAC  = ZI(JLIAC-1+II) 
            POSIT  = NBLIAC + LLF + LLF1 + LLF2 + 1 
            CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2, 
     +                                 RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF0)
            ZR(JMU-1+3*NBLIAI+LLIAC) = 0.0D0 
 7       CONTINUE 
C ======================================================================
      ELSE
         SPLIAI = 0
         AJLIAI = 0
         INDIC  = 0
         INDFAC = 1
C ======================================================================
C --- TRAITEMENT DU CONTACT APRES REACTUALISATION GEOMETRIQUE ----------
C ======================================================================
         IF(LREAC(1)) THEN 
            BTOTAL  = NBLIAC + LLF + LLF1 + LLF2 
            DO 5 II = 1,NBLIAI 
               AJEU = ZR(JAPJEU+II-1) 
               IF (AJEU.LE.R8PREM()) THEN 
                  TROUAC = .FALSE. 
                  DO 940 JJ = 1, BTOTAL 
                     IF (ZI(JLIAC-1+JJ).EQ.II) THEN 
                        TROUAC = .TRUE. 
                     ENDIF 
 940              CONTINUE 
                  IF (.NOT.TROUAC) THEN 
                   POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1 
                   CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     +                                    RESOCO,TYPEAJ,POSIT,II,TYPEC0)
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
      NLFIN  = LLF
      IF ( NIV .EQ. 2 ) WRITE(IFM,*)'NBLIACI',NBLCIN,'LLFI',NLFIN 
      XMUL = ZR(JMU+6*NBLIAI-1) 
 100  CONTINUE 
C ======================================================================
C --- MISE A JOUR DE L'INCREMENT DE DEPLACEMENT A CALCULER -------------
C ======================================================================
      DO 122 II = 1,NEQ 
         ZR(JDELTA-1+II) = ZR(JDELT0-1+II) - ZR(JRESU-1+II)
 122  CONTINUE
 300  CONTINUE
C ======================================================================
C --- CALCUL DE A.C-1.AT S'IL EXISTE AU MOINS UNE LIAISON DE CONTACT ---
C ======================================================================
      IF (NBLIAC.NE.0) THEN 
C ======================================================================
C --- CALCUL DE -A.C-1.AT COLONNE PAR COLONNE (A PARTIR DE IDEBUT) 
C ======================================================================
         CALL CFACAT(NDIM, INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1,
     +    LLF2, INDFAC,NESMAX,DEFICO,RESOCO,LMAT,CINE,NBLIAI,XJVMAX)
C ======================================================================
C --- ELIMINATION DES PIVOTS NULS 
C ======================================================================
         CALL ELPIV2(XJVMAX, NDIM, INDIC, NBLIAC, AJLIAI, SPLIAI,
     +                    LLF, LLF1, LLF2, NOMA, DEFICO, RESOCO)
         IF (INDIC .EQ. -1) THEN
            GOTO 300
         ENDIF
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
         IF (INDFAC.LE.(NBLIAC+(NDIM-1)*LLF+LLF1+LLF2)) THEN
            CALL TLDLGG (2,LDSCON,INDFAC,NBLIAC+(NDIM-1)*LLF+LLF1+LLF2, 
     &                                        0,NDECI,ISINGU,NPVNEG,IER)
            INDFAC = NBLIAC + (NDIM-1)*LLF + LLF1 + LLF2 + 1
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
C --- INITIALISATION DU SECOND MEMBRE ----------------------------------
C ======================================================================
         DO 25 II = 1,NDIM*NBLIAI 
            ZR(JMU-1+II)= 0.D0 
 25      CONTINUE 
C ======================================================================
C --- APPEL DE LA ROUTINE DE CALCUL DU SECOND MEMBRE -------------------
C ======================================================================
         CALL CFADU(RESOCO, DEPDEL, NEQ, NDIM, NBLIAC, LLF, 
     +                                               LLF1, LLF2, NESMAX)
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
C --- ON REORDONNE LE VECTEUR MU ---------------------------------------
C ======================================================================
         CALL CFMAJM(RESOCO, NDIM, NBLIAC, LLF, LLF1, LLF2)
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU ------------------------------
C ======================================================================
         CALL CFMAJU(RESOCO, NEQ, NDIM, NBLIAI, NBLIAC, LLF, LLF1, LLF2)
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
        BTOTAL = NBLIAC + LLF + LLF1 + LLF2 
        DO 70 II = 1, NBLIAI 
C ======================================================================
C - LA LIAISON II EST-ELLE ACTIVE ? (-> TROUAC) ------------------------
C ======================================================================
            DO 700 ILIAC = 1, BTOTAL 
               IF (ZI(JLIAC-1+ILIAC).EQ.II) GOTO 70 
 700        CONTINUE 
C ======================================================================
C - CALCUL DE A.DELTA SI LA LIAISON II N'EST PAS ACTIVE 
C ======================================================================
            JDECAL = ZI(JAPPTR+II-1) 
            NBDDL  = ZI(JAPPTR+II) - ZI(JAPPTR+II-1) 
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
               CALL CFELPV(II, TYPEC0, RESOCO, NBLIAI, LELPIV)
               IF (LELPIV) GOTO 70
C               DO  71 IOTE= 1,ZI(JLIOT+4*NBLIAI) 
C                  IF (ZI(JLIOT-1+IOTE).EQ.II) GOTO 70 
C 71            CONTINUE 
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
         POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1 
         CALL CFTABL( INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1, LLF2, 
     +                             RESOCO, TYPEAJ, POSIT, NUMIN, TYPEC0)
         IF (ITERAT.EQ.0) THEN
C ======================================================================
C -  ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL 
C ======================================================================
            POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1 
            CALL CFELPV(NUMIN, TYPEF0, RESOCO, NBLIAI, LELPIV)
            IF (.NOT.LELPIV) THEN
               CALL CFELPV(NUMIN, TYPEF1, RESOCO, NBLIAI, LELPI1)
               IF (LELPI1) THEN
                  CALL CFTABL( INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1,
     +                       LLF2, RESOCO, TYPEAJ, POSIT, NUMIN, TYPEF2)
               ELSE
                  CALL CFELPV(NUMIN, TYPEF2, RESOCO, NBLIAI, LELPI2)
                  IF (LELPI2) THEN
                     CALL CFTABL( INDIC, NBLIAC, AJLIAI, SPLIAI, LLF,
     +                 LLF1, LLF2, RESOCO, TYPEAJ, POSIT, NUMIN, TYPEF1)
                  ELSE
                     CALL CFTABL( INDIC, NBLIAC, AJLIAI, SPLIAI, LLF,
     +                 LLF1, LLF2, RESOCO, TYPEAJ, POSIT, NUMIN, TYPEF0)
                  ENDIF
               ENDIF
            ENDIF           
         ENDIF 
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
C --- RECUPERATION DU DEPLACEMENT FINAL --------------------------------
C ======================================================================
      DO 150 II = 1,NEQ 
          ZR(JRESU -1+II) = ZR(JRESU-1+II) + ZR(JDELTA-1+II) 
          ZR(JDELTA-1+II) = ZR(JRESU-1+II) + ZR(JDEPDE-1+II)
 150  CONTINUE 
C ======================================================================
C --- ON ENLEVE TOUTES LES LIAISONS POUR LESQUELLES --------------------
C --- LA PRESSION EST NEGATIVE -----------------------------------------
C ======================================================================
      IF (NBLIAC.NE.0) THEN
         CALL CFNEG(RESOCO, NDIM, INDIC, NBLIAI, NBLIAC, AJLIAI, SPLIAI,
     +                                                  LLF, LLF1, LLF2)
      ENDIF
C ======================================================================
C --- LES LIAISONS CONSIDEREES ADHERENTES LE SONT-ELLES VRAIMENT ? -----
C ======================================================================
         CALL CFADH(NDIM, INDIC, NBLIAC, NBLIAI, AJLIAI, SPLIAI,
     +                                  LLF, LLF1, LLF2, RESOCO, DEFICO)
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
            DO 156 II = 1, BTOTAL
               IF (ZK8(JVECC-1+II).EQ.TYPEC0) THEN
                  COMPT0 = COMPT0 + 1
                  AJEUFX = 0.0D0 
                  AJEUFY = 0.0D0 
                  LLIAC  = ZI(JLIAC-1+II)
                  JDECAL = ZI(JAPPTR+LLIAC-1) 
                  NBDDL  = ZI(JAPPTR+LLIAC  ) - ZI(JAPPTR+LLIAC-1) 
C ======================================================================
C --- CALCUL DU JEU SUIVANT LA PREMIERE DIRECTION ----------------------
C ======================================================================
                  CALL CFELPV(LLIAC, TYPEF1, RESOCO, NBLIAI, GLISS1)
C               GLISS1 = .FALSE.
C               DO 1782 KK = 1,ZI(JLIOT+4*NBLIAI+2) 
C                  IF (ZI(JLIOT-1+2*NBLIAI+KK).EQ.LLIAC) THEN 
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
C                     GLISS1 = .TRUE. 
C                  ENDIF
C 1782          CONTINUE
                  IF (.NOT.GLISS1) THEN 
                     CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL), 
     &                                 ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
                     AJEUFX = VAL 
                  ENDIF 
C ======================================================================
C --- CALCUL DU JEU SUIVANT LA SECONDE DIRECTION -----------------------
C ======================================================================
                  CALL CFELPV(LLIAC, TYPEF2, RESOCO, NBLIAI, GLISS2)
C               GLISS2 = .FALSE. 
C               DO 1784 KK = 1,ZI(JLIOT+4*NBLIAI+3) 
C                  IF (ZI(JLIOT-1+3*NBLIAI+KK).EQ.LLIAC) THEN 
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
C                     GLISS2 = .TRUE. 
C                  ENDIF 
C 1784          CONTINUE 
                  IF (.NOT.GLISS2) THEN 
                     CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                                 ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
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
                     DO 157 KK = II + 1, BTOTAL 
                        LLKAC = ZI(JLIAC-1+KK)  
                        IF (LLKAC.EQ.LLIAC) THEN 
C ======================================================================
C --- LA LIAISON EST DEJA ADHERENTE ------------------------------------
C ======================================================================
                           TROUAC = .TRUE. 
                        ENDIF 
 157                 CONTINUE
                     IF (TROUAC) GOTO 156
C                  DO 160 KK = 1,ZI(JLIOT+4*NBLIAI+1) 
C                     IF (ZI(JLIOT-1+NBLIAI+KK).EQ.LLIAC) THEN 
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LES DEUX DIRECTIONS -----------
C ======================================================================
C 160              CONTINUE 
                     CALL CFELPV(LLIAC, TYPEF0, RESOCO, NBLIAI, TROUAC)
                     IF (.NOT.TROUAC) THEN 
C                    DO 176 KK = 1,ZI(JLIOT+4*NBLIAI+2) 
C                       IF (ZI(JLIOT-1+2*NBLIAI+KK).EQ.LLIAC) THEN 
                        CALL CFELPV(LLIAC,TYPEF1,RESOCO,NBLIAI,GLISS1)
                        IF (GLISS1) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
                           LFMIN2 = LFMIN2 + 1 
                           ZI(JGLI2-1+LFMIN2) = LLIAC 
                           GO TO 156 
                        ENDIF
C 176                 CONTINUE 
C                     DO 177 KK = 1,ZI(JLIOT+4*NBLIAI+3) 
C                        IF (ZI(JLIOT-1+3*NBLIAI+KK).EQ.LLIAC) THEN 
                        CALL CFELPV(LLIAC,TYPEF2,RESOCO,NBLIAI,GLISS2)
                        IF (GLISS2) THEN
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
                           LFMIN1 = LFMIN1 + 1 
                           ZI(JGLI1-1+LFMIN1) = LLIAC 
                           GO TO 156 
                        ENDIF 
C 177                 CONTINUE 
                        LFMIN = LFMIN + 1 
                        ZI(JADHR-1+LFMIN) = LLIAC 
                     ENDIF 
                  ENDIF 
               ENDIF 
 156        CONTINUE
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
     +                                 RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF0)
 197           CONTINUE 
               DO 198 LL=1,LFMIN1 
                  LLIAC = ZI(JGLI1-1+LL) 
                  POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1
                  CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2, 
     +                                 RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF1)
 198           CONTINUE 
               DO 199 LL=1,LFMIN2 
                  LLIAC = ZI(JGLI2-1+LL) 
                  POSIT = NBLIAC + LLF + LLF1 + LLF2 + 1
                  CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2, 
     +                                 RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF2)
 199           CONTINUE 
               CALL JEDETR(NMGLI1) 
               CALL JEDETR(NMGLI2) 
               CALL JEDETR(NMADHR) 
               XVAL = XMUL**2 
               IF (XVAL.LT.(1.0D0/R8PREM())) THEN 
                  XMUL = XMUL*SQRT(10.D0) 
               ENDIF 
               ZR(JMU+6*NBLIAI-1) = XMUL 
               GO TO 100 
            ENDIF 
            CALL JEDETR(NMGLI1) 
            CALL JEDETR(NMGLI2) 
            CALL JEDETR(NMADHR) 
         ENDIF 
C      ENDIF
C ======================================================================
C --- INITIALISATION DE ATMU ET DE AFMU --------------------------------
C ======================================================================
      DO 110 II = 1, NEQ 
         ZR(JATMU+II-1) = 0.0D0 
         ZR(JAFMU+II-1) = 0.0D0 
 110  CONTINUE
      IF (NBLIAC.EQ.0) GOTO 999
C ======================================================================
C --- CALCUL DE AT.MU --------------------------------------------------
C ======================================================================
      CALL CFATMU(NEQ , NESMAX, NDIM, NBLIAC, LLF, LLF1, LLF2, RESOCO) 
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
            CALL JEVEUO ( JEXNUM(CM2A,II       ), 'E', JCM2A1 ) 
            CALL JEVEUO ( JEXNUM(CM2A,II+NBLIAI), 'E', JCM2A2 ) 
            DO 2192 LL = 1, NEQ 
               ZR(JCM2A1-1+LL) = 0.0D0 
               ZR(JCM2A2-1+LL) = 0.0D0 
 2192       CONTINUE
            CALL JELIBE(JEXNUM(CM2A,II       )) 
            CALL JELIBE(JEXNUM(CM2A,II+NBLIAI))
            GOTO 200
         ENDIF
         CALL CFELPV(LLIAC, TYPEF0, RESOCO, NBLIAI, TROUAC)
C         DO 168 KK = 1,ZI(JLIOT+4*NBLIAI+1) 
C            IF (ZI(JLIOT-1+NBLIAI+KK).EQ.LLIAC) THEN 
C               TROUAC = .TRUE. 
C            ENDIF 
C 168     CONTINUE 
         IF ( .NOT.TROUAC ) THEN
            AJEUFX = 0.0D0 
            AJEUFY = 0.0D0
            XK     = ZR(IFRO-1+LLIAC) * ZR(JMU-1+JJC)
            JDECAL = ZI(JAPPTR+LLIAC-1) 
            NBDDL  = ZI(JAPPTR+LLIAC)-ZI(JAPPTR+LLIAC-1) 
C ======================================================================
C            GLISS1 = .FALSE. 
C            DO 172 KK = 1,ZI(JLIOT+4*NBLIAI+2) 
C               IF (ZI(JLIOT-1+2*NBLIAI+KK).EQ.LLIAC) THEN 
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
C                  GLISS1 = .TRUE. 
C               ENDIF 
C 172        CONTINUE 
C ======================================================================
            CALL CFELPV(LLIAC, TYPEF1, RESOCO, NBLIAI, GLISS1)
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
C            GLISS2 = .FALSE. 
C            DO 171 KK = 1,ZI(JLIOT+4*NBLIAI+3) 
C              IF (ZI(JLIOT-1+3*NBLIAI+KK).EQ.LLIAC) THEN 
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA SECONDE DIRECTION ----------
C ======================================================================
C                  GLISS2 = .TRUE. 
C               ENDIF 
C 171        CONTINUE 
C ======================================================================
            CALL CFELPV(LLIAC, TYPEF2, RESOCO, NBLIAI, GLISS2)
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
      DO 1300 II = 1, NBLIAI 
         TROUAC = .TRUE. 
         LLIAC = II
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
C         DO 175 KK = 1,ZI(JLIOT+4*NBLIAI+1) 
C            IF (ZI(JLIOT-1+NBLIAI+KK).EQ.LLIAC) THEN 
C              TROUAC = .TRUE. 
C           ENDIF 
C 175     CONTINUE 
         CALL JEVEUO(JEXNUM(CM3A,II),'E',JCM3A) 
         DO 303 LL = 1, NEQ 
            ZR(JCM3A-1+LL) = 0.0D0 
 303     CONTINUE
         IF (TROUAC) THEN
            CALL JELIBE(JEXNUM(CM3A,II)) 
            GOTO 1300
         ENDIF
         CALL CFELPV(LLIAC, TYPEF0, RESOCO, NBLIAI, TROUAC)
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
C            GLISS1 = .FALSE. 
C            DO 173 KK = 1,ZI(JLIOT+4*NBLIAI+2) 
C               IF (ZI(JLIOT-1+2*NBLIAI+KK).EQ.LLIAC) THEN 
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA PREMIERE DIRECTION ---------
C ======================================================================
C                  GLISS1 = .TRUE. 
C               ENDIF 
C 173        CONTINUE 
            CALL CFELPV(LLIAC, TYPEF1, RESOCO, NBLIAI, GLISS1)
C ======================================================================
            IF (.NOT.GLISS1) THEN 
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL), 
     &                                 ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
               AJEUFX = VAL 
            ENDIF 
C ======================================================================
C            GLISS2 = .FALSE. 
C            DO 174 KK = 1,ZI(JLIOT+4*NBLIAI+3) 
C               IF (ZI(JLIOT-1+3*NBLIAI+KK).EQ.LLIAC) THEN 
C ======================================================================
C --- LA LIAISON EST A PIVOT NUL SUIVANT LA SECONDE DIRECTION ----------
C ======================================================================
C                  GLISS2 = .TRUE. 
C               ENDIF 
C 174        CONTINUE 
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
     &                                      ZI(JAPDDL+JDECAL),ZR(JCM3A))
         ENDIF 
         CALL JELIBE(JEXNUM(CM3A,II)) 
 1300 CONTINUE
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
      ZI(JCOCO+1) = INDIC 
      ZI(JCOCO+2) = NBLIAC 
      ZI(JCOCO+3) = AJLIAI 
      ZI(JCOCO+4) = SPLIAI 
      ZI(JCOCO+5) = LLF 
      ZI(JCOCO+6) = LLF1 
      ZI(JCOCO+7) = LLF2
C ======================================================================
      DO 400 II=1,NBLIAI 
         JDECAL = ZI(JAPPTR+II-1) 
         NBDDL  = ZI(JAPPTR+II) - ZI(JAPPTR+II-1) 
         CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL), 
     &                                  ZI(JAPDDL+JDECAL),ZR(JRESU),VAL)
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
 9990 CONTINUE 
C ======================================================================
      CALL JEDEMA () 
C ======================================================================
 1000 FORMAT ('<CONTACT_2> ',A9,A8,A14,A14,A8) 
 1010 FORMAT ('<CONTACT_3> ',A27,I5,A3,E10.3) 
C ======================================================================
      END 
