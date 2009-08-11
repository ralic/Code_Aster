      SUBROUTINE FROGDP(DEFICO,RESOCO,LMAT  ,NOMA  ,RESU  ,
     &                  RESIGR,REAPRE,DEPDEL,CTCFIX)
C 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/08/2009   AUTEUR DESOZA T.DESOZA 
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
      REAL*8       RESIGR
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*19 RESU,DEPDEL
      LOGICAL      REAPRE,CTCFIX
      INTEGER      LMAT
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
C
C ALGO. POUR CONTACT	: PENALISATION
C ALGO. POUR FROTTEMENT : PENALISATION
C
C ----------------------------------------------------------------------
C
C
C RESO. DE : C.DU + KC ACT.AC.DU + KG AGT.AG.DU = F - KG AGT.AG (E-U)
C            AC. (U+DU)      <= E  (= POUR LES LIAISONS ACTIVES)
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
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C VAR RESU   : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS L'ITERATION
C              DE NEWTON PRECEDENTE
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C IN  RESIGR : RESI_GLOB_RELA
C ON UTILISE UNIQUEMENT LE VECTEUR AFMU CAR LES DONNEES DE ATMU SONT
C NECESSAIRE POUR LE CALCUL DE LA MATRICE TANGENTE QUI SE FAIT
C A L'AIDE DU VECTEUR AFMU
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
      INTEGER      IFM,NIV
      INTEGER      NDLMAX,NMULT
      PARAMETER   (NDLMAX = 30)
      INTEGER      IBID,IER,JFRO11,JFRO12
      INTEGER      ICOMA,II,JJ,KK,JAPJFX,JAPJFY,JZOCO
      INTEGER      JRESU,JMU,JATMU,NDIM
      INTEGER      JDEPDE,JDELT0,JDELTA,JLIAC,JCOCO,JAPMEM
      INTEGER      NEQ,NESCL,NBLIAC,NBLIAI,NBLCIN,NBDDL
      INTEGER      LLIAC,JDECAL,IPENA
      INTEGER      JAPPAR,JAPPTR,JAPCOE,JAPJEU,JAPDDL,JNOCO,JMACO
      INTEGER      JAPCOF,JAFMU,LMAF1,JENAT,JFRO2,IFRO,ITER
      INTEGER      JDIM,NESMAX,LLF,LLF1,LLF2,AJLIAI,SPLIAI,INDIC,POSIT
      REAL*8       AJEUFX,AJEUFY,XF,XX,XK,XMU,VAL
      REAL*8       BETA,VAL1,VAL2,R8BID
      CHARACTER*1  TYPEAJ
      CHARACTER*2  TYPEC0
      CHARACTER*14 NUMEDD
      CHARACTER*19 AFMU,MAT,ENAT,FRO1,FRO2,MACT,MAF1,MAF2,MAFROT,MATEMP
      CHARACTER*19 LIAC,MU,ATMU,DELT0,DELTA,COCO
      CHARACTER*24 APJEFX,APJEFY,NOZOCO
      CHARACTER*24 APPARI,APPOIN,APCOEF,APJEU,APDDL,APMEMO
      CHARACTER*24 NDIMCO,CONTNO,CONTMA,APCOFR,FROTE,PENAL,COMAFO
      LOGICAL      CFEXCL

C
C ======================================================================
C
C ======================================================================
C             INITIALISATIONS DES OBJETS ET DES ADRESSES
C ======================================================================
C
C U      : DEPTOT + RESU+
C DEPTOT : DEPLACEMENT TOTAL OBTENU A L'ISSUE DE L'ITERATION DE NEWTON
C          PRECEDENTE. C'EST U/I/N.
C RESU   : INCREMENT DEPUIS DEPTOT
C          C'EST DU/K OU DU/K+1.
C DELTA  : INCREMENT DONNE PAR CHAQUE ITERATION DE CONTRAINTES ACTIVES.
C          C'EST D/K+1.
C DELT0  : INCREMENT DE DEPLACEMENT DEPUIS LA DERNIERE ITERATION DE
C          NEWTON SANS TRAITER LE CONTACT. C'EST C-1.F.
C ======================================================================
      CALL INFNIV (IFM,NIV)
      IF(NIV.GE.2) THEN
        WRITE (IFM,*) '<CONTACT> <> ALGO_CONTACT   : PENALISATION'
        WRITE (IFM,*) '<CONTACT> <> ALGO_FROTTEMENT: PENALISATION'
      ENDIF
      CALL JEMARQ ()
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
      CONTNO   = DEFICO(1:16)//'.NOEUCO'
      NOZOCO   = DEFICO(1:16)//'.NOZOCO'
      CONTMA   = DEFICO(1:16)//'.MAILCO'
      APPARI   = RESOCO(1:14)//'.APPARI'
      APPOIN   = RESOCO(1:14)//'.APPOIN'
      APCOEF   = RESOCO(1:14)//'.APCOEF'
      APCOFR   = RESOCO(1:14)//'.APCOFR'
      APJEU    = RESOCO(1:14)//'.APJEU'
      APJEFX   = RESOCO(1:14)//'.APJEFX'
      APJEFY   = RESOCO(1:14)//'.APJEFY'
      APDDL    = RESOCO(1:14)//'.APDDL'
      APMEMO   = RESOCO(1:14)//'.APMEMO'
      LIAC     = RESOCO(1:14)//'.LIAC'
      MU       = RESOCO(1:14)//'.MU'
      ATMU     = RESOCO(1:14)//'.ATMU'
      AFMU     = RESOCO(1:14)//'.AFMU'
      DELT0    = RESOCO(1:14)//'.DEL0'
      DELTA    = RESOCO(1:14)//'.DELT'
      ENAT     = RESOCO(1:14)//'.ENAT'
      FRO1     = RESOCO(1:14)//'.FRO1'
      FRO2     = RESOCO(1:14)//'.FRO2'
      MAFROT   = RESOCO(1:8)//'.MAFR'
      MACT     = '&&FROPGD.MACT'
      MAF1     = '&&FROPGD.MAF1'
      MAF2     = '&&FROPGD.MAF2'
      MATEMP   = '&&FROPGD.MATP'
      FROTE    = DEFICO(1:16)//'.FROTE'
      PENAL    = DEFICO(1:16)//'.PENAL'
      COMAFO   = DEFICO(1:16)//'.COMAFO'
      NDIMCO   = DEFICO(1:16)//'.NDIMCO'
      COCO     = RESOCO(1:14)//'.COCO'
      MAT      = ZK24(ZI(LMAT+1))
C ======================================================================
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(NOZOCO,'L',JZOCO)
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APCOFR,'L',JAPCOF)
      CALL JEVEUO(APJEU, 'E',JAPJEU)
      CALL JEVEUO(APJEFX,'E',JAPJFX)
      CALL JEVEUO(APJEFY,'E',JAPJFY)
      CALL JEVEUO(APMEMO,'L',JAPMEM)
      CALL JEVEUO(APDDL, 'L',JAPDDL)
      CALL JEVEUO(LIAC,  'E',JLIAC)
      CALL JEVEUO(MU,    'E',JMU)
      CALL JEVEUO(ATMU,  'E',JATMU)
      CALL JEVEUO(AFMU , 'E',JAFMU)
      CALL JEVEUO(DELT0, 'E',JDELT0)
      CALL JEVEUO(DELTA, 'E',JDELTA)
      CALL JEVEUO(RESU(1:19)//'.VALE'  ,'L',JRESU)
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      CALL JEVEUO(FROTE,'L',IFRO)
      CALL JEVEUO(PENAL,'L',IPENA)
      CALL JEVEUO(COMAFO,'L',ICOMA)
      CALL JEVEUO(NDIMCO,'L',JDIM)
      CALL JEVEUO(COCO,'E',JCOCO)
      CALL DISMOI('F','NOM_NUME_DDL',MAT,'MATR_ASSE',IBID,NUMEDD,IER)
C ======================================================================
C --- INITIALISATION DE VARIABLES
C --- NESCL  : NOMBRE DE NOEUDS ESCLAVES SUSCEPTIBLES D'ETRE EN CONTACT
C --- NBLIAI : NOMBRE DE LIAISONS DE CONTACT
C --- NBLIAC : NOMBRE DE LIAISONS ACTIVES
C --- NEQ    : NOMBRE D'EQUATIONS DU MODELE
C --- ITEMAX : NOMBRE D'ITERATIONS DE CONTACT MAXI
C --- NESMAX : NOMBRE MAXI DE NOEUDS ESCLAVES
C              SERT AU DECALAGE DANS LES ROUTINES DE FROTTEMENT 3D
C              (VAUT DONC ZERO SI SANS FROTTEMENT OU FROTTEMENT 2D)
C --- INDFAC : INDICE DE DEBUT DE LA FACTORISATION
C --- INDIC  : 0  INITIALISATION,
C             +1 ON A RAJOUTE UNE LIAISON
C             -1 ON A ENLEVE UNE LIAISON
C --- SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR ENAT
C --- AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON CORRECTE DU CALCUL
C              DE LA MATRICE DE CONTACT AENATT
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
      ITER   = 0
      LLF    = 0
      LLF1   = 0
      LLF2   = 0
      TYPEAJ = 'A'
      TYPEC0 = 'C0'
      AJLIAI = 0
      SPLIAI = 0
      INDIC  = 0
C ======================================================================
C --- SOUVENIRS DE L'ETAT DE CONTACT -> ON NE PEUT PAS S'EN SERVIR
C --- AU DEBUT CAR SI ON A REAPPARIE LES LIAISONS SONT DIFFERENTES
C ======================================================================
      CALL CFDISD(JCOCO,
     &            NDIM,NBLIAC,LLF,LLF1,LLF2)
C ======================================================================
C --- CREATION DE DELTA0 = C-1B
C ======================================================================
      DO 1 II = 1, NEQ
         ZR(JDELT0-1+II) = ZR(JRESU-1+II)
         ZR(JDELTA-1+II) = ZR(JRESU-1+II)+ZR(JDEPDE-1+II)
 1    CONTINUE
C ======================================================================
C --- INITIALISATION DE AFMU
C ======================================================================
      DO 2 II = 1, NEQ
         ZR(JAFMU+II-1) = 0.0D0
 2    CONTINUE
C
C ======================================================================
C
      NBLCIN = NBLIAC

      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1000) NBLIAI
        WRITE(IFM,1005) NBLIAC
      ENDIF
C
      NBLIAC = 0
C
C ======================================================================
C --- RECUPERATION DES JEUX NEGATIFS ET CREATION DU SECOND
C --- MEMBRE ATMU = -E_N*AT*JEU
C ======================================================================
C
      IF (REAPRE) THEN
         DO 10 II = 1,NBLIAI
            ZR(JMU-1+  NBLIAI+II) = 0.D0
            ZR(JMU-1+2*NBLIAI+II) = 0.D0
 10      CONTINUE
      ENDIF

      DO 50 II = 1,NBLIAI
         ZR(JMU-1+         II) = 0.D0
         ZR(JMU-1+3*NBLIAI+II) = 0.D0
         JDECAL = ZI(JAPPTR+II-1)
         NBDDL  = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
         CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL)
         IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
           ZR(JAPJEU+II-1) = ZR(JAPJEU+II-1) - VAL

           IF ( ZR(JAPJEU+II-1).LT.0.0D0 ) THEN
             POSIT  = NBLIAC + 1
             CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &                                   RESOCO,TYPEAJ,POSIT,II,TYPEC0)
             ZR(JMU-1+NBLIAC) = -ZR(JAPJEU+II-1)*ZR(IPENA-1+2*II-1)
             CALL CALATM(NEQ,NBDDL,ZR(JMU-1+NBLIAC),ZR(JAPCOE+JDECAL),
     &                             ZI(JAPDDL+JDECAL),ZR(JATMU))
           ENDIF
         ENDIF
 50   CONTINUE
C
      IF (NBLIAC.EQ.0) THEN
        GOTO 999
      ENDIF
C
C ======================================================================
C --- CALCUL DES COEFFICIENTS DE LAGRANGE MU POUR LE FROTTEMENT
C ======================================================================
C
      DO 100 II = 1, NBLIAC
         AJEUFX = 0.D0
         AJEUFY = 0.D0
         LLIAC  = ZI(JLIAC-1+II)
         JDECAL = ZI(JAPPTR+LLIAC-1)
         NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
         CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
         CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
         VAL = VAL1 + VAL2
         AJEUFX = ZR(JAPJFX-1+LLIAC) - VAL
         IF (NDIM.EQ.3) THEN
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                             ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
            CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                             ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
            VAL = VAL1 + VAL2
            AJEUFY = ZR(JAPJFY-1+LLIAC) - VAL
         ENDIF
         XK = ZR(IFRO-1+LLIAC)
         IF ( ZR(JMU-1+II) .GT. 0.D0) THEN
            XK = XK*ZR(JMU-1+II)
         ELSE
            XK = 0.D0
         ENDIF
         XF = SQRT(ZR(IPENA-1+2*LLIAC))
         XX = SQRT( AJEUFX**2 + AJEUFY**2 )
         IF ( ZR(JMU-1+2*NBLIAI+LLIAC).NE.0.D0) THEN
            IF ( XX .LE. XK/XF**2 ) THEN
               ZR(JMU-1+3*NBLIAI+LLIAC) = XF
            ELSE
               ZR(JMU-1+3*NBLIAI+LLIAC) = SQRT(XK/XX)
            ENDIF
         ELSE
            ZR(JMU-1+3*NBLIAI+LLIAC) = 0.D0
         ENDIF
 100  CONTINUE
C
C ======================================================================
C --- CREATION DE LA MATRICE DE CONTACT ENAT = E_N*AT
C ======================================================================
C
      DO 160 II = 1, NBLIAI
         CALL JEVEUO ( JEXNUM(ENAT,II), 'E', JENAT )
         DO 150 KK = 1, NDLMAX
            ZR(JENAT-1+KK) = 0.0D0
 150     CONTINUE
         IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
           IF ( ZR(JAPJEU+II-1).LT.0.0D0 ) THEN
             JDECAL = ZI(JAPPTR+II-1)
             NBDDL  = ZI(JAPPTR+II)-ZI(JAPPTR+II-1)
             XMU    = SQRT(ZR(IPENA-1+2*II-1))
             CALL DAXPY(NBDDL,XMU,ZR(JAPCOE+JDECAL),1,ZR(JENAT),1)
             ZR(JMU-1+2*NBLIAI+II) = 1.D0
           ENDIF
         ENDIF
         CALL JELIBE(JEXNUM(ENAT,II))
 160  CONTINUE
C
C ======================================================================
C --- CREATION DE MACT = E_N*AT*A
C ======================================================================
C
      NMULT = 1
      CALL ATASMO(NEQ,ENAT,ZI(JAPDDL),
     &                     ZI(JAPPTR),NUMEDD,MACT,'V',NBLIAI,NMULT)
C
C ======================================================================
C --- CREATION DE LA MATRICE FRO1 = E_N*AT
C ======================================================================
C
      IF (NDIM.EQ.3) THEN
         DO 210 II = 1, NBLIAI
            CALL JEVEUO ( JEXNUM(FRO1,II)       ,   'E', JFRO11 )
            CALL JEVEUO ( JEXNUM(FRO1,II+NBLIAI),   'E', JFRO12 )
            DO 200 KK = 1, NDLMAX
               ZR(JFRO11-1+KK) = 0.0D0
               ZR(JFRO12-1+KK) = 0.0D0
 200        CONTINUE
            IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
              IF ( ZR(JAPJEU+II-1).LT.0.0D0 ) THEN
                JDECAL = ZI(JAPPTR+II-1)
                NBDDL  = ZI(JAPPTR+II)   - ZI(JAPPTR+II-1)
                XMU    = ZR(JMU-1+3*NBLIAI+II)
                CALL DAXPY(NBDDL,XMU,ZR(JAPCOF+JDECAL),1,ZR(JFRO11),1)
                CALL DAXPY(NBDDL,XMU,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                             1,ZR(JFRO12),1)
              ENDIF
            ENDIF
            CALL JELIBE(JEXNUM(FRO1,II)       )
            CALL JELIBE(JEXNUM(FRO1,II+NBLIAI))
 210     CONTINUE
      ELSE
         DO 216 II = 1, NBLIAI
            CALL JEVEUO ( JEXNUM(FRO1,II)       , 'E', JFRO11 )
            DO 202 KK = 1, NDLMAX
               ZR(JFRO11-1+KK) = 0.0D0
 202        CONTINUE
            IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
              IF ( ZR(JAPJEU+II-1).LT.0.0D0 ) THEN
                JDECAL = ZI(JAPPTR+II-1)
                NBDDL  = ZI(JAPPTR+II)   - ZI(JAPPTR+II-1)
                XMU  = ZR(JMU-1+3*NBLIAI+II)
                CALL DAXPY(NBDDL,XMU,ZR(JAPCOF+JDECAL),1,ZR(JFRO11),1)
              ENDIF
            ENDIF
            CALL JELIBE(JEXNUM(FRO1,II))
 216     CONTINUE
      ENDIF
C
C ======================================================================
C --- CREATION DE LA MATRICE MAF1 = E_N*AT*A
C ======================================================================
C
      NMULT = NDIM - 1
      CALL ATASMO (NEQ,FRO1,ZI(JAPDDL),
     &                      ZI(JAPPTR),NUMEDD,MAF1,'V',NBLIAI,NMULT)
C
C ======================================================================
C --- RECUPERATION DU SECOND MEMBRE
C --- CE VECTEUR EST REAFFECTE DANS ZR(JAFMU)
C ======================================================================
C
      CALL MTDSCR( MAF1 )
      CALL JEVEUO( MAF1//'.&INT', 'L', LMAF1 )
      CALL MRMULT('ZERO', LMAF1, ZR(JDELTA), 'R', ZR(JAFMU), 1 )
C
C ======================================================================
C --- CREATION DE FRO2 = E_T*AT
C ======================================================================
C
      DO 300 II = 1, NBLIAI
         CALL JEVEUO ( JEXNUM(FRO2,II), 'E', JFRO2 )
         DO 310 KK = 1, NDLMAX
            ZR(JFRO2-1+KK) = 0.0D0
 310     CONTINUE
         IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
           IF ( ZR(JAPJEU+II-1).LT.0.0D0 ) THEN
             AJEUFX = 0.D0
             AJEUFY = 0.D0
             JDECAL = ZI(JAPPTR+II-1)
             NBDDL  = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
             CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                   ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
             CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                   ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
             VAL = VAL1 + VAL2
             AJEUFX = ZR(JAPJFX-1+II)-VAL
             IF (NDIM.EQ.3) THEN
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                      ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                   ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
               VAL = VAL1 + VAL2
               AJEUFY = ZR(JAPJFY-1+II)-VAL
             ENDIF
             XK = ZR(IFRO-1+II)
             DO 320 JJ = 1,NBLIAC
               LLIAC = ZI(JLIAC-1+JJ)
               IF(LLIAC.EQ.II) THEN
                  IF ( ZR(JMU-1+JJ) .GT. 0.D0 ) THEN
                     XK = XK*ZR(JMU-1+JJ)
                  ELSE
                     XK = 0.D0
                  ENDIF
               ENDIF
 320         CONTINUE
             IF ( XK . EQ . 0.D0) THEN
               BETA = 0.D0
               GOTO 305
             ENDIF
             XF = SQRT(ZR(IPENA-1+2*II))
             XX = SQRT( AJEUFX**2 + AJEUFY**2 )
             IF ( ZR(JMU-1+2*NBLIAI+II).NE.0.D0) THEN
               IF ( XX .LE. XK/XF**2 ) THEN
                  BETA = 0.D0
               ELSE
                  BETA = SQRT(1.D0/(XK*XX))
               ENDIF
             ELSE
               BETA = 0.D0
             ENDIF
             IF ( RESIGR .GE. 1.0D-03 ) THEN
               XMU = SQRT(ZR(ICOMA-1+II))
               BETA = BETA*XMU
             ENDIF
             CALL CALAPR(NBDDL,BETA,ZR(JAFMU),
     &                              ZI(JAPDDL+JDECAL),ZR(JFRO2))
             ZR(JMU-1+2*NBLIAI+II) = 1.D0
           ELSE
             ZR(JMU-1+2*NBLIAI+II) = 0.D0
           ENDIF
         ENDIF
 305     CONTINUE
         CALL JELIBE(JEXNUM(FRO2,II))
 300  CONTINUE
C
C ======================================================================
C --- CREATION DE LA SECONDE PARTIE DE LA MATRICE DE FROTTEMENT MAF2
C ======================================================================
C
      NMULT = 1
      CALL ATASMO(NEQ,FRO2,ZI(JAPDDL),
     &                     ZI(JAPPTR),NUMEDD,MAF2,'V',NBLIAI,NMULT)
C
C ======================================================================
C --- CALCUL DE LA MATRICE TANGENTE MAFROT = MACT+MAF1+MAF2
C ======================================================================
C
      CALL CFFROT(MAF1,'-',MAF2  ,MATEMP)
      CALL CFFROT(MACT,'+',MATEMP,MAFROT)
C ======================================================================
C --- CALCUL DES FORCES DE CONTACT (AT.MU) ET FROTTEMENT (AF.MU)
C ======================================================================
      DO 350 II = 1,NEQ
          ZR(JAFMU-1+II) = ZR(JAFMU-1+II) + ZR(JATMU-1+II)
          ZR(JATMU-1+II) = 0.D0
 350  CONTINUE
C ======================================================================
C --- STOCKAGE DE L'ETAT DE CONTACT DEFINITIF
C ======================================================================
 999  CONTINUE
C --- ATTENTE POINT FIXE
      IF ( NBLIAC.NE.NBLCIN ) THEN
        CTCFIX = .TRUE.
      ENDIF
      ZI(JCOCO+2) = NBLIAC
C ======================================================================
C --- AFFICHAGE FINAL
C ======================================================================
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1003) NBLIAC
        WRITE(IFM,*)'<CONTACT> <> LIAISONS FINALES '
        CALL CFIMP1(DEFICO,RESOCO,NOMA,NBLIAI,IFM)
      END IF
C
C --- SAUVEGARDE DES INFOS DE DIAGNOSTIC 
C
      CALL CFITER(RESOCO,'E','CONT',ITER  ,R8BID)
      CALL CFITER(RESOCO,'E','LIAC',NBLIAC,R8BID)
C
      CALL JEDEMA ()
C
 1000 FORMAT (' <CONTACT> <> NOMBRE DE LIAISONS POSSIBLES: ',I6)
 1003 FORMAT (' <CONTACT> <> NOMBRE DE LIAISONS CONTACT FINALES:',
     &       I6,')')
 1005 FORMAT (' <CONTACT> <> NOMBRE DE LIAISONS CONTACT INITIALES:',
     &       I6,')')
C
C ======================================================================
C
      END
