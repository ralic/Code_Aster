      SUBROUTINE FROGDP(DEFICO,RESOCO,LMAT  ,RESU  ,RESIGR,
     &                  DEPDEL)
C 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/09/2010   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*19 RESU,DEPDEL
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
      INTEGER      IBID,IER,JFRO11,JFRO12,ILIAC
      INTEGER      II,JJ,KK,ILIAI,JAPJFX,JAPJFY,JAPJEU
      INTEGER      JRESU,JMU,JATMU,NDIM
      INTEGER      JDEPDE,JDELT0,JDELTA,JLIAC
      INTEGER      NEQ,NBLIAC,NBLIAI,NBDDL
      INTEGER      LLIAC,JDECAL
      INTEGER      CFDISD
      INTEGER      JAPPTR,JAPCOE,JAPDDL,JNOCO,JMACO
      INTEGER      JAPCOF,JAFMU,LMAF1,JENAT,JFRO2,ITER
      INTEGER      NESMAX
      REAL*8       AJEUFX,AJEUFY,XF,XX,XK,XMU,VAL,XMU1
      REAL*8       BETA,VAL1,VAL2,R8BID,AJEU
      CHARACTER*14 NUMEDD,NUMACT,NUMAF1,NUMAF2,NUTEMP,NUFROT
      CHARACTER*19 AFMU,MAT,ENAT,FRO1,FRO2,MACT,MAF1,MAF2,MAFROT,MATEMP
      CHARACTER*19 LIAC,MU,ATMU,DELT0,DELTA
      CHARACTER*24 APJEFX,APJEFY,APJEU
      CHARACTER*24 APPOIN,APCOEF,APDDL
      CHARACTER*24 CONTNO,CONTMA,APCOFR
      CHARACTER*24 TACFIN
      INTEGER      JTACF
      INTEGER      CFMMVD,ZTACF 
      REAL*8       COEFPN,COEFPT,COEFTE
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
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CONTMA = DEFICO(1:16)//'.MAILCO'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APCOFR = RESOCO(1:14)//'.APCOFR'
      APJEFX = RESOCO(1:14)//'.APJEFX'
      APJEFY = RESOCO(1:14)//'.APJEFY'
      APDDL  = RESOCO(1:14)//'.APDDL'
      APJEU  = RESOCO(1:14)//'.APJEU'
      LIAC   = RESOCO(1:14)//'.LIAC'
      MU     = RESOCO(1:14)//'.MU'
      ATMU   = RESOCO(1:14)//'.ATMU'
      AFMU   = RESOCO(1:14)//'.AFMU'
      DELT0  = RESOCO(1:14)//'.DEL0'
      DELTA  = RESOCO(1:14)//'.DELT'
      ENAT   = RESOCO(1:14)//'.ENAT'
      FRO1   = RESOCO(1:14)//'.FRO1'
      FRO2   = RESOCO(1:14)//'.FRO2'
      MAFROT = RESOCO(1:14)//'.MAFR'
      MACT   = '&&FROGDP.MACT'
      MAF1   = '&&FROGDP.MAF1'
      MAF2   = '&&FROGDP.MAF2'
      MATEMP = '&&FROGDP.MATP'
      TACFIN = RESOCO(1:14)//'.TACFIN' 
      MAT      = ZK24(ZI(LMAT+1))(1:19)
C ======================================================================
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APCOFR,'L',JAPCOF)
      CALL JEVEUO(APJEFX,'E',JAPJFX)
      CALL JEVEUO(APJEFY,'E',JAPJFY)
      CALL JEVEUO(APDDL, 'L',JAPDDL)
      CALL JEVEUO(APJEU ,'L',JAPJEU)
      CALL JEVEUO(LIAC,  'E',JLIAC)
      CALL JEVEUO(MU,    'E',JMU)
      CALL JEVEUO(ATMU,  'E',JATMU)
      CALL JEVEUO(AFMU , 'E',JAFMU)
      CALL JEVEUO(DELT0, 'E',JDELT0)
      CALL JEVEUO(DELTA, 'E',JDELTA)
      CALL JEVEUO(RESU(1:19)//'.VALE'  ,'L',JRESU)
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      CALL JEVEUO(TACFIN,'L',JTACF )
      CALL DISMOI('F','NOM_NUME_DDL',MAT,'MATR_ASSE',IBID,NUMEDD,IER)
      ZTACF  = CFMMVD('ZTACF') 
C ======================================================================
C --- INITIALISATION DE VARIABLES
C --- NBLIAI : NOMBRE DE LIAISONS DE CONTACT
C --- NBLIAC : NOMBRE DE LIAISONS ACTIVES
C --- NEQ    : NOMBRE D'EQUATIONS DU MODELE
C --- ITEMAX : NOMBRE D'ITERATIONS DE CONTACT MAXI
C --- NESMAX : NOMBRE MAXI DE NOEUDS ESCLAVES
C              SERT AU DECALAGE DANS LES ROUTINES DE FROTTEMENT 3D
C --- INDFAC : INDICE DE DEBUT DE LA FACTORISATION
C --- INDIC  : 0  INITIALISATION,
C             +1 ON A RAJOUTE UNE LIAISON
C             -1 ON A ENLEVE UNE LIAISON
C --- SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
C              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR ENAT
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
C ======================================================================
      NBLIAI = CFDISD(RESOCO,'NBLIAI')
      NEQ    = CFDISD(RESOCO,'NEQ'   )
      NDIM   = CFDISD(RESOCO,'NDIM'  )
      NESMAX = CFDISD(RESOCO,'NESMAX')
      NBLIAC = CFDISD(RESOCO,'NBLIAC')   
      ITER   = 0
C ======================================================================
C --- CREATION DE DELTA0 = C-1B
C ======================================================================
      DO 1 II = 1, NEQ
         ZR(JDELTA-1+II) = ZR(JRESU-1+II)+ZR(JDEPDE-1+II)
 1    CONTINUE
C
C ======================================================================
C --- RECUPERATION DES JEUX NEGATIFS ET CREATION DU SECOND
C --- MEMBRE ATMU = -E_N*AT*JEU
C ======================================================================
C
      ILIAC = 1
      DO 50 ILIAI = 1,NBLIAI
         ZR(JMU-1+         ILIAI) = 0.D0
         ZR(JMU-1+3*NBLIAI+ILIAI) = 0.D0
         JDECAL = ZI(JAPPTR+ILIAI-1)
         NBDDL  = ZI(JAPPTR+ILIAI) - ZI(JAPPTR+ILIAI-1)
         AJEU   = ZR(JAPJEU+ILIAI-1)
         IF ( AJEU.LT.0.D0 ) THEN
           COEFPN = ZR(JTACF+ZTACF*(ILIAI-1)+1)
           ZR(JMU-1+ILIAC) = -AJEU*COEFPN
           CALL CALATM(NEQ,NBDDL,ZR(JMU-1+ILIAC),ZR(JAPCOE+JDECAL),
     &                 ZI(JAPDDL+JDECAL),ZR(JATMU))
           ILIAC = ILIAC + 1
         ENDIF         
 50   CONTINUE
      CALL ASSERT(NBLIAC.EQ.(ILIAC-1))
C
      IF (NBLIAC.EQ.0) THEN
        GOTO 999
      ENDIF
C
C ======================================================================
C --- CALCUL DES COEFFICIENTS DE LAGRANGE MU POUR LE FROTTEMENT
C ======================================================================
C
      DO 100 ILIAI = 1, NBLIAC
         AJEUFX = 0.D0
         AJEUFY = 0.D0
         LLIAC  = ZI(JLIAC-1+ILIAI)
         JDECAL = ZI(JAPPTR+LLIAC-1)
         NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
         CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &               ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
         CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
         VAL    = VAL1 + VAL2
         AJEUFX = ZR(JAPJFX-1+LLIAC) - VAL
         IF (NDIM.EQ.3) THEN
           CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                 ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
           CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                 ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
           VAL = VAL1 + VAL2
           AJEUFY = ZR(JAPJFY-1+LLIAC) - VAL
         ENDIF
         
         XK = ZR(JTACF+ZTACF*(LLIAC-1)+0)

         IF ( ZR(JMU-1+ILIAI) .GT. 0.D0) THEN
            XK = XK*ZR(JMU-1+ILIAI)
         ELSE
            XK = 0.D0
         ENDIF 

         COEFPT = ZR(JTACF+ZTACF*(LLIAC-1)+2)
         XF     = SQRT(COEFPT)
         XX     = SQRT(AJEUFX**2 + AJEUFY**2)

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
      DO 160 ILIAI = 1, NBLIAI
         CALL JEVEUO(JEXNUM(ENAT,ILIAI), 'E', JENAT )
         DO 150 KK = 1, NDLMAX        
           ZR(JENAT-1+KK) = 0.0D0
 150     CONTINUE
         AJEU   = ZR(JAPJEU+ILIAI-1)
         IF ( AJEU.LT.0.D0 ) THEN
           JDECAL = ZI(JAPPTR+ILIAI-1)
           NBDDL  = ZI(JAPPTR+ILIAI)-ZI(JAPPTR+ILIAI-1)
           COEFPN = ZR(JTACF+ZTACF*(ILIAI-1)+1)
           XMU1   = SQRT(COEFPN)
           CALL DAXPY(NBDDL,XMU1,ZR(JAPCOE+JDECAL),1,ZR(JENAT),1)
           ZR(JMU-1+2*NBLIAI+ILIAI) = 1.D0
         ENDIF

         CALL JELIBE(JEXNUM(ENAT,ILIAI))
 160  CONTINUE
C
C ======================================================================
C --- CREATION DE LA PSEUDO-MATRICE MAF1 = E_N*AT*A
C --- CETTE MATRICE NE SERT QU'AU CALCUL DU SECOND MEMBRE
C ======================================================================
C
      NMULT = 1
      NUMACT = '&&FROGDP.NUCT'
      CALL ATASMO(NEQ   ,ENAT  ,ZI(JAPDDL),ZI(JAPPTR),NUMEDD,MACT  ,'V',
     &            NBLIAI,NMULT ,NUMACT)
C
C ======================================================================
C --- CREATION DE LA MATRICE FRO1 = E_N*AT
C ======================================================================
C
      IF (NDIM.EQ.3) THEN
         DO 210 ILIAI = 1, NBLIAI
            CALL JEVEUO ( JEXNUM(FRO1,ILIAI)       ,   'E', JFRO11 )
            CALL JEVEUO ( JEXNUM(FRO1,ILIAI+NBLIAI),   'E', JFRO12 )
            DO 200 KK = 1, NDLMAX
               ZR(JFRO11-1+KK) = 0.0D0
               ZR(JFRO12-1+KK) = 0.0D0
 200        CONTINUE
            AJEU   = ZR(JAPJEU+ILIAI-1)
            IF ( AJEU.LT.0.D0 ) THEN
              JDECAL = ZI(JAPPTR+ILIAI-1)
              NBDDL  = ZI(JAPPTR+ILIAI)   - ZI(JAPPTR+ILIAI-1)
              XMU    = ZR(JMU-1+3*NBLIAI+ILIAI)
              CALL DAXPY(NBDDL,XMU,ZR(JAPCOF+JDECAL),1,ZR(JFRO11),1)
              CALL DAXPY(NBDDL,XMU,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                           1,ZR(JFRO12),1)
            ENDIF
            CALL JELIBE(JEXNUM(FRO1,ILIAI)       )
            CALL JELIBE(JEXNUM(FRO1,ILIAI+NBLIAI))
 210     CONTINUE
      ELSE
         DO 216 ILIAI = 1, NBLIAI
            CALL JEVEUO ( JEXNUM(FRO1,ILIAI)       , 'E', JFRO11 )
            DO 202 KK = 1, NDLMAX
               ZR(JFRO11-1+KK) = 0.0D0
 202        CONTINUE
            AJEU   = ZR(JAPJEU+ILIAI-1)
            IF ( AJEU.LT.0.D0 ) THEN
              JDECAL = ZI(JAPPTR+ILIAI-1)
              NBDDL  = ZI(JAPPTR+ILIAI)   - ZI(JAPPTR+ILIAI-1)
              XMU  = ZR(JMU-1+3*NBLIAI+ILIAI)
              CALL DAXPY(NBDDL,XMU,ZR(JAPCOF+JDECAL),1,ZR(JFRO11),1)
            ENDIF
        
            CALL JELIBE(JEXNUM(FRO1,ILIAI))
 216     CONTINUE
      ENDIF
C
C ======================================================================
C --- CREATION DE LA MATRICE MAF1 = E_N*AT*A
C ======================================================================
C
      NMULT = NDIM - 1
      NUMAF1 = '&&FROGDP.NUF1'
      CALL ATASMO(NEQ   ,FRO1  ,ZI(JAPDDL),ZI(JAPPTR),NUMEDD,MAF1  ,'V',
     &            NBLIAI,NMULT ,NUMAF1)
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
      DO 300 ILIAI = 1, NBLIAI
         CALL JEVEUO ( JEXNUM(FRO2,ILIAI), 'E', JFRO2 )
         DO 310 KK = 1, NDLMAX
            ZR(JFRO2-1+KK) = 0.0D0
 310     CONTINUE
           AJEU   = ZR(JAPJEU+ILIAI-1)
           IF ( AJEU.LT.0.D0 ) THEN
             AJEUFX = 0.D0
             AJEUFY = 0.D0
             JDECAL = ZI(JAPPTR+ILIAI-1)
             NBDDL  = ZI(JAPPTR+ILIAI) - ZI(JAPPTR+ILIAI-1)
             CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                   ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
             CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                   ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
             VAL = VAL1 + VAL2
             AJEUFX = ZR(JAPJFX-1+ILIAI)-VAL
             IF (NDIM.EQ.3) THEN
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                      ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL1)
               CALL CALADU (NEQ,NBDDL,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                   ZI(JAPDDL+JDECAL),ZR(JDEPDE),VAL2)
               VAL = VAL1 + VAL2
               AJEUFY = ZR(JAPJFY-1+ILIAI)-VAL
             ENDIF
             XK = ZR(JTACF+ZTACF*(ILIAI-1)+0)
             DO 320 JJ = 1,NBLIAC
               LLIAC = ZI(JLIAC-1+JJ)
               IF(LLIAC.EQ.ILIAI) THEN
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
             COEFPT = ZR(JTACF+ZTACF*(ILIAI-1)+2)
             XF     = SQRT(COEFPT)
             XX = SQRT( AJEUFX**2 + AJEUFY**2 )
             IF ( ZR(JMU-1+2*NBLIAI+ILIAI).NE.0.D0) THEN
               IF ( XX .LE. XK/XF**2 ) THEN
                  BETA = 0.D0
               ELSE
                  BETA = SQRT(1.D0/(XK*XX))
               ENDIF
             ELSE
               BETA = 0.D0
             ENDIF
             IF ( RESIGR .GE. 1.0D-03 ) THEN
               COEFTE = ZR(JTACF+ZTACF*(ILIAI-1)+3)
               XMU    = SQRT(COEFTE)
               BETA   = BETA*XMU
             ENDIF
             CALL CALAPR(NBDDL,BETA,ZR(JAFMU),
     &                              ZI(JAPDDL+JDECAL),ZR(JFRO2))
             ZR(JMU-1+2*NBLIAI+ILIAI) = 1.D0
           ELSE
             ZR(JMU-1+2*NBLIAI+ILIAI) = 0.D0
           ENDIF
 305     CONTINUE
         CALL JELIBE(JEXNUM(FRO2,ILIAI))
 300  CONTINUE
C
C ======================================================================
C --- CREATION DE LA SECONDE PARTIE DE LA MATRICE DE FROTTEMENT MAF2
C ======================================================================
C
      NMULT = 1
      NUMAF2 = '&&FROGDP.NUF2'
      CALL ATASMO(NEQ   ,FRO2  ,ZI(JAPDDL),ZI(JAPPTR),NUMEDD,MAF2  ,'V',
     &            NBLIAI,NMULT ,NUMAF2)
C
C ======================================================================
C --- CALCUL DE LA MATRICE TANGENTE MAFROT = MACT+MAF1+MAF2
C ======================================================================
C
      NUTEMP = '&&FROGDP.NUTP'
      NUFROT = '&&FROGDP.NUFR'
      CALL CFFROT(MAF1,'-',MAF2  ,MATEMP,NUTEMP)
      CALL CFFROT(MACT,'+',MATEMP,MAFROT,NUFROT)
C ======================================================================
C --- CALCUL DES FORCES DE CONTACT (AT.MU) ET FROTTEMENT (AF.MU)
C ======================================================================
      DO 350 II = 1,NEQ
          ZR(JAFMU-1+II) = ZR(JAFMU-1+II) + ZR(JATMU-1+II)
          ZR(JATMU-1+II) = 0.D0
 350  CONTINUE

 999  CONTINUE
C      
C --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
C
      CALL CFECRD(RESOCO,'NBLIAC',NBLIAC)
C
C --- SAUVEGARDE DES INFOS DE DIAGNOSTIC 
C
      CALL CFITER(RESOCO,'E','CONT',ITER  ,R8BID )
      CALL CFITER(RESOCO,'E','LIAC',NBLIAC,R8BID )
C
      CALL JEDEMA()
C
C ======================================================================
C
      END
