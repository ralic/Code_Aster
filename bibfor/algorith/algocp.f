      SUBROUTINE ALGOCP(RESOCO,LMAT  ,RESU  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C
      IMPLICIT     NONE
      INTEGER      LMAT
      CHARACTER*24 RESOCO
      CHARACTER*19 RESU
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
C
C ALGO. POUR CONTACT    : PENALISATION
C ALGO. POUR FROTTEMENT : SANS
C
C ----------------------------------------------------------------------
C
C
C RESOLUTION DE : C.DU + K ATA.DU  = F- K ATA(E-U)
C                 A(U+DU)      <= E (= POUR LES LIAISONS ACTIVES)
C
C AVEC E = JEU COURANT (CORRESPONDANT A U/I/N)
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
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C VAR RESU   : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS L'ITERATION
C              DE NEWTON PRECEDENTE
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C
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
      CHARACTER*24 ATMU,AFMU,APCOEF,APDDL,APJEU
      INTEGER      JATMU,JAFMU,JAPCOE,JAPDDL,JAPJEU
      CHARACTER*24 MU,ENAT
      INTEGER      JMU,JENAT
      CHARACTER*24 APPOIN
      INTEGER      JAPPTR
      INTEGER      NBLIAI,NEQ,NBLIAC
      INTEGER      NBDDL
      CHARACTER*19 MAFROT
      INTEGER      IBID,ILIAI,IER,IRET,KK,ITER,ILIAC
      INTEGER      CFDISD
      CHARACTER*19 MAT
      CHARACTER*14 NUMEDD,NUFROT
      INTEGER      JRESU
      INTEGER      JDECAL
      REAL*8       XMU,R8BID,AJEU
      CHARACTER*24 TACFIN
      INTEGER      JTACF
      INTEGER      CFMMVD,ZTACF
      REAL*8       COEFPN
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
C DELT0  : INCREMENT DE DEPLACEMENT DEPUIS LA DERNIERE ITERATION DE
C          NEWTON SANS TRAITER LE CONTACT. C'EST C-1.F.
C ======================================================================
      CALL INFDBG('CONTACT',IFM,NIV)
      IF(NIV.GE.2) THEN
        WRITE (IFM,*) '<CONTACT> <> ALGO_CONTACT   : PENALISATION'
        WRITE (IFM,*) '<CONTACT> <> ALGO_FROTTEMENT: SANS'
      ENDIF
      CALL JEMARQ ()
C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
C LIAC   : LISTE DES INDICES DES LIAISONS ACTIVES
C MU     : MULTIPLICATEURS DE LAGRANGE DU CONTACT (DOIVENT ETRE > 0)
C ATMU   : FORCES DE CONTACT
C CM1A   : C-1.AT AVEC C MATRICE DE RIGIDITE TANGENTE,
C          ET A MATRICE DE CONTACT (AT SA TRANSPOSEE)
C ======================================================================
      APPOIN = RESOCO(1:14)//'.APPOIN'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APDDL  = RESOCO(1:14)//'.APDDL'
      APJEU  = RESOCO(1:14)//'.APJEU'
      MU     = RESOCO(1:14)//'.MU'
      ATMU   = RESOCO(1:14)//'.ATMU'
      AFMU   = RESOCO(1:14)//'.AFMU'
      ENAT   = RESOCO(1:14)//'.ENAT'
      MAFROT = RESOCO(1:14)//'.MAFR'
      TACFIN = RESOCO(1:14)//'.TACFIN'
      MAT    = ZK24(ZI(LMAT+1))(1:19)
C ======================================================================
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APDDL, 'L',JAPDDL)
      CALL JEVEUO(APJEU ,'L',JAPJEU)
      CALL JEVEUO(MU,    'E',JMU)
      CALL JEVEUO(ATMU,  'E',JATMU)
      CALL JEVEUO(AFMU , 'E',JAFMU)
      CALL JEVEUO(RESU(1:19)//'.VALE'  ,'L',JRESU)
      CALL JEVEUO(TACFIN,'E',JTACF )

      ZTACF  = CFMMVD('ZTACF')
C ======================================================================
C --- INITIALISATION DE VARIABLES
C --- NESCL  : NOMBRE DE NOEUDS ESCLAVES SUSCEPTIBLES D'ETRE EN CONTACT
C --- NBLIAI : NOMBRE DE LIAISONS DE CONTACT
C --- NEQ    : NOMBRE D'EQUATIONS DU MODELE
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
      NBLIAC = CFDISD(RESOCO,'NBLIAC')
      ITER   = 0
      CALL DISMOI ('F','NOM_NUME_DDL',MAT,'MATR_ASSE',IBID,NUMEDD,IER)

C ======================================================================
C --- RECUPERATION DES JEUX NEGATIFS ET CREATION DU SECOND
C --- MEMBRE AFMU = -E_N*AT*JEU
C ======================================================================

      ILIAC = 1
      DO 50 ILIAI = 1,NBLIAI
         ZR(JMU-1+ILIAI) = 0.0D0
         JDECAL = ZI(JAPPTR+ILIAI-1)
         NBDDL  = ZI(JAPPTR+ILIAI) - ZI(JAPPTR+ILIAI-1)
         AJEU   = ZR(JAPJEU+ILIAI-1)
         IF ( AJEU.LT.0.D0 ) THEN
           COEFPN = ZR(JTACF+ZTACF*(ILIAI-1)+1)
           ZR(JMU-1+ILIAC) = -AJEU*COEFPN
           CALL CALATM(NEQ,NBDDL,ZR(JMU-1+ILIAC),ZR(JAPCOE+JDECAL),
     &                 ZI(JAPDDL+JDECAL),ZR(JAFMU))
           ILIAC = ILIAC + 1
         ENDIF
 50   CONTINUE
      CALL ASSERT(NBLIAC.EQ.(ILIAC-1))
C
      IF (NBLIAC.EQ.0) THEN
        GOTO 999
      ENDIF
C ======================================================================
C --- CREATION DE LA MATRICE DE CONTACT ENAT = E_N*AT
C ======================================================================
      DO 210 ILIAI = 1, NBLIAI
         CALL JEVEUO ( JEXNUM(ENAT,ILIAI), 'E', JENAT )
         DO 200 KK = 1, NDLMAX
            ZR(JENAT-1+KK) = 0.0D0
 200     CONTINUE
         AJEU   = ZR(JAPJEU+ILIAI-1)
         IF ( AJEU.LT.0.D0 ) THEN
           JDECAL = ZI(JAPPTR+ILIAI-1)
           NBDDL  = ZI(JAPPTR+ILIAI)-ZI(JAPPTR+ILIAI-1)
           COEFPN = ZR(JTACF+ZTACF*(ILIAI-1)+1)
           XMU    = SQRT(COEFPN)
           CALL DAXPY(NBDDL,XMU,ZR(JAPCOE+JDECAL),1,ZR(JENAT),1)
         ENDIF
         CALL JELIBE(JEXNUM(ENAT,ILIAI))
 210  CONTINUE
C ======================================================================
C --- CREATION DE MAFROT = E_N*AT*A
C ======================================================================
C
C --- DESTRUCTION ANCIENNE MATRICE MAFROT
C
      CALL EXISD('MATR_ASSE',MAFROT,IRET)
      IF (IRET.NE.0) THEN
         CALL DISMOI('F','NOM_NUME_DDL',MAFROT,'MATR_ASSE',IBID,NUFROT,
     &               IER)
         CALL DETRSD('NUME_DDL' ,NUFROT)
         CALL DETRSD('MATR_ASSE',MAFROT)
      ENDIF
C
      NUFROT = '&&ALGOCP.NUFR'
      NMULT = 1
      CALL ATASMO(NEQ   ,ENAT  ,ZI(JAPDDL),ZI(JAPPTR),NUMEDD,MAFROT,'V',
     &            NBLIAI,NMULT ,NUFROT)
C ======================================================================
C --- CALCUL DES FORCES DE CONTACT (AT.MU)
C ======================================================================
      DO 211 ILIAI = 1, NEQ
         ZR(JATMU+ILIAI-1)  = 0.D0
 211  CONTINUE
C ======================================================================
C --- STOCKAGE DE L'ETAT DE CONTACT DEFINITIF
C ======================================================================
 999  CONTINUE
C
C --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
C
      CALL CFECRD(RESOCO,'NBLIAC',NBLIAC)
C
C --- SAUVEGARDE DES INFOS DE DIAGNOSTIC
C
      CALL CFITER(RESOCO,'E','CONT',ITER  ,R8BID)
      CALL CFITER(RESOCO,'E','LIAC',NBLIAC,R8BID)
C
      CALL JEDEMA()
C ======================================================================

      END
