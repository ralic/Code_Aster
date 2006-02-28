      SUBROUTINE ALGOCP(DEFICO,RESOCO,LMAT,NOMA,
     &                  RESU,DEPTOT,LREAC,DEPDEL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/02/2006   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT     NONE
      LOGICAL      LREAC(4)
      INTEGER      LMAT
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO
      CHARACTER*24 RESOCO
      CHARACTER*24 RESU
      CHARACTER*24 DEPTOT
      CHARACTER*24 DEPDEL
C
C ======================================================================
C ROUTINE APPELEE PAR : CFALGO
C ======================================================================
C
C ALGO DE CONTACT
C
C ALGO. POUR CONTACT	: PENALISATION
C ALGO. POUR FROTTEMENT : SANS
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
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C                'E':  RESOCO(1:14)//'.APJEU'
C                'E':  RESOCO(1:14)//'.LIAC'
C                'E':  RESOCO(1:14)//'.MU'
C                'E':  RESOCO(1:14)//'.DEL0'
C                'E':  RESOCO(1:14)//'.DELT'
C                'E':  RESOCO(1:14)//'.COCO'
C                'E':  RESOCO(1:14)//'.CM1A'
C                'E':  RESOCO(1:14)//'.AFMU'
C                'E':  RESOCO(1:14)//'.ATMU'
C IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  NOMA   : NOM DU MAILLAGE
C VAR RESU   : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS DEPTOT
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C IN  DEPTOT : DEPLACEMENT TOTAL OBTENU A L'ISSUE DE L'ITERATION
C               DE NEWTON PRECEDENTE
C IN  LREAC  : ETAT DU CONTACT
C              (1) = TRUE  SI REACTUALISATION A FAIRE
C              (2) = TRUE  SI ATTENTE POINT FIXE CONTACT
C              (3) = TRUE  SI METHODE CONTINUE
C              (4) = TRUE  SI MODELISATION DU CONTACT
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
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
      CHARACTER*24 APPARI,ATMU,AFMU,DELT0,DELTA,APCOEF,APDDL
      INTEGER      JAPPAR,JATMU,JAFMU,JDELT0,JDELTA,JAPCOE,JAPDDL
      CHARACTER*24 APJEU,MU,CM1A,PENAL,APMEMO
      INTEGER      JAPJEU,JMU,JCM1A,IPENA,JCOCO,JAPMEM
      CHARACTER*24 CONTNO,CONTMA,LIAC,NOZOCO,COCO,APPOIN
      INTEGER      JNOCO,JMACO,JLIAC,JZOCO,JAPPTR
      INTEGER      NESCL,NBLIAI,NEQ,NBLIAC,AJLIAI,SPLIAI,INDIC
      INTEGER      NBDDL,NBLCIN,NBLIG,LLF,LLF1,LLF2
      CHARACTER*19 MAFROT
      INTEGER      IBID,II,IER,KK,ITER
      CHARACTER*19 MAT
      CHARACTER*14 NUMEDD
      CHARACTER*1  TYPEAJ
      CHARACTER*2  TYPEC0
      INTEGER      JRESU,JDEPP,JDEPDE
      INTEGER      JDECAL,POSIT
      REAL*8       VAL,XMU,R8BID
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
        WRITE (IFM,*) '<CONTACT> <> ALGO_FROTTEMENT: SANS'
      ENDIF
      CALL JEMARQ ()
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
      APPARI   = RESOCO(1:14)//'.APPARI'
      CONTNO   = DEFICO(1:16)//'.NOEUCO'
      CONTMA   = DEFICO(1:16)//'.MAILCO'
      NOZOCO   = DEFICO(1:16)//'.NOZOCO'
      APPOIN   = RESOCO(1:14)//'.APPOIN'
      APCOEF   = RESOCO(1:14)//'.APCOEF'
      APJEU    = RESOCO(1:14)//'.APJEU'
      APMEMO   = RESOCO(1:14)//'.APMEMO'
      APDDL    = RESOCO(1:14)//'.APDDL'
      LIAC     = RESOCO(1:14)//'.LIAC'
      MU       = RESOCO(1:14)//'.MU'
      ATMU     = RESOCO(1:14)//'.ATMU'
      AFMU     = RESOCO(1:14)//'.AFMU'
      DELT0    = RESOCO(1:14)//'.DEL0'
      DELTA    = RESOCO(1:14)//'.DELT'
      CM1A     = RESOCO(1:14)//'.CM1A'
      MAFROT   = RESOCO(1:8)//'.MAFR'
      PENAL    = DEFICO(1:16)//'.PENAL'
      COCO     = RESOCO(1:14)//'.COCO'
      MAT      = ZK24(ZI(LMAT+1))
C ======================================================================
      CALL JEVEUO (CONTNO,'L',JNOCO)
      CALL JEVEUO (NOZOCO,'L',JZOCO)
      CALL JEVEUO (CONTMA,'L',JMACO)
      CALL JEVEUO (APPOIN,'L',JAPPTR)
      CALL JEVEUO (APCOEF,'L',JAPCOE)
      CALL JEVEUO (APJEU, 'E',JAPJEU)
      CALL JEVEUO (APMEMO,'L',JAPMEM)
      CALL JEVEUO (APDDL, 'L',JAPDDL)
      CALL JEVEUO (LIAC,  'E',JLIAC)
      CALL JEVEUO (MU,    'E',JMU)
      CALL JEVEUO (ATMU,  'E',JATMU)
      CALL JEVEUO (AFMU , 'E',JAFMU)
      CALL JEVEUO (DELT0, 'E',JDELT0)
      CALL JEVEUO (DELTA, 'E',JDELTA)
      CALL JEVEUO (RESU(1:19)//'.VALE'  ,'L',JRESU)
      CALL JEVEUO (DEPTOT(1:19)//'.VALE','L',JDEPP)
      CALL JEVEUO (DEPDEL(1:19)//'.VALE', 'L', JDEPDE)
      CALL JEVEUO (COCO,'E',JCOCO)
      CALL JEVEUO (PENAL,'L',IPENA)
      CALL JEVEUO (APPARI,'L',JAPPAR)
C ======================================================================
C --- INITIALISATION DE VARIABLES
C --- NESCL  : NOMBRE DE NOEUDS ESCLAVES SUSCEPTIBLES D'ETRE EN CONTACT
C --- NBLIAI : NOMBRE DE LIAISONS DE CONTACT
C --- NEQ    : NOMBRE D'EQUATIONS DU MODELE
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
C                          -- ZERO --
C --- LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               PREMIERE DIRECTION (EN 3D)
C                          -- ZERO --
C --- LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               SECONDE DIRECTION (EN 3D)
C                          -- ZERO --
C ======================================================================
      NESCL  = ZI(JAPPAR)
      NBLIAI = NESCL
      NEQ    = ZI(LMAT+2)
      ITER   = 0
      LLF    = 0
      LLF1   = 0
      LLF2   = 0
      CALL DISMOI ('F','NOM_NUME_DDL',MAT,'MATR_ASSE',IBID,NUMEDD,IER)
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
     &            IBID,NBLIAC,IBID,IBID,IBID)

C ======================================================================
C --- RECOPIE DANS DELT0 DU CHAMP DE DEPLACEMENTS OBTENU SANS
C --- TRAITER LE CONTACT (LE DDEPLA DONNE PAR STAT_NON_LINE)
C --- CREATION DE DELTA0 = C-1B
C --- INITIALISATION DE AFMU CAR LES DONNEES DE AFMU SONT
C --- NECESSAIRE POUR LE CALCUL DE LA MATRICE TANGENTE QUI SE FAIT
C --- A L'AIDE DU VECTEUR AFMU
C ======================================================================
      DO 1 II = 1, NEQ
         ZR(JAFMU+II-1)  = 0.0D0
         ZR(JDELT0-1+II) = ZR(JRESU-1+II)
         ZR(JDELTA-1+II) = ZR(JRESU-1+II)+ZR(JDEPDE-1+II)
 1    CONTINUE
C
C ======================================================================
C
      NBLCIN = NBLIAC
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1000) NBLIAI
        WRITE(IFM,1005) NBLIAC
      ENDIF
C
      NBLIAC = 0
C ======================================================================
C --- RECUPERATION DES JEUX NEGATIFS ET CREATION DU SECOND
C --- MEMBRE AFMU = -E_N*AT*JEU
C ======================================================================

      DO 50 II = 1,NBLIAI
         ZR(JMU-1+II) = 0.0D0
         JDECAL = ZI(JAPPTR+II-1)
         NBDDL  = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
         CALL JEVEUO ( JEXNUM(CM1A,II), 'E', JCM1A )
         DO 20 KK = 1, NEQ
            ZR(JCM1A-1+KK) = 0.0D0
 20      CONTINUE

         CALL CALADU (NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL)
         IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
           ZR(JAPJEU+II-1) = ZR(JAPJEU+II-1) - VAL
           IF ( ZR(JAPJEU+II-1).LT.0.0D0 ) THEN
             POSIT  = NBLIAC + 1
             CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,LLF1,LLF2,
     &                RESOCO,TYPEAJ,POSIT,II,TYPEC0)
             IF (NIV.GE.2) THEN
               CALL CFIMP2(IFM,NOMA,II,TYPEC0,TYPEAJ,'ALG',
     &                ZR(JAPJEU+II-1),JAPPAR,JNOCO,JMACO)
             END IF
             XMU    = 1.D0
             CALL CALATM(NEQ,NBDDL,XMU,ZR(JAPCOE+JDECAL),
     &                   ZI(JAPDDL+JDECAL),ZR(JCM1A))
             ZR(JMU-1+NBLIAC) = -ZR(JAPJEU+II-1)*ZR(IPENA-1+2*II-1)
             CALL DAXPY(NEQ,ZR(JMU-1+NBLIAC),ZR(JCM1A),1,ZR(JAFMU),1)
           ENDIF
         ENDIF
         CALL JELIBE(JEXNUM(CM1A,II))
 50   CONTINUE
C
      IF (NBLIAC.EQ.0) THEN
        GOTO 999
      ENDIF
C ======================================================================
C --- CREATION DE LA MATRICE DE CONTACT CM1A = E_N*AT
C ======================================================================
      DO 210 II = 1, NBLIAI
         CALL JEVEUO ( JEXNUM(CM1A,II), 'E', JCM1A )
         DO 200 KK = 1, NEQ
            ZR(JCM1A-1+KK) = 0.0D0
 200     CONTINUE
         IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
           IF ( ZR(JAPJEU+II-1).LT.0.0D0 ) THEN
             JDECAL = ZI(JAPPTR+II-1)
             NBDDL  = ZI(JAPPTR+II)-ZI(JAPPTR+II-1)
             XMU    = SQRT(ZR(IPENA-1+2*II-1))
             CALL CALATM(NEQ,NBDDL,XMU,ZR(JAPCOE+JDECAL),
     &                    ZI(JAPDDL+JDECAL),ZR(JCM1A))
           ENDIF
         ENDIF
         CALL JELIBE(JEXNUM(CM1A,II))
 210  CONTINUE
C ======================================================================
C --- CREATION DE MAFROT = E_N*AT*A
C ======================================================================
      NBLIG = NBLIAI
      CALL ATASMO(CM1A,NUMEDD,MAFROT,'V',NBLIG)
      CALL DETRSD('MATR_ASSE', MAT   )
C ======================================================================
C --- CALCUL DES FORCES DE CONTACT (AT.MU)
C ======================================================================
      DO 211 II = 1, NEQ
         ZR(JATMU+II-1)  = 0.0D0
 211  CONTINUE
C ======================================================================
C --- STOCKAGE DE L'ETAT DE CONTACT DEFINITIF
C ======================================================================
C --- ATTENTE POINT FIXE
      IF ( NBLIAC.NE.NBLCIN ) THEN
        LREAC(2) = .TRUE.
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
 999  CONTINUE
C ======================================================================
C --- SAUVEGARDE DES INFOS DE DIAGNOSTIC (NOMBRE D'ITERATIONS)
C ======================================================================
      CALL CFITER(RESOCO,'E','ITER',ITER,R8BID)
C
      CALL JEDEMA()
C
 1000 FORMAT (' <CONTACT> <> NOMBRE DE LIAISONS POSSIBLES: ',I6)
 1003 FORMAT (' <CONTACT> <> NOMBRE DE LIAISONS CONTACT FINALES:',
     &       I6,')')
 1005 FORMAT (' <CONTACT> <> NOMBRE DE LIAISONS CONTACT INITIALES:',
     &       I6,')')
C ======================================================================

      END
