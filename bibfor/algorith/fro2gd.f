       SUBROUTINE FRO2GD(SDSTAT,DEFICO,RESOCO,LMAT  ,LDSCON,
     &                   NOMA  ,DEPDEL,RESU  ,CTCCVG)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/07/2011   AUTEUR ABBAS M.ABBAS 
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
C TOLE CRP_20
C
      IMPLICIT     NONE
      INTEGER      LMAT,LDSCON
      CHARACTER*8  NOMA
      CHARACTER*24 SDSTAT
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*19 RESU,DEPDEL
      INTEGER      CTCCVG
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
C
C ALGO. POUR CONTACT    : DUALISATION (LAGRANGIEN)
C ALGO. POUR FROTTEMENT : DUALISATION (LAGRANGIEN 2D)
C
C ----------------------------------------------------------------------
C
C        
C RESOLUTION DE : C.DU + ACT.AC.MUC + ASGT.ASG.MUSG + AGT.AG.MUG = F
C                 AC. (U+DU)      <= E  (POUR LES LIAISONS ACTIVES)
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
C IN  SDSTAT : SD STATISTIQUES
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  LDSCON : DESCRIPTEUR DE LA MATRICE -A.C-1.AT
C IN  NOMA   : NOM DU MAILLAGE
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C VAR RESU   : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS L'ITERATION
C              DE NEWTON PRECEDENTE
C                EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C OUT CTCCVG : CODE RETOUR CONTACT DISCRET
C                0 - OK
C                1 - NOMBRE MAXI D'ITERATIONS
C                2 - MATRICE SINGULIERE
C
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------
C
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
      INTEGER      CFDISI,CFDISD
      LOGICAL      DELPOS,LELPIV
      INTEGER      IER,IFM,NIV,NDECI,ISINGU,NPVNEG,ITEMAX
      INTEGER      JJ,KK,IOTE,ILIDEB,ILIFIN
      INTEGER      ILIAI,ILIAC,NUMIN,ITEMUL,ISTO
      INTEGER      JRESU,NDIM,NEQMAX,ITER
      INTEGER      NEQ,NBLIAC,NBLIAI,INDIC,INDFAC
      INTEGER      LLIAC,JDECAL,NBDDL
      INTEGER      LLF1,LLF2,NESMAX,BTOTAL
      INTEGER      LLF,LFMIN
      INTEGER      AJLIAI,SPLIAI,POSIT,COMPT0
      REAL*8       R8MAEM,R8PREM,R8MIEM,AJEU,RHO,RHORHO,AADELT
      REAL*8       VAL,ZMU,XFORC,XJVMAX
      REAL*8       AJEUFX,XPDT,XK,XCOMP,XCOS,X1
      COMPLEX*16   CBID
      CHARACTER*1  TYPEAJ
      CHARACTER*2  TYPEC0, TYPEF0
      CHARACTER*19 LIAC,LIOT,MU,DELT0,DELTA,AFMU,TYPL
      INTEGER      JLIAC,JLIOT,JMU,JDELT0,JDELTA,JAFMU,JTYPL
      CHARACTER*24 APPOIN,APCOEF,APJEU,APDDL
      CHARACTER*24 APCOFR,APJEFX
      INTEGER      JAPPTR,JAPCOE,JAPJEU,JAPDDL
      INTEGER      JAPCOF,JAPJFX
      CHARACTER*24 TACFIN
      INTEGER      JTACF
      INTEGER      CFMMVD,ZTACF      
C
C ======================================================================
C
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
      CALL INFDBG('CONTACT',IFM,NIV)
      IF(NIV.GE.2) THEN
        WRITE (IFM,*) '<CONTACT> <> ALGO_CONTACT   : DUALISATION'
        WRITE (IFM,*) '<CONTACT> <> ALGO_FROTTEMENT: DUALISATION (2D)'
      ENDIF
      CALL JEMARQ ()
C 
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C 
      APPOIN   = RESOCO(1:14)//'.APPOIN'
      APCOEF   = RESOCO(1:14)//'.APCOEF'
      APCOFR   = RESOCO(1:14)//'.APCOFR'
      APJEU    = RESOCO(1:14)//'.APJEU'
      APJEFX   = RESOCO(1:14)//'.APJEFX'
      APDDL    = RESOCO(1:14)//'.APDDL'
      LIAC     = RESOCO(1:14)//'.LIAC'
      LIOT     = RESOCO(1:14)//'.LIOT'
      MU       = RESOCO(1:14)//'.MU'
      AFMU     = RESOCO(1:14)//'.AFMU'
      DELT0    = RESOCO(1:14)//'.DEL0'
      DELTA    = RESOCO(1:14)//'.DELT'
      TYPL     = RESOCO(1:14)//'.TYPL'
      TACFIN   = RESOCO(1:14)//'.TACFIN'       
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APCOFR,'L',JAPCOF)
      CALL JEVEUO(APJEU, 'E',JAPJEU)
      CALL JEVEUO(APJEFX,'E',JAPJFX)
      CALL JEVEUO(APDDL, 'L',JAPDDL)
      CALL JEVEUO(TYPL  ,'L',JTYPL)
      CALL JEVEUO(LIAC,  'E',JLIAC)
      CALL JEVEUO(LIOT,  'E',JLIOT)
      CALL JEVEUO(MU,    'E',JMU)
      CALL JEVEUO(AFMU , 'E',JAFMU)
      CALL JEVEUO(DELT0, 'E',JDELT0)
      CALL JEVEUO(DELTA, 'E',JDELTA)
      CALL JEVEUO(TACFIN,'L',JTACF )      
      CALL JEVEUO(RESU(1:19)//'.VALE'  ,'E',JRESU)
      ZTACF  = CFMMVD('ZTACF')      
C ======================================================================
C --- INITIALISATION DE VARIABLES
C --- NBLIAI : NOMBRE EFFECTIF DE LIAISONS DE CONTACT
C --- NEQ    : NOMBRE D'EQUATIONS DU MODELE
C --- ITEMUL : NOMBRE PAR LEQUEL IL FAUT MULTIPLIER LE NOMBRE DE
C              LIAISONS DE CONTACT POUR OBTENIR LE NOMBRE MAXI
C              D'ITERATIONS DANS L'ALGO ITEMAX=ITEMUL*NBLIAI
C --- ISTO   : ACTION STOP_SINGULIER='OUI' OU 'NON'
C --- ITEMAX : NOMBRE D'ITERATIONS DE CONTACT DANS L'ALGO
C --- XJVMAX : VALEUR MAXI DU PIVOT DE LA MATRICE DE CONTACT
C --- NESMAX : NOMBRE MAXI DE NOEUDS ESCLAVES
C              SERT AU DECALAGE DANS LES ROUTINES DE FROTTEMENT 3D
C --- INDFAC : INDICE DE DEBUT DE LA FACTORISATION
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
C --- LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               PREMIERE DIRECTION (EN 3D)
C --- LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               SECONDE DIRECTION (EN 3D)
C ======================================================================
      NBLIAI = CFDISD(RESOCO,'NBLIAI')
      NEQ    = CFDISD(RESOCO,'NEQ'   )
      NDIM   = CFDISD(RESOCO,'NDIM'  )
      ITEMUL = CFDISI(DEFICO,'ITER_CONT_MULT')
      ITEMAX = ITEMUL*NBLIAI
      ISTO   = CFDISI(DEFICO,'STOP_SINGULIER')
      NESMAX = CFDISD(RESOCO,'NESMAX')
      NBLIAC = CFDISD(RESOCO,'NBLIAC')      
      LLF    = CFDISD(RESOCO,'LLF'   )
      LLF1   = CFDISD(RESOCO,'LLF1'  )
      LLF2   = CFDISD(RESOCO,'LLF2'  )
      AJLIAI = CFDISD(RESOCO,'AJLIAI')
      SPLIAI = CFDISD(RESOCO,'SPLIAI')       
      IF (NBLIAC.GT.0) THEN
        INDIC  = 1
      ELSE 
        INDIC  = 0  
      ENDIF
      INDFAC = 1     
      TYPEAJ = 'A'
      TYPEC0 = 'C0'
      TYPEF0 = 'F0'
      ITER   = 0
      XJVMAX = 0.D0
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1001) ITEMAX
      ENDIF
C
C ======================================================================
C                    REPRISE DE LA BOUCLE PRINCIPALE
C ======================================================================
C
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
        CTCCVG = 1
        GOTO 999
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
     &              LLF   ,LLF1  ,LLF2  ,INDFAC,NESMAX,
     &              DEFICO,RESOCO,LMAT  ,NBLIAI,XJVMAX)
C
C --- ELIMINATION DES PIVOTS NULS
C
        CALL ELPIV2(XJVMAX,NDIM  ,INDIC ,NBLIAC,AJLIAI,
     &              SPLIAI,LLF   ,LLF1  ,LLF2  ,NOMA  ,
     &              DEFICO,RESOCO)
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
C --- SI ON ENLEVE LA DERNIERE LIAISON (INDFAC > NBLIAC),PAS BESOIN DE
C --- REFACTORISER : L'INSTRUCTION ZI(LDSCON+2) = NBLIAC ECRITE PLUS
C --- LOIN FERA QUE RLDLGG PRENDRA LA BONNE TAILLE DE MATRICE, QUI
C --- EST DEJA FACTORISEE (SI ON REFACTORISAIT A PARTIR DE 1, ON
C --- FACTORISERAIT LA FACTORISEE, CE QUI EST GENANT, CAR FACTORISATION
C --- EN PLACE)
C ======================================================================
C 
        IF (INDFAC.LE.(NBLIAC+LLF)) THEN
          IF(NIV.GE.2) THEN
            WRITE(IFM,*)'<CONTACT> <> FACTORISATION MATRICE CONTACT '
          ENDIF
          ILIDEB = INDFAC
          ILIFIN = NBLIAC+LLF
          CALL TLDLGG(2     ,LDSCON,ILIDEB,ILIFIN,0     ,
     &                NDECI ,ISINGU,NPVNEG,IER   )
          INDFAC = ILIFIN + 1
C
C --- LA MATRICE DE CONTACT EST-ELLE SINGULIERE ?
C
           IF (IER.GT.ISTO) THEN
             CTCCVG = 2
             GOTO 999
           END IF
         END IF
C ======================================================================
C --- SECOND MEMBRE : ON MET JEU(DEPTOT) - A.DELT0 DANS MU
C ======================================================================
         DO 25 ILIAI = 1,2*NBLIAI
            ZR(JMU-1+ILIAI)= 0.D0
 25      CONTINUE
C ======================================================================
C --- APPEL DE LA ROUTINE DE CALCUL DU SECOND MEMBRE
C ======================================================================
         CALL CFADU (RESOCO,DEPDEL,NEQ   ,NDIM  ,NBLIAC,
     &               LLF   ,LLF1  ,LLF2  ,NESMAX)
C ======================================================================
C --- RESOLUTION POUR OBTENIR MU : -A.C-1.AT.MU = JEU(DEPTOT) - A.DELT0
C --- ON TRUANDE LA SD MATR_ASSE POUR NE RESOUDRE LE SYSTEME QUE
C --- DE 1 A ILIFIN
C ======================================================================
         NEQMAX       = ZI(LDSCON+2)
         ZI(LDSCON+2) = ILIFIN
         CALL RLDLGG(LDSCON,ZR(JMU),CBID,1)     
         ZI(LDSCON+2) = NEQMAX
C ======================================================================
C --- ON REORDONNE LE VECTEUR MU
C ======================================================================
         CALL CFMAJM(RESOCO,NDIM  ,NBLIAC,LLF   ,LLF1  ,
     &               LLF2  )
C ======================================================================
C --- CALCUL DE DELTA = DELT0 - C-1.AT.MU
C ======================================================================
         CALL CFMAJU(RESOCO,NEQ   ,NDIM  ,NBLIAI,NBLIAC,
     &               LLF   ,LLF1  ,LLF2  )
C ======================================================================
C --- LES LIAISONS CONSIDEREES GLISSANTES LE SONT-ELLES VRAIMENT ?
C ======================================================================
         LFMIN = 0
         
         IF (LLF.LT.NBLIAC) THEN
            BTOTAL = NBLIAC + LLF
            DO 710 ILIAI = 1, BTOTAL
               LLIAC  = ZI(JLIAC-1+ILIAI)
               IF (ZK8(JTYPL-1+ILIAI).EQ.TYPEC0) THEN
                  DO 720 JJ = ILIAI+1, BTOTAL
                     IF (ZI(JLIAC-1+JJ).EQ.LLIAC) THEN
                        GOTO 710
                     ENDIF
 720              CONTINUE
                  DO  721 IOTE= 1, ZI(JLIOT+4*NBLIAI+1)
                     IF (ZI(JLIOT-1+IOTE+NBLIAI).EQ.LLIAC) THEN
                        GOTO 710
                     ENDIF
 721              CONTINUE
                  XK     = ZR(JTACF+ZTACF*(LLIAC-1)+0)
                  
                  JDECAL = ZI(JAPPTR+LLIAC-1)
                  NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
                  CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                        ZI(JAPDDL+JDECAL),ZR(JDELTA),VAL)
                  AJEUFX = ZR(JAPJFX-1+LLIAC) + VAL
                  CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                        ZI(JAPDDL+JDECAL),ZR(JRESU),VAL)
                  AJEUFX = AJEUFX + VAL
                  XPDT   = ZR(JMU-1+3*NBLIAI+LLIAC)*AJEUFX

                  IF (XPDT.LT.0.D0) THEN
C ======================================================================
C --- LA LIAISON EST EN FAITE ADHERENTE
C ======================================================================
                     POSIT = NBLIAC + LLF + 1
                     CALL CFTABL(INDIC,NBLIAC,AJLIAI,SPLIAI,LLF,
     &                       LLF1,LLF2,RESOCO,TYPEAJ,POSIT,LLIAC,TYPEF0)
                     IF (NIV.GE.2) THEN
                       CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,LLIAC ,
     &                             'F3'  ,TYPEAJ,'ALG' ,AJEUFX)
                     ENDIF
                     LFMIN = LFMIN + 1
                     ZR(JMU-1+3*NBLIAI+LLIAC) = 0.D0
                  ELSE IF (XPDT.EQ.0.D0) THEN
C ======================================================================
C --- LA LIAISON EST BIEN GLISSANTE
C --- ON MET A JOUR MU_G EN FONCTION DE L'INCREMENT DE DEPLACEMENT
C ======================================================================
                     CALL CALADU(NEQ,NBDDL,ZR(JAPCOF+JDECAL),
     &                           ZI(JAPDDL+JDECAL),ZR(JDELT0),VAL)
                     AJEUFX = ZR(JAPJFX-1+LLIAC) + VAL
                     IF (ABS(AJEUFX).LE.R8MIEM()) THEN
                       ZR(JMU-1+3*NBLIAI+LLIAC) = 0.D0
                     ELSE
                       XCOMP  = ABS( AJEUFX )
                       XCOS   = AJEUFX / XCOMP
                       ZR(JMU-1+3*NBLIAI+LLIAC) = XK * XCOS
                     ENDIF
                  ENDIF
               ENDIF
 710        CONTINUE
C ======================================================================
            IF (LFMIN.NE.0)  THEN
C ======================================================================
C --- S'IL EXISTE AU MOINS UNE LIAISON ADHERENTE SUPPLEMENTAIRE,
C --- ON NE PREND PAS EN COMPTE LES INCREMENTS DE DEPLACEMENTS CALCULES
C --- ET ON RECOMMENCE LES CALCULS (NOTAMMENT POUR MU_SG)
C ======================================================================
               GOTO 100
            ENDIF
         ENDIF
C ======================================================================
C --- LES LIAISONS CONSIDEREES ADHERENTES LE SONT-ELLES VRAIMENT ?
C ======================================================================
        IF ( LLF.NE. 0 ) THEN
          CALL CFADH(RESOCO,DEFICO,NOMA  ,NDIM  ,INDIC ,
     &               NBLIAC,NBLIAI,AJLIAI,SPLIAI,LLF   ,
     &               LLF1  ,LLF2  )
        ENDIF
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
         BTOTAL = NBLIAC + LLF
         DO 70 ILIAI = 1, NBLIAI
C ======================================================================
C - LA LIAISON ILIAI EST-ELLE ACTIVE ? (-> TROUAC)
C ======================================================================
            DO 700 ILIAC = 1, BTOTAL
               IF (ZI(JLIAC-1+ILIAC).EQ.ILIAI) THEN
                 GOTO 70
               ENDIF
 700        CONTINUE
C ======================================================================
C - CALCUL DE A.DELTA SI LA LIAISON ILIAI N'EST PAS ACTIVE
C ======================================================================
            JDECAL = ZI(JAPPTR+ILIAI-1)
            NBDDL  = ZI(JAPPTR+ILIAI) - ZI(JAPPTR+ILIAI-1)
            CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                  ZI(JAPDDL+JDECAL),ZR(JDELTA),AADELT)
C ======================================================================
C - SI A.DELTA EST POSITIF POUR ILIAI : CALCUL DE E(DEPTOT) - A.RESU
C - RHO = MIN ( ( E(DEPTOT) - A.RESU )ILIAI / (A.DELTA)ILIAI )
C - ON STOCKE DANS LLMIN LE NUMERO DE LA LIAISON REALISANT LE
C - MINIMUM (CE SERA LA LIAISON LA PLUS VIOLEE)
C ======================================================================
            IF (AADELT.GT.R8PREM()) THEN
C ======================================================================
C -- ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
C ======================================================================
               CALL CFELPV(ILIAI,TYPEC0,RESOCO,NBLIAI,LELPIV)
               IF (LELPIV) THEN
                 GOTO 70
               ENDIF
C ======================================================================
C -  FIN ELIMINATION LIAISON A PIVOT NUL
C ======================================================================
               DELPOS = .TRUE.
               CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),
     &                                  ZI(JAPDDL+JDECAL),ZR(JRESU),VAL)
               
               AJEU = ZR(JAPJEU+ILIAI-1) - VAL
               AJEU = AJEU/AADELT
               IF (AJEU.LT.RHO) THEN
                 RHO   = AJEU
                 NUMIN = ILIAI
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
      X1 = 1.D0
      RHORHO = MIN(RHO,X1)
C ======================================================================
C -- SI RHO < 1 (AU MOINS UNE LIAISON SUPPOSEE NON ACTIVE EST VIOLEE) :
C -- ON AJOUTE A L'ENSEMBLE DES LIAISONS ACTIVES LA PLUS VIOLEE (NUMIN)
C ======================================================================
      IF (RHORHO.LT.1.0D0) THEN
         POSIT = NBLIAC + LLF + 1
         CALL CFTABL(INDIC ,NBLIAC,AJLIAI,SPLIAI,LLF   , 
     &               LLF1  ,LLF2  ,RESOCO,TYPEAJ,POSIT , 
     &               NUMIN, TYPEC0)
         IF (NIV.GE.2) THEN
           CALL CFIMP2(DEFICO,RESOCO,NOMA  ,IFM   ,NUMIN ,
     &                 TYPEC0,TYPEAJ,'ALG' ,AADELT)
         END IF
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
C                            ON A CONVERGE
C ======================================================================
      IF (NBLIAC.EQ.0) THEN
        GOTO 145
      ENDIF
C ======================================================================
C --- ON ENLEVE TOUTES LES LIAISONS DE CONTACT POUR LESQUELLES
C --- LA PRESSION EST NEGATIVE
C ======================================================================
      CALL CFNEG (RESOCO,DEFICO,NOMA  ,NDIM  ,INDIC ,
     &            NBLIAI,NBLIAC,AJLIAI,SPLIAI,LLF   ,
     &            LLF1  ,LLF2)
C ======================================================================
C --- CALCUL DES FORCES DE CONTACT (AT.MU)
C ======================================================================
      CALL CFATMU(NEQ   ,NESMAX,NDIM  ,NBLIAC,1     ,
     &            LLF   ,LLF1  ,LLF2  ,RESOCO)
C ======================================================================
C --- CALCUL DES FORCES DE FROTTEMENT (AF.MU)
C ======================================================================
      COMPT0 = 0
      DO 140 ILIAC = 1, NBLIAC + LLF
         IF (ZK8(JTYPL-1+ILIAC).EQ.TYPEC0) THEN
            COMPT0 = COMPT0 + 1
            LLIAC  = ZI(JLIAC-1+ILIAC)
            JDECAL = ZI(JAPPTR+LLIAC-1)
            NBDDL  = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
            XFORC  = ZR(JMU-1+COMPT0)
            ZMU    = XFORC * ZR(JMU-1+3*NBLIAI+LLIAC)
            CALL CALATM(NEQ, NBDDL, ZMU, ZR(JAPCOF+JDECAL),
     &                  ZI(JAPDDL+JDECAL), ZR(JAFMU))
         ENDIF
 140  CONTINUE
C ======================================================================
 145  CONTINUE
C ======================================================================
C --- RECUPERATION DU DEPLACEMENT FINAL
C ======================================================================
      DO 150 ILIAI = 1,NEQ
        ZR(JRESU-1+ILIAI) = ZR(JRESU-1+ILIAI) + ZR(JDELTA-1+ILIAI)
 150  CONTINUE    
C
C --- CODE RETOUR
C
      CTCCVG = 0
C      
C --- ETAT DES VARIABLES DE CONTROLE DU CONTACT
C
      CALL CFECRD(RESOCO,'INDIC' ,INDIC )
      CALL CFECRD(RESOCO,'AJLIAI',AJLIAI)
      CALL CFECRD(RESOCO,'SPLIAI',SPLIAI)            
      CALL CFECRD(RESOCO,'NBLIAC',NBLIAC)      
      CALL CFECRD(RESOCO,'LLF'   ,LLF   )
      CALL CFECRD(RESOCO,'LLF1'  ,LLF1  )
      CALL CFECRD(RESOCO,'LLF2'  ,LLF2  )
C
      IF ( NIV .GE. 2 ) THEN
        WRITE(IFM,1002) ITER
      END IF
C
 999  CONTINUE
C 
C --- SAUVEGARDE DES INFOS DE DIAGNOSTIC 
C
      CALL NMRVAI(SDSTAT,'CTCD_ALGO_ITER','E',ITER  )
      CALL NMRVAI(SDSTAT,'CONT_NBLIAC'   ,'E',NBLIAC)
      CALL NMRVAI(SDSTAT,'CONT_NBLIAF'   ,'E',LLF   )

C ======================================================================
      CALL JEDEMA ()
C ======================================================================
 1001 FORMAT (' <CONTACT> <> DEBUT DES ITERATIONS (MAX: ',I6,')')
 1002 FORMAT (' <CONTACT> <> FIN DES ITERATIONS (NBR: ',I6,')')

C ======================================================================
      END
