      SUBROUTINE ALGOCG(DEFICO,RESOCO,LMAT,NOMA,CINE,RESU,DEPTOT,
     &                  LICCVG,LREAC,ITERAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/07/2006   AUTEUR PABHHHH N.TARDIEU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C TOLE CRP_20
C ======================================================================
      IMPLICIT     NONE
      LOGICAL LREAC(4)
      CHARACTER*8 NOMA
      CHARACTER*24 DEFICO,RESOCO,CINE,RESU,DEPTOT
      INTEGER LMAT,ITERAT,LICCVG(5)
C ======================================================================
C ROUTINE APPELEE PAR : CFALGO
C ======================================================================
C
C ALGO DE CONTACT
C
C ALGO. POUR CONTACT    : GRADIENT CONJUGUE PROJETE
C ALGO. POUR FROTTEMENT : SANS
C
C RESOLUTION DE : C.DU + AT.MU  = F
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
C IN  DEFICO  : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO  : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C                'E':  RESOCO(1:14)//'.APJEU'
C                'E':  RESOCO(1:14)//'.LIAC'
C                'E':  RESOCO(1:14)//'.LIOT'
C                'E':  RESOCO(1:14)//'.MU'
C                'E':  RESOCO(1:14)//'.DEL0'
C                'E':  RESOCO(1:14)//'.DELT'
C                'E':  RESOCO(1:14)//'.COCO'
C IN  LMAT    : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  NOMA    : NOM DU MAILLAGE
C IN  CINE    : CHAM_NO CINEMATIQUE
C IN  DEPTOT  : DEPLACEMENT TOTAL OBTENU A L'ISSUE DE L'ITERATION
C               DE NEWTON PRECEDENTE
C VAR RESU    : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS DEPTOT
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C OUT LICCVG  : CODES RETOURS D'ERREUR
C                       (1) PILOTAGE
C                       (2) LOI DE COMPORTEMENT
C                       (3) CONTACT/FROTTEMENT: NOMBRE MAXI D'ITERATIONS
C                       (4) CONTACT/FROTTEMENT: MATRICE SINGULIERE
C
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------

      CHARACTER*32 JEXNUM
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

C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------

      INTEGER ZTOLE
      PARAMETER (ZTOLE=6)
      LOGICAL CONJUG,GCPC
      INTEGER IFM,NIV,IZONE,IBID,NZOCO,JTOLE,FREQ,ISMAEM
      INTEGER II,ITER,ILIAC,JRESU,LLIAC,JDECAL
      INTEGER NEQ,NESCL,NBLIAC,NBLIAI,NBDDL,JDMU
      INTEGER JAPPAR,JAPPTR,JAPCOE,JAPJEU,JAPDDL,JZOCO
      INTEGER JNOCO,JMACO,JCONV,JSECMB,JVEZER,JMUM,JVECC,JMU0
      INTEGER JLIAC,JMU,JDELT0,JDELTA,JCOCO,JATMU,JMETH,JSLVK
      INTEGER ITEMAX,ITEMUL,JRCINE,JSGRAM,JSGRAP,JDIREM,JDIREP
      REAL*8 AJEU,VAL,R8PREM,R8VIDE,DDOT,TOLE
      REAL*8 NUMER,DENOM,NINF,ALPHA,EPSI,R8BID,GAMMA,NUMER2
      CHARACTER*2 TYPEC0
      CHARACTER*19 LIAC,MU,DELT0,DELTA,COCO,ATMU,SGRADM,SGRADP,MU0
      CHARACTER*19 DIRECM,DIRECP,MUM,SECMBR,VEZERO,SOLVEU,CONVEC
      CHARACTER*24 APPARI,APPOIN,APCOEF,APJEU,APDDL,NOZOCO
      CHARACTER*24 CONTNO,CONTMA,CONVCO,TOLECO,METHCO,DELTAM

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
C ======================================================================
      CALL INFNIV(IFM,NIV)
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<CONTACT> <> ALGO_CONTACT   : GRADIENT '//
     &    'CONJUGUE PROJETE'
        WRITE (IFM,*) '<CONTACT> <> ALGO_FROTTEMENT: SANS'
      END IF

      CALL JEMARQ()

C ======================================================================
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================

      METHCO = DEFICO(1:16)//'.METHCO'
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      CONTMA = DEFICO(1:16)//'.MAILCO'
      NOZOCO = DEFICO(1:16)//'.NOZOCO'
      CONVCO = DEFICO(1:16)//'.CONVCO'
      TOLECO = DEFICO(1:16)//'.TOLECO'
      APPARI = RESOCO(1:14)//'.APPARI'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APJEU = RESOCO(1:14)//'.APJEU'
      APDDL = RESOCO(1:14)//'.APDDL'
      LIAC = RESOCO(1:14)//'.LIAC'
      MU = RESOCO(1:14)//'.MU'
      DELT0 = RESOCO(1:14)//'.DEL0'
      DELTA = RESOCO(1:14)//'.DELT'
      ATMU = RESOCO(1:14)//'.ATMU'
      COCO = RESOCO(1:14)//'.COCO'
      SOLVEU = '&&OP0070.SOLVEUR'

C     SI SOLVEUR GCPC, ON S'ARRETE EN FATALE
C     ---------------------------------------
      CALL JEVEUO(SOLVEU//'.SLVK','L',JSLVK)
      GCPC = (ZK24(JSLVK-1+1).EQ.'GCPC')
      IF (GCPC) THEN
        CALL UTMESS('F','ALGOCG','ON NE PEUT UTILISER '//
     &              'LE SOLVEUR GCPC')
      ENDIF

C ======================================================================
      CALL JEVEUO(COCO,'E',JCOCO)
      CALL JEVEUO(NOZOCO,'L',JZOCO)
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(CONVCO,'L',JCONV)
      CALL JEVEUO(APPARI,'L',JAPPAR)
      CALL JEVEUO(APPOIN,'L',JAPPTR)
      CALL JEVEUO(APCOEF,'L',JAPCOE)
      CALL JEVEUO(APJEU,'E',JAPJEU)
      CALL JEVEUO(APDDL,'L',JAPDDL)
      CALL JEVEUO(LIAC,'E',JLIAC)
      CALL JEVEUO(ATMU,'E',JATMU)
      CALL JEVEUO(MU,'E',JMU)
      CALL JEVEUO(DELT0,'E',JDELT0)
      CALL JEVEUO(DELTA,'E',JDELTA)
      CALL JEVEUO(RESU(1:19)//'.VALE','E',JRESU)
      CALL JEVEUO(CINE(1:19)//'.VALE','L',JRCINE)
C ======================================================================
C --- INITIALISATION DE VARIABLES
C --- NESCL  : NOMBRE DE NOEUDS ESCLAVES SUSCEPTIBLES D'ETRE EN CONTACT
C --- NBLIAI : NOMBRE DE LIAISONS DE CONTACT
C --- NBLIAC : NOMBRE DE LIAISONS ACTIVES
C --- NEQ    : NOMBRE D'EQUATIONS DU MODELE
C --- ITEMUL : NOMBRE PAR LEQUEL IL FAUT MULTIPLIER LE NOMBRE DE
C              LIAISONS DE CONTACT POUR OBTENIR LE NOMBRE MAXI
C              D'ITERATIONS DANS L'ALGO ITEMAX=ITEMUL*NBLIAI
C                   <!> FIXE A 10 POUR CET ALGO <!>
C --- ITEMAX : NOMBRE D'ITERATIONS DE CONTACT DANS L'ALGO
C ======================================================================
      NESCL = ZI(JAPPAR)
      NBLIAI = NESCL
      NEQ = ZI(LMAT+2)
      ITEMUL = 50
      ITEMAX = ITEMUL*NBLIAI
      TYPEC0 = 'C0'

C ======================================================================
C                             INITIALISATIONS
C ======================================================================

      NBLIAC = 0
      ITER = 1
      CONJUG = .FALSE.

C     RECUPERATION DU CRITERE DE CONVERGENCE ET DE LA FREQUENCE DE
C     REACTUALISATION DES DIRECTIONS (MIN DE TOUS)
C     --------------------------------------------------------------

      EPSI = 1.D0/R8PREM()
      FREQ = ISMAEM()
      CALL JEVEUO(METHCO,'L',JMETH)
      CALL JEVEUO(TOLECO,'L',JTOLE)
      NZOCO = ZI(JMETH)
      DO 10 IZONE = 1,NZOCO
        EPSI = MIN(EPSI,ZR(JTOLE+ZTOLE* (IZONE-1)+4))
        FREQ = MIN(FREQ,INT(ZR(JTOLE+ZTOLE* (IZONE-1)+5)))
   10 CONTINUE


C     CREATION DES VECTEURS DE TRAVAIL
C     ---------------------------------

C     SS-GRADIENT ITERATION (-) ET (+)
      SGRADM = '&&ALGOCG.SGRADM'
      SGRADP = '&&ALGOCG.SGRADP'

C     SS-GRADIENT ITERATION (-) ET (+)
      DIRECM = '&&ALGOCG.DIRECM'
      DIRECP = '&&ALGOCG.DIRECP'

C     MULT. LAGRANGE ITERATION (-)
      MUM = '&&ALGOCG.MUM'

C     MULT. LAGRANGE ITERATION DE NEWTON (-)
      MU0 = '&&ALGOCG.MU0'

C     INCREMENT DE MULT. LAGRANGE
      DELTAM = '&&ALGOCG.DELTAM'

C     SECOND MEMBRE ET VECTEUR NUL
      SECMBR = '&&ALGOCG.SECMBR'
      VEZERO = '&&ALGOCG.VEZERO'

      IF (ITERAT.EQ.0 .OR. LREAC(1)) THEN
        CALL INITIA(NBLIAI,.FALSE.,0,R8BID,ZR(JMU))
      END IF
C
      CALL JEDUPO(MU,'V',SGRADM,.FALSE.)
      CALL JEDUPO(MU,'V',SGRADP,.FALSE.)
      CALL JEDUPO(MU,'V',DIRECM,.FALSE.)
      CALL JEDUPO(MU,'V',DIRECP,.FALSE.)
      CALL JEDUPO(MU,'V',MUM,.FALSE.)
      CALL JEDUPO(MU,'V',MU0,.FALSE.)
      CALL JEDUPO(MU,'V',DELTAM,.FALSE.)
      CALL JEDUPO(ATMU,'V',SECMBR,.FALSE.)
      CALL JEDUPO(ATMU,'V',VEZERO,.FALSE.)
C
      CALL JEVEUO(SGRADM,'E',JSGRAM)
      CALL JEVEUO(SGRADP,'E',JSGRAP)
      CALL JEVEUO(DIRECM,'E',JDIREM)
      CALL JEVEUO(DIRECP,'E',JDIREP)
      CALL JEVEUO(MUM,'E',JMUM)
      CALL JEVEUO(MU0,'E',JMU0)
      CALL JEVEUO(DELTAM,'E',JDMU)
      CALL JEVEUO(SECMBR,'E',JSECMB)
      CALL JEVEUO(VEZERO,'E',JVEZER)
      CALL INITIA(NEQ,.FALSE.,0,R8BID,ZR(JVEZER))


C     IMPRESSIONS INITIALES
C     ---------------------
      IF (NIV.EQ.2) THEN
        WRITE (IFM,*) '<CONTACT> <> LIAISONS INITIALES '
      END IF

      IF (NIV.GE.2) THEN
        WRITE (IFM,9000) NBLIAI
        WRITE (IFM,9010) ITEMAX
      END IF

C     TOUTES LES LIAISONS SONT DES LIAISONS DE CONTACT
C     ET PORTENT LEUR NUMERO "NATUREL"
      CONVEC = RESOCO(1:14)//'.CONVEC'
      CALL JEVEUO(CONVEC,'E',JVECC)
      DO 20 II = 1,NBLIAI
        ZK8(JVECC-1+II) = TYPEC0
        ZI(JLIAC-1+II) = II
   20 CONTINUE


C ======================================================================
C =========================== BOUCLE PRINCIPALE ========================
C ======================================================================

   30 CONTINUE

C ======================================================================
C --- CALCUL ET PROJECTION DU SOUS-GRADIENT
C --- EVALUATION DE LA CONVERGENCE
C --- IMPRESSION DE DEBUGGAGE SUR DEMANDE
C ======================================================================

      IF (NIV.EQ.2) THEN
        WRITE (IFM,*) '<CONTACT> <> -----------------------------------'
        WRITE (IFM,*) '<CONTACT> <> ITERATION DE GCP = ',ITER
      END IF

      NINF = 0.D0
      NBLIAC = 0
      DO 40 II = 1,NBLIAI
        JDECAL = ZI(JAPPTR+II-1)
        NBDDL = ZI(JAPPTR+II) - ZI(JAPPTR+II-1)
        CALL CALADU(NEQ,NBDDL,ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),
     &              ZR(JRESU),VAL)
        IF (ZR(JAPJEU+II-1).NE.R8VIDE()) THEN
          AJEU = ZR(JAPJEU+II-1) - VAL
        ELSE
          AJEU = 1.D0/R8PREM()
        END IF
C       PROJECTION DU SOUS-GRADIENT
        ZR(JSGRAP-1+II) = -AJEU
        IF (ZR(JMU-1+II).LT.EPSI) THEN
          ZR(JSGRAP-1+II) = MAX(-1.D0*AJEU,0.D0)
        END IF
C       NORME INFINIE DU SOUS-GRADIENT
        NINF = MAX(ABS(ZR(JSGRAP-1+II)),NINF)
   40 CONTINUE


C     ON A CONVERGE
      IF (NINF.LT.EPSI) GO TO 90
      IF (NIV.EQ.2) THEN
        WRITE (IFM,9060) NINF,EPSI
      END IF

C ======================================================================
C --- CONJUGAISON
C ======================================================================

C     ON FAIT LA CONJUGAISON DE POLAK-RIBIERE : 
C        - SI L'ETAT DE CONTACT EST LE MEME D'UNE ITERATION SUR L'AUTRE
C        - TOUTES LES FREQ ITERATIONS
C        - SI DIR+ EST UNE DIRECTION DE DESCENTE I.E. (DIR+)'.(SGRAD+)>0
C     NB : LA CONJUGAISON DE FLETCHER-REEVES EST : GAMMA = NUMER/DENOM
      GAMMA = 0.D0
      IF (CONJUG) THEN
        NUMER  = DDOT(NBLIAI,ZR(JSGRAP),1,ZR(JSGRAP),1)
        NUMER2 = DDOT(NBLIAI,ZR(JSGRAP),1,ZR(JSGRAM),1)
        DENOM  = DDOT(NBLIAI,ZR(JSGRAM),1,ZR(JSGRAM),1)
        GAMMA  = (NUMER-NUMER2)/DENOM
      END IF


C     DIRECP = GAMMA DIRECM + SSGRAP
      CALL DCOPY(NBLIAI,ZR(JSGRAP),1,ZR(JDIREP),1)
      CALL DAXPY(NBLIAI,GAMMA,ZR(JDIREM),1,ZR(JDIREP),1)
      
C     DIR+ EST-ELLE UNE DIRECTION DE DESCENTE
      NUMER2= DDOT(NBLIAI,ZR(JDIREP),1,ZR(JSGRAP),1)
      IF (NUMER2.LE.0.D0) THEN
        CONJUG=.FALSE.
        GAMMA=0.D0
        CALL DCOPY(NBLIAI,ZR(JSGRAP),1,ZR(JDIREP),1)
      ENDIF

      IF (CONJUG) THEN
        IF (NIV.EQ.2) THEN
          WRITE (IFM,*) '<CONTACT> <> CONJUGAISON DES DIRECTIONS '//
     &      'DE RECHERCHE, GAMMA=',GAMMA
        END IF
      END IF


C ======================================================================
C --- RECHERCHE LINEAIRE
C ======================================================================

C     INITIALISATION DE DELTA
      CALL INITIA(NEQ,.FALSE.,0,R8BID,ZR(JDELTA))

C     K DELTA = A' DIRECP
      DO 50 ILIAC = 1,NBLIAI
        LLIAC = ZI(JLIAC+ILIAC-1)
        JDECAL = ZI(JAPPTR+LLIAC-1)
        NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
        CALL CALATM(NEQ,NBDDL,ZR(JDIREP-1+ILIAC),ZR(JAPCOE+JDECAL),
     &              ZI(JAPDDL+JDECAL),ZR(JDELTA))
   50 CONTINUE
      CALL DCOPY(NEQ,ZR(JDELTA),1,ZR(JSECMB),1)
      CALL NMRLDB(LMAT,ZR(JVEZER),ZR(JDELTA),1)

C     PRODUIT SCALAIRE  NUMER=DIRECHP' DIRECHP
      NUMER = DDOT(NBLIAI,ZR(JDIREP),1,ZR(JDIREP),1)


C     PRODUIT SCALAIRE  DENOM=DIRECHP' A K-1 A' DIRECHP
      DENOM = DDOT(NEQ,ZR(JDELTA),1,ZR(JSECMB),1)

      IF (DENOM.LT.0.D0) THEN
        CALL UTMESS('F','ALGOCG','DENOM EST NEGATIF : '//
     &              'CONTACTER LES DEVELOPPEURS')
      END IF

      ALPHA = NUMER/DENOM

      IF (NIV.EQ.2) THEN
        WRITE (IFM,9040) ALPHA
      END IF

C ======================================================================
C --- PROJECTION ET MISES A JOUR
C ======================================================================

C     PROJECTION DU PAS D'AVANCEMENT POUR RESPECTER LES CONTRAINTES
      DO 60 ILIAC = 1,NBLIAI
        IF (ZR(JDIREP-1+ILIAC).LT.0.D0) THEN
          ALPHA = MIN(ALPHA,-ZR(JMU-1+ILIAC)/ZR(JDIREP-1+ILIAC))
        END IF
   60 CONTINUE


      IF (ALPHA.LT.0.D0) THEN
        CALL UTMESS('F','ALGOCG','LE PAS D''AVANCEMENT EST NEGATIF')
      END IF

      IF (NIV.EQ.2) THEN
        WRITE (IFM,9050) ALPHA
      END IF

C     MISE A JOUR DE MU PUIS DE DELTA
      CALL DAXPY(NBLIAI,ALPHA,ZR(JDIREP),1,ZR(JMU),1)
      CALL DAXPY(NEQ,-ALPHA,ZR(JDELTA),1,ZR(JRESU),1)


C     ON VERIFIE SI L'ETAT DE CONTACT A CHANGE (ON NE CONJUGUE PAS)
C     ON REINITIALISE LA DIRECTION DE RECHERCHE TOUTES LES FREQ
C     ITERATIONS
      CONJUG = .TRUE.
      TOLE = 1.D-15
      IF (MOD(ITER,FREQ).NE.0) THEN
        DO 70 II = 1,NBLIAI
          IF (((ZR(JMU-1+II).GT.TOLE).AND.(ZR(JMUM-1+II).LT.TOLE)) .OR.
     &        ((ZR(JMU-1+II).LT.TOLE).AND.(ZR(JMUM-1+II).GT.TOLE))) THEN
            CONJUG = .FALSE.
            IF (NIV.EQ.2) THEN
              WRITE (IFM,*) '<CONTACT> <>'//
     &          '  CHANGEMENT DE L''ETAT DE CONTACT'
            END IF
            GO TO 80
          END IF
   70   CONTINUE
      ELSE
        CONJUG = .FALSE.
      END IF
   80 CONTINUE


C     MISE À JOUR DES GRADIENTS ET DES DIRECTIONS DE RECHERCHE
      CALL DCOPY(NBLIAI,ZR(JSGRAP),1,ZR(JSGRAM),1)
      CALL DCOPY(NBLIAI,ZR(JDIREP),1,ZR(JDIREM),1)
      CALL DCOPY(NBLIAI,ZR(JMU),1,ZR(JMUM),1)

      ITER = ITER + 1

C ======================================================================
C --- A-T-ON DEPASSE LE NOMBRE D'ITERATIONS DE CONTACT AUTORISE ?
C ======================================================================
      IF (ITER.GT.ITEMAX+1) THEN
        LICCVG(3) = 1
        GO TO 120
      END IF


      GO TO 30

C ======================================================================
C =========================== ON A CONVERGE! ===========================
C ======================================================================

   90 CONTINUE

C     ON CALCULE L'INCREMENT DE MULTIPLICATEUR MU
C     CAR C'EST CE DONT LE RESIDU A BESOIN
      CALL DCOPY(NBLIAI,ZR(JMU),1,ZR(JDMU),1)
      CALL DAXPY(NBLIAI,-1.D0,ZR(JMU0),1,ZR(JDMU),1)
      
      CALL INITIA(NEQ,.FALSE.,0,R8BID,ZR(JATMU))
      DO 100 ILIAC = 1,NBLIAI
        LLIAC = ZI(JLIAC+ILIAC-1)
        JDECAL = ZI(JAPPTR+LLIAC-1)
        NBDDL = ZI(JAPPTR+LLIAC) - ZI(JAPPTR+LLIAC-1)
        CALL CALATM(NEQ,NBDDL,ZR(JDMU-1+ILIAC),ZR(JAPCOE+JDECAL),
     &              ZI(JAPDDL+JDECAL),ZR(JATMU))
  100 CONTINUE

      NBLIAC = 0
      DO 110 II = 1,NBLIAI
        IF (ZR(JMU-1+II).GT.EPSI) THEN
          NBLIAC = NBLIAC + 1
          ZI(JLIAC-1+NBLIAC) = II
        END IF
  110 CONTINUE


C ======================================================================
C --- STOCKAGE DE L'ETAT DE CONTACT DEFINITIF
C ======================================================================
C --- VALEUR DES VARIABLES DE CONVERGENCE
      LICCVG(3) = 0
      LICCVG(4) = 0

      ZI(JCOCO+2) = NBLIAC

C ======================================================================
C --- CALCUL DU JEU FINAL
C ======================================================================
      CALL CFJEFI(NEQ,NBLIAI,JAPPTR,JAPCOE,JAPDDL,JRESU,JAPJEU,0,0)

C ======================================================================
C --- AFFICHAGE FINAL
C ======================================================================
      IF (NIV.GE.2) THEN
        WRITE (IFM,9020) ITER
        WRITE (IFM,9030) NBLIAC
        WRITE (IFM,*) '<CONTACT> <> LIAISONS FINALES '
        CALL CFIMP1(DEFICO,RESOCO,NOMA,NBLIAI,IFM)
      END IF

  120 CONTINUE

C ======================================================================
C --- SAUVEGARDE DES INFOS DE DIAGNOSTIC (NOMBRE D'ITERATIONS)
C ======================================================================
      CALL CFITER(RESOCO,'E','ITER',ITER,R8BID)
C      
C      DO 1 II=1,NBLIAI
C        WRITE(6,*) ZR(JMU-1+II)
C 1    CONTINUE
C
C      DO 2 II=1,NEQ
C        WRITE(6,*) ZR(JRESU-1+II)
C 2    CONTINUE

C ======================================================================
C --- DESTRUCTION DES VECTEURS INUTILES
C ======================================================================
      CALL JEDETR(SGRADM)
      CALL JEDETR(SGRADP)
      CALL JEDETR(DIRECM)
      CALL JEDETR(DIRECP)
      CALL JEDETR(SECMBR)
      CALL JEDETR(MUM)
      CALL JEDETR(MU0)
      CALL JEDETR(DELTAM)
      CALL JEDETR(VEZERO)

      CALL JEDEMA()

C ======================================================================

 9000 FORMAT (' <CONTACT> <> NOMBRE DE LIAISONS POSSIBLES: ',I6)
 9010 FORMAT (' <CONTACT> <> DEBUT DES ITERATIONS (MAX: ',I6,')')
 9020 FORMAT (' <CONTACT> <> FIN DES ITERATIONS (NBR: ',I6,')')
 9030 FORMAT (' <CONTACT> <> NOMBRE DE LIAISONS CONTACT FINALES:',I6,
     &       ')')
 9040 FORMAT (' <CONTACT> <> PAS D''AVANCEMENT INITIAL : ',D12.5)
 9050 FORMAT (' <CONTACT> <> PAS D''AVANCEMENT APRES PROJECTION : ',
     &       D12.5)
 9060 FORMAT (' <CONTACT> <> NORME INFINIE DU RESIDU : ',D12.5,' (CRIT',
     &       'ERE: ',D12.5,')')
      END
