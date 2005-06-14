      SUBROUTINE FROGDP(DEFICO,RESOCO,LMAT,LDSCON,NOMA,RESU,
     &                  DEPTOT,ITERAT,LREAC,CONV,DEPDEL)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/05/2005   AUTEUR MABBAS M.ABBAS 
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
      IMPLICIT     NONE
      LOGICAL      LREAC(4)
      REAL*8       CONV(*)
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO
      CHARACTER*24 RESOCO
      CHARACTER*24 RESU
      CHARACTER*24 DEPTOT
      CHARACTER*24 DEPDEL
      INTEGER      LMAT
      INTEGER      LDSCON
      INTEGER      ITERAT
C
C ======================================================================
C ROUTINE APPELEE PAR : CFALGO
C ======================================================================
C
C ALGO DE CONTACT
C
C ALGO. POUR CONTACT	: PENALISATION
C ALGO. POUR FROTTEMENT : PENALISATION
C
C RESO. DE : C.DU + kc AcT.Ac.DU + kg AgT.Ag.DU = F - kg AgT.Ag (E-U)
C            Ac. (U+DU)      <= E  (= POUR LES LIAISONS ACTIVES)
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
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
C IN  LDSCON : DESCRIPTEUR DE LA MATRICE -A.C-1.AT
C IN  DEPTOT : DEPLACEMENT TOTAL OBTENU A L'ISSUE DE L'ITERATION
C               DE NEWTON PRECEDENTE
C VAR RESU   : INCREMENT "DDEPLA" DE DEPLACEMENT DEPUIS DEPTOT
C                 EN ENTREE : SOLUTION OBTENUE SANS TRAITER LE CONTACT
C                 EN SORTIE : SOLUTION CORRIGEE PAR LE CONTACT
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C OUT LICCVG : CODES RETOURS D'ERREUR
C                       (1) PILOTAGE
C                       (2) LOI DE COMPORTEMENT
C                       (3) CONTACT/FROTTEMENT: NOMBRE MAXI D'ITERATIONS
C                       (4) CONTACT/FROTTEMENT: MATRICE SINGULIERE
C IN  LREAC  : ETAT DU CONTACT 
C              (1) = TRUE  SI REACTUALISATION A FAIRE  
C              (2) = TRUE  SI ATTENTE POINT FIXE CONTACT
C              (3) = TRUE  SI METHODE CONTINUE
C              (4) = TRUE  SI MODELISATION DU CONTACT
C IN  CONV   : INFORMATIONS SUR LA CONVERGENCE DU CALCUL
C                     1 - RESI_DUAL_ABSO      (LAGRANGIEN AUGMENTE)
C                     2 - RESI_PRIM_ABSO      (LAGRANGIEN AUGMENTE)
C                     3 - NOMBRE D'ITERATIONS DUAL (LAGRANGIEN AUGMENTE)
C                     4 - NUMERO ITERATION BFGS (LAGRANGIEN AUGMENTE)
C                    10 - NOMBRE D'ITERATIONS (RECHERCHE LINEAIRE)
C                    11 - RHO                 (RECHERCHE LINEAIRE)
C                    20 - RESI_GLOB_RELA
C                    21 - RESI_GLOB_MAXI
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
      INTEGER      IBID,IER,IFM,NIV,JCM2A1,JCM2A2,JCM2A3
      INTEGER      ICOMA,II,JJ,KK,JAPJFX,JAPJFY,JZOCO
      INTEGER      JRESU,JDEPP,JMU,JATMU,NDIM
      INTEGER      JDEPDE,JDELT0,JDELTA,JLIAC,JVALE,JCOCO,JAPMEM
      INTEGER      NEQ,NESCL,NBLIAC,NBLIAI,NBLCIN,NBLIG,NBDDL
      INTEGER      LLIAC,JDECAL,IPENA
      INTEGER      JAPPAR,JAPPTR,JAPCOE,JAPJEU,JAPDDL,JNOCO,JMACO
      INTEGER      JAPCOF,JAFMU,LMAF1,JCM1A,JCM3A,IFRO
      INTEGER      JDIM,NESMAX,LLF,LLF1,LLF2,AJLIAI,SPLIAI,INDIC,POSIT
      REAL*8       AJEUFX,AJEUFY,XF,XX,XK,XMU,VAL,XMU1,XMU2
      REAL*8       BETA,RESIGR,VAL1,VAL2
      CHARACTER*1  TYPEAJ
      CHARACTER*2  TYPEC0
      CHARACTER*14 NUMEDD
      CHARACTER*19 AFMU,MAT,CM1A,CM2A,CM3A,MAF1,MAF2,MAFROT
      CHARACTER*19 LIAC,MU,ATMU,DELT0,DELTA,MATR,COCO
      CHARACTER*24 MACONT,APJEFX,APJEFY,NOZOCO
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
      CM1A     = RESOCO(1:14)//'.CM1A'
      CM2A     = RESOCO(1:14)//'.CM2A'
      CM3A     = RESOCO(1:14)//'.CM3A'
      MATR     = RESOCO(1:14)//'.MATR'
      MAFROT   = RESOCO(1:8)//'.MAFR'
      MAF1     = '&&FROPGD.MAF1'
      MAF2     = '&&FROPGD.MAF2'
      FROTE    = DEFICO(1:16)//'.FROTE'
      PENAL    = DEFICO(1:16)//'.PENAL'
      COMAFO   = DEFICO(1:16)//'.COMAFO'
      NDIMCO   = DEFICO(1:16)//'.NDIMCO'
      COCO     = RESOCO(1:14)//'.COCO'
      MACONT   = ZK24(ZI(LDSCON+1))
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
      CALL JEVEUO(DEPTOT(1:19)//'.VALE','L',JDEPP)
      CALL JEVEUO(DEPDEL(1:19)//'.VALE','L',JDEPDE)
      CALL JEVEUO(FROTE,'L',IFRO)
      CALL JEVEUO(PENAL,'L',IPENA)
      CALL JEVEUO(COMAFO,'L',ICOMA)
      CALL JEVEUO(NDIMCO,'L',JDIM)
      CALL JEVEUO(COCO,'E',JCOCO)
      CALL JEECRA(MACONT(1:19)//'.REFA','DOCU',IBID,'ASSE')
      CALL JEVEUO(JEXNUM(MATR//'.VALE',1),'E',JVALE)
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
      NESCL  = ZI(JAPPAR)
      NESMAX = ZI(JDIM+8)
      NBLIAI = NESCL
      RESIGR = CONV(20)
      NEQ    = ZI(LMAT+2)
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
      IF (ITERAT.EQ.0) THEN
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
     +                                   RESOCO,TYPEAJ,POSIT,II,TYPEC0) 
             XMU    = 1.D0
             CALL CALATM (NEQ,NBDDL,XMU,ZR(JAPCOE+JDECAL),
     &                                ZI(JAPDDL+JDECAL),ZR(JCM1A))
             ZR(JMU-1+NBLIAC) = -ZR(JAPJEU+II-1)*ZR(IPENA-1+2*II-1)
             CALL DAXPY(NEQ,ZR(JMU-1+NBLIAC),ZR(JCM1A),1,ZR(JATMU),1) 
           ENDIF
         ENDIF
         CALL JELIBE(JEXNUM(CM1A,II))
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
C --- CREATION DE LA PSEUDO-MATRICE CM2A = E_N*AT
C --- CETTE MATRICE NE SERT QU'AU CALCUL DU SECOND MEMBRE
C ======================================================================
C
      IF (NDIM.EQ.3) THEN
         DO 210 II = 1, NBLIAI
            CALL JEVEUO ( JEXNUM(CM2A,II)       ,   'E', JCM2A1 )
            CALL JEVEUO ( JEXNUM(CM2A,II+NBLIAI),   'E', JCM2A2 )
            CALL JEVEUO ( JEXNUM(CM2A,II+2*NBLIAI), 'E', JCM2A3 )
            DO 200 KK = 1, NEQ
               ZR(JCM2A1-1+KK) = 0.0D0
               ZR(JCM2A2-1+KK) = 0.0D0
               ZR(JCM2A3-1+KK) = 0.0D0
 200        CONTINUE
            IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
              IF ( ZR(JAPJEU+II-1).LT.0.0D0 ) THEN
                JDECAL = ZI(JAPPTR+II-1)
                NBDDL  = ZI(JAPPTR+II)   - ZI(JAPPTR+II-1)
                XMU    = ZR(JMU-1+3*NBLIAI+II)
                CALL CALATM(NEQ,NBDDL,XMU,ZR(JAPCOF+JDECAL+30*NESMAX),
     &                      ZI(JAPDDL+JDECAL),ZR(JCM2A3))
                CALL CALATM(NEQ,NBDDL,XMU,ZR(JAPCOF+JDECAL),
     &                      ZI(JAPDDL+JDECAL),ZR(JCM2A2))
              ENDIF
            ENDIF
            CALL JELIBE(JEXNUM(CM2A,II)       )
            CALL JELIBE(JEXNUM(CM2A,II+NBLIAI))
            CALL JELIBE(JEXNUM(CM2A,II+2*NBLIAI))
 210     CONTINUE
      ELSE
         DO 216 II = 1, NBLIAI
            CALL JEVEUO ( JEXNUM(CM2A,II)       , 'E', JCM2A1 )
            CALL JEVEUO ( JEXNUM(CM2A,II+NBLIAI), 'E', JCM2A2 )
            DO 202 KK = 1, NEQ
               ZR(JCM2A1-1+KK) = 0.0D0
               ZR(JCM2A2-1+KK) = 0.0D0
 202        CONTINUE
            DO 218 KK = 1, NBLIAC
               IF ( ZI(JLIAC-1+KK).EQ.II ) THEN
                 JDECAL = ZI(JAPPTR+II-1)
                 NBDDL  = ZI(JAPPTR+II)   - ZI(JAPPTR+II-1)
                 XMU  = ZR(JMU-1+3*NBLIAI+II)
                 CALL CALATM (NEQ,NBDDL,XMU,ZR(JAPCOF+JDECAL),
     &                                     ZI(JAPDDL+JDECAL),ZR(JCM2A2))
                 GOTO 222
               ENDIF
 218        CONTINUE
 222        CONTINUE
            CALL JELIBE(JEXNUM(CM2A,II))
            CALL JELIBE(JEXNUM(CM2A,II+NBLIAI))
 216     CONTINUE
      ENDIF
C
C ======================================================================
C --- CREATION DE LA PSEUDO-MATRICE MAF1 = E_N*AT*A 
C --- CETTE MATRICE NE SERT QU'AU CALCUL DU SECOND MEMBRE 
C ======================================================================
C
      NBLIG = NBLIAI*NDIM
      CALL ATA000(CM2A,NUMEDD,400.D0,MAF1,'V',NBLIG)
C
C ======================================================================
C --- RECUPERATION DU SECOND MEMBRE
C --- CE VECTEUR EST REAFFECTE DANS ZR(JAFMU)
C ======================================================================
C
      CALL MTDSCR( MAF1 )
      CALL JEVEUO( MAF1//'.&INT', 'E', LMAF1 )
      CALL MRMULT('ZERO', LMAF1, ZR(JDELTA), 'R', ZR(JAFMU), 1 )
      CALL DETRSD('MATR_ASSE', MAF1  )
C
C ======================================================================
C --- CREATION DE CM2A = E_N*AT
C ======================================================================
C
      IF (NDIM.EQ.3) THEN
         DO 220 II = 1, NBLIAI
            CALL JEVEUO ( JEXNUM(CM2A,II),          'E', JCM2A1 )
            CALL JEVEUO ( JEXNUM(CM2A,II+NBLIAI),   'E', JCM2A2 )
            CALL JEVEUO ( JEXNUM(CM2A,II+2*NBLIAI), 'E', JCM2A3 )
            DO 230 KK = 1, NEQ
               ZR(JCM2A1-1+KK) = 0.0D0
               ZR(JCM2A2-1+KK) = 0.0D0
               ZR(JCM2A3-1+KK) = 0.0D0
 230        CONTINUE
            IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
              IF ( ZR(JAPJEU+II-1).LT.0.0D0 ) THEN
                JDECAL = ZI(JAPPTR+II-1)
                NBDDL  = ZI(JAPPTR+II)-ZI(JAPPTR+II-1)
                XMU1   = SQRT(ZR(IPENA-1+2*II-1))
                CALL CALATM(NEQ,NBDDL,XMU1,ZR(JAPCOE+JDECAL),
     &                      ZI(JAPDDL+JDECAL),ZR(JCM2A1))
                ZR(JMU-1+2*NBLIAI+II) = 1.D0
                XMU2   = ZR(JMU-1+3*NBLIAI+II)
                CALL CALATM(NEQ,NBDDL,XMU2,ZR(JAPCOF+JDECAL),
     &                      ZI(JAPDDL+JDECAL),ZR(JCM2A2))
                CALL CALATM(NEQ,NBDDL,XMU2,
     &                      ZR(JAPCOF+JDECAL+30*NESMAX),
     &                      ZI(JAPDDL+JDECAL),ZR(JCM2A3))
              ENDIF
            ENDIF
            CALL JELIBE(JEXNUM(CM2A,II))
            CALL JELIBE(JEXNUM(CM2A,II+NBLIAI))
            CALL JELIBE(JEXNUM(CM2A,II+2*NBLIAI))
 220     CONTINUE
      ELSE
         DO 224 II = 1, NBLIAI
            CALL JEVEUO ( JEXNUM(CM2A,II),          'E', JCM2A1 )
            CALL JEVEUO ( JEXNUM(CM2A,II+NBLIAI),   'E', JCM2A2 )
            DO 232 KK = 1, NEQ
               ZR(JCM2A1-1+KK) = 0.0D0
               ZR(JCM2A2-1+KK) = 0.0D0
 232        CONTINUE
            IF (.NOT.CFEXCL(JAPPAR,JAPMEM,II)) THEN
              IF ( ZR(JAPJEU+II-1).LT.0.0D0 ) THEN
                JDECAL = ZI(JAPPTR+II-1)
                NBDDL  = ZI(JAPPTR+II)-ZI(JAPPTR+II-1)
                XMU1   = SQRT(ZR(IPENA-1+2*II-1))
                CALL CALATM(NEQ,NBDDL,XMU1,ZR(JAPCOE+JDECAL),
     &                      ZI(JAPDDL+JDECAL),ZR(JCM2A1))
                ZR(JMU-1+2*NBLIAI+II) = 1.D0
                XMU2   = ZR(JMU-1+3*NBLIAI+II)
                CALL CALATM(NEQ,NBDDL,XMU2,ZR(JAPCOF+JDECAL),
     &                     ZI(JAPDDL+JDECAL),ZR(JCM2A2))
              ENDIF
            ENDIF
            CALL JELIBE(JEXNUM(CM2A,II))
            CALL JELIBE(JEXNUM(CM2A,II+NBLIAI))
 224     CONTINUE
      ENDIF
C
C ======================================================================
C --- CREATION DE LA PREMIERE PARTIE DE LA MATRICE DE FROTTEMENT MAF1
C ======================================================================
C
      NBLIG = NBLIAI*NDIM
      CALL ATA000 (CM2A,NUMEDD,400.D0,MAF1,'V',NBLIG)
C
C ======================================================================
C --- CREATION DE CM3A = E_T*AT
C ======================================================================
C
      DO 300 II = 1, NBLIAI
         CALL JEVEUO ( JEXNUM(CM3A,II), 'E', JCM3A )
         DO 310 KK = 1, NEQ
            ZR(JCM3A-1+KK) = 0.0D0
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
             CALL CALAPR(NEQ,NBDDL,BETA,ZR(JAFMU),
     &                                ZI(JAPDDL+JDECAL),ZR(JCM3A))
             ZR(JMU-1+2*NBLIAI+II) = 1.D0
           ELSE
             ZR(JMU-1+2*NBLIAI+II) = 0.D0 
           ENDIF
         ENDIF
 305     CONTINUE
         CALL JELIBE(JEXNUM(CM3A,II))
 300  CONTINUE
C
C ======================================================================
C --- CREATION DE LA SECONDE PARTIE DE LA MATRICE DE FROTTEMENT MAF2
C ======================================================================
C
      NBLIG = NBLIAI
      CALL ATA000(CM3A,NUMEDD,400.D0,MAF2,'V',NBLIG)
C
C ======================================================================
C --- CALCUL DE LA MATRICE TANGENTE AVEC FROTTEMENT MAFROT = MAF1+MAF2
C ======================================================================
C
      CALL CFFROT(MAT,MAF1,MAF2,MAFROT)
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
 9999 CONTINUE
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
