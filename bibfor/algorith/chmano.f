      SUBROUTINE CHMANO(IZONE,IESCL0,NZOCO,NSYME,NOMA,NEWGEO,DEFICO,
     &                  RESOCO,IESCL)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/10/2004   AUTEUR MABBAS M.ABBAS 
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
C TOLE CRP_20
      IMPLICIT     NONE
      INTEGER      IZONE
      INTEGER      IESCL0
      INTEGER      NSYME
      INTEGER      NZOCO
      CHARACTER*8  NOMA
      CHARACTER*24 NEWGEO
      CHARACTER*24 DEFICO
      CHARACTER*24 RESOCO
      INTEGER      IESCL
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : RECHME
C ----------------------------------------------------------------------
C
C RECHERCHE DE LA MAILLE LA PLUS PROCHE CONNAISSANT LE NOEUD LE PLUS
C PROCHE.
C
C IN  IZONE  : NUMERO DE LA ZONE DE CONTACT ACTUELLE
C IN  IESCL0 : INDICE DU PREMIER NOEUD ESCLAVE A EXAMINER
C IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
C IN  NSYME  : NOMBRE DE ZONES DE CONTACT SYMETRIQUES
C IN  NOMA   : NOM DU MAILLAGE
C IN  NEWGEO : GEOMETRIE ACTUALISEE EN TENANT COMPTE DU CHAMP DE
C              DEPLACEMENTS COURANT
C IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
C VAR RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C I/O IESCL  : NUMERO DU DERNIER NOEUD ESCLAVE CONNU (PEUT DIMINUER SI
C              LE NOEUD ESCLAVE EST EXCLU POUR CAUSE DE PIVOT NUL DANS
C              LE CAS DE L'APPARIEMENT SYMETRIQUE)
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
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
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      ZAPPAR
      PARAMETER    (ZAPPAR=3)
      INTEGER      IFM,NIV,L
      INTEGER      TYPALC,TYPALF,FROT3D,MATTAN
      INTEGER      NESMAX,NBNO,NBMA,NBDDL,NDIM,NESCL,NNOCO

      CHARACTER*24 NDIMCO,PMANO,CONTNO,APPARI,CONTMA,MANOCO,APPOIN
      INTEGER      JDIM,JPOMA,JNOCO,JAPPAR,JMANO,JMACO,JAPPTR
 
      CHARACTER*24 PDDL,NORMCO,TANGCO,APMEMO,APCOEF,APJEU,APDDL
      INTEGER      JPDDL,JNORMO,JTANGO,JAPMEM,JAPCOE,JAPJEU,JAPDDL

      CHARACTER*24 APJEFX,APJEFY,APCOFR,CHAMCO,COEFCO
      INTEGER      JAPCOF,JAPJFX,JAPJFY,JCHAM,JCOEF

      CHARACTER*24 NOMACO,PNOMA,SANSNO,PSANS,PZONE,PSURNO
      INTEGER      JNOMA,JPONO,JPSANS,JSANS,JZONE,JSUNO

      INTEGER      IND,IMA,POSNOE,POSNOM,JDEC,POSMA
      INTEGER      PROJ,POSNO(10),DDL(30)
      REAL*8       JEUMIN,R8GAEM,OLDJEU,JEU
      REAL*8       NORM(3),TANG(6),COEF(30),COFX(30),COFY(30)
      REAL*8       CMULT,ECAN,R8MIEM,R8PREM
      LOGICAL      MULNOR
      CHARACTER*3  PROJOP
      INTEGER      REAC,PROYES,POSMIN,PROMIN
      INTEGER      ZONESY,NZOCP,JSYEXC,TYPSUP,IZONPR,IZONSY,ISURFM
      INTEGER      NBNOM,JDECM,SUPPOK,NUMNOM
      INTEGER      POSSUP,ISUPP
      INTEGER      POSSTC,NUMSTC
      INTEGER      POSPIV,NUMPIV
   
      
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()

      CALL INFNIV(IFM,NIV)
C
      IF (NIV.GE.2) THEN
       WRITE (IFM,*) '<CONTACT> <> <> APPARIEMENT M/E - MAILLE MAITRE'
      ENDIF
C
C --- INFOS SUR LA CHARGE DE CONTACT
C
      CALL CFDISC(DEFICO,RESOCO(1:14),TYPALC,TYPALF,FROT3D,MATTAN)
C
C ======================================================================
C ---  LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C ======================================================================
C
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APDDL  = RESOCO(1:14)//'.APDDL'
      APJEU  = RESOCO(1:14)//'.APJEU'
      APMEMO = RESOCO(1:14)//'.APMEMO'
      APPARI = RESOCO(1:14)//'.APPARI'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      CHAMCO = DEFICO(1:16)//'.CHAMCO'
      COEFCO = DEFICO(1:16)//'.COEFCO'
      CONTMA = DEFICO(1:16)//'.MAILCO'
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      MANOCO = DEFICO(1:16)//'.MANOCO'
      NDIMCO = DEFICO(1:16)//'.NDIMCO'
      NOMACO = DEFICO(1:16)//'.NOMACO'
      NORMCO = RESOCO(1:14)//'.NORMCO'
      PDDL   = DEFICO(1:16)//'.PDDLCO'
      PMANO  = DEFICO(1:16)//'.PMANOCO'
      PNOMA  = DEFICO(1:16)//'.PNOMACO'
      TANGCO = RESOCO(1:14)//'.TANGCO'
      SANSNO = DEFICO(1:16)//'.SSNOCO'
      PSANS  = DEFICO(1:16)//'.PSSNOCO'
      PZONE  = DEFICO(1:16)//'.PZONECO'
      PSURNO = DEFICO(1:16)//'.PSUNOCO'
C ======================================================================
      CALL JEVEUO(APCOEF,'E',JAPCOE)
      CALL JEVEUO(APDDL, 'E',JAPDDL)
      CALL JEVEUO(APJEU, 'E',JAPJEU)
      CALL JEVEUO(APMEMO,'E',JAPMEM)
      CALL JEVEUO(APPARI,'E',JAPPAR)
      CALL JEVEUO(APPOIN,'E',JAPPTR)
      CALL JEVEUO(CHAMCO,'L',JCHAM )
      CALL JEVEUO(COEFCO,'L',JCOEF)
      CALL JEVEUO(CONTMA,'L',JMACO)
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(MANOCO,'L',JMANO)
      CALL JEVEUO(NDIMCO,'L',JDIM )
      CALL JEVEUO(NOMACO,'L',JNOMA)
      CALL JEVEUO(NORMCO,'E',JNORMO)
      CALL JEVEUO(PDDL,  'L',JPDDL)
      CALL JEVEUO(PMANO, 'L',JPOMA)
      CALL JEVEUO(PNOMA, 'L',JPONO)
      CALL JEVEUO(TANGCO,'E',JTANGO)
      CALL JEVEUO(PZONE, 'L',JZONE )
      CALL JEVEUO(PSURNO,'L',JSUNO )
      CALL JEVEUO(SANSNO,'L',JSANS )
      CALL JEVEUO(PSANS, 'L',JPSANS)


      IF (TYPALF.NE.0) THEN
        APJEFX = RESOCO(1:14)//'.APJEFX'
        APJEFY = RESOCO(1:14)//'.APJEFY'
        APCOFR = RESOCO(1:14)//'.APCOFR'
        CALL JEVEUO(APCOFR,'E',JAPCOF)
        CALL JEVEUO(APJEFX,'E',JAPJFX)
        CALL JEVEUO(APJEFY,'E',JAPJFY)
      ENDIF

C
C ======================================================================
C --- INITIALISATION DE VARIABLES 
C ======================================================================
C
C --- VALEUR DE REAC POUR QUE LA PROJECTION NE SOIT PAS REFAITE POUR CE
C --- NOEUD ESCLAVE DANS PROJCO SI ELLE VIENT D'ETRE FAITE DANS RECHCO
C
      REAC   = 0
C
C --- COEFFICIENT MULTIPLICATEUR DE LA RELATION UNILATERALE
C
      CMULT  = ZR(JCOEF+IZONE-1)
C
C --- MULNOR : LOGIQUE QUI VAUT 1 LORSQU'ON MULTIPLIE LES COEFFICIENTS
C --- DE LA RELATION UNILATERALE PAR LES COMPOSANTES DES NORMALES
C --- (I.E. : ON EST DANS UNE ZONE OU LA RELATION UNILATERALE PORTE
C --- SUR LE DEPLACEMENT)
C
      MULNOR = (ABS(ZI(JCHAM+IZONE-1)).EQ.1)
C
C --- DIMENSION DU PROBLEME
C
      NDIM   = ZI(JDIM)
C
C --- NOMBRE MAXIMUM DE NOEUDS ESCLAVES  POUR TOUTES LES ZONES
C
      NESMAX = ZI(JDIM+8)
C
C --- NOMBRE DE NOEUDS ESCLAVES POUR LA ZONE COURANTE
C
      NESCL  = ZI(JDIM+8+IZONE)
C
C --- NOMBRE TOTAL DE NOEUDS DES SURFACES DE CONTACT
C
      NNOCO  = ZI(JDIM+4)
C
C --- ZONE SYMETRIQUE
C --- 0: PAS D'APPARIEMENT SYMETRIQUE
C --- 1: APPARIEMENT SYMETRIQUE IZONE EST UNE ZONE DE CONTACT PRINCIPALE
C --- 2: APPARIEMENT SYMETRIQUE IZONE EST UNE ZONE DE CONTACT SYMETRIQUE
C
      ZONESY = 0
      IF (NSYME.NE.0) THEN
        NZOCP = NZOCO-NSYME
        IF (IZONE.GT.NZOCP) THEN
          ZONESY = 2
        ELSE
          ZONESY = 1
        ENDIF
      ENDIF

C
C --- CREATION DES VECTEURS DE TRAVAIL POUR STOCKER LES NOEUDS ESCLAVES
C --- SE PROJETANT SUR UN NOEUD MAITRE EXACTEMENT DANS LE CAS D'UN 
C --- APPARIEMENT SYMETRIQUE
C

      IF (NIV.GE.2) THEN
       WRITE (IFM,*) '<CONTACT> <> <> APPARIEMENT M/E - PROJECTION'
      ENDIF

      IF ((IZONE.EQ.1).AND.(ZONESY.EQ.1)) THEN
        CALL WKVECT('&&CHMANO.SYMEXC','V V I',NNOCO+1,JSYEXC)
C
C --- VERIF DANS LES SURFACES SYMETRIQUES QUE LEURS NOEUDS MAITRES
C --- N'ONT PAS ETE EXCLUS PAR 'SANS_GROUP_NO' DANS LA ZONE PRINCIPALE
C --- ASSOCIEE
C
        IF (NIV.GE.2) THEN
         WRITE (IFM,*) '<CONTACT> <> <> APPARIEMENT M/E - PREPARATION'
        ENDIF
        DO 100 IZONSY=NZOCP,NZOCO
C --- ZONE PRINCIPALE ASSOCIE A LA ZONE SYMETRIQUE
          IZONPR = IZONSY - NSYME
C --- SURFACE MAITRE
          ISURFM = ZI(JZONE+IZONSY-1) + 1
C --- NOMBRE DE NOEUDS MAITRES
          NBNOM  = ZI(JSUNO+ISURFM)-ZI(JSUNO+ISURFM-1)
C --- DECALAGE DANS TABLEAU
          JDECM  = ZI(JSUNO+ISURFM-1)
C --- BOUCLE SUR LES NOEUDS MAITRES
          DO 110 L = 1,NBNOM            
            POSNOM = JDECM+L
            NUMNOM = ZI(JNOCO+POSNOM-1)
            SUPPOK = 0
            CALL CFELSN(IZONPR,NOMA,NUMNOM,POSNOM,
     &               JAPMEM,JNOCO,JPSANS,JSANS,
     &               SUPPOK)
            IF (SUPPOK.EQ.1) THEN
              ZI(JSYEXC) = ZI(JSYEXC)+1
              ZI(JSYEXC+ZI(JSYEXC)) = NUMNOM 
            ENDIF
  110     CONTINUE
  100   CONTINUE

      ENDIF
      IF ((IZONE.NE.1).AND.(ZONESY.NE.0)) THEN
        CALL JEVEUO('&&CHMANO.SYMEXC','E',JSYEXC)
      ENDIF
C
C
C ======================================================================
C --- BOUCLE SUR LES NOEUDS ESCLAVES
C ======================================================================
C
      IF (NIV.GE.2) THEN
       WRITE (IFM,*) '<CONTACT> <> <> APPARIEMENT M/E - PROJECTION'
      ENDIF

      IND    = IESCL0
C
C --- BOUCLAGE SUR LES NOEUDS ESCLAVES
C
  70  CONTINUE
C
C --- INDICE DU NOEUD ESCLAVE ET INDICE DU NOEUD MAITRE LE PLUS PROCHE
C
        POSNOE = ZI(JAPPAR+ZAPPAR*(IND-1)+1)
        POSNOM = ABS(ZI(JAPPAR+ZAPPAR*(IND-1)+2))
C
C --- NOMBRE DE MAILLES CONTENANT LE NOEUD MAITRE
C
        NBMA   = ZI(JPOMA+POSNOM) - ZI(JPOMA+POSNOM-1)
C 
C --- DECALAGE DANS LES TABLEAUX POUR LE NOEUD MAITRE
C
        JDEC   = ZI(JPOMA+POSNOM-1)
C
C --- INDICATEUR DE REACTUALISATION (PROJECTION, NORMALES)
C
        PROJ   = ABS(ZI(JAPPAR+ZAPPAR*(IND-1)+3))
C
C ---
C
        JEUMIN = R8GAEM()
        PROJOP = 'NOP'
C
C --- BOUCLE SUR LES MAILLES CONTENANT LE NOEUD MAITRE
C 
        DO 60 IMA = 1,NBMA

          POSMA = ZI(JMANO+JDEC+IMA-1)

C
C --- CALCUL DE LA PROJECTION SUR LA MAILLE, ET DU JEU MINIMUM
C          
          CALL PROJEC(IZONE,NDIM,NOMA,NEWGEO,RESOCO,DEFICO,
     &                PROJ,MULNOR,CMULT,ZONESY,
     &                POSNOE,POSMA,
     &                NORM,TANG,
     &                COEF,COFX,COFY,OLDJEU,JEU,PROYES,
     &                NBNO,POSNO,NBDDL,DDL)
C
C --- CHOIX DE L'APPARIEMENT SUIVANT LE RESULTAT DE LA PROJECTION
C
C --- PROJECTION SUR ARETE/DIAGONALE OU SIMPLEMENT SUR UNE MAILLE
C ---                             -> ON APPARIE 
C --- PROJECTION SUR UN NOEUD
C --- 1/ PAS D'APPARIEMENT SYMETRIQUE 
C ---                             -> ON APPARIE
C --- 2/ APPARIEMENT SYMETRIQUE 
C ---     PASSAGE DANS LES ZONES PRINCIPALES
C ---                             -> ON STOCKE L'INFO
C --- 3/ APPARIEMENT SYMETRIQUE 
C ---     PASSAGE DANS LES ZONES SYMETRIQUES
C ---                             -> ON VERIFIE UN EVENTUEL 
C                                    PIVOT NUL
C --- PROJECTION HORS MAILLE INTERDITE     
C ---                             -> ON SUPPRIME L'APPARIEMENT   
C

C
C --- ECART ANCIEN/NOUVEAU
C
          IF (JEUMIN.EQ.0.D0) THEN
             ECAN = 1.D0
          ELSE 
             ECAN = ABS((OLDJEU-JEUMIN)/JEUMIN)
          ENDIF

          IF ((OLDJEU.LE.JEUMIN).AND. 
     &           (ECAN.GT.1D-15)) THEN

            JEUMIN = OLDJEU
            POSMIN = POSMA
            PROMIN = PROYES

            IF (PROYES.LE.-1000) THEN
              IF (PROJOP.EQ.'ADD') THEN
                 IF (ECAN.GE.1.D-8) THEN
                    PROJOP = 'SUP'
                 ENDIF
              ELSE
                 PROJOP = 'SUP'
              ENDIF
            ELSE IF ((PROYES.GT.-1000).AND.(PROYES.LE.-300)) THEN
              IF (ZONESY.EQ.0) THEN
                PROJOP = 'ADD'
              ELSE IF (ZONESY.EQ.1) THEN
                PROJOP = 'STC'
              ELSE IF (ZONESY.EQ.2) THEN
                PROJOP = 'PIV'
              ENDIF
            ELSE IF (PROYES.GT.-300) THEN
               PROJOP = 'ADD'
            ENDIF
          ELSE
            IF (PROJOP.EQ.'ADD') THEN
               PROJOP =  'NOP'
            ENDIF
          ENDIF


          IF ((PROJOP.EQ.'ADD').OR.(PROJOP.EQ.'STC').OR.
     &        (PROJOP.EQ.'PIV')) THEN
C --- ON APPARIE (NOUVEAU OU REMPLACE ANCIEN)
            CALL CFADDM(JAPPTR,JAPPAR,JAPMEM,JAPJEU,JNORMO,JPDDL, 
     &              JTANGO,JAPCOF,JAPCOE,JAPJFX,JAPJFY,JAPDDL,
     &              TYPALF,FROT3D,
     &              POSNOE,POSMIN,IND,NESMAX,REAC,
     &              NORM,TANG,
     &              COEF,COFX,COFY,JEU,
     &              NBNO,POSNO,
     &              NBDDL,DDL)           
          ELSE IF (PROJOP.EQ.'SUP') THEN
C --- ON SUPPRIME APPARIEMENT
          ELSE IF (PROJOP.EQ.'NOP') THEN
C --- ON NE FAIT RIEN
          ENDIF

   60   CONTINUE
C
C --- 
C
        IF (PROJOP.EQ.'NOP') THEN
          TYPSUP = 0
        ELSE IF (PROJOP.EQ.'ADD') THEN
          TYPSUP = 0
        ELSE IF (PROJOP.EQ.'SUP') THEN
C 
C --- LE NOEUD ESCLAVE EST EXCLU (VA AU COIN !)
C
          TYPSUP = -3
          POSSUP = POSNOE
          ISUPP  = IND
        ELSE IF (PROJOP.EQ.'STC') THEN
C
C --- LE NOEUD ESCLAVE SE PROJETE 'PILE' SUR UN NOEUD MAITRE
C --- ON STOCKE CE NOEUD MAITRE POUR VERIFIER PLUS TARD LES PIVOTS NULS
C 
C
C --- NUMERO D'ORDRE DU NOEUD DANS LA MAILLE MAITRE
C
          PROMIN = ABS(PROMIN) - 300
          IF ((PROMIN.GT.9).OR.(PROMIN.LE.0)) THEN
            CALL UTMESS ('F','CHMANO','NUMERO NOEUD MAITRE INCORRECT')
          ENDIF
C
C --- INDICE DANS CONTNO DU NOEUD MAITRE A STOCKER
C --- NUMERO ABSOLU DU NOEUD MAITRE A STOCKER
C
          POSSTC = ZI(JNOMA+ZI(JPONO+POSMIN-1)+PROMIN-1)
          NUMSTC = ZI(JNOCO+POSSTC-1) 
C
C --- STOCKAGE 
C
          ZI(JSYEXC) = ZI(JSYEXC)+1
          ZI(JSYEXC+ZI(JSYEXC)) = NUMSTC
C 
C --- LE NOEUD ESCLAVE EST ACCEPTE
C
          TYPSUP = 0
        ELSE IF (PROJOP.EQ.'PIV') THEN
C
C --- LE NOEUD ESCLAVE SE PROJETTE 'PILE' SUR UN NOEUD MAITRE
C --- ON VERIFIE QUE CE NOEUD MAITRE NE S'EST PAS DEJA PROJETE SUR LE 
C --- NOEUD ESCLAVE OU QU'IL N'EST PAS EXCLU PAR SANS_GROUP_NO
C 
C
C --- NUMERO D'ORDRE DU NOEUD DANS LA MAILLE MAITRE
C
          PROMIN = ABS(PROMIN) - 300
          IF ((PROMIN.GT.9).OR.(PROMIN.LE.0)) THEN
            CALL UTMESS ('F','CHMANO','NUMERO NOEUD MAITRE INCORRECT')
          ENDIF
C
C --- INDICE DANS CONTNO DU NOEUD MAITRE A VERIFIER
C --- NUMERO ABSOLU DU NOEUD MAITRE A VERIFIER
C
          POSPIV = ZI(JNOMA+ZI(JPONO+POSMIN-1)+PROMIN-1)
          NUMPIV = ZI(JNOCO+POSPIV-1)
C
C --- STOCKAGE 
C
          TYPSUP = 0
          DO 11 L=1,ZI(JSYEXC)
            IF  (ZI(JSYEXC+L).EQ.NUMPIV) THEN
C 
C --- LE NOEUD ESCLAVE EST EXCLU (VA AU COIN !)
C
              TYPSUP = -2
              POSSUP = POSNOE
              ISUPP  = IND      
              GOTO 12          
            ENDIF
 11       CONTINUE
 12       CONTINUE

        ELSE
          CALL UTMESS('F','CHAMNO','OPERATION D''APPARIEMENT INCONNUE')
        ENDIF
C
C --- CHOIX DU BOUCLAGE
C
        IF (TYPSUP.EQ.0) THEN
C
C --- BOUCLAGE SUR LE NOEUD ESCLAVE SUIVANT
C --- MODIF DE APPARI POUR QUE LA PROJECTION NE SOIT PAS REFAITE POUR CE
C --- NOEUD ESCLAVE DANS PROJCO SI ELLE VIENT D'ETRE FAITE DANS RECHCO
C
          ZI(JAPPAR+ZAPPAR* (IND-1)+3) = 0 
          IND = IND + 1 
        ELSE
C
C --- EXCLUSION DU NOEUD ESCLAVE 
C
          ZI(JAPPAR+ZAPPAR* (IND-1)+3) = 0 
          CALL CFSUPM(NOMA,JAPPAR,JNOCO,JAPMEM,
     &                ISUPP,POSSUP,TYPSUP,NESMAX)

          NESCL  = NESCL -1
          IESCL  = IESCL -1

        ENDIF

      IF (IND.LE.(IESCL0 + NESCL - 1)) THEN
C       ON CONTINUE LA BOUCLE
        GOTO 70
      ELSE
C       ON SORT DE LA BOUCLE
        GOTO 80
      ENDIF
C --- BOUCLAGE SUR LES NOEUDS ESCLAVES

  80  CONTINUE
C
C --- DESTRUCTION DES VECTEURS DE TRAVAIL 
C
      IF ((IZONE.EQ.NZOCO).AND.(ZONESY.EQ.2)) THEN   
        CALL JEDETR('&&CHMANO.SYMEXC')
      ENDIF
C
C ---
C
      CALL JEDEMA()
      END
