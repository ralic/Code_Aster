      SUBROUTINE MMMRES(FONACT,DEFICO,RESOCO,DEPDEL,NOMA  ,
     &                  CNSINR,CNSPER,INST  ,VEASSE)
C 
C MODIF ALGORITH  DATE 17/11/2009   AUTEUR DESOZA T.DESOZA 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_20
C
      IMPLICIT     NONE
      LOGICAL      FONACT(*)
      CHARACTER*8  NOMA
      REAL*8       INST(*)
      CHARACTER*19 CNSINR,CNSPER
      CHARACTER*19 VEASSE(*)
      CHARACTER*24 DEFICO,RESOCO
      CHARACTER*24 DEPDEL
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - POST-TRAITEMENT)
C
C CREER LE CHAM_NO_S POUR L ARCHIVAGE DU CONTACT PAR NMARCH
C
C ----------------------------------------------------------------------
C
C
C IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
C IN  DEFICO : SD DE DEFINITION DU CONTACT
C IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE
C IN  NOMA   : NOM DU MAILLAGE
C IN  INST   : PARAMETRES INTEGRATION EN TEMPS (T+, DT, THETA)
C IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
C OUT CNSINR : CHAM_NO_S POUR L'ARCHIVAGE DU CONTACT
C OUT CNSPER : CHAM_NO_S POUR L'ARCHIVAGE DES PERCUSSIONS
C
C ------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32 JEXATR
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
C --------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------
C 
      INTEGER      INO,NDD1,INOE,IN
      INTEGER      IBID,ILONG,IMA,ICMP,I,IZONE,IPC
      INTEGER      NBNO,NBPC,NBNOT,NNOM,NNOE
      INTEGER      JCONT,JFROT,ICONEX,IATYMA
      INTEGER      NBZONE,NBNOM,NTPC,NTMAE
      INTEGER      JCONTA,JPREMI,JEUTEM,JGLIE,JGLIM
      INTEGER      JFROL,JCONL,JDEPDL,JCOOR,JDEPDE,JCOODE
      INTEGER      DIM
      INTEGER      CFMMVD,ZTABF,ZETAT,ZRESU,ZPERC,ZMAES,ZNOES
      INTEGER      NUMAMA,NUMAES,NUNO,NUNOE
      REAL*8       GLI1,GLI2,GLI
      REAL*8       RN,RNX,RNY,RNZ
      REAL*8       RTAX,RTAY,RTAZ,RTGX
      REAL*8       RTGY,RTGZ,X(2),FF(9)
      REAL*8       DEPLPM(3),DEPLPE(3),CONT,LAGSF,TAU1(3),TAU2(3)
      REAL*8       COOR(3),COORE(3),ERR(3),EPS
      CHARACTER*8  LICMP4(4),LICMP6(6),LICNT3(3)
      CHARACTER*8  ALIAS,KBID
      CHARACTER*19 FCONTS,FFROTS,DEPDES,DEPCN
      CHARACTER*19 FCTCN,FFROCN
      CHARACTER*19 NMCHEX,FCONT,FFROT
      CHARACTER*24 JEU,GLIE,GLIM,CONTAC,PREMIE
      CHARACTER*24 TABFIN,ETATCT,NDIMCO,NOESCL,CONTNO,JEUCON
      CHARACTER*24 JEUSUR,CARACF,MAESCL,NOZOCO
      INTEGER      JTABF ,JETAT ,JDIM  ,JNOESC,JNOCO ,JJEU
      INTEGER      JUSU  ,JCMCF ,JMAESC,JZOCO
      INTEGER      TYCO
      REAL*8       DELTAT,R,RX,RY,RZ,IMP,IMPX,IMPY,IMPZ
      INTEGER      IFM,NIV
      CHARACTER*24 K24BLA,K24BID
      REAL*8       R8BID
      LOGICAL      LBID
      LOGICAL      ISFONC,LCTFC,LFROTT
      INTEGER      JCNSVR,JCNSLR,JCNSVP,JCNSLP    
      PARAMETER (EPS=1.D-6)
      
C ----------------------------------------------------------------------
      DATA LICMP4
     &   / 'DX'     ,'DY'      ,
     &     'LAGS_C' ,'LAGS_F1' /   
      DATA LICMP6
     &   / 'DX'     ,'DY'      ,'DZ'      ,
     &     'LAGS_C' ,'LAGS_F1' ,'LAGS_F2' /   
      DATA LICNT3
     &   / 'DX'     ,'DY'      ,'DZ'      /      
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

      FCONT  = '&&MMMRES.CONT'
      FCONTS = '&&MMMRES.CONT_S'
      FCTCN  = '&&MMMRES.FCTCN'
      FFROT  = '&&MMMRES.FROT'
      FFROTS = '&&MMMRES.FROT_S'
      FFROCN = '&&MMMRES.FROTCN'
      DEPDES = '&&MMMRES.DEPDES'
      DEPCN  = '&&MMMRES.DEPCN'
      JEU    = '&&MMMRES.JEU'
      GLIE   = '&&MMMRES.GLIE'
      GLIM   = '&&MMMRES.GLIM'
      CONTAC = '&&MMMRES.CONTAC'
      PREMIE = '&&MMMRES.PREMIE'    
C
C --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
C 
      TABFIN = DEFICO(1:16)//'.TABFIN'
      ETATCT = RESOCO(1:14)//'.ETATCT'
      NDIMCO = DEFICO(1:16)//'.NDIMCO'
      NOESCL = DEFICO(1:16)//'.NOESCL'
      CONTNO = DEFICO(1:16)//'.NOEUCO'
      JEUCON = DEFICO(1:16)//'.JEUCON'
      JEUSUR = DEFICO(1:16)//'.JEUSUR'
      CARACF = DEFICO(1:16)//'.CARACF'
      MAESCL = DEFICO(1:16)//'.MAESCL'
      NOZOCO = DEFICO(1:16)//'.NOZOCO'
      CALL JEVEUO(TABFIN,'L',JTABF)
      CALL JEVEUO(ETATCT,'E',JETAT)
      CALL JEVEUO(NDIMCO,'L',JDIM)
      CALL JEVEUO(NOESCL,'L',JNOESC)
      CALL JEVEUO(CONTNO,'L',JNOCO)
      CALL JEVEUO(JEUCON,'L',JJEU)
      CALL JEVEUO(JEUSUR,'L',JUSU)
      CALL JEVEUO(CARACF,'L',JCMCF)
      CALL JEVEUO(MAESCL,'L',JMAESC)        
      CALL JEVEUO(NOZOCO,'L',JZOCO)
C      
      ZTABF  = CFMMVD('ZTABF')
      ZETAT  = CFMMVD('ZETAT')
      ZPERC  = CFMMVD('ZPERC') 
      ZRESU  = CFMMVD('ZRESU')
      ZMAES  = CFMMVD('ZMAES')
      ZNOES  = CFMMVD('ZNOES')
      DELTAT = INST(2)
      NBZONE = NINT(ZR(JCMCF))
      DIM    = ZI(JDIM)
      NTMAE  = ZI(JMAESC)
      K24BLA = ' '
C
C --- ACCES AU MAILLAGE
C      
      CALL JEVEUO(NOMA(1:8)//'.TYPMAIL','L',IATYMA)
      CALL JEVEUO(JEXATR(NOMA(1:8)//'.CONNEX','LONCUM'),'L',ILONG)
      CALL JEVEUO(NOMA(1:8)//'.CONNEX','L',ICONEX)
      CALL JEVEUO(NOMA(1:8)//'.COORDO    .VALE','L',JCOOR)
      CALL JEVEUO(NOMA(1:8)//'.COORDO    .DESC','L',JCOODE)
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOM,KBID,IBID)
C
C --- SAUVEGARDE DE L ETAT DE CONTACT EN CAS DE REDECOUPAGE
C
      NTPC = NINT(ZR(JTABF-1+1))
      DO 1000 IPC = 1,NTPC
C       STATUT DE CONTACT
        ZR(JETAT-1+ZETAT*(IPC-1)+1) = ZR(JTABF+ZTABF*(IPC-1)+13)
C       SEUIL DE FROTTEMENT
        ZR(JETAT-1+ZETAT*(IPC-1)+2) = ZR(JTABF+ZTABF*(IPC-1)+14)
C       COMPLIANCE (ASPERITE)
        ZR(JETAT-1+ZETAT*(IPC-1)+3) = ZR(JTABF+ZTABF*(IPC-1)+21)
C       MEMOIRE DE GLISSIERE
        ZR(JETAT-1+ZETAT*(IPC-1)+4) = ZR(JTABF+ZTABF*(IPC-1)+30)
1000  CONTINUE
C
C --- RECHERCHE DE LA METHODE D INTEGRATION
C
      DO 10 IZONE = 1,NBZONE
        CALL MMINFP(IZONE ,DEFICO,K24BLA,'INTEGRATION'      ,
     &              TYCO  ,R8BID ,K24BID,LBID)      
        IF (TYCO.NE.1) THEN
          CALL U2MESS('A','CONTACT3_16')
          GOTO 999
        ENDIF    
   10 CONTINUE
C
C --- FROTTEMENT SUR AU MOINS UNE ZONE ?
C
      LCTFC  = ISFONC(FONACT,'FROT_CONTINU')     
C
C --- TRANSFORMATION DU CHAM_NO DEPDEL
C     
      CALL CNOCNS(DEPDEL,'V',DEPDES)
C
C --- REDUCTION DU CHAM_NO_S DES DDL EN UN CHAM_NO_S DES LAGRANGES
C --- DE CONTACT/FROTTEMENT
C     
      IF (DIM.EQ.3) THEN
        IF ( LCTFC ) THEN
          NDD1 = 6
        ELSE
          NDD1 = 4
        ENDIF
        CALL CNSRED(DEPDES,0,0,NDD1,LICMP6,'V',DEPCN)
      ELSE IF (DIM.EQ.2) THEN
        IF ( LCTFC ) THEN
          NDD1 = 4
        ELSE
          NDD1 = 3
        ENDIF
        CALL CNSRED(DEPDES,0,0,NDD1,LICMP4,'V',DEPCN)
      ELSE
        CALL ASSERT(.FALSE.)   
      END IF
C
C --- ACCES AU CHAM_NO_S POUR LES DEPLACEMENTS/LAGRANGES
C
      CALL JEVEUO(DEPCN(1:19)//'.CNSV','L',JDEPDE)
      CALL JEVEUO(DEPCN(1:19)//'.CNSL','L',JDEPDL)
C
C --- ACCES AU CHAM_NO_S POUR LE CONTACT
C
      CALL JEVEUO(CNSINR(1:19)//'.CNSV','E',JCNSVR)
      CALL JEVEUO(CNSINR(1:19)//'.CNSL','E',JCNSLR)
C
C --- ACCES AU CHAM_NO_S POUR LES PERCUSSIONS
C --- ON NE REMET PAS A ZERO D'UN PAS A L'AUTRE
C
      CALL JEVEUO(CNSPER(1:19)//'.CNSV','E',JCNSVP)
      CALL JEVEUO(CNSPER(1:19)//'.CNSL','E',JCNSLP)
C
C --- INITIALISATIONS DES CHAM_NO_S
C
      DO 710 INO = 1,NBNOM
        DO 711 ICMP = 1,ZRESU
          ZL(JCNSLR-1+ZRESU*(INO-1)+ICMP) = .TRUE.
          ZR(JCNSVR-1+ZRESU*(INO-1)+ICMP) = 0.D0
 711    CONTINUE    
        DO 712 ICMP = 1,ZPERC
          ZL(JCNSLP-1+ZPERC*(INO-1)+ICMP) = .TRUE.
 712    CONTINUE        
 710  CONTINUE 
C
C --- FORCES NODALES DE CONTACT
C
      FCONT  = NMCHEX(VEASSE,'VEASSE','CNCTCC')
      CALL CNOCNS(FCONT,'V',FCONTS)
      
      CALL CNSRED(FCONTS,0,0,DIM,LICNT3,'V',FCTCN)
      CALL JEVEUO(FCTCN//'.CNSV','L',JCONT)
      CALL JEVEUO(FCTCN//'.CNSL','L',JCONL)
C
C --- FORCES NODALES DE FROTTEMENT
C
      IF (LCTFC) THEN
        FFROT  = NMCHEX(VEASSE,'VEASSE','CNCTCF')
        CALL CNOCNS(FFROT,'V',FFROTS)
        CALL CNSRED(FFROTS,0,0,DIM,LICNT3,'V',FFROCN)
        CALL JEVEUO(FFROCN//'.CNSV','L',JFROT)
        CALL JEVEUO(FFROCN//'.CNSL','L',JFROL) 
      ENDIF   
C
C --- NOMBRE DE POINTS D'INTEGRATION DE CONTACT
C
      NBNO  = 0    
      DO 20 IMA = 1,NTMAE
        NBPC = ZI(JMAESC+ZMAES*(IMA-1)+3)
        NBNO = NBNO + NBPC
   20 CONTINUE
C
C --- CREATION D OBJETS DE TRAVAIL
C      * JEU DES POINTS DE CONTACT
C      * VECTEUR DE GLISSEMENT DU NOEUD ESCLAVE
C      * VECTEUR DE GLISSEMENT DU NOEUD MAITRE
C      * INDICATEUR DE CONTACT
C      * VECTEUR LOGIQUE INDIQUANT UN PREMIER PASSAGE SUR UN NOEUD
C        CE VECTEUR EST DIMENSIONNE AU NBRE TOTAL DE NOEUD DE
C         /TOUT\ LE MAILLAGE CAR ON Y ACCEDE PAR LE NUMERO ABSOLU DU
C          NOEUD
C
      CALL WKVECT(JEU   ,'V V R',NBNO  ,JEUTEM)
      CALL WKVECT(GLIE  ,'V V R',2*NBNO,JGLIE)
      CALL WKVECT(GLIM  ,'V V R',2*NBNO,JGLIM)
      CALL WKVECT(CONTAC,'V V R',NBNO  ,JCONTA)
      CALL WKVECT(PREMIE,'V V L',NBNOM ,JPREMI)
      NBNOT = ZI(JDIM+4)
C
C --- TRAITEMENT EN DIMENSION TROIS
C
      IF (DIM.EQ.3) THEN
C
C --- CALCUL DU JEU AUX NOEUDS EN PRENANT LE MIN DES JEUX AUX NOEUDS,
C --- DU GLISSEMENT EN PRENANT LE MAX DU GLISSEMENT AUX NOEUDS
C --- DE L INDICATEUR DE CONTACT EN CONSIDERANT QU UN NOEUD EST EN
C --- CONTACT A PARTIR DU MOMENT OU IL L EST SUR AU MOINS UNE
C --- DES MAILLES
C
        NTPC  = 0
        DO 60 IMA = 1,NTMAE
          NBPC  = ZI(JMAESC+ZMAES*(IMA-1)+3)
          IZONE  = ZI(JMAESC+ZMAES*(IMA-1)+2)
C
          DO 50 INO = 1,NBPC
            NUMAES = NINT(ZR(JTABF+ZTABF*(NTPC+INO-1)+1))
            NUMAMA = NINT(ZR(JTABF+ZTABF*(NTPC+INO-1)+2))
C
C --- VECTEURS DIRECTEURS DU PLAN DE CONTACT
C
            TAU1(1) = ZR(JTABF+ZTABF*(NTPC+INO-1)+6)
            TAU1(2) = ZR(JTABF+ZTABF*(NTPC+INO-1)+7)
            TAU1(3) = ZR(JTABF+ZTABF*(NTPC+INO-1)+8)
            TAU2(1) = ZR(JTABF+ZTABF*(NTPC+INO-1)+9)
            TAU2(2) = ZR(JTABF+ZTABF*(NTPC+INO-1)+10)
            TAU2(3) = ZR(JTABF+ZTABF*(NTPC+INO-1)+11)
C
C --- DEPLACEMENT DU NOEUD ESCLAVE DE LA MAILLE ESCLAVE
C
            CALL MMELTY(NOMA,NUMAES,ALIAS,NNOE,IBID)
            X(1) = ZR(JTABF+ZTABF*(NTPC+INO-1)+3)
            X(2) = ZR(JTABF+ZTABF*(NTPC+INO-1)+12)
            CALL MMNONF(DIM,NNOE,ALIAS,X(1),X(2),FF)
            DEPLPE(1) = 0.D0
            DEPLPE(2) = 0.D0
            DEPLPE(3) = 0.D0
C
C --- DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
C --- POUR LE CALCUL DU GLISSEMENT
C
            DO 30 I = 1,NNOE
              NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAES)+I-2)
              CALL ASSERT(ZL(JDEPDL-1+NDD1*(NUNO-1)+1))
              CALL ASSERT(ZL(JDEPDL-1+NDD1*(NUNO-1)+2))
              CALL ASSERT(ZL(JDEPDL-1+NDD1*(NUNO-1)+3))
              DEPLPE(1) = DEPLPE(1)+ZR(JDEPDE-1+NDD1*(NUNO-1)+1)*FF(I)
              DEPLPE(2) = DEPLPE(2)+ZR(JDEPDE-1+NDD1*(NUNO-1)+2)*FF(I)
              DEPLPE(3) = DEPLPE(3)+ZR(JDEPDE-1+NDD1*(NUNO-1)+3)*FF(I)
   30       CONTINUE
C
C --- DEPLACEMENT DU NOEUD MAITRE,
C --- PROJETE DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
C
            CALL MMELTY(NOMA,NUMAMA,ALIAS,NNOM,IBID)
            X(1) = ZR(JTABF+ZTABF*(NTPC+INO-1)+4)
            X(2) = ZR(JTABF+ZTABF*(NTPC+INO-1)+5)
            CALL MMNONF(DIM,NNOM,ALIAS,X(1),X(2),FF)
            DEPLPM(1) = 0.D0
            DEPLPM(2) = 0.D0
            DEPLPM(3) = 0.D0
C
C --- DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
C
            DO 40 I = 1,NNOM
              NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAMA)+I-2)
              CALL ASSERT(ZL(JDEPDL-1+NDD1*(NUNO-1)+1))
              CALL ASSERT(ZL(JDEPDL-1+NDD1*(NUNO-1)+2))
              CALL ASSERT(ZL(JDEPDL-1+NDD1*(NUNO-1)+3))
              DEPLPM(1) = DEPLPM(1)+ZR(JDEPDE-1+NDD1*(NUNO-1)+1)*FF(I)
              DEPLPM(2) = DEPLPM(2)+ZR(JDEPDE-1+NDD1*(NUNO-1)+2)*FF(I)
              DEPLPM(3) = DEPLPM(3)+ZR(JDEPDE-1+NDD1*(NUNO-1)+3)*FF(I)
   40       CONTINUE
C
C --- ECRITURE SUR LES VECTEURS DE TRAVAIL DES JEUX, DU GLISSEMENT ET DE
C --- L INDICATEUR DE CONTACT
C
            ZR(JGLIE+2* (NTPC+INO-1))   = DEPLPE(1)*TAU1(1) +
     &                                    DEPLPE(2)*TAU1(2) +
     &                                    DEPLPE(3)*TAU1(3)
            ZR(JGLIE+2* (NTPC+INO-1)+1) = DEPLPE(1)*TAU2(1) +
     &                                    DEPLPE(2)*TAU2(2) + 
     &                                    DEPLPE(3)*TAU2(3)
            ZR(JGLIM+2* (NTPC+INO-1))   = DEPLPM(1)*TAU1(1) +
     &                                    DEPLPM(2)*TAU1(2) +
     &                                    DEPLPM(3)*TAU1(3)
            ZR(JGLIM+2* (NTPC+INO-1)+1) = DEPLPM(1)*TAU2(1) +
     &                                    DEPLPM(2)*TAU2(2) + 
     &                                    DEPLPM(3)*TAU2(3)
   50     CONTINUE
          NTPC = NTPC + NBPC
   60   CONTINUE
C
C --- BOUCLE SUR TOUS LES NOEUDS DE CONTACT
C
        DO 100 INO = 1,NBNOT
          IF (ZR(JNOESC+ZNOES*(INO-1)).EQ.-1.D0) THEN
            GLI1 = 0.D0
            GLI2 = 0.D0
            GLI  = 0.D0
            RTAX = 0.D0
            RTAY = 0.D0
            RTAZ = 0.D0
            RTGX = 0.D0
            RTGY = 0.D0
            RTGZ = 0.D0
            RN   = 0.D0
            RNX  = 0.D0
            RNY  = 0.D0
            RNZ  = 0.D0
C
C --- COORD ABSOLUES DU NOEUD TRAITE: COOR
C
            NUNOE   = ZI(JNOCO+INO-1)
            ZL(JPREMI-1+NUNOE) = .FALSE.
            COOR(1) = ZR(JCOOR-1+3*(NUNOE-1)+1)
            COOR(2) = ZR(JCOOR-1+3*(NUNOE-1)+2)
            COOR(3) = ZR(JCOOR-1+3*(NUNOE-1)+3)
C
C --- ON REBOUCLE SUR LES POINTS D INTEGRATION POUR TESTER
C --- LEUR COINCIDENCE AVEC LES NOEUDS DE CONTACT
C
            NTPC = 0
            INOE = 0
            DO 90 IMA = 1,NTMAE
              NBPC   = ZI(JMAESC+ZMAES*(IMA-1)+3)
              IZONE  = ZI(JMAESC+ZMAES*(IMA-1)+2)
              DO 80 IN = 1,NBPC
                NUMAES = NINT(ZR(JTABF+ZTABF*(NTPC+IN-1)+1))
C
C --- COORD ABSOLUES DU POINT D INTEGRATION: COORE
C
                CALL MMELTY(NOMA,NUMAES,ALIAS,NNOE,IBID)
                X(1) = ZR(JTABF+ZTABF*(NTPC+IN-1)+3)
                X(2) = ZR(JTABF+ZTABF*(NTPC+IN-1)+12)
                CALL MMNONF(DIM,NNOE,ALIAS,X(1),X(2),FF)
                COORE(1) = 0.D0
                COORE(2) = 0.D0
                COORE(3) = 0.D0
C
C --- DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
C --- POUR LE CALCUL DU GLISSEMENT
C
                DO 70 I = 1,NNOE
                  NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAES)+I-2)
                  COORE(1) = COORE(1)+ZR(JCOOR-1+3*(NUNO-1)+1)*FF(I)
                  COORE(2) = COORE(2)+ZR(JCOOR-1+3*(NUNO-1)+2)*FF(I)
                  COORE(3) = COORE(3)+ZR(JCOOR-1+3*(NUNO-1)+3)*FF(I)
   70           CONTINUE
                IF (COOR(1).NE.0) THEN
                  ERR(1) = ABS((COOR(1)-COORE(1))/COOR(1))
                ELSE
                  ERR(1) = ABS(COORE(1))
                END IF
                IF (COOR(2).NE.0) THEN
                  ERR(2) = ABS((COOR(2)-COORE(2))/COOR(2))
                ELSE
                  ERR(2) = ABS(COORE(2))
                END IF
                IF (COOR(3).NE.0) THEN
                  ERR(3) = ABS((COOR(3)-COORE(3))/COOR(3))
                ELSE
                  ERR(3) = ABS(COORE(3))
                END IF
                INOE = INOE + 1
C
C --- POST-TRAITEMENT DES RESULTATS
C --- RECUPERATION DES DONNEES AUX NOEUDS
C
                IF (ERR(1).LE.EPS .AND. ERR(2).LE.EPS .AND.
     &              ERR(3).LE.EPS) THEN
                  IF (.NOT.ZL(JPREMI-1+NUNOE)) THEN
                    ZL(JPREMI-1+NUNOE) = .TRUE.
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+2) = ZR(JJEU-1+INOE)
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+20)= ZR(JUSU-1+INOE)
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+1) = ZR(JTABF+
     &                                              ZTABF*(INOE-1)+13)
                    GLI1 = ZR(JGLIE+2* (INOE-1)) -
     &                     ZR(JGLIM+2* (INOE-1))
                    GLI2 = ZR(JGLIE+2* (INOE-1)+1) -
     &                     ZR(JGLIM+2* (INOE-1)+1)
                    GLI  = SQRT(GLI1**2+GLI2**2)
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+7) = GLI1
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+8) = GLI2
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+9) = GLI
                  ELSE
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+2) = 
     &                MIN(ZR(JCNSVR-1+ZRESU*(NUNOE-1)+2),
     &                    ZR(JJEU-1+INOE))
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+1) = 
     &                MAX(ZR(JCNSVR-1+ZRESU*(NUNOE-1)+1),
     &                    ZR(JTABF+ZTABF*(INOE-1)+13))
                    GLI1 = ZR(JGLIE+2* (INOE-1)) -
     &                     ZR(JGLIM+2* (INOE-1))
                    GLI2 = ZR(JGLIE+2* (INOE-1)+1) -
     &                     ZR(JGLIM+2* (INOE-1)+1)
                    GLI  = SQRT(GLI1**2+GLI2**2)
                    IF (GLI.GT.ZR(JCNSVR-1+ZRESU*(NUNOE-1)+9)) THEN
                      ZR(JCNSVR-1+ZRESU* (NUNOE-1)+9) = GLI
                      ZR(JCNSVR-1+ZRESU* (NUNOE-1)+7) = GLI1
                      ZR(JCNSVR-1+ZRESU* (NUNOE-1)+8) = GLI2
                    END IF
                  END IF
C
C --- ETAT DU CONTACT: CONT
C                  
                  CONT = ZR(JCNSVR-1+ZRESU* (NUNOE-1)+1)
C
C --- RECUPERATION DES FORCES NODALES DE CONTACT
C
                  IF (CONT.GE.1.D0) THEN
                    CALL ASSERT(ZL(JCONL-1+3*(NUNOE-1)+1))
                    CALL ASSERT(ZL(JCONL-1+3*(NUNOE-1)+2))
                    CALL ASSERT(ZL(JCONL-1+3*(NUNOE-1)+3))
                    RNX = ZR(JCONT-1+3*(NUNOE-1)+1)
                    RNY = ZR(JCONT-1+3*(NUNOE-1)+2)
                    RNZ = ZR(JCONT-1+3*(NUNOE-1)+3)
                    RN  = SQRT(RNX**2+RNY**2+RNZ**2)
C
C --- Y-A-T-IL DU FROTTEMENT ?
C
                    IZONE = ZI(JZOCO-1+INO)
                    CALL MMINFP(IZONE ,DEFICO,K24BLA,'FROTTEMENT_ZONE',
     &                          IBID  ,R8BID ,K24BID,LFROTT)
                    IF (LFROTT) THEN
C
C --- NORME DU MULTIPLICATEUR DE LAGRANGE DU FROTTEMENT
C
                      LAGSF = SQRT((ZR(JDEPDE-1+6*(NUNOE-1)+5))**2+
     &                             (ZR(JDEPDE-1+6*(NUNOE-1)+6))**2)
                      CALL ASSERT(ZL(JFROL-1+3* (NUNOE-1)+1))
                      CALL ASSERT(ZL(JFROL-1+3* (NUNOE-1)+2))
                      CALL ASSERT(ZL(JFROL-1+3* (NUNOE-1)+3))
                      IF (LAGSF.GE.0.999D0) THEN
C
C --- LE NOEUD EST GLISSANT
C
                        RTGX = ZR(JFROT-1+3* (NUNOE-1)+1)
                        RTGY = ZR(JFROT-1+3* (NUNOE-1)+2)
                        RTGZ = ZR(JFROT-1+3* (NUNOE-1)+3)
                        ZR(JCNSVR-1+ZRESU* (NUNOE-1)+1) = 2.D0
                      ELSE
C
C --- LE NOEUD EST ADHERENT
C
                        RTAX = ZR(JFROT-1+3* (NUNOE-1)+1)
                        RTAY = ZR(JFROT-1+3* (NUNOE-1)+2)
                        RTAZ = ZR(JFROT-1+3* (NUNOE-1)+3)
                      END IF
                    ELSE
                      LAGSF = 0.D0
                    ENDIF
                  END IF
                END IF
   80         CONTINUE
              NTPC = NTPC + NBPC
   90       CONTINUE
C
C --- CALCUL DES PERCUSSIONS
C
              R = SQRT((RNX+RTAX+RTGX)**2.D0+(RNY+RTAY+RTGY)**2.D0+
     &                 (RNZ+RTAZ+RTGZ)**2.D0)
     
                   
              ZL(JCNSLP-1+ZPERC*(NUNOE-1)+1) = .TRUE.
              ZL(JCNSLP-1+ZPERC*(NUNOE-1)+2) = .TRUE.
              ZL(JCNSLP-1+ZPERC*(NUNOE-1)+3) = .TRUE.
              ZL(JCNSLP-1+ZPERC*(NUNOE-1)+4) = .TRUE.
C              
              IF (R .LE. EPS) THEN           
                IMP    = 0.D0
                IMPX   = 0.D0
                IMPY   = 0.D0
                IMPZ   = 0.D0
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+1) = 0.D0
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+2) = 0.D0
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+3) = 0.D0
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+4) = 0.D0
              ELSE    
                RX     = RNX + RTAX + RTGX
                RY     = RNY + RTAY + RTGY
                RZ     = RNZ + RTAZ + RTGZ                     
                     
                IMP    = ZR(JCNSVP-1+ZPERC*(NUNOE-1)+1) + R*DELTAT
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+1) = IMP
                IMPX   = ZR(JCNSVP-1+ZPERC*(NUNOE-1)+2) + RX*DELTAT
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+2) = IMPX                
                IMPY   = ZR(JCNSVP-1+ZPERC*(NUNOE-1)+3) + RY*DELTAT
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+3) = IMPY   
                IMPZ   = ZR(JCNSVP-1+ZPERC*(NUNOE-1)+4) + RZ*DELTAT
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+4) = IMPZ  
                         
              ENDIF
C
C --- ARCHIVAGE DES RESULTATS DANS LE CHAM_NO_S CREE
C
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+3) = RN
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+4) = RNX
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+5) = RNY
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+6) = RNZ
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+10)= RTAX
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+11)= RTAY
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+12)= RTAZ
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+13)= RTGX
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+14)= RTGY
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+15)= RTGZ
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+16)= RNX + RTAX + RTGX
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+17)= RNY + RTAY + RTGY
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+18)= RNZ + RTAZ + RTGZ
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+19)= SQRT((RNX+RTAX+RTGX)**2 +
     &                                              (RNY+RTAY+RTGY)**2 +
     &                                              (RNZ+RTAZ+RTGZ)**2)
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+21)= IMP
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+22)= IMPX
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+23)= IMPY
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+24)= IMPZ
C              
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+1) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+2) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+3) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+4) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+5) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+6) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+7) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+8) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+9) = .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+10)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+11)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+12)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+13)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+14)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+15)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+16)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+17)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+18)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+19)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+20)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+21)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+22)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+23)= .TRUE.
              ZL(JCNSLR-1+ (NUNOE-1)*ZRESU+24)= .TRUE. 
            END IF
  100     CONTINUE
C
C --- TRAITEMENT EN DIMENSION DEUX
C     
      ELSE IF (DIM.EQ.2) THEN
C
C --- CALCUL DU JEU AUX NOEUDS EN PRENANT LE MIN DES JEUX AUX NOEUDS,
C --- DU GLISSEMENT EN PRENANT LE MAX DU GLISSEMENT AUX NOEUDS
C --- DE L INDICATEUR DE CONTACT EN CONSIDERANT QU UN NOEUD EST EN
C --- CONTACT A PARTIR DU MOMENT OU IL L EST SUR AU MOINS UNE
C --- DES MAILLES
C
        NTPC = 0
        DO 140 IMA = 1,NTMAE
          IZONE  = ZI(JMAESC+ZMAES*(IMA-1)+2)
          NBPC  = ZI(JMAESC+ZMAES* (IMA-1)+3)
          DO 130 INO = 1,NBPC
            NUMAES = NINT(ZR(JTABF+ZTABF*(NTPC+INO-1)+1))
            NUMAMA = NINT(ZR(JTABF+ZTABF*(NTPC+INO-1)+2))
C
C --- VECTEURS DIRECTEURS DU PLAN DE CONTACT
C
            TAU1(1) = ZR(JTABF+ZTABF*(NTPC+INO-1)+6)
            TAU1(2) = ZR(JTABF+ZTABF*(NTPC+INO-1)+7)
C
C --- DEPLACEMENT DU NOEUD ESCLAVE DE LA MAILLE ESCLAVE
C
            CALL MMELTY(NOMA,NUMAES,ALIAS,NNOE,IBID)
            X(1) = ZR(JTABF+ZTABF*(NTPC+INO-1)+3)
            X(2) = ZR(JTABF+ZTABF*(NTPC+INO-1)+12)
            CALL MMNONF(DIM,NNOE,ALIAS,X(1),X(2),FF)
            DEPLPE(1) = 0.D0
            DEPLPE(2) = 0.D0
C
C --- DEPLACEMENT TANGENTIEL DU NOEUD ESCLAVE DE LA MAILLE ESCLAVE
C
            DO 110 I = 1,NNOE
              NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAES)+I-2)
              CALL ASSERT(ZL(JDEPDL-1+NDD1*(NUNO-1)+1))
              CALL ASSERT(ZL(JDEPDL-1+NDD1*(NUNO-1)+2))
              DEPLPE(1) = DEPLPE(1)+ZR(JDEPDE-1+NDD1*(NUNO-1)+1)*FF(I)
              DEPLPE(2) = DEPLPE(2)+ZR(JDEPDE-1+NDD1*(NUNO-1)+2)*FF(I)
  110       CONTINUE
C
C --- DEPLACEMENT DU NOEUD MAITRE,
C --- PROJETE DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
C
            CALL MMELTY(NOMA,NUMAMA,ALIAS,NNOM,IBID)
            X(1) = ZR(JTABF+ZTABF*(NTPC+INO-1)+4)
            X(2) = ZR(JTABF+ZTABF*(NTPC+INO-1)+5)
            CALL MMNONF(DIM,NNOM,ALIAS,X(1),X(2),FF)
            DEPLPM(1) = 0.D0
            DEPLPM(2) = 0.D0
C
C --- DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
C
            DO 120 I = 1,NNOM
              NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAMA)+I-2)
              CALL ASSERT(ZL(JDEPDL-1+NDD1*(NUNO-1)+1))
              CALL ASSERT(ZL(JDEPDL-1+NDD1*(NUNO-1)+2))
              DEPLPM(1) = DEPLPM(1)+ZR(JDEPDE-1+NDD1*(NUNO-1)+1)*FF(I)
              DEPLPM(2) = DEPLPM(2)+ZR(JDEPDE-1+NDD1*(NUNO-1)+2)*FF(I)
  120       CONTINUE
C
C --- ECRITURE SUR LES VECTEURS DE TRAVAIL DES JEUX, DU GLISSEMENT ET DE
C --- L INDICATEUR DE CONTACT
C
            ZR(JGLIE+NTPC+INO-1) = DEPLPE(1)*TAU1(1) +
     &                             DEPLPE(2)*TAU1(2)
            ZR(JGLIM+NTPC+INO-1) = DEPLPM(1)*TAU1(1) +
     &                             DEPLPM(2)*TAU1(2)

  130     CONTINUE
          NTPC = NTPC + NBPC
  140   CONTINUE
C
C --- BOUCLE SUR TOUS LES NOEUDS DE CONTACT
C
        DO 180 INO = 1,NBNOT
          IF (ZR(JNOESC+ZNOES*(INO-1)).EQ.-1.D0) THEN
            GLI1 = 0.D0
            GLI  = 0.D0
            RTAX = 0.D0
            RTAY = 0.D0
            RTGX = 0.D0
            RTGY = 0.D0
            RNX  = 0.D0
            RNY  = 0.D0
            RN   = 0.D0
C
C --- NUMERO ABSOLU DU NOEUD ESCL: NUNOE ET COORD ABSOLUES: COOR
C
            NUNOE = ZI(JNOCO+INO-1)
            COOR(1) = ZR(JCOOR-1+3* (NUNOE-1)+1)
            COOR(2) = ZR(JCOOR-1+3* (NUNOE-1)+2)
            COOR(3) = ZR(JCOOR-1+3* (NUNOE-1)+3)
C
C --- ON REBOUCLE SUR LES POINTS D INTEGRATION POUR TESTER
C --- LEUR COINCIDENCE AVEC LES NOEUDS DE CONTACT
C
            NTPC = 0
            INOE = 0
            ZL(JPREMI-1+NUNOE) = .FALSE.
            DO 170 IMA = 1,NTMAE
              NBPC  = ZI(JMAESC+ZMAES*(IMA-1)+3)
              IZONE  = ZI(JMAESC+ZMAES*(IMA-1)+2)
              DO 160 IN = 1,NBPC
                NUMAES = NINT(ZR(JTABF+ZTABF*(NTPC+IN-1)+1))
C
C --- COORD ABSOLUES DU POINT D INTEGRATION: COORE
C
                CALL MMELTY(NOMA,NUMAES,ALIAS,NNOE,IBID)
                X(1) = ZR(JTABF+ZTABF*(NTPC+IN-1)+3)
                X(2) = ZR(JTABF+ZTABF*(NTPC+IN-1)+12)
                CALL MMNONF(DIM,NNOE,ALIAS,X(1),X(2),FF)
                COORE(1) = 0.D0
                COORE(2) = 0.D0
C
C --- DEPLACEMENT DE LA PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
C --- POUR LE CALCUL DU GLISSEMENT
C                
                DO 150 I = 1,NNOE
                  NUNO = ZI(ICONEX+ZI(ILONG-1+NUMAES)+I-2)
                  COORE(1) = COORE(1)+ZR(JCOOR-1+3*(NUNO-1)+1)*FF(I)
                  COORE(2) = COORE(2)+ZR(JCOOR-1+3*(NUNO-1)+2)*FF(I)
  150           CONTINUE
                IF (COOR(1).NE.0) THEN
                  ERR(1) = ABS((COOR(1)-COORE(1))/COOR(1))
                ELSE
                  ERR(1) = ABS(COORE(1))
                END IF
                IF (COOR(2).NE.0) THEN
                  ERR(2) = ABS((COOR(2)-COORE(2))/COOR(2))
                ELSE
                  ERR(2) = ABS(COORE(2))
                END IF
                INOE = INOE + 1
C
C --- POST-TRAITEMENT DES RESULTATS
C --- RECUPERATION DES DONNEES AUX NOEUDS
C
                IF (ERR(1).LE.EPS .AND. ERR(2).LE.EPS) THEN
                  IF (.NOT.ZL(JPREMI-1+NUNOE)) THEN
                    ZL(JPREMI-1+NUNOE) = .TRUE.
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+2) = ZR(JJEU-1+INOE)
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+20)= ZR(JUSU-1+INOE)
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+1) = ZR(JTABF+
     &                                             ZTABF*(INOE-1)+13)

                    GLI1 = ZR(JGLIE+INOE-1) - ZR(JGLIM+INOE-1)
                    GLI  = SQRT(GLI1**2)
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+7) = GLI1
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+9) = GLI
                  ELSE
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+2) =
     &                MIN(ZR(JCNSVR-1+ZRESU*(NUNOE-1)+2),
     &                    ZR(JJEU-1+INOE))
                    ZR(JCNSVR-1+ZRESU*(NUNOE-1)+1) =
     &                MAX(ZR(JCNSVR-1+ZRESU*(NUNOE-1)+1),
     &                    ZR(JTABF+ZTABF*(INOE-1)+13))
                    GLI1 = ZR(JGLIE+INOE-1) - ZR(JGLIM+INOE-1)
                    GLI  = SQRT(GLI1**2)
                    IF (GLI.GT.ZR(JCNSVR-1+ZRESU*(NUNOE-1)+9)) THEN
                      ZR(JCNSVR-1+ZRESU*(NUNOE-1)+9) = GLI
                      ZR(JCNSVR-1+ZRESU*(NUNOE-1)+7) = GLI1
                    END IF
                  END IF
C
C --- ETAT DU CONTACT: CONT
C
                  CONT = ZR(JCNSVR-1+ZRESU*(NUNOE-1)+1)
C
C --- RECUPERATION DES FORCES NODALES DE CONTACT
C                    
                  IF (CONT.GE.1.D0) THEN
                    CALL ASSERT(ZL(JCONL-1+2*(NUNOE-1)+1))
                    CALL ASSERT(ZL(JCONL-1+2*(NUNOE-1)+2))
                    RNX = ZR(JCONT-1+2*(NUNOE-1)+1)
                    RNY = ZR(JCONT-1+2*(NUNOE-1)+2)
                    
                    RN  = SQRT(RNX**2+RNY**2)
C
C --- Y-A-T-IL DU FROTTEMENT ?
C
                    IZONE = ZI(JZOCO-1+INO)
                    CALL MMINFP(IZONE ,DEFICO,K24BLA,'FROTTEMENT_ZONE',
     &                          IBID  ,R8BID ,K24BID,LFROTT)
                    IF (LFROTT) THEN
C
C --- NORME DU MULTIPLICATEUR DE LAGRANGE DU FROTTEMENT
C
                      LAGSF = ABS(ZR(JDEPDE-1+4* (NUNOE-1)+4))
                      CALL ASSERT(ZL(JFROL-1+2* (NUNOE-1)+1))
                      CALL ASSERT(ZL(JFROL-1+2* (NUNOE-1)+2))
                      IF (LAGSF.GE.0.999D0) THEN
C
C --- LE NOEUD EST GLISSANT
C
                        RTGX = ZR(JFROT-1+2* (NUNOE-1)+1)
                        RTGY = ZR(JFROT-1+2* (NUNOE-1)+2)
                        ZR(JCNSVR-1+ZRESU*(NUNOE-1)+1) = 2.D0
                      ELSE
C
C --- LE NOEUD EST ADHERENT
C
                        RTAX = ZR(JFROT-1+2* (NUNOE-1)+1)
                        RTAY = ZR(JFROT-1+2* (NUNOE-1)+2)
                      END IF
                    ELSE
                      LAGSF = 0.D0
                    ENDIF
                  END IF
                END IF
  160         CONTINUE
              NTPC = NTPC + NBPC
  170       CONTINUE
C
C --- CALCUL DES PERCUSSIONS
C
              R = SQRT((RNX+RTAX+RTGX)**2.D0+(RNY+RTAY+RTGY)**2.D0)
              ZL(JCNSLP-1+ZPERC*(NUNOE-1)+1) = .TRUE.
              ZL(JCNSLP-1+ZPERC*(NUNOE-1)+2) = .TRUE.
              ZL(JCNSLP-1+ZPERC*(NUNOE-1)+3) = .TRUE.
C
              IF (R .LE. EPS) THEN
                IMP    = 0.D0
                IMPX   = 0.D0
                IMPY   = 0.D0
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+1) = 0.D0
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+2) = 0.D0
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+3) = 0.D0
              ELSE
                RX     = RNX + RTAX + RTGX
                RY     = RNY + RTAY + RTGY
                IMP    = ZR(JCNSVP-1+ZPERC*(NUNOE-1)+1) + R*DELTAT
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+1) = IMP
                IMPX   = ZR(JCNSVP-1+ZPERC*(NUNOE-1)+2) + RX*DELTAT
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+2) = IMPX
                IMPY   = ZR(JCNSVP-1+ZPERC*(NUNOE-1)+3) + RY*DELTAT
                ZR(JCNSVP-1+ZPERC*(NUNOE-1)+3) = IMPY
              ENDIF
C
C --- ARCHIVAGE DES RESULTATS DANS LE CHAM_NO_S CREE
C
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+3) = RN
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+4) = RNX
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+5) = RNY
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+10)= RTAX
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+11)= RTAY
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+13)= RTGX
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+14)= RTGY
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+16)= RNX + RTAX + RTGX
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+17)= RNY + RTAY + RTGY
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+19)= R
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+21)= IMP
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+22)= IMPX
              ZR(JCNSVR-1+ZRESU*(NUNOE-1)+23)= IMPY
C
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+1) = .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+2) = .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+3) = .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+4) = .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+5) = .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+7) = .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+9) = .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+10)= .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+11)= .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+13)= .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+14)= .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+16)= .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+17)= .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+19)= .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+20)= .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+21)= .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+22)= .TRUE.
              ZL(JCNSLR-1+(NUNOE-1)*ZRESU+23)= .TRUE.
            END IF
  180     CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      END IF
C
C --- MENAGE
C
      CALL JEDETR(FCONT)
      CALL DETRSD('CHAMP',FCONTS)
      CALL DETRSD('CHAMP',FCTCN)
      CALL JEDETR(FFROT)
      CALL DETRSD('CHAMP',FFROTS)
      CALL DETRSD('CHAMP',FFROCN)
      CALL DETRSD('CHAMP',DEPDES)
      CALL DETRSD('CHAMP',DEPCN)
      CALL JEDETR(JEU)
      CALL JEDETR(GLIE)
      CALL JEDETR(GLIM)
      CALL JEDETR(CONTAC)
      CALL JEDETR(PREMIE)
C
  999 CONTINUE
      CALL JEDEMA()
      END
