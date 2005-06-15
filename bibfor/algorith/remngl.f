      SUBROUTINE REMNGL(NOMRES,TYPSD,MODCYC,PROFNO,INDIRF,MAILSK)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C MODIF ALGORITH  DATE 15/06/2005   AUTEUR VABHHTS J.PELLET 
C
C  BUT:  < RESTITUTION MAC-NEAL GLOBALE >
C
C  RESTITUER LES RESULTATS ISSUS D'UN CALCUL CYCLIQUE AVEC DES
C  INTERFACES DE TYPE MAC-NEAL
C     => RESULTAT COMPOSE DE TYPE MODE_MECA DEJA ALLOUE PAR LA
C        ROUTINE APPELLANTE
C
C  DONNEES DU PROFCHNO DEJA CONSTITUE ET DE LA TABLE INDIRECTION
C  DES NUMEROS EQUATIONS CORRESPONDANTES (COLLECTION NUMEROTEE
C  POINTEE PAR LES NUMEROS DE SECTEUR)
C-----------------------------------------------------------------------
C
C NOMRES  /I/: NOM UT DU CONCEPT RESULTAT A REMPLIR
C MODCYC  /I/: NOM UT DU RESULTAT ISSU DU CALCUL CYCLIQUE
C PROFNO  /I/: NOM K19 DU PROFIL CHAMNO DEJA CONSTITUE
C INDIRF  /I/: NOM K24 DE LA FAMILLE DES INDIRECTIONS
C MAILSK  /I/: NOM K8 DU MAILLAGE SKELETTE
C TYPSD   /I/: NOM DU TYPE DE STRUCTURE DE DONNEES RESULTAT
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*32 JEXNUM
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*8   NOMRES,BASMOD,MODCYC,INTF,KBID,MAILSK,K8B
      CHARACTER*16  DEPL,TYPSD,TYPSUP(2)
      CHARACTER*19  CHAMVA,NUMDDL,PROFNO,MASS
      CHARACTER*24  FLEXDR,FLEXGA,FLEXAX,TETGD,TETAX
      CHARACTER*24  INDIRF,CREFE(2)
      COMPLEX*16    DEPHC,DEPHCO
      REAL*8        PARA(2),DEPI,R8DEPI,FACT,GENEK,BETA
C
C-----------------------------------------------------------------------
C     
      DATA DEPL   /'DEPL            '/
      DATA TYPSUP /'BASE_MODALE     ','MODE_MECA       '/
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      DEPI = R8DEPI()
C
C-----VERIFICATION DU TYPE DE STRUCTURE RESULTAT------------------------
C
      IF (TYPSD.NE.TYPSUP(1) .AND. TYPSD.NE.TYPSUP(2)) THEN
        CALL UTDEBM('F','REMNGL',
     +              'ARRET SUR TYPE DE RESULTAT NON SUPPORTE')
        CALL UTIMPK('L',' TYPE DONNE --> ',1,TYPSD)
        CALL UTIMPK('L',' TYPES SUPPORTES --> ',2,TYPSUP)
        CALL UTFINM
      ENDIF
C
C-----REMPLISSAGE DU CREFE POUR CREATION CHAMNO-------------------------
C
      CREFE(1) = MAILSK
      CREFE(2) = PROFNO
C
C-----RECUPERATION DE LA BASE MODALE AMONT------------------------------
C
      CALL JEVEUO(MODCYC//'      .CYCL.REFE','L',LLREF)
      BASMOD = ZK24(LLREF+2)
C
C-----RECUPERATION DU .DESC---------------------------------------------
C
      CALL JEVEUO(MODCYC//'      .CYCL.DESC','L',LLDESC)
      NBMOD = ZI(LLDESC)
      NBDDR = ZI(LLDESC+1)
      NBDAX = ZI(LLDESC+2)
C
C-----RECUPERATION DU NOMBRE DE SECTEURS--------------------------------
C
      CALL JEVEUO(MODCYC//'      .CYCL.NBSC','L',LLNSEC)
      NBSEC  = ZI(LLNSEC)
      MDIAPA = INT(NBSEC/2)*INT(1-NBSEC+(2*INT(NBSEC/2)))
C
C-----RECUPERATION DES NOMBRES DE DIAMETRES NODAUX----------------------
C
      CALL JEVEUO(MODCYC//'      .CYCL.DIAM','L',LLDIAM)
      CALL JELIRA(MODCYC//'      .CYCL.DIAM','LONMAX',NBDIA,K8B)
      NBDIA = NBDIA / 2
C
C-----RECUPERATION DU NOMBRE DE DDL PHYSIQUES DU SECTEUR----------------
C
      CALL JEVEUO(BASMOD//'           .REFD','L',LLREF)
      INTF   = ZK24(LLREF)
      NUMDDL = ZK24(LLREF+1)
      CALL DISMOI('F','NB_EQUA',NUMDDL,'NUME_DDL',NEQSEC,K8B,IER)
      CALL DISMOI('F','NB_CMP_MAX',INTF,'INTERF_DYNA',NBCMP,K8B,IER)
C
C-----RECUPERATION DU NOMBRE DE DDL PHYSIQUES GLOBAUX-------------------
C
      CALL JELIRA(PROFNO//'.DEEQ','LONMAX',NEQ,K8B)
      NEQ = NEQ / 2
C
C-----RECUPERATION DES FREQUENCES---------------------------------------
C
      CALL JEVEUO(MODCYC//'     .CYCL.FREQ','L',LLFREQ)
C
C-----RECUPERATION MATRICE DE MASSE-------------------------------------
C
      CALL JEVEUO(BASMOD//'           .REFD','L',LLREF)
      MASS = ZK24(LLREF+3)
      CALL MTEXIS(MASS,IER)
      IF (IER.EQ.0) THEN
        CALL UTDEBM('F','REMNGL','ARRET SUR MATRICE INEXISTANTE')
        CALL UTIMPK('L','MATRICE:',1,MASS(1:8))
        CALL UTFINM
      ENDIF
      CALL MTDSCR(MASS)
      CALL JEVEUO(MASS(1:19)//'.&INT','E',LMASS)
C
C-----ALLOCATION DES VECTEURS DE TRAVAIL--------------------------------
C
      CALL WKVECT('&&REMNGL.VEC.TRAVC','V V C',NEQSEC,LTVEZT)
      CALL WKVECT('&&REMNGL.VEC.COMP' ,'V V C',NEQSEC,LTVECO)
      CALL WKVECT('&&REMNGL.VEC.REEL' ,'V V R',NEQSEC,LTVERE)
C
C-----RECUPERATION DES NUMEROS D'INTERFACE------------------------------
C
      CALL JEVEUO(MODCYC//'      .CYCL.NUIN','L',LLNUMI)
      NUMD = ZI(LLNUMI)
      NUMG = ZI(LLNUMI+1)
      NUMA = ZI(LLNUMI+2)
C
C-----CALCUL DES MATRICES DE FLEXIBILITE RESIDUELLE---------------------
C
      KBID=' '
      CALL BMNODI(BASMOD,KBID,'         ',NUMD,0,IBIB,NBDDR)
      FLEXDR='&&REMNGL.FLEX.DROITE'
      CALL WKVECT(FLEXDR,'V V R',NBDDR*NEQSEC,LTFLDR)
      IBID = 0
      CALL FLEXIB(BASMOD,NBMOD,ZR(LTFLDR),NEQSEC,NBDDR,IBID,NUMD)
C
      FLEXGA='&&REMNGL.FLEX.GAUCHE'
      CALL WKVECT(FLEXGA,'V V R',NBDDR*NEQSEC,LTFLGA)
      CALL FLEXIB(BASMOD,NBMOD,ZR(LTFLGA),NEQSEC,NBDDR,IBID,NUMG)
C
      IF (NUMA.GT.0) THEN
        FLEXAX='&&REMNGL.FLEX.AXE'
        KBID=' '
        CALL BMNODI(BASMOD,KBID,'         ',NUMA,0,IBID,NBDAX)
        CALL WKVECT(FLEXAX,'V V R',NBDAX*NEQSEC,LTFLAX)
        CALL FLEXIB(BASMOD,NBMOD,ZR(LTFLAX),NEQSEC,NBDDR,IBID,NUMA)
      ENDIF
C
C-----CALCUL DES MATRICES DE CHANGEMENT DE BASE TETA--------------------
C
      TETGD='&&REMNGL.TETGD'
      CALL WKVECT(TETGD,'V V R',NBDDR*NBDDR,LTETGD)
      CALL CTETGD(BASMOD,NUMD,NUMG,NBSEC,ZR(LTETGD),NBDDR)
C
      IF (NUMA.GT.0) THEN
        TETAX='&&REMNGL.TETAX'
        CALL WKVECT(TETAX,'V V R',NBDAX*NBDAX,LTETAX)
        CALL CTETGD(BASMOD,NUMA,NUMG,NBSEC,ZR(LTETAX),NBDAX)
      ENDIF
C
C-----CLASSEMENT DES MODES PROPRES--------------------------------------
C
      NBMOC = 0
      DO 5 IDDI=1,NBDIA
        NBMOC = NBMOC + ZI(LLDIAM+NBDIA+IDDI-1)
 5    CONTINUE
      CALL WKVECT('&&REMNGL.ORDRE.FREQ','V V I',NBMOC,LTORF)
      CALL WKVECT('&&REMNGL.ORDRE.TMPO','V V I',NBMOC,LTORTO)
      CALL ORDR8(ZR(LLFREQ),NBMOC,ZI(LTORTO))
      NBORC = 0
      DO 6 II=1,NBMOC
        IORMO  = ZI(LTORTO+II-1)
        ICOMP  = 0
        IDICOU = 0
        DO 7 JJ=1,NBDIA
          ICOMP = ICOMP + ZI(LLDIAM+NBDIA+JJ-1)
          IF (ICOMP.GE.IORMO .AND. IDICOU.EQ.0) IDICOU = JJ
 7      CONTINUE
        NBORC = NBORC + 1
        ZI(LTORF+IORMO-1) = NBORC
        IDIAM = ZI(LLDIAM+IDICOU-1)
        IF (IDIAM.NE.0 .AND. IDIAM.NE.MDIAPA) NBORC = NBORC + 1
 6    CONTINUE
      CALL JEDETR('&&REMNGL.ORDRE.TMPO')
C
C-----RECUPERATION DES MODES COMPLEXES----------------------------------
C
      CALL JEVEUO(MODCYC//'     .CYCL.CMODE','L',LLMOC)
C
C-----CALCUL DU TETA DE CHAQUE SECTEUR----------------------------------
C
      CALL WKVECT('&&REMNGL.TETA_SECTEUR','V V R',NBSEC,LTTSC)
      DO 8 I=1,NBSEC
        ZR(LTTSC+I-1) = DEPI*(I-1) / NBSEC
 8    CONTINUE
C
C-----RECUPERATION DE L'INDIRECTION SQUELETTE---------------------------
C
      CALL JEVEUO(MAILSK//'.INV.SKELETON','L',LLINSK)
      CALL DISMOI('F','NB_NO_MAILLA',MAILSK,'MAILLAGE',NBNOT,K8B,IER)
C
C***********************************************************************
C     RESTITUTION
C***********************************************************************
C
      NBDDG = NBMOD + NBDDR + NBDAX
      ICOMP = 0
C
C  BOUCLE SUR LES DIAMETRES NODAUX
C
      DO 10 IDI=1,NBDIA
C
C  CALCUL DU DEPHASAGE INTER-SECTEUR
C
        IDIAM = ZI(LLDIAM+IDI-1)
        BETA  = (DEPI/NBSEC)*IDIAM
        DEPHC = DCMPLX(COS(BETA),SIN(BETA))
C
C  BOUCLE SUR LES MODES PROPRES DU DIAMETRE COURANT
C
        DO 15 I=1,ZI(LLDIAM+NBDIA+IDI-1)
          ICOMP = ICOMP + 1
          IORC  = ZI(LTORF+ICOMP-1)
          IAD   = LLMOC + ((ICOMP-1)*NBDDG)
C
C***********************************************************************
C      RESTITUTION DU MODE PROPRE REEL (PARTIE RELLE)
C***********************************************************************
C
          CALL RSEXCH(NOMRES,DEPL,IORC,CHAMVA,IER)
          CALL VTCREA(CHAMVA,CREFE,'G','R',NEQ)
          CALL RSNOCH(NOMRES,DEPL,IORC,' ')
          CALL JEVEUO(CHAMVA//'.VALE','E',LLCHAM)
C
C  CALCUL MODE COMPLEXE SECTEUR DE BASE
C
          CALL REMNBN(BASMOD,NBMOD,NBDDR,NBDAX,FLEXDR,FLEXGA,FLEXAX,
     +                TETGD,TETAX,ZC(IAD),ZC(LTVECO),NEQSEC,BETA)
C
C  CALCUL MASSE GENERALISEE
C
          CALL GENECY(ZC(LTVECO),ZC(LTVECO),NEQSEC,LMASS,PARA,NBSEC,
     +                BETA,BETA,ZC(LTVEZT))
C
C  COMMUN POUR MODE_MECA ET BASE_MODALE
C
          CALL RSADPA(NOMRES,'E',1,'FREQ'     ,IORC,0,LDFREQ,K8B)
          CALL RSADPA(NOMRES,'E',1,'RIGI_GENE',IORC,0,LDKGE ,K8B)
          CALL RSADPA(NOMRES,'E',1,'MASS_GENE',IORC,0,LDMGE ,K8B)
          CALL RSADPA(NOMRES,'E',1,'OMEGA2'   ,IORC,0,LDOM2 ,K8B)
          CALL RSADPA(NOMRES,'E',1,'NUME_MODE',IORC,0,LDOMO ,K8B)
          FACT  = 1.D0 / (PARA(1)**0.5D0)
          GENEK = (ZR(LLFREQ+ICOMP-1)*DEPI)**2
          ZR(LDFREQ) = ZR(LLFREQ+ICOMP-1)
          ZR(LDKGE)  = GENEK
          ZR(LDMGE)  = 1.D0
          ZR(LDOM2)  = GENEK
          ZI(LDOMO)  = IORC
C
C  SPECIFIQUE A BASE_MODALE
C
          IF (TYPSD.EQ.TYPSUP(1)) THEN
            CALL RSADPA(NOMRES,'E',1,'TYPE_DEFO',IORC,0,LDTYD,K8B)
            ZK16(LDTYD) = 'PROPRE          '
          ENDIF
C
C  BOUCLE SUR LES SECTEURS
C
          DO 20 K=1,NBSEC
            IF (K.GT.1) THEN
              DEPHCO = DEPHC
            ELSE
              DEPHCO = DCMPLX(1.D0,0.D0)
            ENDIF
            DO 30 J=1,NEQSEC
              ZC(LTVECO+J-1) = ZC(LTVECO+J-1)*DEPHCO
              ZR(LTVERE+J-1) = DBLE(ZC(LTVECO+J-1))
 30         CONTINUE
            CALL JEVEUO(JEXNUM(INDIRF,K),'L',LTINDS)
            CALL JELIRA(JEXNUM(INDIRF,K),'LONMAX',NDDCOU,K8B)
            NDDCOU = NDDCOU/2
            DO 40 J=1,NDDCOU
              IEQI = ZI(LTINDS+(J-1)*2)
              IEQF = ZI(LTINDS+(J-1)*2+1)
              ZR(LLCHAM+IEQF-1) = ZR(LTVERE+IEQI-1)*FACT
 40         CONTINUE
 20       CONTINUE
C
C  PRISE EN COMPTE ROTATION SUR CHAQUE SECTEUR
C
          CALL ROTCHM(PROFNO,ZR(LLCHAM),ZR(LTTSC),NBSEC,ZI(LLINSK),
     +                NBNOT,NBCMP,3)
C
C***********************************************************************
C      EVENTUELLE RESTITUTION DE LA PARTIE IMAGINAIRE
C***********************************************************************
C
          IF (IDIAM.NE.0 .AND. IDIAM.NE.MDIAPA) THEN
            IORC = IORC + 1
C
C  CALCUL MODE COMPLEXE SECTEUR DE BASE
C
            CALL REMNBN(BASMOD,NBMOD,NBDDR,NBDAX,FLEXDR,FLEXGA,FLEXAX,
     +                  TETGD,TETAX,ZC(IAD),ZC(LTVECO),NEQSEC,BETA)
C
C  CALCUL MASSE GENERALISEE
C
            CALL GENECY(ZC(LTVECO),ZC(LTVECO),NEQSEC,LMASS,PARA,NBSEC,
     +                  BETA,BETA,ZC(LTVEZT))
C
            CALL RSEXCH(NOMRES,DEPL,IORC,CHAMVA,IER)
            CALL VTCREA(CHAMVA,CREFE,'G','R',NEQ)
            CALL RSNOCH(NOMRES,DEPL,IORC,' ')
            CALL JEVEUO(CHAMVA//'.VALE','E',LLCHAM)
C
C  COMMUN POUR MODE_MECA ET BASE_MODALE
C
            CALL RSADPA(NOMRES,'E',1,'FREQ'     ,IORC,0,LDFREQ,K8B)
            CALL RSADPA(NOMRES,'E',1,'RIGI_GENE',IORC,0,LDKGE ,K8B)
            CALL RSADPA(NOMRES,'E',1,'MASS_GENE',IORC,0,LDMGE ,K8B)
            CALL RSADPA(NOMRES,'E',1,'OMEGA2'   ,IORC,0,LDOM2 ,K8B)
            CALL RSADPA(NOMRES,'E',1,'NUME_MODE',IORC,0,LDOMO ,K8B)
            FACT  = 1.D0 / (PARA(2)**0.5D0)
            GENEK = (ZR(LLFREQ+ICOMP-1)*DEPI)**2
            ZR(LDFREQ) = ZR(LLFREQ+ICOMP-1)
            ZR(LDKGE)  = GENEK
            ZR(LDMGE)  = 1.D0
            ZR(LDOM2)  = GENEK
            ZI(LDOMO)  = IORC
C
C  SPECIFIQUE A BASE_MODALE
C
            IF (TYPSD.EQ.TYPSUP(1)) THEN
              CALL RSADPA(NOMRES,'E',1,'TYPE_DEFO',IORC,0,LDTYD,K8B)
              ZK16(LDTYD) = 'PROPRE          '
            ENDIF
C
C  BOUCLE SUR LES SECTEURS
C
            DO 50 K=1,NBSEC
              IF (K.GT.1) THEN
                DEPHCO = DEPHC
              ELSE
                DEPHCO = DCMPLX(1.D0,0.D0)
              ENDIF
              DO 60 J=1,NEQSEC
                ZC(LTVECO+J-1) = ZC(LTVECO+J-1)*DEPHCO
                ZR(LTVERE+J-1) = DIMAG(ZC(LTVECO+J-1))
 60           CONTINUE
              CALL JEVEUO(JEXNUM(INDIRF,K),'L',LTINDS)
              CALL JELIRA(JEXNUM(INDIRF,K),'LONMAX',NDDCOU,K8B)
              NDDCOU = NDDCOU / 2
              DO 70 J=1,NDDCOU
                IEQI = ZI(LTINDS+(J-1)*2)
                IEQF = ZI(LTINDS+(J-1)*2+1)
                ZR(LLCHAM+IEQF-1) = ZR(LTVERE+IEQI-1)*FACT
 70           CONTINUE
 50         CONTINUE
C
C  PRISE EN COMPTE ROTATION SUR CHAQUE SECTEUR
C
            CALL ROTCHM(PROFNO,ZR(LLCHAM),ZR(LTTSC),NBSEC,ZI(LLINSK),
     +                  NBNOT,NBCMP,3)
C
          ENDIF
C
 15     CONTINUE
 10   CONTINUE
C
      CALL JEDETR ( '&&REMNGL.VEC.TRAVC'   )
      CALL JEDETR ( '&&REMNGL.VEC.COMP'    )
      CALL JEDETR ( '&&REMNGL.VEC.REEL'    )
      CALL JEDETR ( '&&REMNGL.FLEX.DROITE' )
      CALL JEDETR ( '&&REMNGL.FLEX.GAUCHE' )
      CALL JEDETR ( '&&REMNGL.TETGD'       )
      CALL JEDETR ( '&&REMNGL.TETA_SECTEUR')
      CALL JEDETR ( '&&REMNGL.ORDRE.FREQ'  )
      IF (NUMA.GT.0) THEN
         CALL JEDETR ( '&&REMNGL.FLEX.AXE' )
         CALL JEDETR ( '&&REMNGL.TETAX'    )
      ENDIF
C
      CALL JEDEMA()
      END
