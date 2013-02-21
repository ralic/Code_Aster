      SUBROUTINE MODCOQ(BASE,NUOR,NBM,MATER1,MATER2,NOMA,NOMGRP,IAXE,
     &                  KEC,GEOM,VICOQ,TORCO,TCOEF,IFREBA)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/02/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C CARACTERISATION DES DEFORMEES DES MODES PRIS EN COMPTE POUR LE
C COUPLAGE : POUR CHAQUE MODE ET SUR CHACUNE DES DEUX COQUES,
C DETERMINATION DE L'ORDRE DE COQUE ET DES COEFFICIENTS DE LA DEFORMEE
C AXIALE DE POUTRE
C APPELANT : FLUST4
C-----------------------------------------------------------------------
C  IN : BASE   : NOM DU CONCEPT DE TYPE MODE_MECA DEFINISSANT LA BASE
C                MODALE
C  IN : NUOR   : LISTE DES NUMEROS D'ORDRE DES MODES PRIS EN COMPTE POUR
C                LE COUPLAGE FLUIDELASTIQUE
C  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
C  IN : MATER1 : NOM DU CONCEPT DE TYPE MATER (MATERIAU COQUE INTERNE)
C  IN : MATER2 : NOM DU CONCEPT DE TYPE MATER (MATERIAU COQUE EXTERNE)
C  IN : NOMA   : NOM DU CONCEPT DE TYPE MAILLAGE
C  IN : NOMGRP : LISTE DES NOMS DES GROUPES DE NOEUDS/GROUPES DE MAILLES
C                CORRESPONDANT AUX COQUES (LES GROUPES DE NOEUDS ONT ETE
C                PREALABLEMENT CREES A PARTIR DES GROUPES DE MAILLES ET
C                ON LEUR A AFFECTE LES MEMES NOMS)
C  IN : IAXE   : INDICE CARACTERISANT L'AXE DE REVOLUTION DES COQUES
C                IAXE = 1 : AXE X DU REPERE GLOBAL
C                IAXE = 2 : AXE Y DU REPERE GLOBAL
C                IAXE = 3 : AXE Z DU REPERE GLOBAL
C  IN : KEC    : INDICE CARACTERISTIQUE DU SENS DE L'ECOULEMENT
C                KEC =  1 : ECOULEMENT DANS LE SENS CROISSANT DU
C                PARAMETRE LE LONG DE L'AXE DE REVOLUTION DES COQUES
C                KEC = -1 : ECOULEMENT DANS LE SENS DECROISSANT
C  IN : GEOM   : VECTEUR DE GRANDEURS GEOMETRIQUES CARACTERISTIQUES
C OUT : VICOQ  : VECTEUR D'INDICES CARACTERISANT CHAQUE MODE
C                VICOQ(IMOD) = 1 => COQUE INTERNE SEULE EN MVT
C                VICOQ(IMOD) = 2 => COQUE EXTERNE SEULE EN MVT
C                VICOQ(IMOD) = 3 => COQUES INTERNE + EXTERNE EN MVT
C OUT : TORCO  : TABLEAU CONTENANT LES ORDRES DE COQUE ET DEPHASAGES
C OUT : TCOEF  : TABLEAU DES COEFFICIENTS DES DEFORMEES AXIALES
C-----------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      CHARACTER*8  BASE,MATER1,MATER2,NOMA
      INTEGER      NBM,NUOR(NBM),IAXE,KEC,VICOQ(NBM)
      REAL*8       GEOM(9),TORCO(4,NBM),TCOEF(10,NBM)
C
      INTEGER      IDDL(2)
      CHARACTER*1  K1BID
      CHARACTER*3  KMOD
      CHARACTER*8  K8BID,NOMPAR(3)
      CHARACTER*14 NUMDDL
      CHARACTER*19 NOMRC
      CHARACTER*24 REFEBA,MATRIA,COORNO,RCVALK,RCVALR,NOMGRP(*)
      CHARACTER*24 COQUEI,COQUEX
      CHARACTER*32 GRPNO
C
C-----------------------------------------------------------------------
      INTEGER IBI ,ICOOR ,ICOQ ,IDEC ,IDECM ,IDECMN ,IDEFM 
      INTEGER IFM ,IFREBA ,IMOD ,INMAXE ,INMAXI ,INO ,INUNOE 
      INTEGER INUNOI ,IOK1 ,IOK2 ,IOK3 ,IPARA ,IREFBA ,IRET 
      INTEGER IUNIFI ,IVALK ,IVALR ,NBEQ ,NBNOEX ,NBNOIN ,NBNOTO 
      INTEGER NBPARA ,NUMNOE ,NUMOD ,NUNOE0 
      REAL*8 DPMAXE ,DPMAXI ,DPNORM ,DRMAX ,DX1 ,DX2 ,FREMOD 
      REAL*8 POISS1 ,POISS2 ,RHO1 ,RHO2 ,RTEMP ,TOLE ,YOUNG1 
      REAL*8 YOUNG2 
C-----------------------------------------------------------------------
      DATA NOMPAR /'E       ','NU      ','RHO     '/
C
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
C
C --- 1.INITIALISATIONS ET FORMATS D'IMPRESSION
C
      TOLE = 1.D-4
      IF (IAXE.EQ.1) THEN
        IDDL(1) = 2
        IDDL(2) = 3
      ELSE IF (IAXE.EQ.2) THEN
        IDDL(1) = 3
        IDDL(2) = 1
      ELSE
        IDDL(1) = 1
        IDDL(2) = 2
      ENDIF
C
      COQUEI = NOMGRP(1)
      COQUEX = NOMGRP(2)
C
      IFM = IUNIFI('MESSAGE')
 500  FORMAT('*******************************************')
 501  FORMAT('*                                         *')
 502  FORMAT('*  CARACTERISATION DES DEFORMEES MODALES  *')
 503  FORMAT('DEFORMEE DU ',I3,'-IEME MODE RETENU')
 504  FORMAT('================================')
 510  FORMAT('-----  MOUVEMENT DE LA COQUE INTERNE  ------')
 511  FORMAT('--- PAS DE MOUVEMENT DE LA COQUE INTERNE ---')
 520  FORMAT('-----  MOUVEMENT DE LA COQUE EXTERNE  ------')
 521  FORMAT('--- PAS DE MOUVEMENT DE LA COQUE EXTERNE ---')
 530  FORMAT(30X,'---/---')
C
C
C --- 2.RECUPERATION DES CARACTERISTIQUES MATERIAU
C
C --- 2.1.MATERIAU CONSTITUTIF DE LA COQUE INTERNE
C
      NOMRC = MATER1//'.ELAS      '
      RCVALK = NOMRC//'.VALK'
      RCVALR = NOMRC//'.VALR'
      CALL JEEXIN(RCVALK,IRET)
      IF (IRET.EQ.0) CALL U2MESS('F','ALGELINE_92')
      CALL JEVEUO(RCVALK,'L',IVALK)
      CALL JEVEUO(RCVALR,'L',IVALR)
C
      IOK1 = 0
      IOK2 = 0
      IOK3 = 0
      CALL JELIRA(RCVALR,'LONUTI',NBPARA,K1BID)
      DO 10 IPARA = 1,NBPARA
        IF (ZK8(IVALK+IPARA-1).EQ.NOMPAR(1)) THEN
          IOK1   = 1
          YOUNG1 = ZR(IVALR+IPARA-1)
        ELSE IF (ZK8(IVALK+IPARA-1).EQ.NOMPAR(2)) THEN
          IOK2   = 1
          POISS1 = ZR(IVALR+IPARA-1)
        ELSE IF (ZK8(IVALK+IPARA-1).EQ.NOMPAR(3)) THEN
          IOK3   = 1
          RHO1   = ZR(IVALR+IPARA-1)
        ENDIF
  10  CONTINUE
      IF (IOK1.EQ.0 .OR. IOK2.EQ.0 .OR. IOK3.EQ.0) THEN
        CALL U2MESS('F','ALGELINE_93')
      ELSE IF (YOUNG1.EQ.0.D0) THEN
        CALL U2MESS('F','ALGELINE_94')
      ENDIF
C
C --- 2.2.MATERIAU CONSTITUTIF DE LA COQUE EXTERNE
C
      NOMRC = MATER2//'.ELAS      '
      RCVALK = NOMRC//'.VALK'
      RCVALR = NOMRC//'.VALR'
      CALL JEEXIN(RCVALK,IRET)
      IF (IRET.EQ.0) CALL U2MESS('F','ALGELINE_95')
      CALL JEVEUO(RCVALK,'L',IVALK)
      CALL JEVEUO(RCVALR,'L',IVALR)
C
      IOK1 = 0
      IOK2 = 0
      IOK3 = 0
      CALL JELIRA(RCVALR,'LONUTI',NBPARA,K1BID)
      DO 20 IPARA = 1,NBPARA
        IF (ZK8(IVALK+IPARA-1).EQ.NOMPAR(1)) THEN
          IOK1   = 1
          YOUNG2 = ZR(IVALR+IPARA-1)
        ELSE IF (ZK8(IVALK+IPARA-1).EQ.NOMPAR(2)) THEN
          IOK2   = 1
          POISS2 = ZR(IVALR+IPARA-1)
        ELSE IF (ZK8(IVALK+IPARA-1).EQ.NOMPAR(3)) THEN
          IOK3   = 1
          RHO2   = ZR(IVALR+IPARA-1)
        ENDIF
  20  CONTINUE
      IF (IOK1.EQ.0 .OR. IOK2.EQ.0 .OR. IOK3.EQ.0) THEN
        CALL U2MESS('F','ALGELINE_96')
      ELSE IF (YOUNG2.EQ.0.D0) THEN
        CALL U2MESS('F','ALGELINE_97')
      ENDIF
C
C
C --- 3.EXTRACTION DES DEFORMEES MODALES DANS LES DEUX DIRECTIONS DU
C ---   PLAN ORTHOGONAL A L'AXE DE REVOLUTION DES COQUES
C
      REFEBA = BASE//'           .REFD'
      CALL JEVEUO(REFEBA,'L',IREFBA)
      MATRIA = ZK24(IREFBA)
C
      CALL DISMOI('F','NOM_NUME_DDL',MATRIA,'MATR_ASSE',IBI,NUMDDL,IRET)
      CALL DISMOI('F','NB_EQUA',MATRIA,'MATR_ASSE',NBEQ,K8BID,IRET)
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOTO,K8BID,IRET)
C
      CALL WKVECT('&&MODCOQ.TEMP.DEFM','V V R',2*NBNOTO*NBM,IDEFM)
      CALL EXTMOD(BASE,NUMDDL,NUOR,NBM,ZR(IDEFM),NBEQ,NBNOTO,IDDL,2)
C
C.....PERMUTATION DES DDLS DX, DZ -> DZ, DX LORSQUE IAXE = 2
C.....(LE TRIEDRE Z, X, Y EST DIRECT)
C
      IF (IAXE.EQ.2) THEN
        DO 30 IMOD = 1,NBM
          IDECM = 2 * NBNOTO * (IMOD-1)
          DO 31 INO = 1,NBNOTO
            IDECMN = IDECM + 2 * (INO-1)
            RTEMP = ZR(IDEFM+IDECMN)
            ZR(IDEFM+IDECMN) = ZR(IDEFM+IDECMN+1)
            ZR(IDEFM+IDECMN+1) = RTEMP
  31      CONTINUE
  30    CONTINUE
      ENDIF
C
C
C --- 4.ACCES AUX OBJETS DU CONCEPT MAILLAGE
C
      GRPNO = '&&MEFGMN.'//COQUEI(1:8)
      CALL JELIRA(GRPNO,'LONMAX',NBNOIN,K1BID)
      CALL JEVEUO(GRPNO,'L',INUNOI)
      GRPNO = '&&MEFGMN.'//COQUEX(1:8)
      CALL JELIRA(GRPNO,'LONMAX',NBNOEX,K1BID)
      CALL JEVEUO(GRPNO,'L',INUNOE)
C
      COORNO = NOMA//'.COORDO    .VALE'
      CALL JEVEUO(COORNO,'L',ICOOR)
C
C
C --- 5.POUR CHAQUE MODE ET SUR CHACUNE DES DEUX COQUES
C       - DETECTION DU NOEUD DE DEPLACEMENT MAXIMUM DANS LE PLAN
C         PERPENDICULAIRE A L'AXE DE REVOLUTION
C       - DETERMINATION DE L'ORDRE DE COQUE ET DU DEPHASAGE
C       - DETERMINATION DES COEFFICIENTS DE LA DEFORMEE AXIALE
C
      WRITE(IFM,500)
      WRITE(IFM,501)
      WRITE(IFM,502)
      WRITE(IFM,501)
      WRITE(IFM,500)
      WRITE(IFM,*)
C
C
      DO 40 IMOD = 1,NBM
C
        WRITE(IFM,503) IMOD
        WRITE(IFM,504)
        WRITE(IFM,*)
        NUMOD = NUOR(IMOD)
        FREMOD = ZR(IFREBA+NUMOD-1)
C
C ----- 5.1.DETECTION DU DEPLACEMENT MAXIMUM SUR LA COQUE INTERNE
C
        DPMAXI = 0.D0
        INMAXI = 1
        DO 50 INO = 1,NBNOIN
          NUMNOE = ZI(INUNOI+INO-1)
          IDEC  = 2*NBNOTO*(IMOD-1)+2*(NUMNOE-1)
          DX1 = ZR(IDEFM+IDEC)
          DX2 = ZR(IDEFM+IDEC+1)
          DPNORM = DBLE(SQRT(DX1*DX1+DX2*DX2))
          IF (DPNORM.GT.DPMAXI) THEN
            DPMAXI = DPNORM
            INMAXI = INO
          ENDIF
  50    CONTINUE
C
C ----- 5.2.DETECTION DU DEPLACEMENT MAXIMUM SUR LA COQUE EXTERNE
C
        DPMAXE = 0.D0
        INMAXE = 1
        DO 60 INO = 1,NBNOEX
          NUMNOE = ZI(INUNOE+INO-1)
          IDEC  = 2*NBNOTO*(IMOD-1)+2*(NUMNOE-1)
          DX1 = ZR(IDEFM+IDEC)
          DX2 = ZR(IDEFM+IDEC+1)
          DPNORM = DBLE(SQRT(DX1*DX1+DX2*DX2))
          IF (DPNORM.GT.DPMAXE) THEN
            DPMAXE = DPNORM
            INMAXE = INO
          ENDIF
  60    CONTINUE
C
C ----- 5.3.DETERMINATION DE L'ORDRE DE COQUE ET DES COEFFICIENTS DE
C -----     LA DEFORMEE AXIALE
C
C ----- 5.3.1.COQUE INTERNE SEULE EN MOUVEMENT
C
        IF (DPMAXE.LT.DPMAXI*TOLE) THEN
C
          WRITE(IFM,510)
          WRITE(IFM,*)
          ICOQ = 1
          CALL ORDCOQ(IMOD,NBM,ICOQ,NBNOIN,ZI(INUNOI),INMAXI,NBNOTO,
     &                ZR(ICOOR),IAXE,ZR(IDEFM),NUNOE0,DRMAX,TORCO)
          CALL COEDEF(IMOD,FREMOD,NBM,YOUNG1,POISS1,RHO1,ICOQ,NBNOIN,
     &                ZI(INUNOI),NUNOE0,NBNOTO,ZR(ICOOR),IAXE,KEC,GEOM,
     &                ZR(IDEFM),DRMAX,TORCO,TCOEF)
C
          VICOQ(IMOD) = ICOQ
          WRITE(IFM,*)
          WRITE(IFM,521)
          WRITE(IFM,*)
C
C ----- 5.3.2.COQUE EXTERNE SEULE EN MOUVEMENT
C
        ELSE IF (DPMAXI.LT.DPMAXE*TOLE) THEN
C
          WRITE(IFM,511)
          WRITE(IFM,*)
          WRITE(IFM,520)
          WRITE(IFM,*)
          ICOQ = 2
          CALL ORDCOQ(IMOD,NBM,ICOQ,NBNOEX,ZI(INUNOE),INMAXE,NBNOTO,
     &                ZR(ICOOR),IAXE,ZR(IDEFM),NUNOE0,DRMAX,TORCO)
          CALL COEDEF(IMOD,FREMOD,NBM,YOUNG2,POISS2,RHO2,ICOQ,NBNOEX,
     &                ZI(INUNOE),NUNOE0,NBNOTO,ZR(ICOOR),IAXE,KEC,GEOM,
     &                ZR(IDEFM),DRMAX,TORCO,TCOEF)
C
          VICOQ(IMOD) = ICOQ
          WRITE(IFM,*)
C
C ----- 5.3.3.COQUES INTERNE + EXTERNE EN MOUVEMENT
C
        ELSE
C
          WRITE(KMOD,'(I3)') IMOD
          CALL U2MESK('A','ALGELINE_98',1,KMOD)
C
          WRITE(IFM,510)
          WRITE(IFM,*)
          ICOQ = 1
          CALL ORDCOQ(IMOD,NBM,ICOQ,NBNOIN,ZI(INUNOI),INMAXI,NBNOTO,
     &                ZR(ICOOR),IAXE,ZR(IDEFM),NUNOE0,DRMAX,TORCO)
          CALL COEDEF(IMOD,FREMOD,NBM,YOUNG1,POISS1,RHO1,ICOQ,NBNOIN,
     &                ZI(INUNOI),NUNOE0,NBNOTO,ZR(ICOOR),IAXE,KEC,GEOM,
     &                ZR(IDEFM),DRMAX,TORCO,TCOEF)
C
          WRITE(IFM,*)
          WRITE(IFM,520)
          WRITE(IFM,*)
          ICOQ = 2
          CALL ORDCOQ(IMOD,NBM,ICOQ,NBNOEX,ZI(INUNOE),INMAXE,NBNOTO,
     &                ZR(ICOOR),IAXE,ZR(IDEFM),NUNOE0,DRMAX,TORCO)
          CALL COEDEF(IMOD,FREMOD,NBM,YOUNG2,POISS2,RHO2,ICOQ,NBNOEX,
     &                ZI(INUNOE),NUNOE0,NBNOTO,ZR(ICOOR),IAXE,KEC,GEOM,
     &                ZR(IDEFM),DRMAX,TORCO,TCOEF)
C
          VICOQ(IMOD) = 3
          WRITE(IFM,*)
C
        ENDIF
C
        WRITE(IFM,530)
        WRITE(IFM,*)
C
  40  CONTINUE
C
C --- MENAGE
      CALL JEDETR('&&MODCOQ.TEMP.DEFM')
      CALL JEDEMA()
C
      END
