      SUBROUTINE NMCRSU(SDDISC,LISINS,PARCRI,FONACT)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 31/05/2011   AUTEUR GENIAUT S.GENIAUT 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*19 LISINS,SDDISC
      REAL*8       PARCRI(*)
      INTEGER      FONACT(*)
C
C ----------------------------------------------------------------------
C
C ROUTINE *_NON_LINE (STRUCTURES DE DONNES)
C
C CREATION SD DISCRETISATION - SUBDIVISION AUTO
C
C ----------------------------------------------------------------------
C
C
C IN  LISINS : SD_LIST_INST OU SD_LISTR8
C IN  PARCRI : PARAMETRES DES CRITERES DE CONVERGENCE (CF NMDOCN)
C IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
C I/O SDDISC : SD DISCRETISATION
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER      ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8       ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16   ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL      ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8  ZK8
      CHARACTER*16    ZK16
      CHARACTER*24        ZK24
      CHARACTER*32            ZK32
      CHARACTER*80                ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      CHARACTER*16 METHOD,PRED,TYPECO
      INTEGER      NOCC,IRET,IRE1,NSOLVE,GETEXM
      REAL*8       R8B,ELASDT,VALR
      INTEGER      ITER1,ITER2,IBID,IOCC,NIGNO,NFIN
      INTEGER      IFM,NIV,IDIV,ITMX,VALI
      REAL*8       RGMAXI,RGRELA,INIKRY
      CHARACTER*8  K8B,SDLIST,VALK,METLIS,KSTO
      CHARACTER*19 EVEN
      CHARACTER*16 NOPARA,NOMSOL
      INTEGER      JEEVR,JEEVK,JESUR
      INTEGER      LEEVR,LEEVK,LESUR
      PARAMETER   (LEEVR=4,LEEVK=3,LESUR=8)
      LOGICAL      ISFONC,LIMPEX
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE (IFM,*) '<MECANONLINE> ... CREATION SD SUBDIVISION'
      ENDIF
C
C --- INITIALISATIONS
C
      RGMAXI = PARCRI(3)
      RGRELA = PARCRI(2)
      INIKRY = 0.9D0
C
C --- ACCES SD LISTE D'INSTANTS
C
      CALL GETTCO(LISINS, TYPECO)
C
C --- RECUPERATION DES CRITERES DE CONVERGENCE GLOBAUX
C
      CALL GETVIS('CONVERGENCE','ITER_GLOB_MAXI',1,1,1,ITER1,IRET)
C
      ELASDT = 0.D0

      CALL GETVIS('CONVERGENCE','ITER_GLOB_ELAS',1,1,1,ITER2,IRET)
C     SI ON NE DONNE PAS NEWTON/PAS_MINI_ELAS ALORS ON NE DOIT PAS
C     TENIR COMPTE DE ITER_GLOB_ELAS
      CALL GETVR8('NEWTON','PAS_MINI_ELAS',1,1,1,R8B,IRET)
      IF (IRET.LE.0) THEN
         ITER2 = ITER1
      ELSE
         ELASDT = R8B
      ENDIF
C
C --- RECUPERE LA METHODE DE DECOUPAGE ET SES PARAMETRES
C
      IF (TYPECO.EQ.'LISTR8_SDASTER') THEN
C       ON A DONNE UNE LISTE DE R8 : SOUS-DECOUPAGE INTERDIT
        NOCC = 1
        CALL WKVECT(SDDISC//'.EEVR','V V R',  (NOCC+1)*LEEVR,JEEVR)
        CALL WKVECT(SDDISC//'.EEVK','V V K16',(NOCC+1)*LEEVK,JEEVK)
        CALL WKVECT(SDDISC//'.ESUR','V V R'  ,(NOCC+1)*LESUR,JESUR)
C       EVEN = DIVERGENCE_ITER ET METHODE = 'AUCUNE'
        IOCC = 1
        ZR(JEEVR-1+LEEVR*(IOCC-1)+1)=0.D0
        ZR(JESUR-1+LESUR*(IOCC-1)+1)=0.D0
C       EVEN = DIVERGENCE_ERRE ET METHODE = 'AUCUNE'
        IOCC = 2
        ZR(JEEVR-1+LEEVR*(IOCC-1)+1)=1.D0
        ZR(JESUR-1+LESUR*(IOCC-1)+1)=0.D0
      ELSEIF (TYPECO.EQ.'LIST_INST') THEN
C       COPIE LOCALE DES OBJETS DE LA SD_LIST_INST
        SDLIST = LISINS(1:8)
        CALL JEDUP1(SDLIST//'.ECHE.EVENR','V',SDDISC//'.EEVR')
        CALL JEDUP1(SDLIST//'.ECHE.EVENK','V',SDDISC//'.EEVK')
        CALL JEDUP1(SDLIST//'.ECHE.SUBDR','V',SDDISC//'.ESUR')
        CALL JEEXIN(SDDISC//'.EEVR',IRET)
        CALL ASSERT(IRET.NE.0)
        CALL JEEXIN(SDLIST//'.ADAP.EVENR',IRET)
        IF (IRET.NE.0) THEN
          CALL JEDUP1(SDLIST//'.ADAP.EVENR','V',SDDISC//'.AEVR')
          CALL JEDUP1(SDLIST//'.ADAP.EVENK','V',SDDISC//'.AEVK')
          CALL JEDUP1(SDLIST//'.ADAP.TPLUR','V',SDDISC//'.ATPR')
          CALL JEDUP1(SDLIST//'.ADAP.TPLUK','V',SDDISC//'.ATPK')
        ENDIF

C       SI NEWTON/PREDICTION ='DEPL_CALCULE',
C       ALORS ON INTERDIT LA SUBDIVISION
        CALL GETVTX('NEWTON','PREDICTION',1,1,1,PRED,IRET)
        IF (IRET.NE.0) THEN
          IF (PRED.EQ.'DEPL_CALCULE') THEN
            CALL UTDIDT('L',SDDISC,'ECHE',IBID,'NB_OCC',R8B,NOCC,K8B)
            DO 10 IOCC=1,NOCC
C             A-T-ON DEMANDE DU SOUS-DECOUPAGE ?
              CALL UTDIDT('L',SDDISC,'ECHE',IOCC,'SUBD_METH',R8B,IBID,
     &                                                           METHOD)
              IF (METHOD.NE.'AUCUNE') THEN
                CALL U2MESK('A','SUBDIVISE_1',1,METHOD)
                VALK = 'AUCUNE'
                CALL UTDIDT('E',SDDISC,'ECHE',IOCC,'SUBD_METH',R8B,IBID,
     &                                                             VALK)
              ENDIF
 10         CONTINUE
          ENDIF
        ENDIF

      ENDIF
C
C --- VERIFIE COHERENCE SUBD_METHODE / NEWTON
C
      CALL UTDIDT('L',SDDISC,'ECHE',IBID,'NB_OCC',R8B,NOCC,K8B)
      DO 20 IOCC=1,NOCC
        CALL UTDIDT('L',SDDISC,'ECHE',IOCC,'SUBD_METH',R8B,IBID,METHOD)
        IF (METHOD.EQ.'EXTRAP_IGNO') THEN
          CALL UTDIDT('L',SDDISC,'ECHE',IOCC,'SUBD_ITER_IGNO',R8B,NIGNO,
     &                                                              K8B)
          IF (NIGNO.GE.ITER1) CALL U2MESS('F','DISCRETISATION_6')
        ELSE IF (METHOD.EQ.'EXTRAP_FIN') THEN
          CALL UTDIDT('L',SDDISC,'ECHE',IOCC,'SUBD_ITER_FIN',R8B,NFIN,
     &                                                              K8B)
          IF (NFIN.GE.ITER1 ) CALL U2MESS('F','DISCRETISATION_7')
        ENDIF
 20   CONTINUE

C
C --- EN GESTION AUTO, AVEC UN CRITERE D'ADAPTATION EN SEUIL SUR
C     NB_ITER_NEWT, ON MET VALE = ITER_GLOB_MAXI/2 SI VALE N'A PAS
C     ETE RENSIGNE DANS DEFI_LIST_INST
C     ON NE CONSIDERE PAS LE CAS DE DE ITER_GLOB_ELAS CAR C'EST ACTIVE
C     (MATRICE SECANTE) QU'EN CAS DE DIFFICULTE
      CALL UTDIDT('L',SDDISC,'LIST',IBID,'METHODE',R8B,IBID,METLIS)
      IF (METLIS.EQ.'AUTO') THEN
        CALL GETVIS('CONVERGENCE','ITER_GLOB_MAXI',1,1,1,ITMX,IRE1)
        CALL UTDIDT('L',SDDISC,'ADAP',IBID,'NB_OCC',R8B,NOCC,K8B)
        DO 25 IOCC=1,NOCC
          CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'NOM_EVEN',R8B,IBID,EVEN)
          IF (EVEN.NE.'SEUIL_SANS_FORMULE') GOTO 25
          CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'NOM_PARA',R8B,IBID,NOPARA)
          IF (NOPARA.NE.'NB_ITER_NEWT') GOTO 25
          CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'VALE',R8B,VALI,K8B)
          IF (VALI.NE.0) GOTO 25
          VALI = ITMX / 2
          VALR = VALI
          CALL UTDIDT('E',SDDISC,'ADAP',IOCC,'VALE',VALR,VALI,K8B)
 25     CONTINUE
      ENDIF

C     VERIF COHERENCE AVEC IMPLEX
      IF (METLIS.EQ.'AUTO') THEN
        LIMPEX = ISFONC(FONACT,'IMPL_EX')
        CALL UTDIDT('L',SDDISC,'ADAP',IBID,'NB_OCC',R8B,NOCC,K8B)
        DO 27 IOCC=1,NOCC
          CALL UTDIDT('L',SDDISC,'ADAP',IOCC,'METHODE',R8B,IBID,METHOD)
          IF (METHOD.EQ.'IMPLEX') THEN
            IF (.NOT.LIMPEX) CALL U2MESS('F','MECANONLINE6_4')
          ENDIF
 27     CONTINUE
      ENDIF

C     STOCKAGE DE L'INFORMATION SI STOP_SINGULIER='DECOUPE'
      NOMSOL='SOLVEUR'
      IF (GETEXM(NOMSOL,'STOP_SINGULIER').EQ.0) GOTO 800     
      CALL GETFAC(NOMSOL,NSOLVE)
      IF (NSOLVE.EQ.0) GOTO 800
      CALL GETVTX(NOMSOL,'STOP_SINGULIER',1,1,1,KSTO,IBID)
      IF (IBID.EQ.0) GOTO 800
      CALL ASSERT(KSTO.EQ.'OUI'.OR.KSTO.EQ.'NON'.OR.KSTO.EQ.'DECOUPE')
      CALL UTDIDT('L',SDDISC,'LIST',IBID,'ALGO_DECOUPE',R8B,IBID,VALK)
C     SI STOP_SINGULIER='DECOUPE', UN ALGO DE DECOUPE DOIT ETRE AUTORISE
      IF (KSTO.EQ.'DECOUPE'.AND.VALK.EQ.'NON') THEN
        CALL U2MESS('F','ALGORITH11_81')
      ELSEIF (KSTO.EQ.'DECOUPE'.AND.VALK.EQ.'OUI') THEN
        CALL UTDIDT('E',SDDISC,'LIST',IBID,'ALGO_DECOUPE',R8B,IBID,KSTO)
      ENDIF

 800  CONTINUE

C     RECHERCHE DU N� D'OCCURENCE DE L'EVENEMENT 'DIVERGENCE_ITER'
      IDIV=0
      DO 30 IOCC=1,NOCC
        CALL UTDIDT('L',SDDISC,'ECHE',IOCC,'NOM_EVEN',R8B,IBID,EVEN)
        IF (EVEN.EQ.'DIVERGENCE_ITER') IDIV = IOCC
 30   CONTINUE
C
C --- CREATION SD STOCKAGE DES INFOS EN COURS DE CALCUL
C
      CALL NMCERR(SDDISC,IDIV  ,ITER1 ,ITER2 ,ELASDT,
     &            RGMAXI,RGRELA,INIKRY)
C
      CALL JEDEMA()

      END
