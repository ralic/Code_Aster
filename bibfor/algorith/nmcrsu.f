      SUBROUTINE NMCRSU(SDDISC,LISINS,PARCRI,FONACT,SOLVEU)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
      CHARACTER*19 SDDISC,LISINS,SOLVEU
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
C IN  PARCRI : PARAMETRES DES CRITERES DE CONVERGENCE (CF NMDOCN)
C IN  LISINS : SD_LIST_INST OU SD_LISTR8
C IN  SDDISC : SD DISCRETISATION
C IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
C IN  SOLVEU : SD SOLVEUR
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
      CHARACTER*16 PRED
      CHARACTER*16 METLIS,MODETP
      INTEGER      IRET
      REAL*8       R8BID,ELASDT,VALR
      INTEGER      NECHEC
      INTEGER      NADAPT,IADAPT      
      INTEGER      ITER1,ITER2,IBID
      INTEGER      IFM,NIV,ITMX,VALI
      LOGICAL      ISFONC,LIMPEX,LDECO
      REAL*8       RGMAXI,RGRELA,INIKRY
      CHARACTER*8  K8BID
      CHARACTER*19 EVEN
      CHARACTER*16 TYPECO,NOPARA,DECOUP
      CHARACTER*24 LISEVR,LISEVK,LISESU
      CHARACTER*24 LISAVR,LISAVK,LISTPR,LISTPK
      CHARACTER*24 TPSEVR,TPSEVK,TPSESU
      CHARACTER*24 TPSAVR,TPSAVK,TPSTPR,TPSTPK
      INTEGER      IARG
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
      CALL UTDIDT('L'   ,SDDISC,'LIST',IBID  ,'NECHEC',
     &            R8BID ,NECHEC,K8BID )
      CALL UTDIDT('L'   ,SDDISC,'LIST',IBID  ,'NADAPT',
     &            R8BID ,NADAPT,K8BID )
      CALL UTDIDT('L'   ,SDDISC,'LIST',IBID  ,'METHODE',
     &            R8BID ,IBID  ,METLIS)
C
C --- FONCTIONNALITES ACTIVEES
C     
      LIMPEX = ISFONC(FONACT,'IMPL_EX')
C
C --- NOM SDS DE LA LISINS
C 
      LISEVR = LISINS(1:8)//'.ECHE.EVENR'
      LISEVK = LISINS(1:8)//'.ECHE.EVENK'
      LISESU = LISINS(1:8)//'.ECHE.SUBDR'
      LISAVR = LISINS(1:8)//'.ADAP.EVENR'
      LISAVK = LISINS(1:8)//'.ADAP.EVENK'
      LISTPR = LISINS(1:8)//'.ADAP.TPLUR'
      LISTPK = LISINS(1:8)//'.ADAP.TPLUK'      
C
C --- NOM SDS DE LA SDDISC
C 
      TPSEVR = SDDISC(1:19)//'.EEVR'
      TPSEVK = SDDISC(1:19)//'.EEVK'
      TPSESU = SDDISC(1:19)//'.ESUR'
      TPSAVR = SDDISC(1:19)//'.AEVR'
      TPSAVK = SDDISC(1:19)//'.AEVK'
      TPSTPR = SDDISC(1:19)//'.ATPR'
      TPSTPK = SDDISC(1:19)//'.ATPK' 
C
C --- LECTURE DE LA LISTE D'INSTANTS
C
      CALL GETTCO(LISINS,TYPECO)
C
      IF (TYPECO.EQ.'LISTR8_SDASTER') THEN
C
C ----- CREATION EVENEMENTS ERREURS: ARRET
C
        CALL NMCRLD(SDDISC)
      ELSEIF (TYPECO.EQ.'LIST_INST') THEN 
C
C ----- COPIE LOCALE DES OBJETS DE LA LISINS
C
        CALL JEDUP1(LISEVR,'V',TPSEVR)
        CALL JEDUP1(LISEVK,'V',TPSEVK)
        CALL JEDUP1(LISESU,'V',TPSESU)
        IF (NADAPT.NE.0) THEN
          CALL JEDUP1(LISAVR,'V',TPSAVR)
          CALL JEDUP1(LISAVK,'V',TPSAVK)
          CALL JEDUP1(LISTPR,'V',TPSTPR)
          CALL JEDUP1(LISTPK,'V',TPSTPK)
        ENDIF       
      ENDIF
C
C --- RECUPERATION DES CRITERES DE CONVERGENCE GLOBAUX
C
      ITER1  = NINT(PARCRI(1))
      ITMX   = ITER1
      ELASDT = 0.D0
C
C --- SI ON NE DONNE PAS NEWTON/PAS_MINI_ELAS ALORS ON NE DOIT PAS
C --- TENIR COMPTE DE ITER_GLOB_ELAS
C
      CALL GETVR8('NEWTON','PAS_MINI_ELAS',1,IARG,1,VALR  ,IRET)
      IF (IRET.LE.0) THEN
        ITER2  = ITER1
      ELSE
        ELASDT = VALR
        ITER2  = NINT(PARCRI(5))
      ENDIF
C
C --- DECOUPAGE ACTIVE
C
      CALL UTDIDT('L'   ,SDDISC,'LIST',IBID  ,'EXIS_DECOUPE',
     &            R8BID ,IBID  ,DECOUP)
      LDECO = DECOUP.EQ.'OUI'
C
C --- SI NEWTON/PREDICTION ='DEPL_CALCULE', ALORS ON INTERDIT
C --- LA SUBDIVISION
C
      CALL GETVTX('NEWTON','PREDICTION',1,IARG,1,PRED,IRET)
      IF (IRET.NE.0) THEN
        IF (PRED.EQ.'DEPL_CALCULE') THEN
          IF (LDECO) THEN
            CALL U2MESS('F','SUBDIVISE_99') 
          ENDIF
        ENDIF
      ENDIF
C
C --- SI ON DOIT DECOUPER - CAPTURE MATRICE SINGULIERE DANS SOLVEUR
C
      IF (LDECO) THEN
        IF (SOLVEU(1:8).NE.'&&OP0033') THEN
          CALL CRSVSI(SOLVEU)
        ENDIF
      ENDIF
C
C --- EN GESTION AUTO, AVEC UN CRITERE D'ADAPTATION EN SEUIL SUR
C     NB_ITER_NEWT, ON MET VALE = ITER_GLOB_MAXI/2 SI VALE N'A PAS
C     ETE RENSIGNE DANS DEFI_LIST_INST
C     ON NE CONSIDERE PAS LE CAS DE DE ITER_GLOB_ELAS CAR C'EST ACTIVE
C     (MATRICE SECANTE) QU'EN CAS DE DIFFICULTE
      
      IF (METLIS.EQ.'AUTO') THEN
        CALL GETVIS('CONVERGENCE','ITER_GLOB_MAXI',1,IARG,1,ITMX,IRET)
        DO 25 IADAPT=1,NADAPT
          CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'NOM_EVEN',
     &                R8BID ,IBID  ,EVEN  )
          IF (EVEN.EQ.'SEUIL_SANS_FORMULE') THEN
            CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'NOM_PARA',
     &                  R8BID ,IBID  ,NOPARA)
            IF (NOPARA.EQ.'NB_ITER_NEWT') THEN
              CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'VALE',
     &                    R8BID ,VALI  ,K8BID)
              IF (VALI.EQ.0) THEN
                VALI = ITMX / 2
                VALR = VALI
                CALL UTDIDT('E'   ,SDDISC,'ADAP',IADAPT,'VALE',
     &                      VALR  ,IBID  ,K8BID )
              ENDIF
            ENDIF
          ENDIF
 25     CONTINUE
      ENDIF
C
C --- VERIF COHERENCE AVEC IMPLEX
C
      IF (METLIS.EQ.'AUTO') THEN
        DO 27 IADAPT=1,NADAPT
          CALL UTDIDT('L'   ,SDDISC,'ADAP',IADAPT,'METHODE',
     &                R8BID ,IBID  ,MODETP)
          IF (MODETP.EQ.'IMPLEX') THEN
            IF (.NOT.LIMPEX) CALL U2MESS('F','MECANONLINE6_4')
          ENDIF
 27     CONTINUE
      ENDIF
C
C --- CREATION SD STOCKAGE DES INFOS EN COURS DE CALCUL
C
      CALL NMCERR(SDDISC,ITER1 ,ITER2 ,ELASDT,RGMAXI,
     &            RGRELA,INIKRY)
C
      CALL JEDEMA()

      END
