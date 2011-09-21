      SUBROUTINE OP0075()
      IMPLICIT REAL*8(A-H,O-Z)
C-----------------------------------------------------------------------
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C-----------------------------------------------------------------------
C
C     OPERATEUR REST_GENE_PHYS
C
C ----------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ---------------------------
C
      CHARACTER*8 K8B,NOMRES,RESIN,MODE
      CHARACTER*8 K8BID,BLANC,PARAM(3)
      CHARACTER*16 CONCEP,NOMCMD,TYPRES,TYPREP,CHAMP(4)
      CHARACTER*19 PROFNO
      CHARACTER*24 MATGEN,NUMGEN
      LOGICAL PROMES
      INTEGER JORD,NBORD,I,IORD,LPAIN(3),LPAOUT(3)
      INTEGER      IARG
C
C     -----------------------------------------------------------------
      DATA K8B/'        '/
      DATA PARAM/'MODELE','CHAMPMAT','CARAELEM'/

      CALL JEMARQ()
      CALL INFMAJ()
      K8BID='        '
      BLANC='        '
C
C     -----------------------------------------------------------------
C
C
      CALL GETRES(NOMRES,TYPRES,NOMCMD)
C
C --- PHASE DE TEST SUR LES CHAMPS A RESTITUER
      CALL GETVTX(' ','NOM_CHAM',1,IARG,4,CHAMP,NBCHAM)
      IF (NBCHAM.LT.0) THEN
        CALL U2MESS('E','ALGORITH9_44')
      ELSE
        DO 20 I=1,NBCHAM
          DO 10 J=I+1,NBCHAM
            IF (CHAMP(I).EQ.CHAMP(J)) THEN
              CALL U2MESS('E','ALGORITH9_30')
            ENDIF
   10     CONTINUE
          IF (CHAMP(I).EQ.'ACCE_ABSOLU') THEN
            CALL GETVID(' ','ACCE_MONO_APPUI',1,IARG,1,K8B,N1)
            IF (N1.EQ.0) THEN
              CALL U2MESS('E','ALGORITH9_45')
            ENDIF
          ENDIF
   20   CONTINUE
      ENDIF
C
C --- CREATION DU .REFN DU PROFIL :
C     ---------------------------
      PROFNO=NOMRES//'.PROFC.NUME'
      CALL WKVECT(PROFNO(1:19)//'.REFN','V V K24',4,JREFN)
      ZK24(JREFN+1)='DEPL_R'
C
      CALL GETVID(' ','RESU_GENE',1,IARG,1,RESIN,IR1)
      CALL GETTCO(RESIN,CONCEP)

C INDICATEUR CALCUL SANS MATRICE GENERALISEE (PROJ_MESU_MODAL)
      PROMES=.FALSE.

      CALL JEVEUO(RESIN//'           .REFD','L',J1REFE)
      MATGEN=ZK24(J1REFE)
      NUMGEN=ZK24(J1REFE+3)
C LE RESU_GENE VIENT DE PROJ_MESU_MODAL
      IF ((MATGEN(1:8).EQ.BLANC) .AND. (NUMGEN(1:8).EQ.BLANC)) THEN
        PROMES=.TRUE.
        TYPREP=BLANC
      ELSE
        IF (NUMGEN(1:8).EQ.BLANC) THEN
          CALL JEVEUO(MATGEN(1:8)//'           .REFA','L',J2REFE)
          NUMGEN=ZK24(J2REFE+1)(1:14)
        ENDIF
        CALL JEVEUO(NUMGEN(1:14)//'.NUME.REFN','L',J3REFE)
        CALL GETTCO(ZK24(J3REFE),TYPREP)
      ENDIF

C
C     --- DYNAMIQUE TRANSITOIRE ---
C
      IF (CONCEP(1:9).EQ.'TRAN_GENE') THEN
C
        IF (PROMES) THEN
          CALL TRAN75(NOMRES,TYPRES,RESIN,K8B)
C --- CAS DU CALCUL TRANSITOIRE CLASSIQUE
        ELSE
          IF ((TYPREP(1:9).EQ.'MODE_MECA')) THEN
            CALL TRAN75(NOMRES,TYPRES,RESIN,K8BID)
C
          ELSEIF (TYPREP(1:9).EQ.'MODE_GENE') THEN
            CALL GETVID(' ','MODE_MECA',1,IARG,1,MODE,IBID)
            IF (IBID.EQ.0) THEN
              CALL U2MESS('F','ALGORITH9_48')
            ENDIF
            CALL TRAN75(NOMRES,TYPRES,RESIN,MODE)
          ENDIF
C
C
        ENDIF
C
C     --- CALCUL MODAL SANS SOUS-STRUCTURATION
C
      ELSEIF (CONCEP(1:9).EQ.'MODE_GENE') THEN

        CALL REGENE(NOMRES,RESIN)

      ELSEIF (CONCEP(1:9).EQ.'HARM_GENE') THEN

        IF (PROMES) THEN

          CALL HARM75(NOMRES,TYPRES,RESIN,NOMCMD,K8BID)

        ELSE

C     --- CALCUL HARMONIQUE SANS SOUS-STRUCTURATION ---
          IF ((TYPREP(1:9).EQ.'MODE_MECA')) THEN
            CALL HARM75(NOMRES,TYPRES,RESIN,NOMCMD,K8BID)
          ELSEIF (TYPREP(1:9).EQ.'MODE_GENE') THEN
            CALL GETVID(' ','MODE_MECA',1,IARG,1,MODE,IBID)
            IF (IBID.EQ.0) THEN
              CALL U2MESS('F','ALGORITH9_48')
            ENDIF
            CALL HARM75(NOMRES,TYPRES,RESIN,NOMCMD,MODE)
          ENDIF
        ENDIF
      ENDIF
C
C
C --- STOCKAGE
      CALL GETTCO(RESIN,CONCEP)
      IF ((CONCEP(1:9).EQ.'MODE_GENE')) THEN
        CALL JEVEUO(NOMRES//'           .ORDR','L',JORD)
        CALL JELIRA(NOMRES//'           .ORDR','LONUTI',NBORD,K8B)
        DO 50 IORD=1,NBORD
          CALL RSADPA(RESIN,'L',3,PARAM,ZI(JORD+IORD-1),0,LPAIN,K8B)
          CALL RSADPA(NOMRES,'E',3,PARAM,ZI(JORD+IORD-1),0,LPAOUT,K8B)
          DO 40 I=1,3
            ZK8(LPAOUT(I))=ZK8(LPAIN(I))
   40     CONTINUE
   50   CONTINUE
      ENDIF

      CALL JEDEMA()
      END
