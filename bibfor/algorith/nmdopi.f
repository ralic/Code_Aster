      SUBROUTINE NMDOPI (MODELZ, NUMEDD, PILOTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/01/2006   AUTEUR MABBAS M.ABBAS 
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
C RESPONSABLE MABBAS M.ABBAS
      IMPLICIT NONE
      CHARACTER*(*)      MODELZ
      CHARACTER*8        MODELE
      CHARACTER*24       NUMEDD
      CHARACTER*14       PILOTE
C ----------------------------------------------------------------------
C     CONSTRUCTION DE LA SD PILOTAGE
C
C IN     MODELE K8  : MODELE
C IN     NUMEDD K24 : NUME_DDL
C OUT SD PILOTE K14 : PILOTAGE
C                      .PLTK K24
C                       (1) = TYPE DE PILOTAGE
C                       (2) = LIGREL POUR LES PILOTAGES PAR ELEMENTS
C                       (3) = NOM DE LA CARTE DU TYPE (PILO_K)
C                       (4) = NOM DE LA CARTE DU TYPE (PILO_R) MIN/MAX
C                       (5) = PROJECTION 'OUI' OU 'NON' SUR LES BORNES
C                       (6) = TYPE DE SELECTION : 'RESIDU', 
C                               'NORM_INCR_DEPL' OU 'ANGL_INCR_DEPL'
C                      .PLCR K19 COEFFICIENTS DU PILOTAGE
C                      .PLIR R8  PARAMETRES DU PILOTAGE
C                       (1) = COEF_PILO
C                       (2) = ETA_PILO_MAX
C                       (3) = ETA_PILO_MIN
C                       (4) = ETA_PILO_R_MAX
C                       (5) = ETA_PILO_R_MIN
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      LOGICAL       LBEL
      INTEGER       NOCC, NBG,JVALE,NBNO,NUMNOE, NUMEQU, NDDL, I, N
      INTEGER       JGRO, JLICMP, JDDL, JEQU, JINFO, JTYPE
      INTEGER       IBID, IER, N1, N2, NEQ
      REAL*8        COEF, MX, MI, R8VIDE, R8BID, LM(2), R8GAEM
      COMPLEX*16    CBID
      CHARACTER*8   K8BID,NOMA,NOMNOE,NOMDDL,NOMGRP,RESULT,LBORN(2)
      CHARACTER*16  CONCEP, NOMCMD, COMP
      CHARACTER*24  GRNO, LISCMP, LISDDL, LISEQU, TYPE,PROJ
      CHARACTER*19  CHAPIL, LIGREL, LIGRMO, CARTE, CARTE2


      CALL JEMARQ()
      MODELE = MODELZ

C -- PAS DE PILOTAGE

      CALL GETFAC('PILOTAGE',NOCC)
      IF (NOCC .EQ. 0) GOTO 9999


C -- LECTURE DU TYPE ET DE LA ZONE

      CALL WKVECT (PILOTE // '.PLTK','V V K24',6,JTYPE)
      CALL GETVTX('PILOTAGE','TYPE',1,1,1,TYPE,N1)
      ZK24(JTYPE) = TYPE
      CALL GETVTX('PILOTAGE','PROJ_BORNES',1,1,1,PROJ,N1)
      ZK24(JTYPE+4) = PROJ
      CALL GETVTX('PILOTAGE','SELECTION',1,1,1,PROJ,N1)
      ZK24(JTYPE+5) = PROJ


C -- PARAMETRES COEF_MULT ET ETA_PILO_MAX

      CALL WKVECT(PILOTE // '.PLIR','V V R8',5,JINFO)
      CALL GETVR8 ('PILOTAGE','COEF_MULT',1,1,1,COEF  , N1)
      ZR(JINFO) = COEF

      CALL GETVR8('PILOTAGE','ETA_PILO_R_MAX',1,1,1,MX,N1)
      IF (N1.NE.1)  MX = R8GAEM()
      ZR(JINFO+3) = MX

      CALL GETVR8('PILOTAGE','ETA_PILO_R_MIN',1,1,1,MI,N2)
      IF (N2.NE.1)  MI = -R8GAEM()
      ZR(JINFO+4) = MI

      CALL GETVR8('PILOTAGE','ETA_PILO_MAX',1,1,1,MX,N1)
      IF (N1.NE.1)  THEN 
        MX = R8VIDE()
      ELSE
        IF (MX.GT.ZR(JINFO+3)) CALL UTMESS('F','NMDOPI',
     & 'ETA_PILO_MAX DOIT ETRE INFERIEUR A ETA_PILO_R_MAX')
      END IF
      ZR(JINFO+1) = MX

      CALL GETVR8('PILOTAGE','ETA_PILO_MIN',1,1,1,MI,N2)
      IF (N2.NE.1) THEN
        MI = R8VIDE()
      ELSE
        IF (MI.LT.ZR(JINFO+4)) CALL UTMESS('F','NMDOPI',
     & 'ETA_PILO_MIN DOIT ETRE SUPERIEUR A ETA_PILO_R_MIN')
      END IF
      ZR(JINFO+2) = MI


C ======================================================================
C             PILOTAGE PAR PREDICTION ELASTIQUE : PRED_ELAS
C ======================================================================

      IF (TYPE .EQ. 'PRED_ELAS' .OR. 
     &    TYPE .EQ. 'DEFORMATION') THEN

        CALL EXLIMA('PILOTAGE','V',MODELE,'&&LIGRPI',LIGREL)
        ZK24(JTYPE+1) = LIGREL

        CARTE  = '&&NMDOPI.TYPEPILO'
        LIGRMO = MODELE // '.MODELE'
        CALL MECACT('V', CARTE, 'MODELE', LIGRMO, 'PILO_K', 1, 'TYPE',
     &              IBID, R8BID, CBID, TYPE)
        ZK24(JTYPE+2) = CARTE

        LM(1)=MX
        LM(2)=MI
        CARTE2 = '&&NMDOPI.BORNEPILO'
        LBORN(1)='A0'
        LBORN(2)='A1'
        CALL MECACT('V',CARTE2,'MODELE',LIGRMO,'PILO_R', 2, LBORN,
     &                  IBID, LM, CBID, K8BID)
        ZK24(JTYPE+3) = CARTE2
        
        GOTO 9999



C ======================================================================
C              PILOTAGE PAR UN DEGRE DE LIBERTE : DDL_IMPO
C ======================================================================

      ELSE IF (TYPE .EQ. 'DDL_IMPO') THEN
        CHAPIL = PILOTE // '.PLCR'
        CALL VTCREB(CHAPIL,NUMEDD,'V','R',NEQ)
        CALL JEVEUO(CHAPIL//'.VALE','E',JVALE)

        CALL DISMOI('F','NOM_MAILLA',CHAPIL,'CHAM_NO',IBID,NOMA,IER)
        CALL GETVEM(NOMA,'NOEUD','PILOTAGE','NOEUD',1,1,0,K8BID,NBNO)
        NBNO = -NBNO
        IF (NBNO .NE. 0 ) THEN
          IF ( NBNO .GT. 1 ) THEN
            CALL UTMESS('F','NMDOPI_01','IL FAUT '//
     &                  'AU PLUS 1 NOEUD POUR LE PILOTAGE DDL_IMPO')
          ENDIF
          CALL GETVEM(NOMA,'NOEUD','PILOTAGE','NOEUD',
     &            1,1,NBNO,NOMNOE,N1)
          CALL GETVTX ('PILOTAGE','NOM_CMP',   1,1,NBNO,NOMDDL,N1)
          CALL JENONU(JEXNOM(NOMA//'.NOMNOE',NOMNOE),NUMNOE)
          CALL NUEQCH(CHAPIL,1,NUMNOE,NOMDDL,NUMEQU)
          ZR(JVALE-1+NUMEQU) = 1.D0
        ENDIF

        CALL GETVEM(NOMA,'GROUP_NO','PILOTAGE','GROUP_NO',
     &             1,1,0,K8BID,NBG)

        NBG = -NBG
        IF (NBG .NE. 0 ) THEN
          IF ( NBG .GT. 1 ) THEN
            CALL UTMESS('F','NMDOPI_02','IL FAUT '//
     &         'AU PLUS 1 GROUPE DE NOEUD POUR LE PILOTAGE DDL_IMPO')
          ENDIF
          CALL GETVEM(NOMA,'GROUP_NO','PILOTAGE','GROUP_NO',
     +               1,1,1,NOMGRP,N1)
          CALL GETVTX ('PILOTAGE','NOM_CMP',  1,1,1,NOMDDL,N1)
          GRNO=NOMA//'.GROUPENO'
          CALL JELIRA (JEXNOM(GRNO,NOMGRP),'LONMAX',NBNO,K8BID)
          IF ( NBNO .GT. 1 ) THEN
            CALL UTMESS('F','NMDOPI_03','IL FAUT AU PLUS '//
     &        'UN NOEUD DANS LE GROUPE POUR LE PILOTAGE DDL_IMPO')
          ENDIF
          CALL JEVEUO (JEXNOM(GRNO,NOMGRP),'L',JGRO)
          NUMNOE = ZI(JGRO)
          CALL NUEQCH(CHAPIL,1,NUMNOE,NOMDDL,NUMEQU)
          ZR(JVALE-1+NUMEQU) = 1.D0
        ENDIF



C ======================================================================
C      PILOTAGE PAR UNE METHODE DE TYPE LONGUEUR D'ARC : LONG_ARC
C ======================================================================

      ELSE IF (TYPE .EQ. 'LONG_ARC' ) THEN

        CHAPIL = PILOTE // '.PLCR'
        CALL VTCREB(CHAPIL,NUMEDD,'V','R',NEQ)
        CALL JEVEUO(CHAPIL//'.VALE','E',JVALE)

        CALL DISMOI('F','NOM_MAILLA',CHAPIL,'CHAM_NO',IBID,NOMA,IER)

        CALL GETVEM(NOMA,'GROUP_NO','PILOTAGE','GROUP_NO',
     +             1,1,0,K8BID,NBG)
        NBG = -NBG
        IF (NBG.NE.1) CALL UTMESS('F','PILOTAGE (NMDOPI 03)','IL FAUT '
     &    // 'PRECISER UN GROUPE DE NOEUDS DANS LA METHODE LONG_ARC')

        CALL GETVEM(NOMA,'GROUP_NO','PILOTAGE','GROUP_NO',
     +             1,1,1,NOMGRP,N1)

        GRNO=NOMA//'.GROUPENO'
        CALL JELIRA (JEXNOM(GRNO,NOMGRP),'LONMAX',NBNO,K8BID)
        CALL JEVEUO (JEXNOM(GRNO,NOMGRP),'L',JGRO)
        IF (NBNO.EQ.0) CALL UTMESS('F','PILOTAGE (NMDOPI 4)',
     &    'GROUPE DE NOEUD ' // NOMGRP // ' VIDE')
        COEF = 1.D0 / NBNO

        CALL GETVTX ('PILOTAGE','NOM_CMP',  1,1,0,K8BID, NDDL)
        NDDL = -NDDL
        IF (NDDL.EQ.0) CALL UTMESS('F','PILOTAGE (NMDOPI 5)',
     &    'LISTE DE COMPOSANTES VIDE POUR LA METHODE LONG_ARC')
        LISCMP = '&&NMDOPI.LISCMP'
        CALL WKVECT(LISCMP,'V V K8',NDDL,JLICMP)
        CALL GETVTX ('PILOTAGE','NOM_CMP',  1,1,NDDL,
     &                ZK8(JLICMP), NDDL)

        LISDDL = '&&NMDOPI.LISDDL'
        LISEQU = '&&NMDOPI.LISEQU'
        CALL WKVECT (LISDDL,'V V K8',NBNO,JDDL)
        CALL WKVECT (LISEQU,'V V I', NBNO,JEQU)

        DO 10 I = 1,NDDL
          DO 15 N = 1,NBNO
            ZK8(JDDL-1+N) = ZK8(JLICMP-1+I)
 15       CONTINUE

          CALL NUEQCH(CHAPIL, NBNO, ZI(JGRO), ZK8(JDDL), ZI(JEQU))

          DO 20 N = 1,NBNO
            NUMEQU = ZI(JEQU-1+N)
            ZR(JVALE-1+NUMEQU) = COEF
 20       CONTINUE
 10     CONTINUE

        CALL JEDETR(LISDDL)
        CALL JEDETR(LISEQU)

      ENDIF

 9999 CONTINUE
      CALL JEDEMA()
      END
