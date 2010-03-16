      SUBROUTINE NMDOPI(MODELZ,NUMEDD,SDPILO)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/03/2010   AUTEUR ABBAS M.ABBAS 
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
C
      IMPLICIT NONE
      CHARACTER*(*)      MODELZ
      CHARACTER*24       NUMEDD
      CHARACTER*19       SDPILO
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (STRUCTURES DE DONNEES)
C
C CONSTRUCTION DE LA SD PILOTAGE
C
C ----------------------------------------------------------------------
C
C
C IN  MODELE : MODELE
C IN  NUMEDD : NUME_DDL
C OUT SDPILO : SD PILOTAGE
C               .PLTK
C                (1) = TYPE DE PILOTAGE
C                (2) = LIGREL POUR LES PILOTAGES PAR ELEMENTS
C                (3) = NOM DE LA CARTE DU TYPE (PILO_K)
C                (4) = NOM DE LA CARTE DU TYPE (PILO_R) MIN/MAX
C                (5) = PROJECTION 'OUI' OU 'NON' SUR LES BORNES
C                (6) = TYPE DE SELECTION : 'RESIDU',
C                        'NORM_INCR_DEPL' OU 'ANGL_INCR_DEPL'
C                (7) = EVOLUTION DES BORNES 
C                        'CROISSANT', 'DECROISSANT' OU 'SANS'
C               .PLCR  COEFFICIENTS DU PILOTAGE
C               .PLIR  PARAMETRES DU PILOTAGE
C                (1) = COEF_PILO
C                (2) = ETA_PILO_MAX
C                (3) = ETA_PILO_MIN
C                (4) = ETA_PILO_R_MAX
C                (5) = ETA_PILO_R_MIN
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNOM
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER       NOCC, NBG,JVALE,NBNO,NUMNOE, NUMEQU, NDDL, I, N
      INTEGER       JGRO, JLICMP, JDDL, JEQU, JPLIR, JPLTK
      INTEGER       IBID, IER, N1, N2, NEQ
      REAL*8        COEF, R8BID, LM(2)
      REAL*8        R8VIDE,R8GAEM,R8PREM
      COMPLEX*16    C16BID
      CHARACTER*8   K8BID,NOMA,NOMNOE,NOMDDL,NOMGRP,LBORN(2)
      CHARACTER*8   MODELE
      CHARACTER*24  GRNO  ,LISCMP,LISDDL,LISEQU
      CHARACTER*24  TYPPIL,PROJBO,TYPSEL,EVOLPA
      CHARACTER*19  CHAPIL,LIGRMO,LIGRPI
      CHARACTER*19  CARETA,CARTYP
      REAL*8        ETRMAX,ETRMIN,ETAMIN,ETAMAX
      INTEGER       IFM,NIV
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('MECA_NON_LINE',IFM,NIV)
C
C --- INITIALISATIONS
C
      MODELE = MODELZ
C
C --- PAS DE PILOTAGE
C
      CALL GETFAC('PILOTAGE',NOCC)
      IF (NOCC .EQ. 0) THEN
        GOTO 9999
      ELSE
        IF (NIV.GE.2) THEN
          WRITE (IFM,*) '<MECANONLINE> ... CREATION SD PILOTAGE'
        ENDIF
      ENDIF
C
C --- LECTURE DU TYPE ET DE LA ZONE
C
      CALL WKVECT(SDPILO(1:19)// '.PLTK','V V K24',7,JPLTK)
      CALL GETVTX('PILOTAGE','TYPE'           ,1,1,1,TYPPIL,N1)
      ZK24(JPLTK)   = TYPPIL
      CALL GETVTX('PILOTAGE','PROJ_BORNES'    ,1,1,1,PROJBO,N1)
      ZK24(JPLTK+4) = PROJBO
      CALL GETVTX('PILOTAGE','SELECTION'      ,1,1,1,TYPSEL,N1)
      ZK24(JPLTK+5) = TYPSEL
      CALL GETVTX('PILOTAGE','EVOL_PARA'      ,1,1,1,EVOLPA,N1)
      ZK24(JPLTK+6) = EVOLPA    
C
C --- PARAMETRES COEF_MULT ET ETA_PILO_MAX
C
      CALL WKVECT(SDPILO(1:19)// '.PLIR','V V R8',5,JPLIR)
      CALL GETVR8('PILOTAGE','COEF_MULT',1,1,1,COEF  , N1)
      ZR(JPLIR)   = COEF
      
      IF (ABS(COEF).LE.R8PREM()) THEN
        CALL U2MESS('F','PILOTAGE_3')
      ENDIF

      CALL GETVR8('PILOTAGE','ETA_PILO_R_MAX',1,1,1,ETRMAX,N1)
      IF (N1.NE.1)  ETRMAX = R8GAEM()
      ZR(JPLIR+3) = ETRMAX

      CALL GETVR8('PILOTAGE','ETA_PILO_R_MIN',1,1,1,ETRMIN,N2)
      IF (N2.NE.1)  ETRMIN = -R8GAEM()
      ZR(JPLIR+4) = ETRMIN

      CALL GETVR8('PILOTAGE','ETA_PILO_MAX',1,1,1,ETAMAX,N1)
      IF (N1.NE.1)  THEN
        ETAMAX = R8VIDE()
      ELSE
        IF (ETAMAX.GT.ZR(JPLIR+3)) CALL U2MESS('F','PILOTAGE_48')
      END IF
      ZR(JPLIR+1) = ETAMAX

      CALL GETVR8('PILOTAGE','ETA_PILO_MIN',1,1,1,ETAMIN,N2)
      IF (N2.NE.1) THEN
        ETAMIN = R8VIDE()
      ELSE
        IF (ETAMIN.LT.ZR(JPLIR+4)) CALL U2MESS('F','PILOTAGE_49')
      END IF
      ZR(JPLIR+2) = ETAMIN


C ======================================================================
C             PILOTAGE PAR PREDICTION ELASTIQUE : PRED_ELAS
C ======================================================================

      IF (TYPPIL .EQ. 'PRED_ELAS' .OR.
     &    TYPPIL .EQ. 'DEFORMATION') THEN

        CALL EXLIMA('PILOTAGE','V',MODELE,LIGRPI)
        ZK24(JPLTK+1) = LIGRPI


        CARTYP = '&&NMDOPI.TYPEPILO'
        LIGRMO = MODELE // '.MODELE'
        CALL MECACT('V'   ,CARTYP,'MODELE',LIGRMO,'PILO_K',
     &              1     ,'TYPE',IBID    ,R8BID ,C16BID  ,
     &              TYPPIL)
        ZK24(JPLTK+2) = CARTYP

        LM(1)    = ETAMAX
        LM(2)    = ETAMIN
        CARETA   = '&&NMDOPI.BORNEPILO'
        LBORN(1) = 'A0'
        LBORN(2) = 'A1'
        CALL MECACT('V'   ,CARETA,'MODELE',LIGRMO,'PILO_R', 
     &              2     ,LBORN ,IBID    ,LM    ,C16BID  ,
     &              K8BID)
        ZK24(JPLTK+3) = CARETA

        GOTO 9999



C ======================================================================
C              PILOTAGE PAR UN DEGRE DE LIBERTE : DDL_IMPO
C ======================================================================

      ELSE IF (TYPPIL .EQ. 'DDL_IMPO') THEN
        CHAPIL = SDPILO(1:14)//'.PLCR'
        CALL VTCREB(CHAPIL,NUMEDD,'V','R',NEQ)
        CALL JEVEUO(CHAPIL(1:19)//'.VALE','E',JVALE)

        CALL DISMOI('F','NOM_MAILLA',CHAPIL,'CHAM_NO',IBID,NOMA,IER)
        CALL GETVEM(NOMA,'NOEUD','PILOTAGE','NOEUD',1,1,0,K8BID,NBNO)
        NBNO = -NBNO
        IF (NBNO .NE. 0 ) THEN
          IF ( NBNO .GT. 1 ) THEN
            CALL U2MESS('F','PILOTAGE_50')
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
            CALL U2MESS('F','PILOTAGE_51')
          ENDIF
          CALL GETVEM(NOMA,'GROUP_NO','PILOTAGE','GROUP_NO',
     &               1,1,1,NOMGRP,N1)
          CALL GETVTX ('PILOTAGE','NOM_CMP',  1,1,1,NOMDDL,N1)
          GRNO=NOMA//'.GROUPENO'
          CALL JELIRA (JEXNOM(GRNO,NOMGRP),'LONMAX',NBNO,K8BID)
          IF ( NBNO .GT. 1 ) THEN
            CALL U2MESS('F','PILOTAGE_52')
          ENDIF
          CALL JEVEUO (JEXNOM(GRNO,NOMGRP),'L',JGRO)
          NUMNOE = ZI(JGRO)
          CALL NUEQCH(CHAPIL,1,NUMNOE,NOMDDL,NUMEQU)
          ZR(JVALE-1+NUMEQU) = 1.D0
        ENDIF



C ======================================================================
C      PILOTAGE PAR UNE METHODE DE TYPE LONGUEUR D'ARC : LONG_ARC
C ======================================================================

      ELSE IF (TYPPIL .EQ. 'LONG_ARC' ) THEN
        CHAPIL = SDPILO(1:14)//'.PLCR'
        CALL VTCREB(CHAPIL,NUMEDD,'V','R',NEQ)
        CALL JEVEUO(CHAPIL(1:19)//'.VALE','E',JVALE)


        CALL DISMOI('F','NOM_MAILLA',CHAPIL,'CHAM_NO',IBID,NOMA,IER)

        CALL GETVEM(NOMA,'GROUP_NO','PILOTAGE','GROUP_NO',
     &             1,1,0,K8BID,NBG)
        NBG = -NBG
        IF (NBG.NE.1) CALL U2MESS('F','PILOTAGE_53')

        CALL GETVEM(NOMA,'GROUP_NO','PILOTAGE','GROUP_NO',
     &             1,1,1,NOMGRP,N1)

        GRNO=NOMA//'.GROUPENO'
        CALL JELIRA (JEXNOM(GRNO,NOMGRP),'LONMAX',NBNO,K8BID)
        CALL JEVEUO (JEXNOM(GRNO,NOMGRP),'L',JGRO)
        IF (NBNO.EQ.0) CALL U2MESK('F','PILOTAGE_54',1,NOMGRP)
        COEF = 1.D0 / NBNO

        CALL GETVTX ('PILOTAGE','NOM_CMP',  1,1,0,K8BID, NDDL)
        NDDL = -NDDL
        IF (NDDL.EQ.0) CALL U2MESS('F','PILOTAGE_55')
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
