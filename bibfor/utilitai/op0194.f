      SUBROUTINE OP0194()
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C ----------------------------------------------------------------------
C
C
C      OPERATEUR :     CALC_META
C
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER            ZI
      INTEGER VALI
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER      IBID, IRET, N1, N2, N3, NUM, NUMPHA,
     &              NBORDT, NBTROU, IER
      REAL*8       INST, PREC
      REAL*8 VALR
      COMPLEX*16   C16B
      CHARACTER*8  K8B, CRIT, TEMPER, RESULT, MODELE, MATERI
      CHARACTER*16 TYPE, OPER, OPTION
      CHARACTER*24 COMPOR, CHMETA, PHASIN, MATE, K24BID
      CHARACTER*24 VALK
C
C      LOGICAL
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ
C
      CALL GETRES ( RESULT, TYPE, OPER )
C
C- MODELE, CHAM_MATER ET RELATION DE COMPORTEMENT --------------------
C
      CALL GETVID(' ','MODELE'    ,0,1,1,MODELE,IBID)
      CALL GETVID(' ','CHAM_MATER',0,1,1,MATERI,IBID)

      CALL RCMFMC(MATERI,MATE)
      CALL MTDORC(MODELE,COMPOR,K24BID)




C      CALL GETVTX ( ' ', 'OPTION', 0,1,1, OPTION, N1 )
      OPTION = 'META_ELNO'



C----RECUPERATION EVOL-THER

      CALL GETVID(' ','RESULTAT',1,1,1,TEMPER,N1)


C-ETAT INITIAL

      NUMPHA = 0
      CALL GETVID('ETAT_INIT','META_INIT_ELNO',1,1,1,CHMETA,N3)
      IF (N3.GT.0) THEN
        PHASIN = '&&SMEVOL_ZINIT'
        CALL CHPVER('F',CHMETA(1:19),'CART','VAR2_R',IER)
        CALL COPISD('CHAMP_GD','V',CHMETA,PHASIN(1:19))
      ELSE
        CALL GETVID('ETAT_INIT','EVOL_THER',1,1,1,TEMPER,N1)
        CALL GETVIS('ETAT_INIT','NUME_INIT',1,1,1,NUM,N2)
        IF (N2.EQ.0) THEN
          CALL GETVR8 ( 'ETAT_INIT', 'INST_INIT', 1,1,1, INST, N3)
          CALL GETVR8 ( 'ETAT_INIT', 'PRECISION', 1,1,1, PREC, N3)
          CALL GETVTX ( 'ETAT_INIT', 'CRITERE'  , 1,1,1, CRIT, N3)
          NBORDT = 1
          CALL RSORAC ( TEMPER, 'INST', IBID, INST, K8B, C16B, PREC,
     &                  CRIT, NUM, NBORDT, NBTROU )
          IF (NBTROU.EQ.0) THEN
            VALK = TEMPER
            VALR = INST
            CALL U2MESG('F', 'UTILITAI6_51',1,VALK,0,0,1,VALR)
          ELSE IF (NBTROU.GT.1) THEN
            VALK = TEMPER
            VALR = INST
            VALI = NBTROU
            CALL U2MESG('F', 'UTILITAI6_52',1,VALK,1,VALI,1,VALR)
          ENDIF
        ENDIF
        CALL RSEXCH ( TEMPER, 'META_ELNO', NUM, PHASIN, IRET )
        IF (IRET.GT.0) THEN
          CALL U2MESS('F','UTILITAI3_20')
        END IF
        NUMPHA = NUM
      ENDIF


      CALL SMEVOL(TEMPER(1:8),MODELE,MATE,COMPOR,OPTION,PHASIN,NUMPHA)


      CALL JEDETC('G','&&NMDORC',1)
C
      CALL JEDEMA()
      END
