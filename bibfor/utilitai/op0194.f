      SUBROUTINE OP0194 ( IER )
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 01/10/2002   AUTEUR CIBHHLV L.VIVAN 
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
      INTEGER    IER
C ----------------------------------------------------------------------
C
C
C      OPERATEUR :     CALC_META
C
C ----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER      IBID, IRET, N1, N2, N3, NUM, NUMPHA, IOP, JOPT, I, 
     +             NBOPT, NBORDT, NBTROU 
      REAL*8       INST, PREC
      COMPLEX*16   C16B
      CHARACTER*8  K8B, CRIT, TEMPER, TEMPEV, RESULT, MODELE, MATERI
      CHARACTER*16 TYPE, OPER, OPTION, OPT
      CHARACTER*24 COMPOR, LIGRMO, CHMETA, PHASIN, MATE
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
      CALL NMDORC(MODELE,COMPOR)


C OPTION : AU NOEUD OU AU POINT DE GAUSS

      CALL GETVTX ( ' ', 'OPTION', 0,1,1, OPTION, N1 )
      IF (N1.EQ.0) OPTION = 'META_ELGA_TEMP'



C----RECUPERATION EVOL-THER

      CALL GETVID(' ','RESULTAT',1,1,1,TEMPER,N1)


C-ETAT INITIAL

      NUMPHA = 0
      CALL GETVID('ETAT_INIT','META_INIT',1,1,1,CHMETA,N3)
      IF (N3.GT.0) THEN
        PHASIN = '&&SMEVOL_ZINIT'
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
            CALL UTDEBM('F',OPER,'PAS DE CHAMP CORRESPONDANT '//
     &                           'A L''INSTANT DEMANDE.')
            CALL UTIMPK('L','RESULTAT ',1,TEMPER)
            CALL UTIMPR('S',', ACCES "INST_INIT" :',1,INST)
            CALL UTFINM()
          ELSE IF (NBTROU.GT.1) THEN
            CALL UTDEBM('F',OPER,'PLUSIEURS CHAMPS CORRESPONDANT '//
     &                           'A L''INSTANT DEMANDE.')
            CALL UTIMPK('L','RESULTAT ',1,TEMPER)
            CALL UTIMPR('S',', ACCES "INST_INIT" :',1,INST)
            CALL UTIMPI('S',', NOMBRE :',1,NBTROU)
            CALL UTFINM()
          ENDIF
        ENDIF
        CALL RSEXCH ( TEMPER, 'META_ELGA_TEMP', NUM, PHASIN, IRET )
        IF (IRET.GT.0) THEN
          CALL UTMESS('F',OPER,'LE CHAMP DE '//
     &         'META_ELGA_TEMP:ETAT_INIT(NUM_INIT) N''EXISTE PAS.'
     &                  )
        END IF
        NUMPHA = NUM
      ENDIF


      CALL SMEVOL(TEMPER(1:8),MODELE,MATE,COMPOR,OPTION,PHASIN,NUMPHA)


      CALL JEDETC('G','&&NMDORC',1)
      CALL JEDETC('V','.CODI',20)
      CALL JEDETC('V','.MATE_CODE',9)
      CALL JEDETC('V','&&',1)

C       --- ON FAIT LE MENAGE ...
C
      CALL JEDETC ('V','&&',1)
      CALL JEDETC ('V','_',1)
      CALL JEDETC ('V',RESULT(1:8),1)
      CALL JEDETC ('V','.MATE_CODE',9)
C
      CALL JEDEMA()
      END
