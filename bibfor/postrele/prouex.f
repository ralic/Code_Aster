      SUBROUTINE PROUEX ( TYPCO, COURBE, NCHPT, LSTMAC, LSTNAC, NOMA )
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
      IMPLICIT   NONE
      CHARACTER*8         TYPCO, COURBE, NOMA
      CHARACTER*19        NCHPT
      CHARACTER*24        LSTMAC, LSTNAC
C**********************************************************************
C MODIF POSTRELE  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
C
C  OPERATION REALISEE
C  ------------------
C     CONSTRUCTION DES VECTEURS DES MAILLES ACTIFS
C
C  REMARQUE
C  --------
C     DUPLICATION DE RVOUEX
C
C**********************************************************************
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
C
      INTEGER       ADR, ALISTE, ACNCIN, ALSMAC, ALSNAC, ADRVLC, AREPE
      INTEGER       NBTMA, NBM, NBMAC, NBNAC, IMOLO, JCELD, IGREL, IN
      INTEGER       I, N, M, LIBRE, IBID
      CHARACTER*4   DOCU
      CHARACTER*8   K8B
      CHARACTER*15  NCONEC
      CHARACTER*19  NCHP19
      CHARACTER*24  NREPE, NCNCIN
C**********************************************************************
C
      NCHP19 = NCHPT
      IF ( NCHP19(1:1) .EQ. '&' ) GOTO 9999
C
      CALL JEEXIN ( NCHP19//'.DESC', IBID )
      IF ( IBID .GT. 0 ) THEN
         CALL JELIRA ( NCHP19//'.DESC', 'DOCU', IBID, DOCU )
      ELSE
C          -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
         CALL CELVER ( NCHP19, 'NBSPT_1', 'STOP', IBID )
         CALL CELCEL ( 'NBVARI_CST', NCHP19, 'V', '&&PROUEX.CHAMEL' )
         NCHP19 = '&&PROUEX.CHAMEL'
         CALL JELIRA ( NCHP19//'.CELD', 'DOCU', IBID, DOCU )
      END IF
C
      IF ( DOCU .EQ. 'CHML' ) THEN
C         -------------------
C
         IF ( TYPCO .EQ. 'CHEMIN' ) THEN
C
            CALL RVFMAI ( COURBE, LSTMAC )
C
         ELSE
C
            CALL JEVEUO ( NCHP19//'.CELK', 'L', ADR )
            NREPE  = ZK24(ADR)(1:19)//'.REPE'
            CALL JEVEUO ( NREPE, 'L', AREPE )
C
            NCONEC = NOMA//'.CONNEX'
            CALL JELIRA ( NCONEC, 'NMAXOC', NBTMA, K8B )
C
            NCNCIN = '&&OP0051.CONNECINVERSE  '
            CALL JEEXIN ( NCNCIN, IBID )
            IF (IBID .EQ. 0) CALL CNCINV ( NOMA, IBID, 0, 'V', NCNCIN )
C
            CALL JELIRA ( LSTNAC, 'LONMAX', NBNAC, K8B )
            CALL JEVEUO ( LSTNAC, 'L', ALSNAC )
            CALL WKVECT ('&&PROUEX.LISTE.ENTIER','V V I',NBTMA,ALISTE)
            LIBRE = 1
            CALL JEVEUO ( JEXATR(NCNCIN,'LONCUM'), 'L', ADRVLC )
            CALL JEVEUO ( JEXNUM(NCNCIN,1)       , 'L', ACNCIN )
            DO 100, IN = 1, NBNAC, 1
               N   = ZI(ALSNAC + IN-1)
               NBM = ZI(ADRVLC + N+1-1) - ZI(ADRVLC + N-1)
               ADR = ZI(ADRVLC + N-1)
               CALL I2TRGI(ZI(ALISTE),ZI(ACNCIN + ADR-1),NBM,LIBRE)
 100        CONTINUE
            NBMAC = LIBRE - 1
            LIBRE = 1
            CALL JEVEUO ( NCHP19//'.CELD', 'L', JCELD )
            DO 110, I = 1, NBMAC, 1
               M = ZI(ALISTE + I-1)
               IF ( M .NE. 0 ) THEN
                  IGREL = ZI(AREPE + 2*(M-1))
                  IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGREL) +2)
                  IF ( IGREL.NE.0 .AND. IMOLO.GT.0) THEN
                     ZI(ALISTE + LIBRE-1) = ZI(ALISTE + I-1)
                     LIBRE = LIBRE + 1
                  ENDIF
               ENDIF
 110        CONTINUE
            NBMAC = LIBRE - 1
            IF ( NBMAC .GT. 0 ) THEN
               CALL WKVECT ( LSTMAC, 'V V I', NBMAC, ALSMAC )
               DO 120, I = 1, NBMAC, 1
                  ZI(ALSMAC + I-1) = ZI(ALISTE + I-1)
 120           CONTINUE
            ELSE
               CALL UTMESS('F','PROUEX',
     +           'ON N''A PAS TROUVE DE MAILLES, POST_RCCM IMPOSSIBLE')
            ENDIF
         ENDIF
         CALL JEDETR ( '&&PROUEX.LISTE.ENTIER' )
C
C
      ELSEIF ( DOCU .EQ. 'CHNO' ) THEN
C            -------------------
C
         IF ( TYPCO .EQ. 'CHEMIN' ) THEN
C
            CALL I2FNOE ( COURBE, LSTNAC )
C
         ENDIF
C
      ELSE
C
         CALL UTMESS('F','PROUEX','TYPE DE CHAMP INCONNU '//DOCU)
C
      ENDIF
C
 9999 CONTINUE
      END
