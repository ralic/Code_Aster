      SUBROUTINE RSCRMO ( IOCC, NOMSD , NOMJV )
      IMPLICIT   NONE
      INTEGER             IOCC
      CHARACTER*(*)       NOMSD , NOMJV
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32   JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      INTEGER       IBID, NBNOSY, JNUME, INOMSY, I, IM, ISY, NBORDT
      INTEGER       JNOSY, IACELK, IRET, JMODL, NP, NC, N22
      INTEGER       NBMODL, NBMMOD
      REAL*8        PREC
      CHARACTER*8   K8B, DOCU, CRIT
      CHARACTER*16  NOMSYM
      CHARACTER*19  NOMD2, NOCH19
      CHARACTER*24  KNUM
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMD2 = NOMSD
C
      KNUM = '&&RSCRMO.NUME_ORDRE'
      CALL GETVR8 ( 'RESU', 'PRECISION', IOCC,1,1, PREC, NP )
      CALL GETVTX ( 'RESU', 'CRITERE'  , IOCC,1,1, CRIT, NC )
      CALL RSUTNU ( NOMSD, 'RESU', IOCC, KNUM, NBORDT, PREC,CRIT,IRET)
      IF ( IRET .NE. 0 ) GOTO 9999
      CALL JEVEUO ( KNUM, 'L', JNUME )
C
      CALL GETVTX ('RESU', 'NOM_CHAM' , IOCC,1,0, K8B, N22 )
      IF ( N22 .NE. 0 ) THEN
         NBNOSY = - N22
         CALL WKVECT('&&RSCRMO.NOM_SYMBOL','V V K16',NBNOSY,JNOSY)
         CALL GETVTX('RESU','NOM_CHAM',IOCC,1,NBNOSY,ZK16(JNOSY),N22)
      ELSE
         CALL JELIRA ( NOMD2//'.DESC', 'NOMMAX', NBNOSY, K8B )
         IF ( NBNOSY .EQ. 0 ) GOTO 9999
         CALL WKVECT('&&RSCRMO.NOM_SYMBOL','V V K16',NBNOSY,JNOSY)
         DO 10 ISY = 1 , NBNOSY
            CALL JENUNO(JEXNUM(NOMD2//'.DESC',ISY),ZK16(JNOSY-1+ISY))
 10      CONTINUE
      ENDIF
C
      CALL JEEXIN ( NOMJV , IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL WKVECT ( NOMJV, 'V V K24', 10, JMODL )
         NBMODL = 0
         NBMMOD = 10
         CALL JEECRA ( NOMJV , 'LONUTI' , NBMODL , ' ' )
      ELSE
         CALL JEVEUO ( NOMJV, 'E', JMODL )
         CALL JELIRA ( NOMJV, 'LONUTI', NBMODL, K8B )
         CALL JELIRA ( NOMJV, 'LONMAX', NBMMOD, K8B )
      ENDIF
C
      DO 20 I = 1 , NBORDT
         DO 24 ISY = 1 , NBNOSY
            NOMSYM = ZK16(JNOSY+ISY-1)
            CALL RSEXCH(NOMSD,NOMSYM,ZI(JNUME+I-1),NOCH19,IRET)
            IF ( IRET .EQ. 0 ) THEN
               CALL JEEXIN(NOCH19//'.DESC',IBID)
               IF (IBID.GT.0) THEN
                 CALL JELIRA(NOCH19//'.DESC','DOCU',IBID,DOCU)
               ELSE
                 CALL JELIRA(NOCH19//'.CELD','DOCU',IBID,DOCU)
               END IF

               IF ( DOCU(1:4) .EQ. 'CHML' ) THEN
                  CALL JEVEUO ( NOCH19//'.CELK', 'L', IACELK )
                  DO 26 IM = 1 , NBMODL
                     IF ( ZK24(JMODL+IM-1) .EQ. ZK24(IACELK) ) GOTO 28
 26               CONTINUE
                  NBMODL = NBMODL + 1
                  IF ( NBMODL .GT. NBMMOD ) THEN
                     NBMMOD = 2 * NBMMOD
                     CALL JUVECA ( NOMJV , NBMMOD )
                     CALL JEVEUO ( NOMJV, 'E', JMODL )
                  ENDIF
                  ZK24(JMODL+NBMODL-1) = ZK24(IACELK)
                  CALL JEECRA ( NOMJV , 'LONUTI' , NBMODL , ' ' )
 28               CONTINUE
                ENDIF
            ENDIF
 24      CONTINUE
 20   CONTINUE
C
      CALL JEDETR ( KNUM )
      CALL JEDETR ( '&&RSCRMO.NOM_SYMBOL' )
C
 9999 CONTINUE
      CALL JEDEMA()
      END
