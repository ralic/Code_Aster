      SUBROUTINE RSCRMO ( IOCC, NOMSD , NOMJV )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER             IOCC
      CHARACTER*(*)       NOMSD , NOMJV
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
      INTEGER       IBID, NBNOSY, JNUME, I, IM, ISY, NBORDT
      INTEGER       JNOSY, IACELK, IRET, JMODL, NP, NC, N22
      INTEGER       NBMODL, NBMMOD
      REAL*8        PREC
      CHARACTER*8   K8B, DOCU, CRIT
      CHARACTER*16  NOMSYM
      CHARACTER*19  NOMD2, NOCH19
      CHARACTER*24  KNUM
      INTEGER      IARG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMD2 = NOMSD
C
      KNUM = '&&RSCRMO.NUME_ORDRE'
      CALL GETVR8 ( 'RESU', 'PRECISION', IOCC,IARG,1, PREC, NP )
      CALL GETVTX ( 'RESU', 'CRITERE'  , IOCC,IARG,1, CRIT, NC )
      CALL RSUTNU ( NOMSD, 'RESU', IOCC, KNUM, NBORDT, PREC,CRIT,IRET)
      IF ( IRET .NE. 0 ) GOTO 9999
      CALL JEVEUO ( KNUM, 'L', JNUME )
C
      CALL GETVTX ('RESU', 'NOM_CHAM' , IOCC,IARG,0, K8B, N22 )
      IF ( N22 .NE. 0 ) THEN
         NBNOSY = - N22
         CALL WKVECT('&&RSCRMO.NOM_SYMBOL','V V K16',NBNOSY,JNOSY)
         CALL GETVTX('RESU','NOM_CHAM',IOCC,IARG,NBNOSY,ZK16(JNOSY),N22)
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
            CALL RSEXCH(' ',NOMSD,NOMSYM,ZI(JNUME+I-1),NOCH19,IRET)
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
