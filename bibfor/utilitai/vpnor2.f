      SUBROUTINE VPNOR2 ( NOMCON , NBMODE , NUMORD , COEF )
      IMPLICIT   NONE
      INTEGER             NBMODE , NUMORD(*)
      REAL*8              COEF(*)
      CHARACTER*(*)       NOMCON
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 31/08/1999   AUTEUR VABHHTS J.PELLET 
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
C     NORMALISATION DE TOUS LES CHAMPS D'UN MODE_MECA
C
C IN  NOMCON : NOM DU CONCEPT RESULTAT DE TYPE MODE_MECA
C IN  NBMODE : NOMBRE DE MODES
C IN  NUMORD : NUMERO D'ORDRE
C IN  COEF  : COEFFICIENT REEL A APLLIQUER AUX CHAMPS
C     ------------------------------------------------------------------
C
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
      CHARACTER*32   JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      INTEGER       IBID, NBNOSY, ISY, IM, IORDR, IRET, LVALE, NEQ, IEQ
      REAL*8        RCOEF
      CHARACTER*8   K8B, TYPMOD
      CHARACTER*16  NOMSYM
      CHARACTER*19  NOMD2
      CHARACTER*24  VALE
C     ------------------------------------------------------------------
      DATA  VALE  /'                   .VALE'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMD2 = NOMCON
C
      CALL JELIRA ( NOMD2//'.DESC', 'NOMMAX', NBNOSY, K8B )
      IF ( NBNOSY .EQ. 0 ) GOTO 9999
C
      DO 10 ISY = 1 , NBNOSY
         CALL JENUNO ( JEXNUM(NOMD2//'.DESC',ISY), NOMSYM )
         DO 12 IM = 1 , NBMODE
            IORDR = NUMORD(IM)
            CALL RSEXCH ( NOMCON, NOMSYM, IORDR, VALE(1:19), IRET )
            IF ( IRET .EQ. 0 ) THEN
               CALL JEEXIN ( VALE(1:19)//'.VALE',IBID)
               IF (IBID.GT.0) THEN
                 VALE=VALE(1:19)//'.VALE'
               ELSE
                 VALE=VALE(1:19)//'.CELV'
               END IF

               CALL JELIRA ( VALE, 'TYPE', IBID, TYPMOD )
               IF     ( NOMSYM(1:4) .EQ. 'EFGE' .OR.
     +                  NOMSYM(1:4) .EQ. 'SIGM' .OR.
     +                  NOMSYM(1:4) .EQ. 'EPSI' .OR.
     +                  NOMSYM(1:4) .EQ. 'SIEF' .OR.
     +                  NOMSYM(1:4) .EQ. 'FORC' .OR.
     +                  NOMSYM(1:4) .EQ. 'REAC' .OR.
     +                  NOMSYM(1:4) .EQ. 'DEGE' ) THEN
                  RCOEF = COEF(IM)
               ELSEIF ( NOMSYM(1:4) .EQ. 'EQUI' ) THEN
                  CALL UTMESS('A','VPNOR2',
     +                        'OPTION "'//NOMSYM//'" A RECALCULER')
                  GOTO 12
               ELSEIF ( NOMSYM(1:4) .EQ. 'EPOT' .OR.
     +                  NOMSYM(1:4) .EQ. 'ECIN' ) THEN
                  RCOEF = COEF(IM) * COEF(IM)
                  IF ( TYPMOD(1:1) .EQ. 'R' ) THEN
                     CALL PEENC2 ( VALE(1:19), RCOEF )
                  ELSE
                     CALL UTMESS('F','VPNOR2','CONTACTER L''ASSISTANCE')
                  ENDIF
                  GOTO 12
               ELSE
                  GOTO 12
               ENDIF
               CALL JEVEUO ( VALE, 'E', LVALE )
               CALL JELIRA ( VALE, 'LONMAX', NEQ, K8B )
               IF ( TYPMOD(1:1) .EQ. 'R' ) THEN
                  DO 20 IEQ = 0, NEQ-1
                     ZR(LVALE+IEQ) = ZR(LVALE+IEQ) * RCOEF
 20               CONTINUE
               ELSE
                  CALL UTMESS('F','VPNOR2','CONTACTER L''ASSISTANCE')
               ENDIF
            ENDIF
 12      CONTINUE
 10   CONTINUE
C
C
 9999 CONTINUE
      CALL JEDEMA()
      END
