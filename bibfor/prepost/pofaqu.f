      SUBROUTINE POFAQU
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 08/03/2004   AUTEUR VABHHTS J.PELLET 
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
C     -----------------------------------------------------------------
C     COMMANDE POST_FATIGUE
C              CHARGEMENT QUELCONQUE
C     -----------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ------------------------------------------------------------------
      INTEGER       N1, N2, N3, N4, N5, N6, NBF, NBPTOT, NBPTS, I, J,
     +              IBID, IORDO, IFONC1, IFONC, NBPAPF, IDEFP,
     +              ITEMP, IVDOME
      REAL*8        RDOMM, VAL(2)
      COMPLEX*16    CBID
      CHARACTER*8   K8B, NOMTEN(6), NOMMAT, KDOMM, RESULT, NOMP, NOMT,
     +              TXCUM
      CHARACTER*16  NOMCMD
      CHARACTER*24  FVALE(6)
C     --- POST_FATI_QUELC ----------------------------------------------
      PARAMETER    ( NBPAPF = 3  )
      CHARACTER*1   TYPPPF(NBPAPF)
      CHARACTER*16  NOMPPF(NBPAPF)
      DATA  NOMPPF /  'INST' , 'DOMMAGE' , 'DOMM_CUMU' /
      DATA  TYPPPF / 'R' , 'R' , 'R' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETRES ( RESULT, K8B, NOMCMD )
C
C     --- RECUPERATION DE LA FONCTION CHARGEMENT ---
C
      CALL GETVID ( 'HISTOIRE', 'SIGM_XX',1,1,1, NOMTEN(1), N1 )
      CALL GETVID ( 'HISTOIRE', 'SIGM_YY',1,1,1, NOMTEN(2), N2 )
      CALL GETVID ( 'HISTOIRE', 'SIGM_ZZ',1,1,1, NOMTEN(3), N3 )
      CALL GETVID ( 'HISTOIRE', 'SIGM_XY',1,1,1, NOMTEN(4), N4 )
      CALL GETVID ( 'HISTOIRE', 'SIGM_XZ',1,1,1, NOMTEN(5), N5 )
      CALL GETVID ( 'HISTOIRE', 'SIGM_YZ',1,1,1, NOMTEN(6), N6 )
      NBF = N1 + N2 + N3 + N4 + N5 + N6
      CALL GETVID ( 'HISTOIRE', 'EPSP', 1,1,1, NOMP, N1 )
      CALL GETVID ( 'HISTOIRE', 'TEMP', 1,1,1, NOMT, N1 )
C
C     --- CHARGEMENT QUELCONQUE ---
C
      FVALE(1) = NOMTEN(1)//'           .VALE'
      CALL JELIRA ( FVALE(1), 'LONMAX', NBPTS, K8B )
      NBPTOT = NBPTS
      DO 20 I = 2 , NBF
         FVALE(I) = NOMTEN(I)//'           .VALE'
         CALL JELIRA ( FVALE(I), 'LONMAX', NBPTS, K8B )
         IF ( NBPTS .NE. NBPTOT ) CALL UTMESS('F',NOMCMD,
     +                 'L''HISTOIRE DE CHARGEMENT DOIT AVOIR MEME '//
     +                 'DISCRETISATION POUR TOUTES LES COMPOSANTES')
  20  CONTINUE
      CALL WKVECT ( '&&POFAQU.ORDO','V V R',NBPTOT/2*NBF, IORDO )
      CALL JEVEUO ( FVALE(1), 'L', IFONC1 )
      DO 30 I = 2 , NBF
         CALL JEVEUO ( FVALE(I), 'L', IFONC )
         DO 35 J = 1 , NBPTOT/2
            IF (ZR(IFONC+J-1).NE.ZR(IFONC1+J-1)) CALL UTMESS('F',NOMCMD,
     +                   'L''HISTOIRE DE CHARGEMENT DOIT AVOIR MEME '//
     +                   'DISCRETISATION POUR TOUTES LES COMPOSANTES')
            ZR(IORDO+(J-1)*NBF+I-1) = ZR(IFONC+NBPTOT/2+J-1)
  35     CONTINUE
  30  CONTINUE
      NBPTOT = NBPTOT / 2
      DO 40 J = 1 , NBPTOT
         ZR(IORDO+(J-1)*NBF) = ZR(IFONC1+NBPTOT+J-1)
  40  CONTINUE
C
      FVALE(1) = NOMP//'           .VALE'
      CALL JELIRA ( FVALE(1), 'LONMAX', NBPTS, K8B )
      IF ( NBPTS .NE. NBPTOT*2 ) CALL UTMESS('F',NOMCMD,
     +              'L''HISTOIRE DE LA DEFORMATION PLASTIQUE CUMULEE'//
     +              ' DOIT AVOIR MEME DISCRETISATION QUE L''HISTOIRE'//
     +              ' DES CONTRAINTES')
      CALL WKVECT ( '&&POFAQU.DEFPLA', 'V V R', NBPTOT, IDEFP )
      CALL JEVEUO ( FVALE(1), 'L', IFONC )
      DO 45 J = 0 , NBPTOT-1
         IF (ZR(IFONC+J).NE.ZR(IFONC1+J)) CALL UTMESS('F',NOMCMD,
     +                   'L''HISTOIRE DE LA DEFORMATION PLASTIQUE '//
     +                   'CUMULEE DOIT AVOIR MEME DISCRETISATION '//
     +                   'QUE L''HISTOIRE DES CONTRAINTES')
         ZR(IDEFP+J) = ZR(IFONC+NBPTOT+J)
  45  CONTINUE
C
      FVALE(1) = NOMT//'           .VALE'
      CALL JELIRA ( FVALE(1), 'LONMAX', NBPTS, K8B )
      IF ( NBPTS .NE. NBPTOT*2 ) CALL UTMESS('F',NOMCMD,
     +               'L''HISTOIRE DE LA TEMPERATURE DOIT AVOIR MEME '//
     +               'DISCRETISATION QUE L''HISTOIRE DES CONTRAINTES')
      CALL WKVECT ( '&&POFAQU.TEMP', 'V V R', NBPTOT, ITEMP )
      CALL JEVEUO ( FVALE(1), 'L', IFONC )
      DO 46 J = 0 , NBPTOT-1
         IF (ZR(IFONC+J).NE.ZR(IFONC1+J)) CALL UTMESS('F',NOMCMD,
     +                  'L''HISTOIRE DE LA TEMPERATURE DOIT AVOIR '//
     +           'MEME DISCRETISATION QUE L''HISTOIRE DES CONTRAINTES')
         ZR(ITEMP+J) = ZR(IFONC+NBPTOT+J)
  46  CONTINUE
C
C     --- CREATION DE LA TABLE ---
C
      CALL TBCRSD ( RESULT , 'G')
      CALL TBAJPA ( RESULT, NBPAPF, NOMPPF, TYPPPF )
C
      CALL GETVID ( ' ', 'MATER'  , 1,1,1, NOMMAT, N1 )
C
C     --- CALCUL DU DOMMAGE ELEMENTAIRE ---
C
      KDOMM = ' '
      CALL GETVTX ( ' ', 'DOMMAGE', 1,1,1, KDOMM, N1 )
C
      CALL WKVECT ('&&POFAQU.DOMM.ELEM', 'V V R', NBPTOT, IVDOME )
C
C     --- CALCUL DU DOMMAGE ELEMENTAIRE DE LEMAITRE GENERALISE
C         -----------------------------------------------------
      IF ( KDOMM .EQ. 'LEMAITRE' ) THEN
         CALL FGLEMA ( NBF, NBPTOT, ZR(IORDO), ZR(IDEFP), ZR(ITEMP),
     +                 NOMMAT, ZR(IVDOME) )
      ELSE
         CALL UTMESS('F',NOMCMD,'LOI DE DOMMAGE NON COMPATIBLE')
      ENDIF
C
      DO 50 I = 1 , NBPTOT
         VAL(1) = ZR(IFONC1+I-1)
         VAL(2) = ZR(IVDOME+I-1)
         CALL TBAJLI ( RESULT, 2, NOMPPF, IBID,VAL,CBID,K8B, 0 )
 50   CONTINUE
C
C     --- CALCUL DU DOMMAGE TOTAL ---
C
      TXCUM = ' '
      CALL GETVTX ( ' ', 'CUMUL', 1,1,1, TXCUM, N1 )
      IF ( TXCUM .EQ. 'LINEAIRE' ) THEN
C
         CALL FGDOMM ( NBPTOT, ZR(IVDOME), RDOMM )
C
         CALL TBAJLI ( RESULT, 1, NOMPPF(3),IBID,RDOMM,CBID,K8B,0)
C
      ENDIF
C
      CALL JEDETR ( '&&POFAQU.ORDO'      )
      CALL JEDETR ( '&&POFAQU.DEFPLA'    )
      CALL JEDETR ( '&&POFAQU.TEMP'      )
      CALL JEDETR ( '&&POFAQU.DOMM.ELEM' )
C
      CALL JEDEMA()
      END
