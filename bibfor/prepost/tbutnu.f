      SUBROUTINE TBUTNU ( MOTFAC, IOCC, NOMJV, NBINST, NOMTAB, 
     +                                                 PREC, CRIT )
      IMPLICIT   NONE
      INTEGER             IOCC, NBINST
      REAL*8              PREC
      CHARACTER*8         CRIT
      CHARACTER*16        MOTFAC
      CHARACTER*(*)       NOMJV, NOMTAB
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C
C     RECUPERER LES INSTANTS DANS UNE TABLE A PARTIR
C        DU MOT CLE  INST
C        DU MOT CLE  LIST_INST
C        PAR DEFAUT, TOUT_INST
C
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      IBID, NP, NC, N1, N2, JINSTD, JINST, JORDR, II,
     +             NBVAL, NBTROU, NUTROU
      REAL*8       DINST
      REAL*8 VALR
      COMPLEX*16   CBID
      CHARACTER*8  K8B
      CHARACTER*24 VALK
      CHARACTER*19 LISTR
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      CALL GETVR8 ( MOTFAC, 'PRECISION', IOCC,1,1, PREC, NP )
      CALL GETVTX ( MOTFAC, 'CRITERE'  , IOCC,1,1, CRIT, NC )
C
      NBINST = 0
      CALL GETVID ( MOTFAC, 'LIST_INST', IOCC,1,1, LISTR, N1 )
      IF ( N1 .NE. 0 ) THEN
         CALL JEVEUO ( LISTR//'.VALE', 'L', JINSTD )
         CALL JELIRA ( LISTR//'.VALE', 'LONMAX', NBINST, K8B )
         CALL WKVECT ( NOMJV, 'V V R', NBINST, JINST )
         CALL WKVECT ( '&&TBUTNU.ORDRE'   , 'V V I', NBINST, JORDR )
         DO 10 II = 1 , NBINST
            ZR(JINST+II-1) = ZR(JINSTD+II-1)
            ZI(JORDR+II-1) = II
 10      CONTINUE
      ENDIF
C
      CALL GETVR8 ( MOTFAC, 'INST', IOCC,1,0, DINST, N2 )
      IF ( N2 .NE. 0 ) THEN
         NBINST = -N2
         CALL WKVECT ( NOMJV, 'V V R', NBINST, JINST )
         CALL GETVR8 ( MOTFAC, 'INST', IOCC,1,NBINST, ZR(JINST), N2 )
         CALL WKVECT ( '&&TBUTNU.ORDRE'   , 'V V I', NBINST, JORDR )
         DO 12 II = 1 , NBINST
            ZI(JORDR+II-1) = II
 12      CONTINUE
      ENDIF
C
      CALL TBEXV1 ( NOMTAB, 'INST', '&&TBUTNU.INST_D', 'V', NBVAL, K8B )
      CALL JEVEUO ('&&TBUTNU.INST_D', 'L', JINSTD )
      DO 20 II = 1 , NBINST
         DINST = ZR(JINST+II-1)
         CALL RSINDI('R8  ',JINSTD,JORDR,IBID,DINST,K8B,CBID,PREC,CRIT,
     &               NBVAL,NBTROU,NUTROU,1)
         IF ( NBTROU .LT. 1 ) THEN
            VALR = DINST
            VALK = NOMTAB
            CALL U2MESG('F', 'PREPOST5_74',1,VALK,0,0,1,VALR)
         ELSEIF (NBTROU.GT.1) THEN
            VALR = DINST
            VALK = NOMTAB
            CALL U2MESG('F', 'PREPOST5_75',1,VALK,0,0,1,VALR)
         ENDIF
 20   CONTINUE
C
      IF ( NBINST .EQ. 0 ) THEN
         PREC = 1.D-06
         CRIT = 'RELATIF'
         NBINST = NBVAL
         CALL WKVECT ( NOMJV, 'V V R', NBINST, JINST )
         DO 30 II = 1 , NBINST
            ZR(JINST+II-1) = ZR(JINSTD+II-1)
 30      CONTINUE
      ELSE
         CALL JEDETR ( '&&TBUTNU.ORDRE' )
      ENDIF
      CALL JEDETR ( '&&TBUTNU.INST_D' )
C
      CALL JEDEMA ( )
      END
