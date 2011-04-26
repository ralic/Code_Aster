      SUBROUTINE RVCHN2 ( DEPLAZ, NOMJV, NBNO, NUMND, ORIG, AXEZ )
      IMPLICIT   NONE
      INTEGER             NBNO, NUMND(*)
      CHARACTER*(*)       DEPLAZ, NOMJV
      REAL*8              ORIG(3), AXEZ(3)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       IBID, IER, GD, NBEC, IEC, NEC, NCMPMX, ICOMPT, INO,
     &              ICMP, JPRNO, JNUEQ, IAD, TABEC(10), IAVALD, NUNOE,
     &              NUMDX, NUMDY, NUMDZ,NUMDRX, NUMDRY, NUMDRZ, NUDDL,
     &              I, AXYZM
      REAL*8        VALED(3), VALD(3), VALER(3), VALR(3), PSCAL
      REAL*8        XNORMR, EPSI, AXER(3), AXET(3), PGL(3,3)
      CHARACTER*8   K8B, NOMCMP, NOMAIL
      CHARACTER*19  PRNO, DEPLA
      LOGICAL       EXISDG
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      DEPLA = DEPLAZ
      EPSI = 1.0D-6
C
      CALL DISMOI('F','PROF_CHNO' , DEPLAZ, 'CHAM_NO',IBID,PRNO  ,IER)
      CALL DISMOI('F','NUM_GD'    , DEPLAZ, 'CHAM_NO',GD  ,K8B   ,IER)
      CALL DISMOI('F','NOM_GD'    , DEPLAZ, 'CHAM_NO',IBID,K8B   ,IER)
      CALL DISMOI('F','NOM_MAILLA', DEPLAZ, 'CHAM_NO',IBID,NOMAIL,IER)
      IF (K8B(1:6).NE.'DEPL_R') CALL U2MESS('F','POSTRELE_17')
      CALL JEVEUO ( NOMAIL//'.COORDO    .VALE', 'L', AXYZM )
C
      CALL JENONU(JEXNOM(PRNO//'.LILI','&MAILLA'),IBID)
      CALL JEVEUO(JEXNUM(PRNO//'.PRNO',IBID), 'L', JPRNO )
      CALL JEVEUO ( PRNO//'.NUEQ', 'L', JNUEQ )
C
      NEC = NBEC( GD )
      IF ( NEC .GT. 10 ) CALL U2MESS('F','POSTRELE_53')
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',IAD)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,K8B)
C
      CALL JEDUPO ( DEPLA//'.VALE', 'V', NOMJV, .FALSE. )
      CALL JEVEUO ( NOMJV, 'E', IAVALD )
C
      DO 30 INO = 1 , NBNO
C
         NUNOE = NUMND(INO)
C
         AXER(1) = ZR(AXYZM+3*(NUNOE-1)  ) - ORIG(1)
         AXER(2) = ZR(AXYZM+3*(NUNOE-1)+1) - ORIG(2)
         AXER(3) = ZR(AXYZM+3*(NUNOE-1)+2) - ORIG(3)
         PSCAL = AXER(1)*AXEZ(1)+AXER(2)*AXEZ(2)+AXER(3)*AXEZ(3)
         AXER(1) = AXER(1) - PSCAL*AXEZ(1)
         AXER(2) = AXER(2) - PSCAL*AXEZ(2)
         AXER(3) = AXER(3) - PSCAL*AXEZ(3)
         XNORMR = 0.0D0
         DO 36 I = 1,3
            XNORMR = XNORMR + AXER(I)*AXER(I)
 36      CONTINUE
         IF ( XNORMR .LT. EPSI ) THEN
            CALL JENUNO(JEXNUM(NOMAIL//'.NOMNOE',NUNOE),K8B)
            CALL U2MESK('F', 'POSTRELE_30',1,K8B)
         ENDIF
         XNORMR =  SQRT( XNORMR )
         DO 38 I = 1,3
            AXER(I) = AXER(I) / XNORMR
 38      CONTINUE
         AXET(1) = AXEZ(2)*AXER(3) - AXEZ(3)*AXER(2)
         AXET(2) = AXEZ(3)*AXER(1) - AXEZ(1)*AXER(3)
         AXET(3) = AXEZ(1)*AXER(2) - AXEZ(2)*AXER(1)
         DO 40 I = 1,3
            XNORMR = XNORMR + AXET(I)*AXET(I)
 40      CONTINUE
         XNORMR =  SQRT( XNORMR )
         IF ( XNORMR .LT. EPSI ) THEN
            CALL JENUNO(JEXNUM(NOMAIL//'.NOMNOE',NUNOE),K8B)
            CALL U2MESK('F', 'POSTRELE_31',1,K8B)
         ENDIF
         DO 34 I = 1,3
            PGL(1,I) = AXER(I)
            PGL(2,I) = AXEZ(I)
            PGL(3,I) = AXET(I)
 34      CONTINUE
C
         DO 10 IEC = 1 , NEC
            TABEC(IEC)= ZI(JPRNO-1+(NUNOE-1)*(NEC+2)+2+IEC )
 10      CONTINUE
         NUMDX = 0
         NUMDY = 0
         NUMDZ = 0
         NUMDRX = 0
         NUMDRY = 0
         NUMDRZ = 0
         VALED(1) = 0.0D0
         VALED(2) = 0.0D0
         VALED(3) = 0.0D0
         VALER(1) = 0.0D0
         VALER(2) = 0.0D0
         VALER(3) = 0.0D0
         ICOMPT = 0
         DO 20 ICMP = 1 , NCMPMX
            IF ( EXISDG(TABEC,ICMP) ) THEN
               ICOMPT = ICOMPT + 1
               NOMCMP = ZK8(IAD-1+ICMP)
               NUDDL = ZI(JNUEQ+ZI(JPRNO+(NEC+2)*(NUNOE-1))-1)+ICOMPT-1
               IF ( NOMCMP .EQ. 'DX' ) THEN
                  NUMDX = NUDDL
                  VALED(1) = ZR(IAVALD-1+NUMDX)
               ELSEIF ( NOMCMP .EQ. 'DY' ) THEN
                  NUMDY = NUDDL
                  VALED(2) = ZR(IAVALD-1+NUMDY)
               ELSEIF ( NOMCMP .EQ. 'DZ' ) THEN
                  NUMDZ = NUDDL
                  VALED(3) = ZR(IAVALD-1+NUMDZ)
               ELSEIF ( NOMCMP .EQ. 'DRX' ) THEN
                  NUMDRX = NUDDL
                  VALER(1) = ZR(IAVALD-1+NUMDRX)
               ELSEIF ( NOMCMP .EQ. 'DRY' ) THEN
                  NUMDRY = NUDDL
                  VALER(2) = ZR(IAVALD-1+NUMDRY)
               ELSEIF ( NOMCMP .EQ. 'DRZ' ) THEN
                  NUMDRZ = NUDDL
                  VALER(3) = ZR(IAVALD-1+NUMDRZ)
               ENDIF
            ENDIF
 20      CONTINUE
         IF ( (NUMDX+NUMDY+NUMDZ) .EQ. 0 ) GOTO 22
         CALL UTPVGL ( 1 , 3 , PGL, VALED , VALD )
         IF ( NUMDX.NE.0 ) ZR(IAVALD-1+NUMDX) = VALD(1)
         IF ( NUMDY.NE.0 ) ZR(IAVALD-1+NUMDY) = VALD(2)
         IF ( NUMDZ.NE.0 ) ZR(IAVALD-1+NUMDZ) = VALD(3)
 22      CONTINUE
         IF ( (NUMDRX+NUMDRY+NUMDRZ) .EQ. 0 ) GOTO 30
         CALL UTPVGL ( 1 , 3 , PGL, VALER , VALR )
         IF ( NUMDRX.NE.0 ) ZR(IAVALD-1+NUMDRX) = VALR(1)
         IF ( NUMDRY.NE.0 ) ZR(IAVALD-1+NUMDRY) = VALR(2)
         IF ( NUMDRZ.NE.0 ) ZR(IAVALD-1+NUMDRZ) = VALR(3)
 30   CONTINUE
C
      CALL JEDEMA()
      END
