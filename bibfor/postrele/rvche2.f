      SUBROUTINE RVCHE2 ( CHELEZ, NOMJV, NBEL, NUMAIL, ORIG, AXEZ,
     &                    NBNAC, NNOEUD )
      IMPLICIT   NONE
      INTEGER             NBEL, NUMAIL(*), NBNAC, NNOEUD(*)
      CHARACTER*(*)       CHELEZ, NOMJV
      REAL*8              ORIG(3), AXEZ(3)
C ----------------------------------------------------------------------
C MODIF POSTRELE  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI ,DEBUGR
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
      CHARACTER*32     JEXNUM, JEXNOM, JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       JCELD, GD, IAD, NCMPMX, NEC, NBEC, TABEC(10), IBID,
     &              IAVALE, IACELK, IREPE, IM, IMAIL, IGREL, IELG, MODE,
     &              NSCAL, ICOEF, NSCA, NNOE, NCMPP, ICMP, NPCALC, IEL,
     &              NCOU, IACHML, ICOU, INO, ICMPT, NBGREL, IER, DIGDEL,
     &              NUMXX, NUMYY, NUMZZ, NUMXY, NUMXZ, NUMYZ, NUDDL, I,
     &              JLONGR, JLIGR, JPNT, IPOIN, IANOMA, NUNOE,AXYZM,
     &              JCNX,IMODEL,ILONG, IND, INDIIS
      REAL*8        SG(6), SL(6), PGL(3,3), PSCAL
      REAL*8 VALR
      REAL*8        XNORMR, EPSI, AXER(3), AXET(3), R8VIDE
      CHARACTER*8   K8B, NOMCMP, NOMMA, NONOEU, NOMAIL
      CHARACTER*24 VALK(2)
      CHARACTER*16  OPTION
      CHARACTER*19  CHELM, NOLIGR
      LOGICAL       EXISDG, INIVID
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      CHELM = CHELEZ
      EPSI = 1.0D-6
C

C     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :

      CALL JEVEUO ( CHELM//'.CELD', 'L', JCELD )
      GD = ZI(JCELD-1+1)
      CALL JEVEUO ( JEXNUM('&CATA.GD.NOMCMP',GD), 'L', IAD )
      CALL JELIRA ( JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,K8B)
      CALL JEVEUO ('&CATA.TE.MODELOC', 'L', IMODEL )
      CALL JEVEUO (JEXATR('&CATA.TE.MODELOC','LONCUM'),'L',ILONG)
C
      NEC  = NBEC( GD )
      IF ( NEC .GT. 10 ) CALL U2MESS('F','PREPOST_78')
C
      CALL DISMOI('F','NOM_OPTION', CHELEZ, 'CHAM_ELEM',IBID,OPTION,IER)
      IF  ( OPTION(1:14) .EQ. 'SIGM_ELNO_DEPL'  .OR.
     &      OPTION(1:14) .EQ. 'SIEF_ELNO_ELGA'  )THEN
C         COMPOSANTE:  SIXX SIYY SIZZ SIXY SIXZ SIYZ
      ELSEIF ( OPTION(1:14) .EQ. 'EPSI_ELNO_DEPL'  .OR.
     &         OPTION(1:14) .EQ. 'EPSG_ELNO_DEPL'  .OR.
     &         OPTION(1:14) .EQ. 'EPME_ELNO_DEPL'  .OR.
     &         OPTION(1:14) .EQ. 'EPMG_ELNO_DEPL'  )THEN
C         COMPOSANTE:  EPXX EPYY EPZZ EPXY EPXZ EPYZ
      ELSE IF ( OPTION(1:14) .EQ. 'EFGE_ELNO_DEPL' ) THEN
C         COMPOSANTE:  NXX NYY NXY MXX MYY MXY
      ELSE IF ( OPTION(1:14) .EQ. 'DEGE_ELNO_DEPL' ) THEN
C         COMPOSANTE:  N  VY VZ MT MFY MFZ
      ELSE
                  VALK (1) = CHELM
                  VALK (2) = OPTION
         CALL U2MESG('F', 'POSTRELE1_51',2,VALK,0,0,0,0.D0)
      ENDIF
C
      CALL JEDUPO ( CHELM//'.CELV', 'V', NOMJV, .FALSE. )
      CALL JEVEUO ( NOMJV, 'E', IAVALE )
C
      CALL JEVEUO ( CHELM//'.CELK', 'L', IACELK )
      NOLIGR = ZK24(IACELK)(1:19)
      CALL JEVEUO ( NOLIGR//'.REPE', 'L', IREPE )
      CALL JEVEUO ( NOLIGR//'.LIEL', 'L', JLIGR )
      CALL JEVEUO ( JEXATR(NOLIGR//'.LIEL','LONCUM'), 'L', JLONGR )
      CALL JELIRA ( NOLIGR//'.LIEL', 'NUTIOC', NBGREL, K8B )
      CALL JEVEUO ( NOLIGR//'.NOMA', 'L', IANOMA )
      NOMMA = ZK8(IANOMA)
      CALL JEVEUO ( JEXATR(NOMMA//'.CONNEX','LONCUM'), 'L', JPNT )
      CALL JEVEUO ( NOMMA//'.CONNEX', 'L', JCNX )
C
      CALL JEVEUO ( NOMMA//'.COORDO    .VALE', 'L', AXYZM )
C
      DO 20 IM = 1 , NBEL
         IMAIL = NUMAIL(IM)
         IGREL = ZI(IREPE-1+2*(IMAIL-1)+1)
         IELG  = ZI(IREPE-1+2*(IMAIL-1)+2)
         IF ( IGREL .EQ. 0 ) GOTO 20
         MODE=ZI(JCELD-1+ZI(JCELD-1+4+IGREL) +2)
         IF ( MODE .EQ. 0 ) GOTO 20
         CALL DGMODE ( MODE, IMODEL, ILONG, NEC, TABEC )
         NSCAL = DIGDEL( MODE )
         ICOEF=MAX(1,ZI(JCELD-1+4))
         IF (ICOEF.GT.1) CALL U2MESS('F','POSTRELE_47')
         NSCA  = NSCAL*ICOEF
         IPOIN = ZI(JLONGR-1+IGREL)
         IEL   = ZI(JLIGR-1+IPOIN+IELG-1)
         NNOE  = ZI(JPNT-1+IEL+1) - ZI(JPNT-1+IEL)
         NCMPP = 0
         DO 22 ICMP = 1, NCMPMX
            IF ( EXISDG( TABEC, ICMP ) ) THEN
               NCMPP = NCMPP + 1
            ENDIF
 22      CONTINUE
         NPCALC = NSCAL / NCMPP
         NCOU   = NPCALC / NNOE
         DEBUGR=ZI(JCELD-1+ZI(JCELD-1+4+IGREL)+8)
         IACHML =  DEBUGR + NSCA*(IELG-1)
         DO 30 ICOU = 1 , NCOU
            DO 32 INO = 1, NNOE
C
               NUNOE = ZI(JCNX-1+ZI(JPNT-1+IEL)-1+INO)
               INIVID = .FALSE.
               IF ( NBNAC .NE. 0 ) THEN
                  IND = INDIIS ( NNOEUD, NUNOE, 1, NBNAC )
                  IF ( IND.EQ. 0 ) THEN
                     INIVID = .TRUE.
                     DO 730 I = 1,6
                       SL(I) = R8VIDE()
 730                 CONTINUE
                     GOTO 732
                  ENDIF
               ENDIF
C
               AXER(1) = ZR(AXYZM+3*(NUNOE-1)  ) - ORIG(1)
               AXER(2) = ZR(AXYZM+3*(NUNOE-1)+1) - ORIG(2)
               AXER(3) = ZR(AXYZM+3*(NUNOE-1)+2) - ORIG(3)
               PSCAL = AXER(1)*AXEZ(1)+AXER(2)*AXEZ(2)+AXER(3)*AXEZ(3)
               AXER(1) = AXER(1) - PSCAL*AXEZ(1)
               AXER(2) = AXER(2) - PSCAL*AXEZ(2)
               AXER(3) = AXER(3) - PSCAL*AXEZ(3)
               XNORMR = 0.0D0
               DO 40 I = 1,3
                  XNORMR = XNORMR + AXER(I)*AXER(I)
 40            CONTINUE
               IF ( XNORMR .LT. EPSI ) THEN
                 CALL JENUNO(JEXNUM(NOMMA//'.NOMMAI',IMAIL),NOMAIL)
                 CALL JENUNO(JEXNUM(NOMMA//'.NOMNOE',NUNOE),NONOEU)
                  VALK (1) = NOMAIL
                  VALK (2) = NONOEU
                  VALR = ZR(AXYZM+3*(NUNOE-1))
                  CALL U2MESG('F', 'POSTRELE1_52',2,VALK,0,0,1,VALR)
               ENDIF
               XNORMR =  1.0D0 / SQRT( XNORMR )
               DO 42 I = 1,3
                  AXER(I) = AXER(I) * XNORMR
 42            CONTINUE
               AXET(1) = AXEZ(2)*AXER(3) - AXEZ(3)*AXER(2)
               AXET(2) = AXEZ(3)*AXER(1) - AXEZ(1)*AXER(3)
               AXET(3) = AXEZ(1)*AXER(2) - AXEZ(2)*AXER(1)
               DO 44 I = 1,3
                  XNORMR = XNORMR + AXET(I)*AXET(I)
 44            CONTINUE
               XNORMR =  SQRT( XNORMR )
               IF ( XNORMR .LT. EPSI ) THEN
                 CALL JENUNO(JEXNUM(NOMMA//'.NOMMAI',IMAIL),NOMAIL)
                 CALL JENUNO(JEXNUM(NOMMA//'.NOMNOE',NUNOE),NONOEU)
                  VALK (1) = NOMAIL
                  VALK (2) = NONOEU
                  VALR = ZR(AXYZM+3*(NUNOE-1))
                  CALL U2MESG('F', 'POSTRELE1_52',2,VALK,0,0,1,VALR)
               ENDIF
               DO 46 I = 1,3
                  PGL(1,I) = AXER(I)
                  PGL(2,I) = AXEZ(I)
                  PGL(3,I) = AXET(I)
 46            CONTINUE
C
               SG(1) = 0.0D0
               SG(2) = 0.0D0
               SG(3) = 0.0D0
               SG(4) = 0.0D0
               SG(5) = 0.0D0
               SG(6) = 0.0D0
C
 732           CONTINUE
               NUMXX = 0
               NUMYY = 0
               NUMZZ = 0
               NUMXY = 0
               NUMXZ = 0
               NUMYZ = 0
               NUDDL = IACHML-1+NCMPP*ICOEF*(INO-1)
     &                                     +(ICOU-1)*NCMPP*ICOEF*NNOE
C
               ICMPT = 0
               DO 34 ICMP = 1, NCMPMX
                  IF ( EXISDG( TABEC, ICMP ) ) THEN
                     ICMPT = ICMPT + 1
                     NOMCMP = ZK8(IAD-1+ICMP)
                     IF ( NOMCMP .EQ. 'SIXX'  .OR.
     &                    NOMCMP .EQ. 'EPXX'  .OR.
     &                    NOMCMP .EQ. 'NXX'   .OR.
     &                    NOMCMP .EQ. 'N'     ) THEN
                        NUMXX = NUDDL + ICMPT
                        SG(1) = ZR(IAVALE-1+NUMXX)
                     ELSEIF ( NOMCMP .EQ. 'SIYY'  .OR.
     &                        NOMCMP .EQ. 'EPYY'  .OR.
     &                        NOMCMP .EQ. 'NYY'   .OR.
     &                        NOMCMP .EQ. 'VY'    ) THEN
                        NUMYY = NUDDL + ICMPT
                        SG(3) = ZR(IAVALE-1+NUMYY)
                     ELSEIF ( NOMCMP .EQ. 'SIZZ'  .OR.
     &                        NOMCMP .EQ. 'EPZZ'  .OR.
     &                        NOMCMP .EQ. 'NXY'   .OR.
     &                        NOMCMP .EQ. 'VZ'    ) THEN
                        NUMZZ = NUDDL + ICMPT
                        SG(6) = ZR(IAVALE-1+NUMZZ)
                     ELSEIF ( NOMCMP .EQ. 'SIXY'  .OR.
     &                        NOMCMP .EQ. 'EPXY'  .OR.
     &                        NOMCMP .EQ. 'MXX'   .OR.
     &                        NOMCMP .EQ. 'MT'    ) THEN
                        NUMXY = NUDDL + ICMPT
                        SG(2) = ZR(IAVALE-1+NUMXY)
                     ELSEIF ( NOMCMP .EQ. 'SIXZ'  .OR.
     &                        NOMCMP .EQ. 'EPXZ'  .OR.
     &                        NOMCMP .EQ. 'MYY'   .OR.
     &                        NOMCMP .EQ. 'MFY'   ) THEN
                        NUMXZ = NUDDL + ICMPT
                        SG(4) = ZR(IAVALE-1+NUMXZ)
                     ELSEIF ( NOMCMP .EQ. 'SIYZ'  .OR.
     &                        NOMCMP .EQ. 'EPYZ'  .OR.
     &                        NOMCMP .EQ. 'MXY'   .OR.
     &                        NOMCMP .EQ. 'MFZ'   ) THEN
                        NUMYZ = NUDDL + ICMPT
                        SG(5) = ZR(IAVALE-1+NUMYZ)
                     ENDIF
                  ENDIF
  34           CONTINUE
C
               IF ( .NOT. INIVID ) THEN
                  CALL UTPSGL ( 1 , 3 , PGL , SG , SL )
               ENDIF
C
               IF ( NUMXX.NE.0 ) ZR(IAVALE-1+NUMXX) = SL(1)
               IF ( NUMYY.NE.0 ) ZR(IAVALE-1+NUMYY) = SL(3)
               IF ( NUMZZ.NE.0 ) ZR(IAVALE-1+NUMZZ) = SL(6)
               IF ( NUMXY.NE.0 ) ZR(IAVALE-1+NUMXY) = SL(2)
               IF ( NUMXZ.NE.0 ) ZR(IAVALE-1+NUMXZ) = SL(4)
               IF ( NUMYZ.NE.0 ) ZR(IAVALE-1+NUMYZ) = SL(5)
C
  32        CONTINUE
C
  30     CONTINUE
C
  20  CONTINUE
C
      CALL JEDEMA()
      END
