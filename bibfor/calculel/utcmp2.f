      SUBROUTINE UTCMP2 ( NOMGD, MCFAC, IOCC, NOMCMP, NBNOCP,
     &                                        NUMCMP, NBNUCP )
      IMPLICIT   NONE
      INTEGER             IOCC, NBNOCP, NUMCMP(*), NBNUCP
      CHARACTER*(*)       NOMGD, MCFAC, NOMCMP(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      IBID, N2, JNOCP, I, J, II, NBCPT, JNUC2,
     &             JNOC2, IVAL, IRET, IANCMP, LGNCMP
      LOGICAL      MULT
      CHARACTER*8  K8B
      CHARACTER*24 VALK(2)
      CHARACTER*16 NOMCMD
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETRES ( K8B, K8B, NOMCMD )
C
      NBNUCP = 0
C
      NBNOCP = 0
      CALL GETVTX (MCFAC, 'NOM_CMP', IOCC,1,0, K8B, N2 )
      NBNOCP = -N2
      CALL WKVECT ('&&UTCMP2.NOM_CMP', 'V V K8',MAX(NBNOCP,1),JNOCP)
      CALL GETVTX (MCFAC,'NOM_CMP',IOCC,1,NBNOCP,ZK8(JNOCP),N2)

      IF ( NOMGD(1:6) .EQ. 'VARI_R' ) THEN
         MULT = .FALSE.
         NBCPT = NBNOCP
         CALL WKVECT ('&&UTCMP2.NUME_CMP2', 'V V I' , NBCPT, JNUC2 )
         CALL WKVECT ('&&UTCMP2.NOM_CMP2' , 'V V K8', NBCPT, JNOC2 )
         II = 0
         DO 10 I = 1 , NBNOCP
            IF ( ZK8(JNOCP+I-1)(1:5) .EQ. 'VARI_' ) THEN
               K8B = ZK8(JNOCP+I-1)(6:8)//'     '
            ELSEIF ( ZK8(JNOCP+I-1)(1:4) .EQ. 'VARI' ) THEN
               IF ( MULT ) GOTO 22
               IF ( NBNUCP .EQ. 0 )  IVAL = 1
               GOTO 12
            ELSEIF ( ZK8(JNOCP+I-1)(1:1) .EQ. 'V' ) THEN
               K8B = ZK8(JNOCP+I-1)(2:8)//' '
            ELSE
               VALK (1) = ZK8(JNOCP+I-1)
               VALK (2) = 'VARI_R'
               CALL U2MESG('F', 'CALCULEL6_50',2,VALK,0,0,0,0.D0)
            ENDIF
            CALL LXLIIS ( K8B, IVAL, IRET )
            IF ( IRET .NE. 0 ) THEN
               VALK (1) = ZK8(JNOCP+I-1)
               CALL U2MESG('F', 'CALCULEL6_51',1,VALK,0,0,0,0.D0)
            ENDIF
 12         CONTINUE
            K8B = 'V'//'       '
            CALL CODENT( IVAL , 'G' , K8B(2:8)  )
            II = II + 1
            ZK8(JNOC2+II-1) = K8B
            ZI (JNUC2+II-1) = IVAL
 10      CONTINUE
 22      CONTINUE
C
         NBNUCP = II
         NBNOCP = II
C
         DO 30 I = 1 , NBNOCP
            NOMCMP(I) = ZK8(JNOC2+I-1)
            NUMCMP(I) = ZI (JNUC2+I-1)
 30      CONTINUE
         CALL JEDETR ( '&&UTCMP2.NUME_CMP2' )
         CALL JEDETR ( '&&UTCMP2.NOM_CMP2'  )
      ELSE
C
         DO 40 I = 1 , NBNOCP
            NOMCMP(I) = ZK8(JNOCP+I-1)
 40      CONTINUE
C
         CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'L',IANCMP)
        CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',NOMGD),'LONMAX',LGNCMP,K8B)
         CALL KNINCL ( 8, NOMCMP, NBNOCP, ZK8(IANCMP), LGNCMP, IRET )
      ENDIF
C
      IF ( NBNOCP .GT. 50 )  CALL U2MESS('F','CALCULEL5_10')
C
      CALL JEDETR ( '&&UTCMP2.NUME_CMP' )
      CALL JEDETR ( '&&UTCMP2.NOM_CMP' )
C
      CALL JEDEMA()
      END
