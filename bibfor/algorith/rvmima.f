      SUBROUTINE RVMIMA ( NOMRES, IOCC )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER             IOCC
      CHARACTER*(*)       NOMRES
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     COMMANDE : POST_RELEVE, OPERATION='EXTREMA'
C
C ----------------------------------------------------------------------
C
      INTEGER      NBPANO, NBPAN2, NBPAEL, NBPAE2
      PARAMETER  ( NBPANO=8 , NBPAN2=6 , NBPAEL=9 , NBPAE2=7 )
      CHARACTER*16 NOPANO(NBPANO), NOPAEL(NBPAEL)
      CHARACTER*16 NOPAN2(NBPAN2), NOPAE2(NBPAE2)
C
      INTEGER      IBID, N1, NP, NC, IRET
      INTEGER      JORDR, I100, NBORDR, IORD, VALI(2), NBC
      INTEGER      ISPMAX, ISPMIN, ISAMAX, ISAMIN
      REAL*8       PREC, VALR(2), VALMAX, VALMIN, VAAMAX, VAAMIN
      COMPLEX*16   C16B
      CHARACTER*8   CRIT, RESU, MAMAX, NOMAX, MAMIN, NOMIN, TYCH
      CHARACTER*8  MAAMAX, NOAMAX, MAAMIN, NOAMIN
      CHARACTER*8  CPMAX, CPMIN, CPAMAX, CPAMIN
      CHARACTER*16 NOMCHA, VALK(9), INTITU
      CHARACTER*19 KNUM, CHAMP
      INTEGER      IARG
C
      DATA NOPANO / 'INTITULE', 'RESU', 'NOM_CHAM', 'NUME_ORDRE',
     &              'EXTREMA', 'NOEUD', 'CMP', 'VALE' /
      DATA NOPAN2 / 'INTITULE', 'CHAM_GD',
     &              'EXTREMA', 'NOEUD', 'CMP', 'VALE' /
      DATA NOPAEL / 'INTITULE', 'RESU', 'NOM_CHAM', 'NUME_ORDRE',
     &              'EXTREMA', 'MAILLE', 'NOEUD', 'CMP', 'VALE' /
      DATA NOPAE2 / 'INTITULE', 'CHAM_GD',
     &              'EXTREMA', 'MAILLE', 'NOEUD', 'CMP', 'VALE' /
C ---------------------------------------------------------------------

      CALL JEMARQ()
      KNUM = '&&RVMIMA.NUME_ORDRE'
      NBC = 0
C
      CALL GETVTX ( 'ACTION', 'INTITULE', IOCC,IARG,1, INTITU, N1 )
      VALK(1) = INTITU
C
C ----- TRAITEMENT DU CHAMP_GD  -----
C
      CALL GETVID ( 'ACTION', 'CHAM_GD' , IOCC,IARG,1, CHAMP, N1 )
      IF ( N1 .NE. 0 ) THEN
         VALK(2) = CHAMP
         CALL DISMOI('F','TYPE_CHAMP',CHAMP,'CHAMP',IBID,TYCH,IRET)
         IF (TYCH(1:4).EQ.'NOEU') THEN
            CALL PREXNO ( CHAMP,IOCC, NOMAX,CPMAX,VALMAX,
     &                    NOMIN,CPMIN,VALMIN, NOAMAX,CPAMAX,VAAMAX,
     &                                        NOAMIN,CPAMIN,VAAMIN )
            VALR(1) = VALMAX
            VALK(3) = 'MAX'
            VALK(4) = NOMAX
            VALK(5) = CPMAX
            CALL TBAJLI (NOMRES,NBPAN2,NOPAN2,VALI,VALR,C16B,VALK,0)
            VALR(1) = VALMIN
            VALK(3) = 'MIN'
            VALK(4) = NOMIN
            VALK(5) = CPMIN
            CALL TBAJLI (NOMRES,NBPAN2,NOPAN2,VALI,VALR,C16B,VALK,0)
            VALR(1) = VAAMAX
            VALK(3) = 'MAX_ABS'
            VALK(4) = NOAMAX
            VALK(5) = CPAMAX
            CALL TBAJLI (NOMRES,NBPAN2,NOPAN2,VALI,VALR,C16B,VALK,0)
            VALR(1) = VAAMIN
            VALK(3) = 'MIN_ABS'
            VALK(4) = NOAMIN
            VALK(5) = CPAMIN
            CALL TBAJLI (NOMRES,NBPAN2,NOPAN2,VALI,VALR,C16B,VALK,0)
         ELSEIF (TYCH(1:4).EQ.'ELNO') THEN
            CALL PREXEL ( CHAMP, IOCC,
     &                    MAMAX, NOMAX, ISPMAX, CPMAX, VALMAX,
     &                    MAMIN, NOMIN, ISPMIN, CPMIN, VALMIN,
     &                    MAAMAX, NOAMAX, ISAMAX, CPAMAX, VAAMAX,
     &                    MAAMIN, NOAMIN, ISAMIN, CPAMIN, VAAMIN )
C
            VALR(1) = VALMAX
            VALK(3) = 'MAX'
            VALK(4) = MAMAX
            VALK(5) = NOMAX
            VALK(6) = CPMAX
            CALL TBAJLI (NOMRES,NBPAE2,NOPAE2,VALI,VALR,C16B,VALK,0)
            VALR(1) = VALMIN
            VALK(3) = 'MIN'
            VALK(4) = MAMIN
            VALK(5) = NOMIN
            VALK(6) = CPMIN
            CALL TBAJLI (NOMRES,NBPAE2,NOPAE2,VALI,VALR,C16B,VALK,0)
            VALR(1) = VAAMAX
            VALK(3) = 'MAX_ABS'
            VALK(4) = MAAMAX
            VALK(5) = NOAMAX
            VALK(6) = CPAMAX
            CALL TBAJLI (NOMRES,NBPAE2,NOPAE2,VALI,VALR,C16B,VALK,0)
            VALR(1) = VAAMIN
            VALK(3) = 'MIN_ABS'
            VALK(4) = MAAMIN
            VALK(5) = NOAMIN
            VALK(6) = CPAMIN
            CALL TBAJLI (NOMRES,NBPAE2,NOPAE2,VALI,VALR,C16B,VALK,0)
C
         ELSE
            CALL U2MESK('F','ALGORITH10_56',1,TYCH)
         ENDIF
         GOTO 9999
      ENDIF
C
C ----- TRAITEMENT DU RESULTAT  -----
C
      CALL GETVID ( 'ACTION', 'RESULTAT', IOCC,IARG,1, RESU, N1 )
      VALK(2) = RESU
C
      CALL GETVR8 ( 'ACTION', 'PRECISION', IOCC,IARG,1, PREC, NP )
      CALL GETVTX ( 'ACTION', 'CRITERE'  , IOCC,IARG,1, CRIT, NC )
      CALL RSUTNU ( RESU,'ACTION',IOCC, KNUM, NBORDR, PREC,CRIT,IRET )
      IF (IRET.EQ.10) THEN
         CALL U2MESK('F','CALCULEL4_8',1,RESU)
      ENDIF
      IF (IRET.NE.0) THEN
         CALL U2MESS('F','ALGORITH3_41')
      ENDIF
      CALL JEVEUO ( KNUM, 'L', JORDR )
C
      CALL GETVTX ( 'ACTION', 'NOM_CHAM', IOCC,IARG,1,NOMCHA, NBC )
      VALK(3) = NOMCHA
C
      DO 100 I100 = 1 , NBORDR
         IORD = ZI(JORDR+I100-1)
         VALI(1) = IORD
C
         CALL RSEXCH(' ',RESU, NOMCHA, IORD, CHAMP, IRET)
         IF (IRET.NE.0) GOTO 100
         CALL DISMOI('F','TYPE_CHAMP',CHAMP,'CHAMP',IBID,TYCH,IRET)
C
         IF (TYCH(1:4).EQ.'NOEU') THEN
            CALL PREXNO ( CHAMP,IOCC, NOMAX,CPMAX,VALMAX,
     &                    NOMIN,CPMIN,VALMIN, NOAMAX,CPAMAX,VAAMAX,
     &                                        NOAMIN,CPAMIN,VAAMIN )
            VALR(1) = VALMAX
            VALK(4) = 'MAX'
            VALK(5) = NOMAX
            VALK(6) = CPMAX
            CALL TBAJLI (NOMRES,NBPANO,NOPANO,VALI,VALR,C16B,VALK,0)
            VALR(1) = VALMIN
            VALK(4) = 'MIN'
            VALK(5) = NOMIN
            VALK(6) = CPMIN
            CALL TBAJLI (NOMRES,NBPANO,NOPANO,VALI,VALR,C16B,VALK,0)
            VALR(1) = VAAMAX
            VALK(4) = 'MAX_ABS'
            VALK(5) = NOAMAX
            VALK(6) = CPAMAX
            CALL TBAJLI (NOMRES,NBPANO,NOPANO,VALI,VALR,C16B,VALK,0)
            VALR(1) = VAAMIN
            VALK(4) = 'MIN_ABS'
            VALK(5) = NOAMIN
            VALK(6) = CPAMIN
            CALL TBAJLI (NOMRES,NBPANO,NOPANO,VALI,VALR,C16B,VALK,0)
C
         ELSEIF (TYCH(1:4).EQ.'ELNO') THEN
            CALL PREXEL ( CHAMP, IOCC,
     &                    MAMAX, NOMAX, ISPMAX, CPMAX, VALMAX,
     &                    MAMIN, NOMIN, ISPMIN, CPMIN, VALMIN,
     &                    MAAMAX, NOAMAX, ISAMAX, CPAMAX, VAAMAX,
     &                    MAAMIN, NOAMIN, ISAMIN, CPAMIN, VAAMIN )
C
            VALR(1) = VALMAX
            VALK(4) = 'MAX'
            VALK(5) = MAMAX
            VALK(6) = NOMAX
            VALK(7) = CPMAX
            CALL TBAJLI (NOMRES,NBPAEL,NOPAEL,VALI,VALR,C16B,VALK,0)
            VALR(1) = VALMIN
            VALK(4) = 'MIN'
            VALK(5) = MAMIN
            VALK(6) = NOMIN
            VALK(7) = CPMIN
            CALL TBAJLI (NOMRES,NBPAEL,NOPAEL,VALI,VALR,C16B,VALK,0)
            VALR(1) = VAAMAX
            VALK(4) = 'MAX_ABS'
            VALK(5) = MAAMAX
            VALK(6) = NOAMAX
            VALK(7) = CPAMAX
            CALL TBAJLI (NOMRES,NBPAEL,NOPAEL,VALI,VALR,C16B,VALK,0)
            VALR(1) = VAAMIN
            VALK(4) = 'MIN_ABS'
            VALK(5) = MAAMIN
            VALK(6) = NOAMIN
            VALK(7) = CPAMIN
            CALL TBAJLI (NOMRES,NBPAEL,NOPAEL,VALI,VALR,C16B,VALK,0)
C
         ELSE
            CALL U2MESK('F','ALGORITH10_56',1,TYCH)
         ENDIF
C
 100  CONTINUE
C
      CALL JEDETR ( KNUM )
C
 9999 CONTINUE
C
      CALL JEDEMA( )
C
      END
