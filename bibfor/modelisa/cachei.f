      SUBROUTINE CACHEI ( CHAR, LIGRMO, NBCA, NBET, NOMA, FONREE,
     +                    PARAM, MOTCL )
      IMPLICIT   NONE
      INTEGER           NBCA, NBET
      CHARACTER*4       FONREE
      CHARACTER*5       PARAM
      CHARACTER*8       CHAR, NOMA
      CHARACTER*(*)     LIGRMO, MOTCL
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
C BUT : STOCKAGE DES CHARGES DE DEFORMATIONS INITIALES REPARTIES
C       DANS UNE CARTE ALLOUEE SUR LE LIGREL DU MODELE
C
C ARGUMENTS D'ENTREE:
C      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
C      LIGRMO : NOM DU LIGREL DE MODELE
C      NBCA   : NOMBRE D'APPEL A NOCART
C      NBET   : NOMBRE TOTAL DE MAILLES
C      NOMA   : NOM DU MAILLAGE
C      FONREE : FONC OU REEL
C      PARAM  : NOM DU TROISIEME CHAMP DE LA CARTE (EPSIN)
C      MOTCL  : MOT-CLE FACTEUR
C
C-----------------------------------------------------------------------
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
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       IBID, I, NCHEI, NCMP, JVALE, JVALV, JNCMP, IOCC,
     +              NXX, NYY, NZZ, NXY, NXZ, NYZ, NEX, NKY, NKZ, NEXX,
     +              NEYY, NEXY, NKXX, NKYY, NKXY,
     +              NBTOU, IER, NBMA, JMA
      REAL*8        EPXX, EPYY, EPZZ, EPXY, EPXZ, EPYZ, EPX, XKY, XKZ,
     +              XEXX, XEYY, XEXY, XKXX, XKYY, XKXY
      CHARACTER*8   K8B, KEPXX, KEPYY, KEPZZ, KEPXY, KEPXZ, KEPYZ,
     +              MOD, MODELI, TYPMCL(2)
      CHARACTER*16  MOTCLF, MOTCLE(2)
      CHARACTER*19  CARTE
      CHARACTER*24  MESMAI
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MOTCLF = MOTCL
      CALL GETFAC ( MOTCLF , NCHEI )
C
      CARTE  = CHAR//'.CHME.'//PARAM
C
C --- MODELE ASSOCIE AU LIGREL DE CHARGE
C
      CALL DISMOI('F','NOM_MODELE',CHAR(1:8),'CHARGE',IBID,MOD,IER)
C
C --- MODELISATION DU MODELE
C
      CALL DISMOI('F','MODELISATION',MOD,'MODELE',IBID,MODELI,IER)
C
      IF (FONREE.EQ.'REEL') THEN
         CALL ALCART ( 'G', CARTE , NOMA , 'EPSI_R', NBCA+1, NBET )
      ELSE IF (FONREE.EQ.'FONC') THEN
         CALL ALCART ( 'G', CARTE , NOMA , 'EPSI_F', NBCA+1, NBET )
      ELSE
         CALL UTMESS('F','CACHEI','VALEUR INATTENDUE: '//FONREE )
      END IF
C
      CALL JEVEUO ( CARTE//'.NCMP', 'E', JNCMP )
      CALL JEVEUO ( CARTE//'.VALV', 'E', JVALV )
      CALL JEVEUO ( CARTE//'.VALE', 'E', JVALE )
C
      NCMP = 6
      IF (FONREE.EQ.'REEL') NCMP = 15
C
      ZK8(JNCMP-1+1) = 'EPXX'
      ZK8(JNCMP-1+2) = 'EPYY'
      ZK8(JNCMP-1+3) = 'EPZZ'
      ZK8(JNCMP-1+4) = 'EPXY'
      ZK8(JNCMP-1+5) = 'EPXZ'
      ZK8(JNCMP-1+6) = 'EPYZ'
      IF (FONREE.EQ.'REEL') THEN
          ZK8(JNCMP-1+7)   = 'EPX'
          ZK8(JNCMP-1+8)   = 'KY'
          ZK8(JNCMP-1+9)   = 'KZ'
          ZK8(JNCMP-1+10)  = 'EXX'
          ZK8(JNCMP-1+11)  = 'EYY'
          ZK8(JNCMP-1+12)  = 'EXY'
          ZK8(JNCMP-1+13)  = 'KXX'
          ZK8(JNCMP-1+14)  = 'KYY'
          ZK8(JNCMP-1+15)  = 'KXY'
      ENDIF
      IF (FONREE.EQ.'REEL') THEN
         DO 10 I = 1, NCMP
            ZR(JVALV-1+I) = 0.D0
 10      CONTINUE
      ELSE
         DO 12 I = 1, NCMP
            ZK8(JVALV-1+I) = '&FOZERO'
 12      CONTINUE
      END IF
      CALL NOCART (CARTE, 1, ' ', 'NOM', 0, ' ', 0, LIGRMO, NCMP)
C
      MESMAI = '&&CACHEI.MES_MAILLES'
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
C
      DO 20 IOCC = 1, NCHEI
C
         IF (FONREE.EQ.'REEL') THEN
            CALL GETVR8 ( MOTCLF, 'EPXX', IOCC, 1, 1, EPXX, NXX )
            CALL GETVR8 ( MOTCLF, 'EPYY', IOCC, 1, 1, EPYY, NYY )
            CALL GETVR8 ( MOTCLF, 'EPZZ', IOCC, 1, 1, EPZZ, NZZ )
            CALL GETVR8 ( MOTCLF, 'EPXY', IOCC, 1, 1, EPXY, NXY )
            CALL GETVR8 ( MOTCLF, 'EPXZ', IOCC, 1, 1, EPXZ, NXZ )
            CALL GETVR8 ( MOTCLF, 'EPYZ', IOCC, 1, 1, EPYZ, NYZ )
            CALL GETVR8 ( MOTCLF, 'EPX',  IOCC, 1, 1, EPX,  NEX )
            CALL GETVR8 ( MOTCLF, 'KY',   IOCC, 1, 1, XKY,  NKY )
            CALL GETVR8 ( MOTCLF, 'KZ',   IOCC, 1, 1, XKZ,  NKZ )
            CALL GETVR8 ( MOTCLF, 'EXX',  IOCC, 1, 1, XEXX, NEXX)
            CALL GETVR8 ( MOTCLF, 'EYY',  IOCC, 1, 1, XEYY, NEYY)
            CALL GETVR8 ( MOTCLF, 'EXY',  IOCC, 1, 1, XEXY, NEXY)
            CALL GETVR8 ( MOTCLF, 'KXX',  IOCC, 1, 1, XKXX, NKXX)
            CALL GETVR8 ( MOTCLF, 'KYY',  IOCC, 1, 1, XKYY, NKYY)
            CALL GETVR8 ( MOTCLF, 'KXY',  IOCC, 1, 1, XKXY, NKXY)
C
            DO 22 I = 1, NCMP
               ZR(JVALV-1+I) = 0.D0
 22         CONTINUE
C
            IF (NXX .NE. 0) ZR(JVALV-1+1) = EPXX
            IF (NYY .NE. 0) ZR(JVALV-1+2) = EPYY
            IF (NZZ .NE. 0) ZR(JVALV-1+3) = EPZZ
            IF (NXY .NE. 0) ZR(JVALV-1+4) = EPXY
            IF (NXZ .NE. 0) ZR(JVALV-1+5) = EPXZ
            IF (NYZ .NE. 0) ZR(JVALV-1+6) = EPYZ
C
            IF (NEX .NE. 0) ZR(JVALV-1+7) = EPX
            IF (NKY .NE. 0) ZR(JVALV-1+8) = XKY
            IF (NKZ .NE. 0) ZR(JVALV-1+9) = XKZ
            IF (NEXX .NE. 0) ZR(JVALV-1+10) = XEXX
            IF (NEYY .NE. 0) ZR(JVALV-1+11) = XEYY
            IF (NEXY .NE. 0) ZR(JVALV-1+12) = XEXY
            IF (NKXX .NE. 0) ZR(JVALV-1+13) = XKXX
            IF (NKYY .NE. 0) ZR(JVALV-1+14) = XKYY
            IF (NKXY .NE. 0) ZR(JVALV-1+15) = XKXY
            IF ((NKY.NE.0.OR.NKZ.NE.0).AND.(MODELI.EQ.'POU_C_T')) THEN
                 CALL UTMESS('F','CACHEI','LES COURBURES KY ET KZ '//
     +         'NE SONT PAS PRISES EN COMPTE POUR LES POUTRES COURBES')
            ENDIF
         ELSE
            CALL GETVID ( MOTCLF, 'EPXX', IOCC,1,1, KEPXX, NXX )
            CALL GETVID ( MOTCLF, 'EPYY', IOCC,1,1, KEPYY, NYY )
            CALL GETVID ( MOTCLF, 'EPZZ', IOCC,1,1, KEPZZ, NZZ )
            CALL GETVID ( MOTCLF, 'EPXY', IOCC,1,1, KEPXY, NXY )
            CALL GETVID ( MOTCLF, 'EPXZ', IOCC,1,1, KEPXZ, NXZ )
            CALL GETVID ( MOTCLF, 'EPYZ', IOCC,1,1, KEPYZ, NYZ )
            DO 111 I = 1, NCMP
               ZK8(JVALV-1+I) = '&FOZERO'
  111       CONTINUE
            IF (NXX .NE. 0) ZK8(JVALV-1+1) = KEPXX
            IF (NYY .NE. 0) ZK8(JVALV-1+2) = KEPYY
            IF (NZZ .NE. 0) ZK8(JVALV-1+3) = KEPZZ
            IF (NXY .NE. 0) ZK8(JVALV-1+4) = KEPXY
            IF (NXZ .NE. 0) ZK8(JVALV-1+5) = KEPXZ
            IF (NYZ .NE. 0) ZK8(JVALV-1+6) = KEPYZ
         ENDIF
C
         CALL GETVTX ( MOTCLF, 'TOUT', IOCC, 1, 1, K8B, NBTOU )
C
         IF ( NBTOU .NE. 0 ) THEN
C
            CALL NOCART(CARTE, 1, ' ', 'NOM', 0, ' ', 0,LIGRMO, NCMP)
         ELSE
            CALL RELIEM(LIGRMO, NOMA, 'NU_MAILLE', MOTCLF, IOCC, 2,
     +                                  MOTCLE, TYPMCL, MESMAI, NBMA )
            CALL JEVEUO ( MESMAI, 'L', JMA )
            CALL NOCART( CARTE,3,K8B,'NUM',NBMA,K8B,ZI(JMA),' ',NCMP)
            CALL JEDETR ( MESMAI )
         ENDIF
 20   CONTINUE
C
      CALL JEDEMA()
      END
