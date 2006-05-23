      SUBROUTINE CAECHA ( CHAR, LIGRMO, NOMA, NDIM, FONREE )
      IMPLICIT   NONE
      INTEGER           NDIM
      CHARACTER*4       FONREE
      CHARACTER*8       CHAR, NOMA
      CHARACTER*(*)     LIGRMO
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 23/05/2006   AUTEUR CIBHHPD L.SALMONA 
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
C BUT : STOCKAGE DE COEF_H ET TEMP_EXT DANS UNE CARTE ALLOUEE SUR LE
C       LIGREL DU MODELE
C
C ARGUMENTS D'ENTREE:
C      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
C      LIGRMO : NOM DU LIGREL DE MODELE
C      NOMA   : NOM DU MAILLAGE
C      NDIM   : DIMENSION DU PROBLEME (2D OU 3D)
C      FONREE : FONC OU REEL
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER       NECHA, NCMP, JVALV1, JVALV2, JNCMP1, JNCMP2, N,
     +              NCMP1, NCMP2, IOCC, NBTOU, IER, NBMA, JMA
      REAL*8        R8B
      CHARACTER*8   K8B, TYPMCL(2)
      CHARACTER*16  MOTCLF, MOTCLE(2)
      CHARACTER*19  CARTE1, CARTE2
      CHARACTER*24  MESMAI
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      MOTCLF = 'ECHANGE'
      CALL GETFAC ( MOTCLF , NECHA )
C
      CARTE1 = CHAR//'.CHTH.COEFH'
      CARTE2 = CHAR//'.CHTH.T_EXT'
C
      IF (FONREE.EQ.'REEL') THEN
         CALL ALCART ( 'G', CARTE1, NOMA, 'COEH_R')
         CALL ALCART ( 'G', CARTE2, NOMA, 'TEMP_R')
      ELSE IF (FONREE.EQ.'FONC') THEN
         CALL ALCART ( 'G', CARTE1, NOMA, 'COEH_F')
         CALL ALCART ( 'G', CARTE2, NOMA, 'TEMP_F')
      ELSE
         CALL UTMESS('F','CAECHA','VALEUR INATTENDUE: '//FONREE )
      END IF
C
      CALL JEVEUO ( CARTE1//'.NCMP', 'E', JNCMP1 )
      CALL JEVEUO ( CARTE1//'.VALV', 'E', JVALV1 )
      CALL JEVEUO ( CARTE2//'.NCMP', 'E', JNCMP2 )
      CALL JEVEUO ( CARTE2//'.VALV', 'E', JVALV2 )
C
C --- STOCKAGE DE FLUX NULS SUR TOUT LE MAILLAGE
C
      NCMP = 3
      ZK8(JNCMP1-1+1) = 'H'
      ZK8(JNCMP1-1+2) = 'H_INF'
      ZK8(JNCMP1-1+3) = 'H_SUP'
      ZK8(JNCMP2-1+1) = 'TEMP'
      ZK8(JNCMP2-1+2) = 'TEMP_INF'
      ZK8(JNCMP2-1+3) = 'TEMP_SUP'
      IF (FONREE.EQ.'REEL') THEN
         ZR(JVALV1-1+1) = 0.D0
         ZR(JVALV1-1+2) = 0.D0
         ZR(JVALV1-1+3) = 0.D0
         ZR(JVALV2-1+1) = 0.D0
         ZR(JVALV2-1+2) = 0.D0
         ZR(JVALV2-1+3) = 0.D0
      ELSE
         ZK8(JVALV1-1+1) = '&FOZERO'
         ZK8(JVALV1-1+2) = '&FOZERO'
         ZK8(JVALV1-1+3) = '&FOZERO'
         ZK8(JVALV2-1+1) = '&FOZERO'
         ZK8(JVALV2-1+2) = '&FOZERO'
         ZK8(JVALV2-1+3) = '&FOZERO'
      END IF
      CALL NOCART ( CARTE1,1,' ','NOM',0,' ',0, LIGRMO, NCMP )
      CALL NOCART ( CARTE2,1,' ','NOM',0,' ',0, LIGRMO, NCMP )
C
      MESMAI = '&&CAECHA.MES_MAILLES'
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
C
C --- STOCKAGE DANS LA CARTE
C
      DO 120 IOCC = 1, NECHA
         NCMP1 = 0
         NCMP2 = 0
         IF (FONREE.EQ.'REEL') THEN
            CALL GETVR8 (MOTCLF,'COEF_H',IOCC,1,1,R8B,N)
            IF (N.EQ.1) THEN
                 NCMP1= NCMP1 + 1
                 ZK8(JNCMP1-1+NCMP1) = 'H'
                 ZR(JVALV1-1+NCMP1) = R8B
            END IF
            CALL GETVR8(MOTCLF,'COEF_H_INF',IOCC,1,1,R8B,N)
            IF (N.EQ.1) THEN
                 NCMP1 = NCMP1 + 1
                 ZK8(JNCMP1-1+NCMP1) = 'H_INF'
                 ZR(JVALV1-1+NCMP1) = R8B
            END IF
            CALL GETVR8(MOTCLF,'COEF_H_SUP',IOCC,1,1,R8B,N)
            IF (N.EQ.1) THEN
                 NCMP1 = NCMP1 + 1
                 ZK8(JNCMP1-1+NCMP1) = 'H_SUP'
                 ZR(JVALV1-1+NCMP1) = R8B
            END IF
            CALL GETVR8 (MOTCLF,'TEMP_EXT',IOCC,1,1,R8B,N)
            IF (N.EQ.1) THEN
                 NCMP2 = NCMP2 + 1
                 ZK8(JNCMP2-1+NCMP2) = 'TEMP'
                 ZR(JVALV2-1+NCMP2) = R8B
            END IF
            CALL GETVR8(MOTCLF,'TEMP_EXT_INF',IOCC,1,1,R8B,N)
            IF (N.EQ.1) THEN
                 NCMP2 = NCMP2 + 1
                 ZK8(JNCMP2-1+NCMP2) = 'TEMP_INF'
                 ZR(JVALV2-1+NCMP2) = R8B
            END IF
            CALL GETVR8(MOTCLF,'TEMP_EXT_SUP',IOCC,1,1,R8B,N)
            IF (N.EQ.1) THEN
                 NCMP2 = NCMP2 + 1
                 ZK8(JNCMP2-1+NCMP2) = 'TEMP_SUP'
                 ZR(JVALV2-1+NCMP2) = R8B
            END IF
         ELSE
           CALL GETVID(MOTCLF,'COEF_H',IOCC,1,1,K8B,N)
            IF (N.EQ.1) THEN
                 NCMP1 = NCMP1 + 1
                 ZK8(JNCMP1-1+NCMP1) = 'H'
                 ZK8(JVALV1-1+NCMP1) = K8B
            END IF
           CALL GETVID(MOTCLF,'COEF_H_INF',IOCC,1,1,K8B,N)
            IF (N.EQ.1) THEN
                 NCMP1 = NCMP1 + 1
                 ZK8(JNCMP1-1+NCMP1) = 'H_INF'
                 ZK8(JVALV1-1+NCMP1) = K8B
            END IF
           CALL GETVID(MOTCLF,'COEF_H_SUP',IOCC,1,1,K8B,N)
            IF (N.EQ.1) THEN
                 NCMP1 = NCMP1 + 1
                 ZK8(JNCMP1-1+NCMP1) = 'H_SUP'
                 ZK8(JVALV1-1+NCMP1) = K8B
            END IF
           CALL GETVID(MOTCLF,'TEMP_EXT',IOCC,1,1,K8B,N)
            IF (N.EQ.1) THEN
                 NCMP2 = NCMP2 + 1
                 ZK8(JNCMP2-1+NCMP2) = 'TEMP'
                 ZK8(JVALV2-1+NCMP2) = K8B
            END IF
           CALL GETVID(MOTCLF,'TEMP_EXT_INF',IOCC,1,1,K8B,N)
            IF (N.EQ.1) THEN
                 NCMP2 = NCMP2 + 1
                 ZK8(JNCMP2-1+NCMP2) = 'TEMP_INF'
                 ZK8(JVALV2-1+NCMP2) = K8B
            END IF
           CALL GETVID(MOTCLF,'TEMP_EXT_SUP',IOCC,1,1,K8B,N)
            IF (N.EQ.1) THEN
                 NCMP2 = NCMP2 + 1
                 ZK8(JNCMP2-1+NCMP2) = 'TEMP_SUP'
                 ZK8(JVALV2-1+NCMP2) = K8B
            END IF
         END IF
C
         CALL GETVTX ( MOTCLF, 'TOUT', IOCC, 1, 1, K8B, NBTOU )
         IF ( NBTOU .NE. 0 ) THEN
            CALL NOCART (CARTE1,1,' ','NOM',0,' ', 0,LIGRMO,NCMP1)
            CALL NOCART (CARTE2,1,' ','NOM',0,' ', 0,LIGRMO,NCMP2)
C
         ELSE
            CALL RELIEM(LIGRMO, NOMA, 'NO_MAILLE', MOTCLF, IOCC, 2,
     +                                  MOTCLE, TYPMCL, MESMAI, NBMA )
            CALL JEVEUO ( MESMAI, 'L', JMA )
            CALL VETYMA ( NOMA, ZK8(JMA),NBMA, K8B,0, MOTCLF,NDIM,IER)
            CALL NOCART(CARTE1,3,' ','NOM',NBMA,ZK8(JMA),0,LIGRMO,NCMP1)
            CALL NOCART(CARTE2,3,' ','NOM',NBMA,ZK8(JMA),0,LIGRMO,NCMP2)
            CALL JEDETR ( MESMAI )
         ENDIF
C
  120 CONTINUE
      CALL TECART(CARTE1)
      CALL TECART(CARTE2)
C
      CALL JEDEMA()
      END
