      SUBROUTINE CAELEC ( CHAR, LIGRMO, NOMA )
      IMPLICIT   NONE
      CHARACTER*8       CHAR, NOMA
      CHARACTER*(*)     LIGRMO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 19/10/2010   AUTEUR DELMAS J.DELMAS 
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
C BUT : STOCKAGE DES CHARGES REPARTIES DANS UNE CARTE ALLOUEE SUR LE
C       LIGREL DU MODELE
C
C ARGUMENTS D'ENTREE:
C      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
C      LIGRMO : NOM DU LIGREL DE MODELE
C      NOMA   : NOM DU MAILLAGE
C ----------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER       I, N, NBFEL, JVALV, JNCMP, IOCC,
     +              NBTOU, NBMA, JMA
      REAL*8        P1(3), P2(3), ZCOD, D
      CHARACTER*8   K8B, CODE, TYPMCL(2)
      CHARACTER*16  MOTCLF, MOTCLE(2)
      CHARACTER*19  CARTE
      CHARACTER*24  MESMAI
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MOTCLF = 'FORCE_ELEC'
      CALL GETFAC ( MOTCLF , NBFEL )
C
      CARTE = CHAR//'.CHME.'//'FELEC'
      CALL ALCART ( 'G', CARTE, NOMA, 'FELECR')
C
      CALL JEVEUO ( CARTE//'.NCMP', 'E', JNCMP )
      CALL JEVEUO ( CARTE//'.VALV', 'E', JVALV )
C
C --- STOCKAGE DE FORCES NULLES SUR TOUT LE MAILLAGE
C
      ZK8(JNCMP-1+1) = 'X1'
      ZK8(JNCMP-1+2) = 'Y1'
      ZK8(JNCMP-1+3) = 'Z1'
      ZK8(JNCMP-1+4) = 'X2'
      ZK8(JNCMP-1+5) = 'Y2'
      ZK8(JNCMP-1+6) = 'Z2'
      ZK8(JNCMP-1+7) = 'CODE'
      DO 100 I = 1, 7
         ZR(JVALV-1+I) = 0.D0
  100 CONTINUE
      CALL NOCART ( CARTE, 1, ' ', 'NOM', 0, ' ', 0, LIGRMO, 7 )
C
      MESMAI = '&&CAELEC.MES_MAILLES'
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
C
C --- STOCKAGE DANS LA CARTE
C
      DO 120 IOCC = 1, NBFEL
C
         CALL GETVTX ( MOTCLF, 'POSITION', IOCC,1,1, CODE, N)
C
         IF ( N .EQ. 0 ) THEN
            ZCOD = 10.D0
            CALL GETVR8 ( MOTCLF, 'FX', IOCC, 1, 1, P1(1), N)
            CALL GETVR8 ( MOTCLF, 'FY', IOCC, 1, 1, P1(2), N)
            CALL GETVR8 ( MOTCLF, 'FZ', IOCC, 1, 1, P1(3), N)
            P2(1) = 0.D0
            P2(2) = 0.D0
            P2(3) = 0.D0
         ELSE
            IF (CODE.EQ.'PARA') THEN
               CALL GETVR8 ( MOTCLF, 'DIST', IOCC, 1, 1, D, N)
               IF (N.NE.0) THEN
                  ZCOD = 12.D0
                  P1(1)=D
                  P1(2)=0.D0
                  P1(3)=0.D0
                  CALL GETVR8 ( MOTCLF, 'POINT2', IOCC, 1, 3, P2, N)
               ELSE
                  ZCOD = 11.D0
                  CALL GETVR8 ( MOTCLF, 'TRANS', IOCC, 1, 3, P1, N)
                  P2(1)=0.D0
                  P2(2)=0.D0
                  P2(3)=0.D0
               ENDIF
            ELSEIF (CODE.EQ.'INFI') THEN
               ZCOD = 2.D0
               CALL GETVR8 ( MOTCLF, 'POINT1', IOCC, 1, 3, P1, N)
               CALL GETVR8 ( MOTCLF, 'POINT2', IOCC, 1, 3, P2, N)
            ELSEIF (CODE.EQ.'FINI') THEN
               ZCOD = 3.D0
               CALL GETVR8 ( MOTCLF, 'POINT1', IOCC, 1, 3, P1, N)
               CALL GETVR8 ( MOTCLF, 'POINT2', IOCC, 1, 3, P2, N)
            ENDIF
         ENDIF
C
         ZR(JVALV-1+1) = P1(1)
         ZR(JVALV-1+2) = P1(2)
         ZR(JVALV-1+3) = P1(3)
         ZR(JVALV-1+4) = P2(1)
         ZR(JVALV-1+5) = P2(2)
         ZR(JVALV-1+6) = P2(3)
         ZR(JVALV-1+7) = ZCOD
C
         CALL GETVTX ( MOTCLF, 'TOUT', IOCC, 1, 1, K8B, NBTOU )
C
         IF ( NBTOU .NE. 0 ) THEN
C
            CALL NOCART ( CARTE, 1, ' ', 'NOM', 0, ' ', 0,LIGRMO, 7 )
         ELSE
            CALL RELIEM(LIGRMO, NOMA, 'NU_MAILLE', MOTCLF, IOCC, 2,
     +                                  MOTCLE, TYPMCL, MESMAI, NBMA )
            IF (NBMA.EQ.0) GOTO 120
            CALL JEVEUO ( MESMAI, 'L', JMA )
            CALL NOCART ( CARTE,3,K8B,'NUM',NBMA,K8B,ZI(JMA),' ',7)
            CALL JEDETR ( MESMAI )
         ENDIF
C
  120 CONTINUE
C
      CALL JEDEMA()
      END
