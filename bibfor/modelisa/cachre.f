      SUBROUTINE CACHRE ( CHAR, LIGRMO, NOMA, NDIM, FONREE,
     &                    PARAM, MOTCL )
      IMPLICIT   NONE
      INTEGER           NDIM
      CHARACTER*4       FONREE
      CHARACTER*5       PARAM
      CHARACTER*8       CHAR, NOMA
      CHARACTER*(*)     LIGRMO, MOTCL
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
C      NDIM   : DIMENSION DU PROBLEME (2D OU 3D)
C      FONREE : FONC OU REEL
C      PARAM  : NOM DU TROISIEME "CHAMP" DE LA CARTE (F3D3D F2D3D ...)
C      MOTCL  : MOT-CLE FACTEUR
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
      INTEGER       IBID, I, N, NCHRE, NREP, NCMP, JVALV, JNCMP, IOCC,
     &              NFX, NFY, NFZ, NMX, NMY, NMZ, NPLAN,
     &              NBTOU, IER, NBMA, JMA
      REAL*8        FX, FY, FZ, MX, MY, MZ, VPRE
      COMPLEX*16    CFX, CFY, CFZ, CMX, CMY, CMZ, CVPRE
      CHARACTER*8   K8B, KFX, KFY, KFZ, KMX, KMY, KMZ, TYPCH, PLAN,
     &              TYPMCL(2)
      CHARACTER*16  MOTCLF, MOTCLE(2)
      CHARACTER*19  CARTE
      CHARACTER*24  MESMAI
      INTEGER       GETEXM,XTOUT
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MOTCLF = MOTCL
      CALL GETFAC ( MOTCLF , NCHRE )
C
      CARTE  = CHAR(1:8)//'.CHME.'//PARAM(1:5)
C
      IF (FONREE.EQ.'REEL') THEN
         CALL ALCART ('G', CARTE , NOMA , 'FORC_R')
      ELSE IF (FONREE.EQ.'FONC') THEN
         CALL ALCART ('G', CARTE , NOMA , 'FORC_F')
      ELSE IF (FONREE.EQ.'COMP') THEN
         CALL ALCART ('G', CARTE , NOMA , 'FORC_C')
      ELSE
         CALL U2MESK('F','MODELISA2_37',1,FONREE(1:4))
      ENDIF
C
      CALL JEVEUO ( CARTE//'.NCMP', 'E', JNCMP )
      CALL JEVEUO ( CARTE//'.VALV', 'E', JVALV )
C
C --- STOCKAGE DE FORCES NULLES SUR TOUT LE MAILLAGE
C     ET REPERE = 0.(SI 'REEL'),REPERE = 'GLOBAL' (SI FONC) ---
C
      ZK8(JNCMP-1+1) = 'FX'
      ZK8(JNCMP-1+2) = 'FY'
      ZK8(JNCMP-1+3) = 'FZ'
      ZK8(JNCMP-1+4) = 'MX'
      ZK8(JNCMP-1+5) = 'MY'
      ZK8(JNCMP-1+6) = 'MZ'
      ZK8(JNCMP-1+7) = 'REP'
      ZK8(JNCMP-1+8) = 'PLAN'
      IF (FONREE(1:4).EQ.'REEL') THEN
         DO 10 I = 1, 8
            ZR(JVALV-1+I) = 0.D0
 10      CONTINUE
      ELSEIF (FONREE(1:4).EQ.'COMP') THEN
         DO 12 I = 1, 8
            ZC(JVALV-1+I) = DCMPLX( 0.D0 , 0.D0 )
 12      CONTINUE
      ELSE IF (FONREE.EQ.'FONC') THEN
         DO 14 I = 1, 6
            ZK8(JVALV-1+I) = '&FOZERO'
 14      CONTINUE
         ZK8(JVALV-1+7) = 'GLOBAL'
         ZK8(JVALV-1+8) = '&FOZERO'
      ELSE
         CALL U2MESK('F','MODELISA2_37',1,FONREE)
      ENDIF
      CALL NOCART ( CARTE, 1,' ','NOM', 0,' ', 0, LIGRMO, 8 )
C
      MESMAI = '&&CACHRE.MES_MAILLES'
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
C
C --- STOCKAGE DANS LA CARTE ---
C
      DO 20 IOCC = 1, NCHRE
         NREP  = 0
         NCMP  = 0
         IF ( MOTCLF .EQ. 'FORCE_POUTRE' ) THEN
            CALL GETVTX ( MOTCLF, 'TYPE_CHARGE', IOCC,1,1, TYPCH, N )
            IF (TYPCH.EQ.'VENT') NREP = 2
         ENDIF
         IF (FONREE.EQ.'COMP') THEN
            CALL GETVC8 ( MOTCLF, 'FX', IOCC, 1, 1, CFX, NFX )
            CALL GETVC8 ( MOTCLF, 'FY', IOCC, 1, 1, CFY, NFY )
            CALL GETVC8 ( MOTCLF, 'FZ', IOCC, 1, 1, CFZ, NFZ )
            IF ( MOTCLF .NE. 'FORCE_INTERNE' .AND.
     &           MOTCLF .NE. 'FORCE_POUTRE'  .AND.
     &           MOTCLF .NE. 'FORCE_FACE'    ) THEN
              CALL GETVC8 ( MOTCLF, 'MX', IOCC, 1, 1, CMX, NMX )
              CALL GETVC8 ( MOTCLF, 'MY', IOCC, 1, 1, CMY, NMY )
              CALL GETVC8 ( MOTCLF, 'MZ', IOCC, 1, 1, CMZ, NMZ )
            ELSE
              NMX = 0
              NMY = 0
              NMZ = 0
           ENDIF
           IF ( NFX+NFY+NFZ+NMX+NMY+NMZ .EQ. 0 ) THEN
               IF ( MOTCLF .EQ. 'FORCE_POUTRE' ) THEN
                 NREP = 1
                 CALL GETVC8 ( MOTCLF, 'N'  , IOCC, 1, 1, CFX, NFX )
                 CALL GETVC8 ( MOTCLF, 'VY' , IOCC, 1, 1, CFY, NFY )
                 CALL GETVC8 ( MOTCLF, 'VZ' , IOCC, 1, 1, CFZ, NFZ )
C                CALL GETVC8 ( MOTCLF, 'MT' , IOCC, 1, 1, CMX, NMX )
C                CALL GETVC8 ( MOTCLF, 'MFY', IOCC, 1, 1, CMY, NMY )
C                CALL GETVC8 ( MOTCLF, 'MFZ', IOCC, 1, 1, CMZ, NMZ )
               ELSE IF ( MOTCLF .EQ. 'FORCE_COQUE' ) THEN
                 NREP = 1
                 CALL GETVC8 ( MOTCLF, 'PRES', IOCC, 1, 1,CVPRE, NFZ )
                 IF ( NFZ .EQ. 0 ) THEN
                   CALL GETVC8 ( MOTCLF, 'F1' , IOCC, 1, 1, CFX, NFX )
                   CALL GETVC8 ( MOTCLF, 'F2' , IOCC, 1, 1, CFY, NFY )
                   CALL GETVC8 ( MOTCLF, 'F3' , IOCC, 1, 1, CFZ, NFZ )
                   CALL GETVC8 ( MOTCLF, 'MF1', IOCC, 1, 1, CMX, NMX )
                   CALL GETVC8 ( MOTCLF, 'MF2', IOCC, 1, 1, CMY, NMY )
                   NMZ = 0
                 ELSE
                   CFZ  = -CVPRE
                   NFX = 0
                   NFY = 0
                   NMX = 0
                   NMY = 0
                   NMZ = 0
                 ENDIF
               ENDIF
            ENDIF
            IF (NFX .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'FX'
               ZC(JVALV-1+NCMP)  = CFX
            ENDIF
            IF (NFY .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'FY'
               ZC(JVALV-1+NCMP)  = CFY
            ENDIF
            IF (NFZ .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'FZ'
               ZC(JVALV-1+NCMP)  = CFZ
            ENDIF
            IF (NMX .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'MX'
               ZC(JVALV-1+NCMP)  = CMX
            ENDIF
            IF (NMY .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'MY'
               ZC(JVALV-1+NCMP)  = CMY
            ENDIF
            IF (NMZ .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'MZ'
               ZC(JVALV-1+NCMP)  = CMZ
            ENDIF
            IF (NREP .GE. 1) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'REP'
               IF (NREP.EQ.1) ZC(JVALV-1+NCMP)  = DCMPLX(1.D0,1.D0)
               IF (NREP.EQ.2) ZC(JVALV-1+NCMP)  = DCMPLX(2.D0,2.D0)
               IF (NREP.EQ.1) ZC(JVALV-1+NCMP)  = 1.D0
               IF (NREP.EQ.2) ZC(JVALV-1+NCMP)  = 2.D0
            ENDIF
         ELSEIF (FONREE.EQ.'REEL') THEN
            CALL GETVR8 ( MOTCLF, 'FX', IOCC, 1, 1, FX, NFX )
            CALL GETVR8 ( MOTCLF, 'FY', IOCC, 1, 1, FY, NFY )
            CALL GETVR8 ( MOTCLF, 'FZ', IOCC, 1, 1, FZ, NFZ )
            IF ( MOTCLF .NE. 'FORCE_INTERNE' .AND.
     &           MOTCLF .NE. 'FORCE_POUTRE'  .AND.
     &           MOTCLF .NE. 'FORCE_FACE'    ) THEN
              CALL GETVR8 ( MOTCLF, 'MX', IOCC, 1, 1, MX, NMX )
              CALL GETVR8 ( MOTCLF, 'MY', IOCC, 1, 1, MY, NMY )
              CALL GETVR8 ( MOTCLF, 'MZ', IOCC, 1, 1, MZ, NMZ )
            ELSE
              NMX = 0
              NMY = 0
              NMZ = 0
           ENDIF
           IF ( NFX+NFY+NFZ+NMX+NMY+NMZ .EQ. 0 ) THEN
               IF ( MOTCLF .EQ. 'FORCE_POUTRE' ) THEN
                  NREP = 1
                  CALL GETVR8 ( MOTCLF, 'N'  , IOCC, 1, 1, FX, NFX )
                  CALL GETVR8 ( MOTCLF, 'VY' , IOCC, 1, 1, FY, NFY )
                  CALL GETVR8 ( MOTCLF, 'VZ' , IOCC, 1, 1, FZ, NFZ )
C                 CALL GETVR8 ( MOTCLF, 'MT' , IOCC, 1, 1, MX, NMX )
C                 CALL GETVR8 ( MOTCLF, 'MFY', IOCC, 1, 1, MY, NMY )
C                 CALL GETVR8 ( MOTCLF, 'MFZ', IOCC, 1, 1, MZ, NMZ )
               ELSE IF ( MOTCLF .EQ. 'FORCE_COQUE' ) THEN
                  NREP = 1
                  CALL GETVR8 (MOTCLF,'PRES', IOCC, 1, 1,VPRE, NFZ)
                  IF ( NFZ .EQ. 0 ) THEN
                     CALL GETVR8 ( MOTCLF, 'F1' , IOCC, 1, 1, FX, NFX )
                     CALL GETVR8 ( MOTCLF, 'F2' , IOCC, 1, 1, FY, NFY )
                     CALL GETVR8 ( MOTCLF, 'F3' , IOCC, 1, 1, FZ, NFZ )
                     CALL GETVR8 ( MOTCLF, 'MF1', IOCC, 1, 1, MX, NMX )
                     CALL GETVR8 ( MOTCLF, 'MF2', IOCC, 1, 1, MY, NMY )
                     NMZ = 0
                  ELSE
                     FZ  = -VPRE
                     NFX = 0
                     NFY = 0
                     NMX = 0
                     NMY = 0
                     NMZ = 0
                 ENDIF
               ENDIF
            ENDIF
            IF (NFX .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'FX'
               ZR(JVALV-1+NCMP)  = FX
            ENDIF
            IF (NFY .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'FY'
               ZR(JVALV-1+NCMP)  = FY
            ENDIF
            IF (NFZ .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'FZ'
               ZR(JVALV-1+NCMP)  = FZ
            ENDIF
            IF (NMX .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'MX'
               ZR(JVALV-1+NCMP)  = MX
            ENDIF
            IF (NMY .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'MY'
               ZR(JVALV-1+NCMP)  = MY
            ENDIF
            IF (NMZ .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'MZ'
               ZR(JVALV-1+NCMP)  = MZ
            ENDIF
            IF (NREP .GE. 1) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'REP'
               IF (NREP.EQ.1) ZR(JVALV-1+NCMP)  = 1.D0
               IF (NREP.EQ.2) ZR(JVALV-1+NCMP)  = 2.D0
            ENDIF
         ELSE
            CALL GETVID ( MOTCLF, 'FX', IOCC,1,1, KFX, NFX )
            CALL GETVID ( MOTCLF, 'FY', IOCC,1,1, KFY, NFY )
            CALL GETVID ( MOTCLF, 'FZ', IOCC,1,1, KFZ, NFZ )
            IF ( MOTCLF .NE. 'FORCE_INTERNE' .AND.
     &           MOTCLF .NE. 'FORCE_POUTRE'  .AND.
     &           MOTCLF .NE. 'FORCE_FACE'    ) THEN
              CALL GETVID ( MOTCLF, 'MX', IOCC,1,1, KMX, NMX )
              CALL GETVID ( MOTCLF, 'MY', IOCC,1,1, KMY, NMY )
              CALL GETVID ( MOTCLF, 'MZ', IOCC,1,1, KMZ, NMZ )
            ELSE
              NMX = 0
              NMY = 0
              NMZ = 0
           ENDIF
           IF ( NFX+NFY+NFZ+NMX+NMY+NMZ .EQ. 0 ) THEN
               IF ( MOTCLF .EQ. 'FORCE_POUTRE' ) THEN
                  NREP = 1
                  CALL GETVID ( MOTCLF, 'N'  , IOCC,1,1, KFX, NFX )
                  CALL GETVID ( MOTCLF, 'VY' , IOCC,1,1, KFY, NFY )
                  CALL GETVID ( MOTCLF, 'VZ' , IOCC,1,1, KFZ, NFZ )
C                 CALL GETVID ( MOTCLF, 'MT' , IOCC,1,1, KMX, NMX )
C                 CALL GETVID ( MOTCLF, 'MFY', IOCC,1,1, KMY, NMY )
C                 CALL GETVID ( MOTCLF, 'MFZ', IOCC,1,1, KMZ, NMZ )
               ELSE IF ( MOTCLF .EQ. 'FORCE_COQUE' ) THEN
                  NREP = 1
                  CALL GETVID ( MOTCLF, 'PRES', IOCC,1,1, KFZ, NFZ )
                  IF ( NFZ .EQ. 0 ) THEN
                     CALL GETVID ( MOTCLF, 'F1' , IOCC,1,1, KFX, NFX )
                     CALL GETVID ( MOTCLF, 'F2' , IOCC,1,1, KFY, NFY )
                     CALL GETVID ( MOTCLF, 'F3' , IOCC,1,1, KFZ, NFZ )
                     CALL GETVID ( MOTCLF, 'MF1', IOCC,1,1, KMX, NMX )
                     CALL GETVID ( MOTCLF, 'MF2', IOCC,1,1, KMY, NMY )
                     NMZ = 0
                  ELSE
                     NFX = 0
                     NFY = 0
                     NMX = 0
                     NMY = 0
                     NMZ = 0
                     NREP = 3
                  ENDIF
               ENDIF
            ENDIF
            IF (NFX .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'FX'
               ZK8(JVALV-1+NCMP) = KFX
            ENDIF
            IF (NFY .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'FY'
               ZK8(JVALV-1+NCMP) = KFY
            ENDIF
            IF (NFZ .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'FZ'
               ZK8(JVALV-1+NCMP) = KFZ
            ENDIF
            IF (NMX .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'MX'
               ZK8(JVALV-1+NCMP) = KMX
            ENDIF
            IF (NMY .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'MY'
               ZK8(JVALV-1+NCMP) = KMY
            ENDIF
            IF (NMZ .NE. 0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'MZ'
               ZK8(JVALV-1+NCMP) = KMZ
            ENDIF
            IF (NREP .GE. 1) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'REP'
               IF (NREP.EQ.1) ZK8(JVALV-1+NCMP) = 'LOCAL'
               IF (NREP.EQ.2) ZK8(JVALV-1+NCMP) = 'VENT'
               IF (NREP.EQ.3) ZK8(JVALV-1+NCMP) = 'LOCAL_PR'
C --           (NREP=3) CAS D UNE PRESSION --> ON PREND L OPPOSE DE
C --           LA VALEUR LUE DANS LE TE
            ENDIF
         ENDIF
         IF (NCMP.EQ.0) GOTO 20
C
         IF ( MOTCLF .EQ. 'FORCE_COQUE' ) THEN
            CALL GETVTX ( MOTCLF, 'PLAN', IOCC,1,1, PLAN, NPLAN )
            IF (NPLAN.NE.0) THEN
               NCMP = NCMP + 1
               ZK8(JNCMP-1+NCMP) = 'PLAN'
               IF (FONREE.EQ.'REEL') THEN
                  IF (PLAN.EQ.'MAIL') THEN
                     ZR(JVALV-1+NCMP) = DBLE(0)
                  ELSEIF (PLAN.EQ.'INF') THEN
                     ZR(JVALV-1+NCMP) = DBLE(-1)
                  ELSEIF (PLAN.EQ.'SUP') THEN
                     ZR(JVALV-1+NCMP) = DBLE(1)
                  ELSEIF (PLAN.EQ.'MOY') THEN
                     ZR(JVALV-1+NCMP) = DBLE(2)
                  ENDIF
               ELSEIF (FONREE.EQ.'FONC') THEN
                  ZK8(JVALV-1+NCMP) = PLAN
               ENDIF
            ENDIF
         ENDIF
C
         XTOUT=GETEXM( MOTCLF, 'TOUT')
         NBTOU=0
         IF (XTOUT .EQ. 1) THEN
            CALL GETVTX ( MOTCLF, 'TOUT', IOCC, 1, 1, K8B, NBTOU )
         ENDIF
C
         IF ( NBTOU .NE. 0 ) THEN
C
            CALL NOCART (CARTE, 1, ' ', 'NOM', 0, ' ', 0,LIGRMO, NCMP )
         ELSE
            CALL RELIEM(LIGRMO, NOMA, 'NO_MAILLE', MOTCLF, IOCC, 2,
     &                                  MOTCLE, TYPMCL, MESMAI, NBMA )
            IF (NBMA.EQ.0) GOTO 20
            CALL JEVEUO ( MESMAI, 'L', JMA )
            CALL VETYMA ( NOMA, ZK8(JMA),NBMA, K8B,0, MOTCLF,NDIM,IER)
            CALL NOCART (CARTE,3,K8B,'NOM',NBMA,ZK8(JMA),IBID,' ',NCMP)
            CALL JEDETR ( MESMAI )
         ENDIF
C
 20   CONTINUE
C
      CALL TECART ( CARTE )
      CALL JEDEMA()
      END
