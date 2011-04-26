      SUBROUTINE CAFLUX ( CHAR, LIGRMO, NOMA, NDIM, FONREE )
      IMPLICIT NONE
      INTEGER             NDIM
      CHARACTER*4         FONREE
      CHARACTER*8         CHAR, NOMA
      CHARACTER*(*)       LIGRMO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C BUT : STOCKAGE DES FLUX DANS UNE (OU 2) CARTE ALLOUEE SUR LE
C       LIGREL DU MODELE
C
C ARGUMENTS D'ENTREE:
C      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
C      LIGRMO : NOM DU LIGREL DE MODELE
C      NBET   : NOMBRE TOTAL DE MAILLES
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
      INTEGER       IBID, NFLUX, JVALV1, JVALV2, JNCMP1, JNCMP2, IOCC,
     &              N, N1, N2, N3, N4, N5, N6, N7, N8, N11, N12, NGR,
     &              NBTOU, NBMA, JMA, NCMP, NCMP1, NCMP2, IRET, IER
      REAL*8        R8B, AIRE, XLONG
      COMPLEX*16    C16B
      LOGICAL       ICRE1, ICRE2
      CHARACTER*8   K8B, NOMTAB, MONGRM, TYPMCL(2)
      CHARACTER*16  MOTCLF, MOTCLE(2)
      CHARACTER*19  CART1, CART2
      CHARACTER*24  PARA, MESMAI
      CHARACTER*24 VALK(2)
C ----------------------------------------------------------------------
C
C     VERIFICATION DE L'EXCLUSION :   / FLUN FLUN_INF FLUN_SUP
C                                     / FLUX_X FLUX_Y FLUX_Z
C
C     AU PASSAGE, ON NOTE S'IL FAUT CREER 1 OU 2 CARTES :
C       CART1 : CARTE(FLUN)   (ICRE1 = .TRUE.)
C       CART2 : CARTE(FLUX)   (ICRE2 = .TRUE.)
C      LES 2  :               (ICRE1 = ICRE2 = .TRUE. )
C
      CALL JEMARQ()
      ICRE1 = .FALSE.
      ICRE2 = .FALSE.
      MOTCLF = 'FLUX_REP'
      CALL GETFAC ( MOTCLF, NFLUX )
C
      DO 1, IOCC = 1, NFLUX
         N5 = 0
         IF (FONREE.EQ.'REEL') THEN
            CALL GETVR8(MOTCLF,'FLUN'        ,IOCC,1,0,R8B,N11)
            CALL GETVR8(MOTCLF,'FLUN_INF'    ,IOCC,1,0,R8B,N2 )
            CALL GETVR8(MOTCLF,'FLUN_SUP'    ,IOCC,1,0,R8B,N3 )
            CALL GETVID(MOTCLF,'CARA_TORSION',IOCC,1,0,K8B,N12)
            N1 = N11 + N12
         ELSE IF (FONREE.EQ.'FONC') THEN
            CALL GETVID(MOTCLF,'FLUN'    ,IOCC,1,0,K8B,N1)
            CALL GETVID(MOTCLF,'FLUN_INF',IOCC,1,0,K8B,N2)
            CALL GETVID(MOTCLF,'FLUN_SUP',IOCC,1,0,K8B,N3)
            CALL GETVID(MOTCLF,'FLUX_X'  ,IOCC,1,0,K8B,N6)
            CALL GETVID(MOTCLF,'FLUX_Y'  ,IOCC,1,0,K8B,N7)
            CALL GETVID(MOTCLF,'FLUX_Z'  ,IOCC,1,0,K8B,N8)
            N5 = N6+N7+N8
         ELSE
            CALL U2MESK('F','MODELISA2_37',1,FONREE)
         END IF
         N4 = N1+N2+N3
         IF ((N5.NE.0).AND.(N4.NE.0))THEN
           IF (FONREE.EQ.'REEL') THEN
           ELSE IF (FONREE.EQ.'FONC') THEN
            CALL U2MESS('F','MODELISA2_64')
           ENDIF
         ENDIF
         IF (N4.NE.0) ICRE1 = .TRUE.
         IF (N5.NE.0) ICRE2 = .TRUE.
 1    CONTINUE
C
C     ALLOCATION EVENTUELLE DES CARTES CART1 ET CART2 :
C
      CART1= CHAR//'.CHTH.FLURE'
      CART2= CHAR//'.CHTH.FLUR2'
      IF (FONREE.EQ.'REEL') THEN
         IF (ICRE1) CALL ALCART('G',CART1,NOMA,'FLUN_R')
         IF (ICRE2) CALL ALCART('G',CART2,NOMA,'FLUX_R')
      ELSE IF (FONREE.EQ.'FONC') THEN
         IF (ICRE1) CALL ALCART('G',CART1,NOMA,'FLUN_F')
         IF (ICRE2) CALL ALCART('G',CART2,NOMA,'FLUX_F')
      ELSE
         CALL U2MESK('F','MODELISA2_37',1,FONREE)
      END IF
C
      IF (ICRE1) THEN
         CALL JEVEUO (CART1//'.NCMP', 'E', JNCMP1)
         CALL JEVEUO (CART1//'.VALV', 'E', JVALV1)
      END IF
      IF (ICRE2) THEN
         CALL JEVEUO (CART2//'.NCMP', 'E', JNCMP2)
         CALL JEVEUO (CART2//'.VALV', 'E', JVALV2)
      END IF
C
C      STOCKAGE DE FLUX NULS SUR TOUT LE MAILLAGE
C
       IF (ICRE1) THEN
          NCMP=3
          ZK8(JNCMP1-1+1) = 'FLUN'
          ZK8(JNCMP1-1+2) = 'FLUN_INF'
          ZK8(JNCMP1-1+3) = 'FLUN_SUP'
          IF (FONREE.EQ.'REEL') THEN
            ZR(JVALV1-1+1) = 0.D0
            ZR(JVALV1-1+2) = 0.D0
            ZR(JVALV1-1+3) = 0.D0
          ELSE
            ZK8(JVALV1-1+1) = '&FOZERO'
            ZK8(JVALV1-1+2) = '&FOZERO'
            ZK8(JVALV1-1+3) = '&FOZERO'
          END IF
         CALL NOCART (CART1, 1, ' ', 'NOM', 0, ' ', 0, LIGRMO,NCMP)
      END IF
C
       IF (ICRE2) THEN
          NCMP=3
          ZK8(JNCMP2-1+1) = 'FLUX'
          ZK8(JNCMP2-1+2) = 'FLUY'
          ZK8(JNCMP2-1+3) = 'FLUZ'
          IF (FONREE.EQ.'REEL') THEN
            ZR(JVALV2-1+1) = 0.D0
            ZR(JVALV2-1+2) = 0.D0
            ZR(JVALV2-1+3) = 0.D0
          ELSE
            ZK8(JVALV2-1+1) = '&FOZERO'
            ZK8(JVALV2-1+2) = '&FOZERO'
            ZK8(JVALV2-1+3) = '&FOZERO'
          END IF
         CALL NOCART (CART2, 1, ' ', 'NOM', 0, ' ', 0, LIGRMO,NCMP)
      END IF
C
      MESMAI = '&&CAFLUX.MES_MAILLES'
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
C
C     STOCKAGE DANS LES CARTES
C
      DO 120 IOCC = 1, NFLUX
         NCMP1 = 0
         NCMP2 = 0
C
         IF (FONREE.EQ.'REEL') THEN
C
            CALL GETVID ( MOTCLF, 'CARA_TORSION', IOCC,1,1, NOMTAB, N )
            IF (N.EQ.1) THEN
C              VERIFICATION DES PARAMETRES DE LA TABLE 'NOMTAB'
               CALL TBEXP2(NOMTAB,'AIRE')
               CALL TBEXP2(NOMTAB,'LONGUEUR')
               CALL TBEXP2(NOMTAB,'GROUP_MA')
C
               CALL GETVEM (NOMA,'GROUP_MA',MOTCLF,'GROUP_MA',
     &                                      IOCC,1,1,MONGRM,NGR)
               PARA = 'AIRE'
               CALL TBLIVA ( NOMTAB, 1, 'GROUP_MA', IBID, R8B, C16B,
     &                       MONGRM, K8B, R8B, PARA, K8B,
     &                       IBID, AIRE, C16B, K8B, IRET )
               IF ( IRET .EQ. 1 ) THEN
                 VALK (1) = PARA
                 VALK (2) = NOMTAB
                 CALL U2MESG('F', 'MODELISA8_34',2,VALK,0,0,0,0.D0)
               ELSEIF ( IRET .EQ. 2 ) THEN
                 VALK (1) = PARA
                 CALL U2MESG('F', 'MODELISA8_35',1,VALK,0,0,0,0.D0)
               ELSEIF ( IRET .EQ. 3 ) THEN
                 VALK (1) = MONGRM
                 CALL U2MESG('F', 'MODELISA8_36',1,VALK,0,0,0,0.D0)
               END IF
               PARA = 'LONGUEUR'
               CALL TBLIVA ( NOMTAB, 1, 'GROUP_MA', IBID, R8B, C16B,
     &                       MONGRM, K8B, R8B, PARA, K8B,
     &                       IBID, XLONG, C16B, K8B, IRET )
               IF ( IRET .EQ. 1 ) THEN
                 VALK (1) = PARA
                 VALK (2) = NOMTAB
                 CALL U2MESG('F', 'MODELISA8_34',2,VALK,0,0,0,0.D0)
               ELSEIF ( IRET .EQ. 2 ) THEN
                 VALK (1) = PARA
                 CALL U2MESG('F', 'MODELISA8_35',1,VALK,0,0,0,0.D0)
               ELSEIF ( IRET .EQ. 3 ) THEN
                 VALK (1) = MONGRM
                 CALL U2MESG('F', 'MODELISA8_36',1,VALK,0,0,0,0.D0)
               END IF
               NCMP1 = NCMP1 + 1
               ZK8(JNCMP1-1 + NCMP1) = 'FLUN'
               ZR(JVALV1-1 + NCMP1) = 2.0D0 * AIRE /  XLONG
            END IF
            CALL GETVR8 ( MOTCLF, 'FLUN', IOCC,1,1, R8B, N )
            IF (N.EQ.1) THEN
               NCMP1 = NCMP1 + 1
               ZK8(JNCMP1-1 + NCMP1) = 'FLUN'
               ZR(JVALV1-1 + NCMP1)  = R8B
            END IF
            CALL GETVR8 ( MOTCLF, 'FLUN_INF', IOCC,1,1, R8B, N )
            IF (N.EQ.1) THEN
               NCMP1 = NCMP1 + 1
               ZK8(JNCMP1-1 + NCMP1) = 'FLUN_INF'
               ZR(JVALV1-1 + NCMP1)  = R8B
            END IF
            CALL GETVR8 ( MOTCLF, 'FLUN_SUP', IOCC,1,1, R8B, N )
            IF (N.EQ.1) THEN
               NCMP1 = NCMP1 + 1
               ZK8(JNCMP1-1 + NCMP1) = 'FLUN_SUP'
               ZR(JVALV1-1 + NCMP1)  = R8B
            END IF
C
         ELSE
            CALL GETVID ( MOTCLF, 'FLUN', IOCC,1,1, K8B, N )
            IF (N.EQ.1) THEN
               NCMP1 = NCMP1 + 1
               ZK8(JNCMP1-1 + NCMP1) = 'FLUN'
               ZK8(JVALV1-1 + NCMP1) = K8B
            END IF
            CALL GETVID ( MOTCLF, 'FLUN_INF', IOCC,1,1, K8B, N )
            IF (N.EQ.1) THEN
               NCMP1 = NCMP1 + 1
               ZK8(JNCMP1-1 + NCMP1) = 'FLUN_INF'
               ZK8(JVALV1-1 + NCMP1) = K8B
            END IF
            CALL GETVID ( MOTCLF, 'FLUN_SUP', IOCC,1,1, K8B, N )
            IF (N.EQ.1) THEN
               NCMP1 = NCMP1 + 1
               ZK8(JNCMP1-1 + NCMP1) = 'FLUN_SUP'
               ZK8(JVALV1-1 + NCMP1) = K8B
            END IF
C
            CALL GETVID ( MOTCLF, 'FLUX_X', IOCC,1,1, K8B, N )
            IF (N.EQ.1) THEN
               NCMP2 = NCMP2 + 1
               ZK8(JNCMP2-1 + NCMP2) = 'FLUX'
               ZK8(JVALV2-1 + NCMP2) = K8B
            END IF
            CALL GETVID ( MOTCLF, 'FLUX_Y', IOCC,1,1, K8B, N )
            IF (N.EQ.1) THEN
               NCMP2 = NCMP2 + 1
               ZK8(JNCMP2-1 + NCMP2) = 'FLUY'
               ZK8(JVALV2-1 + NCMP2) = K8B
            END IF
            CALL GETVID ( MOTCLF, 'FLUX_Z', IOCC,1,1, K8B, N )
            IF (N.EQ.1) THEN
               NCMP2 = NCMP2 + 1
               ZK8(JNCMP2-1 + NCMP2) = 'FLUZ'
               ZK8(JVALV2-1 + NCMP2) = K8B
            END IF
         END IF
C
         CALL GETVTX ( MOTCLF, 'TOUT', IOCC, 1, 1, K8B, NBTOU )
C
         IF ( NBTOU .NE. 0 ) THEN
            IF (NCMP1.GT.0) THEN
               CALL NOCART (CART1,1,' ','NOM',0,' ', 0,LIGRMO,NCMP1)
            END IF
            IF (NCMP2.GT.0) THEN
               CALL NOCART (CART2,1,' ','NOM',0,' ', 0,LIGRMO,NCMP2)
            END IF
C
         ELSE
            CALL RELIEM(LIGRMO, NOMA, 'NO_MAILLE', MOTCLF, IOCC, 2,
     &                                  MOTCLE, TYPMCL, MESMAI, NBMA )
            IF (NBMA.EQ.0) GOTO 120
            CALL JEVEUO ( MESMAI, 'L', JMA )
            CALL VETYMA ( NOMA, ZK8(JMA),NBMA, K8B,0, MOTCLF,NDIM,IER)
            IF (NCMP1.GT.0) THEN
               CALL NOCART (CART1,3,' ','NOM',NBMA,ZK8(JMA),0,
     &                                                    LIGRMO,NCMP1)
            ENDIF
            IF (NCMP2.GT.0) THEN
               CALL NOCART (CART2,3,' ','NOM',NBMA,ZK8(JMA),0,
     &                                                    LIGRMO,NCMP2)
            ENDIF
            CALL JEDETR ( MESMAI )
         ENDIF
  120 CONTINUE
C
      IF (ICRE1) CALL TECART(CART1)
      IF (ICRE2) CALL TECART(CART2)
C
      CALL JEDEMA()
      END
