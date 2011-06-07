      SUBROUTINE OP0107()
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 06/06/2011   AUTEUR TARDIEU N.TARDIEU 
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
C     OPERATEUR   POST_ELEM
C     ------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      NH, IRET, JCHA, JORDR, N1, N2, NBOCC,
     &             NBORDR, NC, NCHAR, NP, NR, JPARA, IER
      REAL*8       PREC
      CHARACTER*4  CTYP
      CHARACTER*8  K8B, MODELE, CARA, DEFORM, RESUCO, CRIT
      CHARACTER*16 CONCEP, NOMCMD
      CHARACTER*19 RESU, KCHA, KNUM, TABTYP(3)
      CHARACTER*24 MATE, CHDEF
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      CALL GETRES ( RESU, CONCEP, NOMCMD )
C
      CALL GETVID ( ' ', 'RESULTAT' , 0,1,1, RESUCO, NR )
C
      IF(NR.EQ.0) RESUCO='        '
C
      CALL INFMAJ

      KCHA = '&&OP0107.CHARGES'

      CALL GETFAC ( 'TRAV_EXT' , NBOCC )
C                   ----------
      IF ( NBOCC .NE. 0 ) THEN

         CALL PEWEXT ( RESU )

      ENDIF


      CALL GETFAC ( 'CHAR_LIMITE' , NBOCC )
C                   -------------
      IF ( NBOCC .NE. 0 ) THEN
        CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)

         CALL PECHLI ( RESU, MODELE, MATE )

      ENDIF


      CALL GETFAC ( 'AIRE_INTERNE' , NBOCC )
C                   --------------
      IF ( NBOCC .NE. 0 ) THEN
         CALL GETVID ( ' ', 'MODELE', 1,1,1, MODELE, N1 )
         IF(N1.EQ.0)THEN
               CALL GETVR8 ( ' ', 'PRECISION', 1,1,1, PREC  , NP )
               CALL GETVTX ( ' ', 'CRITERE'  , 1,1,1, CRIT  , NC )
               CALL RSUTNU ( RESUCO,' ',0,KNUM,NBORDR,PREC,CRIT,IRET)
               CALL JEVEUO ( KNUM, 'L', JORDR )
            CALL RSADPA(RESUCO,'L',1,'MODELE',ZI(JORDR),0,JPARA,K8B)
            MODELE=ZK16(JPARA)
         ENDIF
         CALL PEAIRE ( RESU, MODELE, NBOCC )

      ENDIF

      CALL GETFAC ( 'MASS_INER' , NBOCC )
C                   -----------
      IF ( NBOCC .NE. 0 ) THEN

         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )
         CHDEF = ' '
         CALL GETVTX ( ' ', 'GEOMETRIE', 1,1,1, DEFORM, N1 )
         IF ( DEFORM .EQ. 'DEFORMEE' ) THEN
            CALL GETVID ( ' ', 'CHAM_GD', 1,1,1, CHDEF, N2 )
            IF ( N2 .EQ. 0 ) THEN
               TABTYP(1)='NOEU#DEPL_R'
               TABTYP(2)='NOEU#TEMP_R'
               TABTYP(3)='ELEM#ENER_R'
               KNUM = '&&OP0107.NUME_ORDRE'
               CALL GETVID ( ' ', 'RESULTAT' , 1,1,1, RESUCO, NR )
               CALL GETVR8 ( ' ', 'PRECISION', 1,1,1, PREC  , NP )
               CALL GETVTX ( ' ', 'CRITERE'  , 1,1,1, CRIT  , NC )
               CALL RSUTNU ( RESUCO,' ',0,KNUM,NBORDR,PREC,CRIT,IRET)
               IF ( NBORDR .NE. 1 ) THEN
                  CALL U2MESS('F','UTILITAI2_80')
               ENDIF
               IF ( IRET .NE. 0 ) GOTO 9999
               CALL JEVEUO ( KNUM, 'L', JORDR )
               CALL RSEXCH ( RESUCO, 'DEPL', ZI(JORDR), CHDEF, IRET )
               IF ( IRET .GT. 0 ) CALL U2MESS('F','CHAMPS_2')
               CALL CHPVE2(CHDEF,3,TABTYP,IER)
            ENDIF
         ENDIF

         CALL PEMAIN ( RESU, MODELE, MATE, CARA, NCHAR, ZK8(JCHA), NH,
     &                 NBOCC, CHDEF )

      ENDIF

      CALL GETFAC ( 'ENER_POT' , NBOCC )
C                   ----------
      IF ( NBOCC .NE. 0 ) THEN
         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )

         CALL PEEPOT ( RESU, MODELE, MATE, CARA, NCHAR, ZK8(JCHA),
     &                 NH, NBOCC )

      ENDIF

      CALL GETFAC ( 'ENER_CIN' , NBOCC )
C                   ----------
      IF ( NBOCC .NE. 0 ) THEN

         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )

         CALL PEECIN ( RESU, MODELE, MATE, CARA, NCHAR, ZK8(JCHA),
     &                 NH, NBOCC )

      ENDIF

      CALL GETFAC ( 'INTEGRALE' , NBOCC )
C                   ----------
      IF ( NBOCC .NE. 0 ) THEN

         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL PEEINT ( RESU, MODELE, NBOCC )

      ENDIF

      CALL GETFAC ( 'NORME' , NBOCC )
C                   ----------
      IF ( NBOCC .NE. 0 ) THEN

C         --- ON RECUPERE LE MODELE
         CALL GETVID ( 'NORME', 'CHAM_GD', 1,1,1, CHDEF, N1 )
         IF (N1.NE.0) THEN
            CALL GETVID ( 'NORME', 'MODELE', 1,1,1, MODELE, N2 )
         ELSE
            CALL GETVID ( 'NORME', 'RESULTAT' , 1,1,1, RESUCO, NR )
            CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         ENDIF

         CALL PENORM ( RESU, MODELE )

      ENDIF

      CALL GETFAC ( 'VOLUMOGRAMME' , NBOCC )
C                   --------------
      IF ( NBOCC .NE. 0 ) THEN

         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL PEVOLU ( RESU, MODELE, CARA, NBOCC )

      ENDIF

      CALL GETFAC ( 'MINMAX' , NBOCC )
C                   ----------
      IF ( NBOCC .NE. 0 ) THEN
         CALL GETVID ( 'MINMAX', 'CHAM_GD', 1,1,1, CHDEF, N1 )
         IF (N1.NE.0) THEN
            CALL GETVID ( 'MINMAX', 'MODELE', 1,1,1, MODELE, N2 )
         ELSE
            CALL GETVID ( 'MINMAX', 'RESULTAT' , 1,1,1, RESUCO, NR )
            CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         ENDIF
         CALL PEMIMA ( N1, CHDEF, RESU, MODELE, NBOCC )
      ENDIF

      CALL GETFAC ( 'WEIBULL' , NBOCC )
C                   ---------
      IF ( NBOCC .NE. 0 ) THEN

         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )

         CALL PEWEIB ( RESU, MODELE, MATE, CARA, K8B, NCHAR, ZK8(JCHA),
     &                 NH, NBOCC, 0, NOMCMD )


      ENDIF

      CALL GETFAC ( 'RICE_TRACEY' , NBOCC )
C                   -------------
      IF ( NBOCC .NE. 0 ) THEN

         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )

         CALL PERITR ( RESU, MODELE, CARA, NCHAR, ZK8(JCHA), NH, NBOCC )

      ENDIF

      CALL GETFAC ( 'CARA_GEOM' , NBOCC )
C                   -----------
      IF ( NBOCC .NE. 0 ) THEN

         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )

         CALL PECAGE ( RESU, MODELE, NBOCC )

      ENDIF

      CALL GETFAC ( 'CARA_POUTRE' , NBOCC )
C                   -------------
      IF ( NBOCC .NE. 0 ) THEN

         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )

         CALL PECAPO ( RESU, MODELE, CARA, NCHAR, ZK8(JCHA), NH )

      ENDIF

      CALL GETFAC ( 'INDIC_ENER' , NBOCC )
C                   ------------
      IF ( NBOCC .NE. 0 ) THEN

         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )

         CALL PEINGL ( RESU, MODELE, MATE, CARA, NCHAR, ZK8(JCHA), NH,
     &                 NBOCC, 'INDIC_ENER' )

      ENDIF

      CALL GETFAC ( 'INDIC_SEUIL' , NBOCC )
C                   -------------
      IF ( NBOCC .NE. 0 ) THEN

         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )

         CALL PEINGL ( RESU, MODELE, MATE, CARA, NCHAR, ZK8(JCHA), NH,
     &                 NBOCC, 'INDIC_SEUIL' )

      ENDIF

      CALL GETFAC ( 'ENER_ELAS' , NBOCC )
C                   -----------
      IF ( NBOCC .NE. 0 ) THEN

         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )

         CALL PEINGL ( RESU, MODELE, MATE, CARA, NCHAR, ZK8(JCHA), NH,
     &                 NBOCC, 'ENER_ELAS' )

      ENDIF

      CALL GETFAC ( 'ENER_TOTALE' , NBOCC )
C                   -------------
      IF ( NBOCC .NE. 0 ) THEN

         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )

         CALL PEINGL ( RESU, MODELE, MATE, CARA, NCHAR, ZK8(JCHA), NH,
     &                 NBOCC, 'ENER_TOTALE' )

      ENDIF
C

      CALL GETFAC ( 'ENER_DISS' , NBOCC )
C                   -----------
      IF ( NBOCC .NE. 0 ) THEN

         NH = 0
         CALL GETVIS ( ' ', 'MODE_FOURIER', 1,1,1, NH, N1 )
         CALL MEDOME (MODELE,MATE,CARA,KCHA,NCHAR,CTYP,RESUCO)
         CALL JEVEUO ( KCHA, 'L', JCHA )

         CALL PEINGL ( RESU, MODELE, MATE, CARA, NCHAR, ZK8(JCHA), NH,
     &                 NBOCC, 'ENER_DISS' )

      ENDIF
 9999 CONTINUE
      CALL TITRE
C
      CALL JEDEMA ( )

      END
