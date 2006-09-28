      SUBROUTINE PALIM2 ( MCFACT, IOCC, NOMAZ, NOMVEI, NOMVEK, IADR )
      IMPLICIT NONE
      INTEGER                     IOCC,                        IADR
      CHARACTER*(*)       MCFACT,       NOMAZ, NOMVEI, NOMVEK
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C IN   NOMAZ  : NOM DU MAILLAGE
C OUT  NOML   : NOM DE L'OBJET JEVEUX CREE SUR LA VOLATILE
C OUT  NBMAIL : NOMBRE DE MAILLES RECUPEREES
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
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       N1, IER, KOTYP, IM, NUMA, ITYP, NUME, IBID, IATYMA,
     &              LGP, LGM, ILIST, KLIST, LXLGUT, NBMC, NBMA, JNOMA
      PARAMETER     ( NBMC = 3 )
      LOGICAL       LOPT, LNOM, LNUME, GETEXM, LMAIL
      CHARACTER*8   K8B, NOMA, OPTION, OLDTYP, PRFM, NOMMAI, KNUME
      CHARACTER*16  TYMOCL(NBMC), MOTCLE(NBMC)
      CHARACTER*24  NOMAMA, NOMATY, NOMJV
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      NOMA   = NOMAZ
      NOMAMA = NOMA//'.NOMMAI'
      NOMATY = NOMA//'.TYPMAIL'
C
      LOPT = GETEXM ( MCFACT, 'OPTION' )
      LNOM = GETEXM ( MCFACT, 'PREF_MAILLE' )
      CALL JEVEUO ( NOMVEI , 'E' , ILIST )
      CALL JEVEUO ( NOMVEK , 'E' , KLIST )
      IER = 0
C
      IF ( LOPT ) THEN
        CALL GETVTX ( MCFACT, 'OPTION', IOCC,1,1, OPTION, N1)
        IF ( OPTION .EQ. 'TRIA6_7' ) THEN
          OLDTYP = 'TRIA6'
        ELSEIF ( OPTION .EQ. 'QUAD8_9' ) THEN
          OLDTYP = 'QUAD8'
        ELSEIF ( OPTION .EQ. 'SEG3_4' ) THEN
          OLDTYP = 'SEG3'
        ENDIF
        CALL JENONU ( JEXNOM('&CATA.TM.NOMTM',OLDTYP), KOTYP )
      ENDIF
C
      LMAIL = .FALSE.
      LNUME = .FALSE.
      IF ( LNOM ) THEN
        CALL GETVTX ( MCFACT, 'PREF_MAILLE', IOCC,1,0, K8B, N1)
        IF ( N1 .NE. 0 ) THEN
          CALL GETVTX ( MCFACT, 'PREF_MAILLE', IOCC,1,1, PRFM, N1)
          LGP = LXLGUT(PRFM)
          LMAIL = .TRUE.
        ELSE
          LGP = 0
          PRFM = ' '
        ENDIF
        CALL GETVIS ( MCFACT, 'PREF_NUME', IOCC,1,0, IBID, N1)
        IF ( N1 .NE. 0 ) THEN
          LNUME = .TRUE.
          CALL GETVIS ( MCFACT, 'PREF_NUME', IOCC,1,1, NUME, N1 )
        ENDIF
      ENDIF
C
      MOTCLE(1) = 'TOUT'
      TYMOCL(1) = 'TOUT'
      MOTCLE(2) = 'GROUP_MA'
      TYMOCL(2) = 'GROUP_MA'
      MOTCLE(3) = 'MAILLE'
      TYMOCL(3) = 'MAILLE'
C
      NOMJV  = '&&OP0167.LISTE_MA'
      CALL RELIEM(' ', NOMA, 'NO_MAILLE', MCFACT, IOCC, NBMC,
     &                      MOTCLE, TYMOCL, NOMJV, NBMA )
      CALL JEVEUO ( NOMJV, 'L', JNOMA )
C
      DO 30 IM = 0 , NBMA-1
         NOMMAI = ZK8(JNOMA+IM)
         CALL JENONU ( JEXNOM(NOMAMA,NOMMAI), NUMA)
         IF ( NUMA .EQ. 0 ) THEN
            IER = IER + 1
            CALL UTMESS('E','PALIM2','LA MAILLE '//NOMMAI//
     &                       ' NE FAIT PAS PARTIE DU MAILLAGE '//NOMA )
C        CALL U2MESK('E','MODELISA6_10', 2 ,VALK)
         ELSE
            IF ( LMAIL ) THEN
              IF ( LNUME ) THEN
                CALL CODENT ( NUME, 'G', KNUME )
                NUME = NUME + 1
                LGM = LXLGUT(KNUME)
                IF ( LGM+LGP .GT. 8 ) CALL U2MESS('F','MODELISA6_11')
                NOMMAI = PRFM(1:LGP)//KNUME
              ELSE
                LGM = LXLGUT(NOMMAI)
                IF ( LGM+LGP .GT. 8 ) CALL U2MESS('F','MODELISA6_12')
                NOMMAI = PRFM(1:LGP)//NOMMAI
              ENDIF
            ENDIF
            IF ( LOPT ) THEN
              CALL JEVEUO ( NOMATY, 'L',IATYMA)
              ITYP=IATYMA-1+NUMA
              IF ( ZI(ITYP) .EQ. KOTYP ) THEN
                CALL I2RDL2 ( NUMA,ZI(ILIST), NOMMAI,ZK8(KLIST), IADR )
              ENDIF
            ELSE
              CALL I2RDL2 ( NUMA, ZI(ILIST), NOMMAI,ZK8(KLIST), IADR )
            ENDIF
         ENDIF
 30   CONTINUE
      CALL JEDETR ( NOMJV )
C
      IF ( IER .NE. 0 ) CALL U2MESS('F','ELEMENTS_94')
C
      CALL JEDEMA ( )
      END
