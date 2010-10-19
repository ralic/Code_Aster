      SUBROUTINE CAFOTU (CHAR,LIGRMO,IALLOC,NOMA,FONREE)
      IMPLICIT   NONE
      INTEGER           NBCA, NBET, IALLOC
      CHARACTER*4       FONREE
      CHARACTER*8       CHAR, NOMA
      CHARACTER*(*)     LIGRMO
C-----------------------------------------------------------------------
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
C BUT : STOCKAGE DE FORCE_TUYAU(PRES) DANS UNE CARTE ALLOUEE SUR LE
C       LIGREL DU MODELE
C
C ARGUMENTS D'ENTREE:
C      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
C      LIGRMO : NOM DU LIGREL DE MODELE
C      IALLOC : 1 SI LA CARTE DE PRESSION ALLOUE PAR CAPRES, 0 SINON
C      NOMA   : NOM DU MAILLAGE
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
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER       NPRES, JNCMP, JVALV, NCMP, IOCC, NPR, IATYMA, NBMA,
     &              I, IMA, IADTYP, JMA, IBID, NMATOT, NBTOU
      CHARACTER*8   K8B, MAILLE, TYPE, TYPMCL(2)
      CHARACTER*16  MOTCLF, MOTCLE(2)
      CHARACTER*19  CARTE
      CHARACTER*24  MESMAI, VALK(4)
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MOTCLF = 'FORCE_TUYAU'
      CALL GETFAC ( MOTCLF , NPRES )
C
      CARTE = CHAR//'.CHME.PRESS'
      IF (IALLOC.EQ.0) THEN
         IF (FONREE.EQ.'REEL') THEN
            CALL ALCART ( 'G', CARTE, NOMA, 'PRES_R')
         ELSE IF (FONREE.EQ.'FONC') THEN
            CALL ALCART ( 'G', CARTE, NOMA, 'PRES_F')
         ELSE
            CALL U2MESK('F','MODELISA2_37',1,FONREE)
         END IF
      END IF
C
      CALL JEVEUO(NOMA//'.TYPMAIL','L',IATYMA)
      CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NMATOT,K8B,IBID)
C
      CALL JEVEUO (CARTE//'.NCMP', 'E', JNCMP)
      CALL JEVEUO (CARTE//'.VALV', 'E', JVALV)
C
C --- STOCKAGE DE PRESSIONS NULLES SUR TOUT LE MAILLAGE
C
      NCMP = 1
      ZK8(JNCMP)   = 'PRES'
      IF (IALLOC.EQ.0) THEN
         IF (FONREE.EQ.'REEL') THEN
            ZR(JVALV)   = 0.D0
         ELSE
            ZK8(JVALV)   = '&FOZERO'
         END IF
         CALL NOCART ( CARTE, 1, ' ', 'NOM', 0,' ',0, LIGRMO, NCMP )
      END IF
C
      MESMAI = '&&CAFOTU.MES_MAILLES'
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
C
C --- STOCKAGE DANS LA CARTE
C
      DO 10 IOCC = 1, NPRES
C
         IF (FONREE.EQ.'REEL') THEN
            CALL GETVR8(MOTCLF,'PRES',IOCC,1,1,ZR(JVALV) ,NPR)
         ELSE
            CALL GETVID(MOTCLF,'PRES',IOCC,1,1,ZK8(JVALV),NPR)
         END IF
C
         CALL GETVTX ( MOTCLF, 'TOUT', IOCC, 1, 1, K8B, NBTOU )
C
         IF ( NBTOU .NE. 0 ) THEN
            DO 12 IMA = 1 , NMATOT
               IADTYP = IATYMA-1+IMA
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(IADTYP)),TYPE)
               IF ((TYPE(1:4).NE.'SEG3').AND.(TYPE(1:4).NE.'SEG4')) THEN
                  CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',IMA),MAILLE)
                  VALK(1) = MAILLE
                  VALK(2) = MOTCLF
                  CALL U2MESG('A','MODELISA9_81',2,VALK,0,0,0,0.D0)
               ENDIF
 12         CONTINUE
            CALL NOCART ( CARTE, 1,' ','NOM',0,' ', 0,LIGRMO, NCMP )
C
         ELSE
            CALL RELIEM(LIGRMO, NOMA, 'NU_MAILLE', MOTCLF, IOCC, 2,
     &                                  MOTCLE, TYPMCL, MESMAI, NBMA )
            IF (NBMA.EQ.0) GOTO 10
            CALL JEVEUO ( MESMAI, 'L', JMA )
            DO 14 I = 1,NBMA
               IMA = ZI(JMA-1+I)
               IADTYP = IATYMA-1+IMA
               CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(IADTYP)),TYPE)
               IF ((TYPE(1:4).NE.'SEG3').AND.(TYPE(1:4).NE.'SEG4')) THEN
                  CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',IMA),MAILLE)
                  VALK(1) = MAILLE
                  VALK(2) = MOTCLF
                  CALL U2MESG('A','MODELISA9_81',2,VALK,0,0,0,0.D0)
               ENDIF
 14         CONTINUE
            CALL NOCART( CARTE,3,K8B,'NUM',NBMA,K8B,ZI(JMA),' ',NCMP)
            CALL JEDETR ( MESMAI )
         ENDIF
C
 10   CONTINUE
C
      CALL JEDETR (CHAR//'.PRES.GROUP')
      CALL JEDETR (CHAR//'.PRES.LISTE')
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
