      SUBROUTINE CALAPL ( CHAR, LIGRMO, NOMA )
      IMPLICIT   NONE
      CHARACTER*8       CHAR, NOMA
      CHARACTER*(*)     LIGRMO
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
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
C BUT : STOCKAGE DES CHARGES REPARTIES DANS UNE CARTE ALLOUEE SUR LE
C       LIGREL DU MODELE ( FORCES DE LAPLACE )
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
      CHARACTER*32        JEXNUM
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER       IMA, NBFLP, JVALV, JNCMP, IOCC, INTO, JTRAN,
     +              JNO,NBTOU, NBMA, JMA, NBMA2, JMA2, NTRA, NSYM,
     +              JNUMA
      REAL*8        RBID
      CHARACTER*8   K8B, TYPMCL(2), TYPMC2(2)
      CHARACTER*16  MOTCLF, MOTCLE(2), MOTCL2(2), LISTMA, LTRANS
      CHARACTER*19  CARTE
      CHARACTER*24  MESMAI, MESMA2, CONNEX
      INTEGER      IARG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      MOTCLF = 'INTE_ELEC'
      CALL GETFAC ( MOTCLF , NBFLP )
C
      MESMAI = '&&CALAPL.MES_MAILLES'
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
C
      MESMA2 = '&&CALAPL.MES_MAILLES_2'
      MOTCL2(1) = 'GROUP_MA_2'
      MOTCL2(2) = 'MAILLE_2'
      TYPMC2(1) = 'GROUP_MA'
      TYPMC2(2) = 'MAILLE'
C
      CONNEX = NOMA//'.CONNEX'
      CALL JELIRA ( CONNEX, 'NMAXOC', INTO, K8B )
C
      DO 10 IOCC = 1, NBFLP
C
         CARTE(1:17)  = CHAR//'.CHME.'//'FL1'
         LISTMA(1:14) = CHAR//'.LISMA'
         LTRANS(1:14) = CHAR//'.TRANS'
         CALL CODENT ( IOCC, 'D0', CARTE(18:19) )
         CALL CODENT ( IOCC, 'D0', LISTMA(15:16) )
         CALL CODENT ( IOCC, 'D0', LTRANS(15:16) )
C
         CALL ALCART ( 'G', CARTE, NOMA, 'LISTMA')
C
         CALL JEVEUO ( CARTE//'.NCMP', 'E', JNCMP )
         CALL JEVEUO ( CARTE//'.VALV', 'E', JVALV )
C
C        STOCKAGE DE VALEURS NULLES SUR TOUT LE MAILLAGE
C
         ZK8(JNCMP)   = 'LISTMA'
         ZK8(JNCMP+1) = 'TRANS'
         ZK16(JVALV)   = ' '
         ZK16(JVALV+1) = ' '
         CALL NOCART ( CARTE, 1, ' ', 'NOM', 0, ' ', 0, LIGRMO, 2)
C
         CALL WKVECT ( LTRANS, 'G V R', 6 , JTRAN )
         ZR(JTRAN)   = 0.D0
         ZR(JTRAN+1) = 0.D0
         ZR(JTRAN+2) = 0.D0
         ZR(JTRAN+3) = 0.D0
         ZR(JTRAN+4) = 0.D0
         ZR(JTRAN+5) = 0.D0
C
         ZK8(JNCMP)   = 'LISTMA'
         ZK8(JNCMP+1) = 'TRANS'
         ZK16(JVALV)   = LISTMA
         ZK16(JVALV+1) = LTRANS
C
         CALL GETVR8 ( MOTCLF, 'TRANS', IOCC,IARG,0, RBID, NTRA )
         CALL GETVR8 ( MOTCLF, 'SYME' , IOCC,IARG,0, RBID, NSYM )
         NTRA = -NTRA
         NSYM = -NSYM
         IF (NTRA.NE.0)
     +      CALL GETVR8 (MOTCLF,'TRANS',IOCC,IARG,NTRA,
     &                   ZR(JTRAN), NTRA)
         IF (NSYM.NE.0)
     +      CALL GETVR8 (MOTCLF,'SYME',IOCC,IARG,NSYM,
     &                   ZR(JTRAN), NSYM)
C
C ------ GEOMETRIE DU CONDUCTEUR SECONDAIRE
C
         CALL RELIEM(LIGRMO, NOMA, 'NU_MAILLE', MOTCLF, IOCC, 2,
     +                                 MOTCL2, TYPMC2, MESMA2, NBMA2 )
         IF (NBMA2.EQ.0) GOTO 10
C
C ------ GEOMETRIE DU CONDUCTEUR PRINCIPAL
C
         CALL GETVTX ( MOTCLF, 'TOUT', IOCC,IARG, 1, K8B, NBTOU )
         IF ( NBTOU .NE. 0 ) THEN
            IF ( NBMA2 .EQ. 0 ) THEN
               CALL WKVECT ( LISTMA, 'G V I', 2*INTO, JNUMA )
               DO 12 IMA = 1 , INTO
                  CALL JEVEUO ( JEXNUM(CONNEX,IMA), 'L', JNO )
                  ZI(JNUMA+2*IMA-2) = ZI(JNO)
                  ZI(JNUMA+2*IMA-1) = ZI(JNO+1)
 12            CONTINUE
            ELSE
               CALL JEVEUO ( MESMA2, 'L', JMA2 )
               CALL WKVECT ( LISTMA, 'G V I', 2*NBMA2, JNUMA)
               DO 14 IMA = 1 , NBMA2
                  CALL JEVEUO ( JEXNUM(CONNEX,ZI(JMA2+IMA-1)), 'L',JNO)
                  ZI(JNUMA+2*IMA-2) = ZI(JNO)
                  ZI(JNUMA+2*IMA-1) = ZI(JNO+1)
 14            CONTINUE
               CALL JEDETR ( MESMA2 )
            ENDIF
            CALL NOCART ( CARTE, 1, ' ', 'NOM', 0, ' ', 0,LIGRMO, 2 )
C
         ELSE
            CALL RELIEM(LIGRMO, NOMA, 'NU_MAILLE', MOTCLF, IOCC, 2,
     +                                  MOTCLE, TYPMCL, MESMAI, NBMA )
            IF (NBMA.EQ.0) GOTO 10
            CALL JEVEUO ( MESMAI, 'L', JMA )
            IF ( NBMA2 .EQ. 0 ) THEN
               CALL WKVECT ( LISTMA, 'G V I', 2*NBMA, JNUMA )
               DO 16 IMA = 1 , NBMA
                  CALL JEVEUO ( JEXNUM(CONNEX,ZI(JMA+IMA-1)), 'L', JNO )
                  ZI(JNUMA+2*IMA-2) = ZI(JNO)
                  ZI(JNUMA+2*IMA-1) = ZI(JNO+1)
 16            CONTINUE
            ELSE
               CALL JEVEUO ( MESMA2, 'L', JMA2 )
               CALL WKVECT ( LISTMA, 'G V I', 2*NBMA2, JNUMA )
               DO 18 IMA = 1 , NBMA2
                  CALL JEVEUO ( JEXNUM(CONNEX,ZI(JMA2+IMA-1)), 'L',JNO)
                  ZI(JNUMA+2*IMA-2) = ZI(JNO)
                  ZI(JNUMA+2*IMA-1) = ZI(JNO+1)
 18            CONTINUE
               CALL JEDETR ( MESMA2 )
            ENDIF
            CALL NOCART ( CARTE,3,K8B,'NUM',NBMA,K8B,ZI(JMA),' ',2)
            CALL JEDETR ( MESMAI )
         ENDIF
C
 10   CONTINUE
C
      CALL JEDEMA()
      END
