      SUBROUTINE OP0006 (  IER )
      IMPLICIT   NONE
      INTEGER              IER
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 25/09/2002   AUTEUR VABHHTS J.PELLET 
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
C     COMMANDE AFFE_MATERIAU
C
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER       N1, NBAPNO, MXMATA, NBOCC, I, NGRO, NBMA, NBNO
      INTEGER       K,JNOMO,JMESNO
      CHARACTER*8   K8B, CHMAT, NOMAIL, NOMODE, TYPMCL(4)
      CHARACTER*16  MOTCLE(4), TYPE, NOMCMD
      CHARACTER*24  MESMAI, MESNOE
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMODE = ' '
C
      CALL GETVID ( ' ', 'MODELE', 1,1,1, NOMODE, N1 )
      IF (N1.GT.0) THEN
         CALL JEVEUO(NOMODE//'.NOEUD','L',JNOMO)
      ELSE
         JNOMO=0
      END IF
C
      CALL GETFAC ( 'AFFE' , NBOCC )
C
      DO 20 I = 1 , NBOCC
         CALL GETVID ( 'AFFE', 'GROUP_NO', I, 1, 0, K8B, NGRO )
         IF ( NGRO .NE. 0  .AND.  N1 .EQ. 0 ) THEN
            CALL UTMESS('F','OP0006','IL FAUT LE MODELE' )
         ENDIF
         CALL GETVID ( 'AFFE', 'NOEUD', I, 1, 0, K8B, NGRO )
         IF ( NGRO .NE. 0  .AND.  N1 .EQ. 0 ) THEN
            CALL UTMESS('F','OP0006','IL FAUT LE MODELE' )
         ENDIF
 20   CONTINUE
C
      CALL GETRES ( CHMAT, TYPE, NOMCMD)
C
      CALL GETVID ( ' ', 'MAILLAGE', 1,1,1, NOMAIL, N1 )
C
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      MOTCLE(3) = 'GROUP_NO'
      MOTCLE(4) = 'NOEUD'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      TYPMCL(3) = 'GROUP_NO'
      TYPMCL(4) = 'NOEUD'
C
      MESMAI = '&&OP0006.MES_MAILLES'
      MESNOE = '&&OP0006.MES_NOEUDS'
C
      NBAPNO = NBOCC
      MXMATA = 0
      DO 10 I = 1 , NBOCC
C
         CALL RELIEM(NOMODE,NOMAIL,'NU_MAILLE','AFFE',I,2, MOTCLE(1),
     +                                      TYPMCL(1), MESMAI, NBMA )
         CALL RELIEM(NOMODE,NOMAIL,'NU_NOEUD','AFFE',I,2, MOTCLE(3),
     +                                      TYPMCL(3), MESNOE, NBNO )


C        -- ON VERIFIE QUE LES NOEUDS AFFECTES FONT PARTIE DU MODELE:
         IF (NBNO.GT.0) THEN
           CALL JEVEUO(MESNOE,'L',JMESNO)
           IER=0
           IF (JNOMO.NE.0) THEN
              DO 11,K=1,NBNO
                 IF (ZI(JNOMO-1+ZI(JMESNO-1+K)).EQ.0) IER=IER+1
 11           CONTINUE
           ELSE
              IER=1
           END IF
           IF (IER.GT.0) CALL UTMESS('F','OP0006','ON CHERCHE A '
     +     //'AFFECTER DES NOEUDS QUI N''APPARTIENNENT PAS AU MODELE.')
         END IF



         IF ( NBMA .NE. 0 )  CALL JEDETR ( MESMAI )
         IF ( NBNO .NE. 0 )  CALL JEDETR ( MESNOE )
         MXMATA = MXMATA + NBMA + NBNO
C
 10   CONTINUE
C
      CALL RCMATE ( CHMAT, NOMAIL, MXMATA, NBAPNO, NOMODE )
      CALL RCTREF ( CHMAT, NOMAIL, MXMATA, NBAPNO, NOMODE )
C
      CALL JEDEMA()
      END
