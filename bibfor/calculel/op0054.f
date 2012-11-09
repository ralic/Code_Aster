      SUBROUTINE OP0054()
      IMPLICIT   NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C      OPERATEUR :     CALC_THETA
C
C     ------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'

      REAL*8             MODULE
C
      INTEGER       NBV, NBR8, NBNO, NOCC2D, NOCC3D, IADRT1
      INTEGER       IADRNO, IMPR, IADRCO, IADRMA, IADRT2, IADRT3,
     &              IADRT4, IADRT5, ICODE, IFIC, N1
      REAL*8        R8B, DIR(3), RINF, RSUP
      LOGICAL       LDIREC, ULEXIS
      CHARACTER*8   K8B, NOMA, MODELE, FOND, RESU, NOEUD, FORMAT
      CHARACTER*16  TYPE, OPER, FICHIE,VALK(2)
      CHARACTER*24  TRAV1, TRAV2, TRAV3, TRAV4, STOK4
      CHARACTER*24  OBJ1, NOMNO, COORN, OBJ2
      CHARACTER*24  THETA, GDTETA
      INTEGER      IARG
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
C
      LDIREC = .FALSE.

C
      CALL GETRES ( RESU, TYPE, OPER )
C
      CALL GETVID (' ', 'MODELE', 0,IARG, 1, MODELE, NBV )
C
      CALL GETFAC ( 'THETA_3D' , NOCC3D )
      CALL GETFAC ( 'THETA_2D' , NOCC2D )
      CALL GETFAC ( 'IMPRESSION' , IMPR )
C
      IF ( IMPR .NE. 0 ) THEN
         CALL GETVTX ( 'IMPRESSION', 'FORMAT ', 1,IARG,1, FORMAT, NBV )
         IFIC   = 0
         FICHIE = ' '
         CALL GETVIS ( 'IMPRESSION', 'UNITE'  , 1,IARG,1, IFIC  , N1 )
         IF ( .NOT. ULEXIS( IFIC ) ) THEN
            CALL ULOPEN ( IFIC, ' ', FICHIE, 'NEW', 'O' )
         ENDIF
      ENDIF
C
C --- CREATION DE LA STRUCTURE DE DONNEES DE TYPE THETA_GEOM QUI EST
C --- ISSUE DE LA COMMANDE :
C     --------------------
      CALL RSCRSD('G',RESU,'THETA_GEOM',1)
C
C --- CREATION DU NOM DU CHAMP CORRESPONDANT AU NOM SYMBOLIQUE THETA
C --- POUR LE NUMERO D'ORDRE 0 :
C     ------------------------
      CALL RSEXCH(' ',RESU,'THETA',0,THETA,ICODE)
C
C --- SI LE CHAMP THETA EXISTE DEJA, ON SORT EN ERREUR FATALE :
C     -------------------------------------------------------
      IF (ICODE.EQ.0) THEN
        VALK(1)='THETA'
        VALK(2)=RESU
        CALL U2MESK('F','RUPTURE1_28',2,VALK)
      ENDIF
C
C --- CREATION DU NOM DU CHAMP CORRESPONDANT AU NOM SYMBOLIQUE
C --- GRAD_NOEU_THETA POUR LE NUMERO D'ORDRE 0 :
C     ----------------------------------------
      CALL RSEXCH(' ',RESU,'GRAD_NOEU_THETA',0,GDTETA,ICODE)
C
C --- SI LE CHAMP THETA EXISTE DEJA, ON SORT EN ERREUR FATALE :
C     -------------------------------------------------------
      IF (ICODE.EQ.0) THEN
        VALK(1)='GRAD_NOEU_THETA'
        VALK(2)=RESU
        CALL U2MESK('F','RUPTURE1_28',2,VALK)
      ENDIF
C
      OBJ1 = MODELE//'.MODELE    .LGRF'
      CALL JEVEUO ( OBJ1, 'L', IADRMA )
      NOMA = ZK8(IADRMA)
      NOMNO = NOMA//'.NOMNOE'
      COORN = NOMA//'.COORDO    .VALE'
      CALL JEVEUO ( COORN, 'L', IADRCO )
C
C     ==================================================================
C                          T H E T A _ 3 D
C     ==================================================================
C
      IF ( NOCC3D .NE. 0 ) THEN
C
         CALL GETVID ( ' ', 'FOND_FISS', 0,IARG,1, FOND, NBV )
C
         CALL GETVR8 ( ' ', 'DIRECTION', 0,IARG, 0, R8B, NBR8)
C
         IF ( NBR8 .NE. 0 ) THEN
            NBR8  = -NBR8
            IF ( NBR8 .NE. 3 ) THEN
               CALL U2MESS('F','RUPTURE1_30')
            ELSE
               CALL GETVR8(' ','DIRECTION', 0,IARG, 3, DIR, NBR8)
               LDIREC = .TRUE.
            ENDIF
         ENDIF
C
C        --- OBJET CONTENANT LES NOEUDS DU FOND DE FISSURE ---
C
         OBJ2  = FOND//'.FOND.NOEU'
         CALL JELIRA ( OBJ2, 'LONMAX', NBNO, K8B )
         CALL JEVEUO ( OBJ2, 'L', IADRNO )
C
         CALL GVERIG ( NOMA, NOCC3D, OBJ2, NBNO, NOMNO, COORN,
     &                 TRAV1, TRAV2, TRAV3, TRAV4 )
C
C        --- CALCUL SUIVANT LA METHODE CHOISIE ---
C
         CALL GCOURO ('G', THETA, NOMA, NOMNO,COORN,NBNO,TRAV1,
     &                TRAV2,TRAV3,DIR,ZK8(IADRNO),FOND,LDIREC,STOK4)
C
C        --- IMPRESSION DES OBJETS DECRIVANT LE CHAMP THETA ---
C
         IF ( IMPR .NE. 0 ) THEN
            CALL JEVEUO ( TRAV1, 'L', IADRT1 )
            CALL JEVEUO ( TRAV2, 'L', IADRT2 )
            CALL JEVEUO ( TRAV3, 'L', IADRT3 )
            CALL JEVEUO ( TRAV4, 'L', IADRT4 )
            CALL JEVEUO ( STOK4, 'L', IADRT5 )
            CALL GIMPTE ( THETA(1:8), ZR(IADRT1), ZR(IADRT2),
     &               ZR(IADRT3), ZK8(IADRNO), ZR(IADRT5), ZR(IADRT4),
     &               NBNO, FORMAT, IFIC )
         ENDIF
C
      ENDIF
C
C     ==================================================================
C                          T H E T A _ 2 D
C     ==================================================================
C
C
      IF ( NOCC2D .NE. 0 ) THEN
C
         CALL GETVR8(' ','DIRECTION', 0,IARG, 0, R8B, NBR8 )
C
         IF ( NBR8 .NE. 0 ) THEN
            NBR8  = -NBR8
            IF ( NBR8 .NE. 3 ) THEN
               CALL U2MESS('F','RUPTURE1_30')
            ELSE
               CALL GETVR8 ( ' ', 'DIRECTION', 0,IARG, 3, DIR, NBR8 )
               LDIREC = .TRUE.
            ENDIF
         ELSE
            CALL U2MESS('F','RUPTURE0_81')
         ENDIF
C
         CALL GVER2D ( NOMA, NOCC2D, 'THETA_2D',NOMNO,
     &                 NOEUD, RINF, RSUP, MODULE )
C
C        --- CALCUL SUIVANT LA METHODE CHOISIE ---
C
         CALL GCOU2D ('G',THETA, NOMA, NOMNO, NOEUD, ZR(IADRCO),
     &                RINF, RSUP, MODULE, LDIREC,DIR )
C
      ENDIF
C
C --- AFFECTATION DU CHAMNO THETA A LA S.D. RESU DE TYPE THETA_GEOM :
C     -------------------------------------------------------------
      CALL RSNOCH( RESU,'THETA',0)
C
      CALL JEDEMA()
      END
