      SUBROUTINE CHCOMA ( TABLEZ, NOMAOU )
      IMPLICIT   NONE
      CHARACTER*8         NOMAOU
      CHARACTER*(*)       TABLEZ
C.======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C      CHCOMA -- IL S'AGIT DE CHANGER LES VALEURS DES COORDONNEES
C                DES NOEUDS DU MAILLAGE DE NOM NOMAOU QUI SONT EXPRIMEES
C                DANS LE REPERE GLOBAL EN LEURS VALEURS EXPRIMEES
C                DANS LE REPERE PRINCIPAL D'INERTIE DE CE MAILLAGE.
C                TRAVAILLER AVEC UN MAILLAGE DONT LES COORDONNEES
C                DES NOEUDS SONT EXPRIMEES DANS LE REPERE PRINCIPAL
C                D'INERTIE EST NECESSAIRE POUR CALCULER LES
C                COEFFICIENTS DE CISAILLEMENT D'UNE POUTRE DONT
C                UNE SECTION EST REPRESENTEE PAR LE MAILLAGE
C                NOMAOU QUI EST CONSTITUE D'ELEMENTS MASSIFS 2D.
C
C
C   ARGUMENT        E/S  TYPE         ROLE
C    TABLEZ         IN    K*      NOM D'UNE TABLE DE TYPE TABL_CARA_GEOM
C                                 ISSUE DE LA COMMANDE POST_ELEM.
C                                 CETTE TABLE CONTIENT LES COORDONNEES
C                                 DE L'ORIGINE DU NOUVEAU REPERE
C                                 (I.E. LE CENTRE DE GRAVITE DE LA
C                                       SECTION)
C                                 ET L'ANGLE FORME PAR LES NOUVEAUX AXES
C                                 (I.E. LES AXES PRINCIPAUX D'INERTIE)
C                                 AVEC LES AXES GLOBAUX.
C    NOMAOU         IN    K*      NOM DU MAILLAGE REPRESENTANT LA
C                                 SECTION DE LA POUTRE MAILLEE AVEC
C                                 DES ELEMENTS MASSIFS 2D, LES
C                                 COORDONNEES DES NOEUDS ETANT DEFINIES
C                                 DANS LE REPERE GLOBAL EN ENTREE
C                                 DE LA ROUTINE ET DANS LE REPERE
C                                 PRINCIPAL D'INERTIE A LA SORTIE DE LA
C                                 ROUTINE.
C.========================= DEBUT DES DECLARATIONS ====================
C ----- COMMUNS NORMALISES  JEVEUX
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
C -----  VARIABLES LOCALES
      INTEGER NGM,IBID, IRET, IDCODE, DIMCOO, NBNO, JCOOR, IDCOOR, INO
      REAL*8        R8B, P(2,2), ALPHA, R8DGRD, XG, YG, XABS, YABS
      COMPLEX*16    C16B
      CHARACTER*8   K8B, NOMA, NOGRMA
      CHARACTER*19  TABLE
      CHARACTER*24  COOVAL, COODES
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ ( )
C
C --- INITIALISATIONS :
C     ---------------
      TABLE  = TABLEZ
      COOVAL = NOMAOU//'.COORDO    .VALE'
      COODES = NOMAOU//'.COORDO    .DESC'
C
C --- VERIFICATION DES PARARAMETRES DE LA TABLE
C     -----------------------------------------
      CALL TBEXP2(TABLE,'LIEU')
      CALL TBEXP2(TABLE,'CDG_X')
      CALL TBEXP2(TABLE,'CDG_Y')
      CALL TBEXP2(TABLE,'ALPHA')
C
C --- RECUPERATION DANS LA TABLE DES COORDONNEES DU CENTRE DE GRAVITE :
C     ---------------------------------------------------------------
      CALL GETVID('REPERE','GROUP_MA',1,1,0,K8B,NGM)
      IF (NGM.NE.0) THEN
          NGM = 1
          CALL GETVID('REPERE','GROUP_MA',1,1,NGM,NOGRMA,NGM)
          NOMA=NOGRMA
          IRET=0
      ELSE
         CALL TBEXP2(TABLE,'MAILLAGE')
         CALL TBLIVA ( TABLE, 0, K8B, IBID, R8B, C16B, K8B, K8B, R8B,
     &                 'MAILLAGE', K8B, IBID, R8B, C16B, NOMA, IRET )
      ENDIF
      IF ( IRET .NE. 0 ) CALL U2MESS('F','ALGELINE_6')
      CALL TBLIVA ( TABLE, 1, 'LIEU', IBID, R8B, C16B, NOMA, K8B,
     &              R8B, 'CDG_X', K8B, IBID, XG, C16B, K8B, IRET )
      IF ( IRET .NE. 0 ) CALL U2MESS('F','MODELISA2_88')
      CALL TBLIVA ( TABLE, 1, 'LIEU', IBID, R8B, C16B, NOMA, K8B,
     &              R8B, 'CDG_Y', K8B, IBID, YG, C16B, K8B, IRET )
      IF ( IRET .NE. 0 ) CALL U2MESS('F','MODELISA2_89')
C
C --- RECUPERATION DANS LA TABLE DE L'ANGLE FAISANT PASSER DU REPERE
C --- PRINCIPAL D'INERTIE AU REPERE GLOBAL :
C     ------------------------------------
      CALL TBLIVA ( TABLE, 1, 'LIEU', IBID, R8B, C16B, NOMA, K8B,
     &              R8B, 'ALPHA', K8B, IBID, ALPHA, C16B, K8B, IRET )
      IF ( IRET .NE. 0 ) CALL U2MESS('F','ALGELINE_7')
C
C --- PASSAGE DE L'ANGLE DE DEGRES EN RADIANS :
C     ---------------------------------------
      ALPHA = ALPHA * R8DGRD()
C
C --- CONSTITUTION DE LA MATRICE DE PASSAGE DU REPERE GLOBAL
C --- AU REPERE D'INERTIE :
C     -------------------
      P(1,1) =  COS(ALPHA)
      P(2,1) =  SIN(ALPHA)
      P(1,2) = -SIN(ALPHA)
      P(2,2) =  COS(ALPHA)
C
C --- RECUPERATION DE LA DIMENSION DU MAILLAGE :
C     ----------------------------------------
      CALL JEVEUO ( COODES, 'L', IDCODE )
      DIMCOO = -ZI(IDCODE+2-1)
C
C --- NOMBRE DE NOEUDS DU MAILLAGE :
C     ----------------------------
      CALL DISMOI('F','NB_NO_MAILLA',NOMAOU,'MAILLAGE',NBNO,K8B,IRET)
C
C --- RECUPERATION DES COORDONNEES DES NOEUDS DU MAILLAGE :
C     ---------------------------------------------------
      CALL JEVEUO ( COOVAL, 'E', JCOOR )
C
C --- CHANGEMENT D'ORIGINE DES COORDONNEES :
C     ------------------------------------
      DO 10 INO = 1, NBNO
C
         IDCOOR       = JCOOR-1+DIMCOO*(INO-1)
         ZR(IDCOOR+1) = ZR(IDCOOR+1) - XG
         ZR(IDCOOR+2) = ZR(IDCOOR+2) - YG
 10   CONTINUE
C
C --- ROTATION D'ANGLE ALPHA DES AXES :
C     -------------------------------
      DO 20 INO = 1, NBNO
C
         IDCOOR       = JCOOR-1+DIMCOO*(INO-1)
         XABS         = ZR(IDCOOR+1)
         YABS         = ZR(IDCOOR+2)
C
         ZR(IDCOOR+1) = P(1,1)*XABS + P(2,1)*YABS
         ZR(IDCOOR+2) = P(1,2)*XABS + P(2,2)*YABS
 20   CONTINUE
C
      CALL JEDEMA ( )
C.============================ FIN DE LA ROUTINE ======================
      END
