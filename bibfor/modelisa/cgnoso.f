      SUBROUTINE CGNOSO ( MOFAZ, IOCC, NOMAZ, LISNOZ, NBNO )
      IMPLICIT  NONE
      INTEGER             IOCC, NBNO
      CHARACTER*(*)       MOFAZ, NOMAZ, LISNOZ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 25/11/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C -------------------------------------------------------
C
C       CGNOSO -- TRAITEMENT DE L'OPTION "SEGM_DROI_ORDO"
C                 DU MOT FACTEUR CREA_GROUP_NO DE
C                 LA COMMANDE DEFI_GROUP
C
C ----------------------------------------------------------------------
C  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_NO'
C  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
C  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
C  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS
C                                   APPARTENANT A L'ENVELOPPE
C                                   DU CYLINDRE.
C  NBNO          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
C ----------------------------------------------------------------------
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
      CHARACTER*32       JEXNUM , JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER        IRET, IDCOOR, JNOEU, NUMORI, NUMEXT, N1,IERA
      REAL*8         TOLE
      CHARACTER*8    NOMA, CRIT, NOM1
      CHARACTER*16   MOTFAC, MOTCLE(2), TYPMCL(2)
      CHARACTER*24   LISNOE, NOMNOE
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS :
C     ---------------
      MOTFAC = MOFAZ
      NOMA   = NOMAZ
      LISNOE = LISNOZ
      IERA = 0
C
      NOMNOE = NOMA//'.NOMNOE'
C
C --- RECUPERATION DES COORDONNES DES NOEUDS DU MAILLAGE :
C     --------------------------------------------------
      CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', IDCOOR )
C
C --- RECUPERATION DES NOEUDS A ORDONNER :
C     ----------------------------------
      MOTCLE(1) = 'GROUP_NO'
      MOTCLE(2) = 'NOEUD'
      TYPMCL(1) = 'GROUP_NO'
      TYPMCL(2) = 'NOEUD'
      CALL RELIEM ( ' ', NOMA, 'NU_NOEUD', MOTFAC, IOCC,
     +                                2, MOTCLE, TYPMCL, LISNOE, NBNO )
      IF ( NBNO .LE. 0 ) THEN
         CALL UTMESS('F','CGNOSO','IL MANQUE L''ENSEMBLE DES NOEUDS '//
     +   'QUE L''ON VEUT ORDONNER, MOTS CLES "NOEUD" ET/OU "GROUP_NO"')
      ENDIF
      CALL JEVEUO ( LISNOE, 'E', JNOEU )
C
C --- RECUPERATION DES NOEUDS EXTREMITES :
C     ----------------------------------
      CALL UTRENO ( MOTFAC, 'ORIG', IOCC, NOMA, NOM1 )
      CALL JENONU ( JEXNOM(NOMNOE,NOM1), NUMORI )

      CALL UTRENO ( MOTFAC, 'EXTR', IOCC, NOMA, NOM1 )
      CALL JENONU ( JEXNOM(NOMNOE,NOM1), NUMEXT )
C
C --- RECUPERATION DE LA PRECISION ET DU CRITERE :
C     ------------------------------------------
      CALL GETVR8 ( MOTFAC, 'PRECISION', IOCC,1,1, TOLE, N1 )
      CALL GETVTX ( MOTFAC, 'CRITERE'  , IOCC,1,1, CRIT, N1 )
C
C --- ON ORDONNE :
C     ----------
      CALL OREINO ( NOMA, ZI(JNOEU), NBNO, NUMORI, NUMEXT, ZR(IDCOOR),
     +              CRIT, TOLE,IERA, IRET )
      IF (IRET.NE.0) CALL UTMESS('F','CGNOSO','ARRET SUR ERREURS')
C
      CALL JEDEMA()
C
      END
