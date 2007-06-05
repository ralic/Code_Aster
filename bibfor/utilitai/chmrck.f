      SUBROUTINE CHMRCK(CHMAT,NOMRC,NOMMAT,NBMTRC)
      IMPLICIT NONE
      CHARACTER*8   CHMAT,NOMMAT(*)
      CHARACTER*16  NOMRC
      INTEGER       NBMTRC
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 04/06/2007   AUTEUR PELLET J.PELLET 
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
C     ==================================================================
C     ! UTILITAIRE - RECHERCHE DES MATERIAUX D'UN CHAM_MATER QUI       !
C     ! UTILISENT UNE RELATION DE COMPORTEMENT DONNEE               RM !
C     ==================================================================
C     !                                                                !
C     !    ETANT DONNES UN CHAM_MATER ET UNE RELATION DE COMPORTEMENT  !
C     !    CHERCHER LES MATERIAUX DU CHAM_MATER QUI UTILISE CETTE RC   !
C     !                                                                !
C     !    DANS UN MATERIAU, IL N'Y A QU'UNE RC D'UN TYPE DONNE        !
C     !                                                                !
C     ==================================================================
C IN  ! CHMAT  ! K8  ! NOM DU CONCEPT CHAM_MATER                       !
C IN  ! NOMRC  ! K8  ! NOM DE LA RC CHERCHEE                           !
C IN  ! NBMTCH ! IS  ! LONGUEUR DU .VALE DU CHAM_MATER                 !
C OUT ! NBMTRC ! IS  ! NOMBRE DE MAT QUI UTILISE LA RC                 !
C OUT ! NOMMAT ! K8  !LISTE DES MAT DU CHAM_MATER QUI UTILISENT LA RC  !
C     ==================================================================
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) ,ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C --- VARIABLES LOCALES ---
      CHARACTER*8  KMAT ,KBID
      CHARACTER*24 KRC
      INTEGER      JVALE,ARC,IMAT,NBRC,IPOS,NCMPMX,JDESC,IZONE,I,NBZONE
      INTEGER      L1,NBZMAX
      PARAMETER (NCMPMX=30)
C
C ====================== DEBUT DU PROGRAMME ============================
C
      CALL JEMARQ()
      KBID = ' '
      CALL JEVEUO(CHMAT//'.CHAMP_MAT .VALE','L',JVALE)
      CALL JELIRA(CHMAT//'.CHAMP_MAT .VALE','LONMAX',L1,KBID)
      CALL JEVEUO(CHMAT//'.CHAMP_MAT .DESC','L',JDESC)
      NBZMAX=ZI(JDESC-1+2)
      NBZONE=ZI(JDESC-1+3)
C     ON VERIFIE QUE LA TAILLE DE LA CARTE EST BIEN TOUJOURS DE 30
C     PAR ZONE
      CALL ASSERT(L1.EQ.(NBZMAX*NCMPMX))

C
      NBMTRC = 0
      DO 50 IZONE = 1, NBZONE
        DO 100 I=1,NCMPMX
          IMAT=(IZONE-1)*NCMPMX+I
          KMAT = ZK8(JVALE-1 + IMAT)
          IF (KMAT.EQ.' ') GOTO 50
          IF (KMAT.EQ.'TREF=>') GOTO 50
          KRC  = KMAT//'.MATERIAU.NOMRC'
          CALL JEVEUO(KRC,'L',ARC)
          CALL JELIRA(KRC,'LONMAX',NBRC,KBID)
          CALL UTFK16(ZK16(ARC),NBRC,NOMRC,IPOS)
          IF (IPOS .GT. 0) THEN
             NBMTRC = NBMTRC + 1
             NOMMAT(NBMTRC) = KMAT
          ENDIF
100     CONTINUE
50    CONTINUE

C
      CALL JEDEMA()
      END
