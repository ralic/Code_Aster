      SUBROUTINE DISMCR(QUESTI,NOMOBZ,REPI,REPKZ,IERD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ======================================================================
      IMPLICIT NONE
C     --     DISMOI(CARA_ELEM)
C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI
      CHARACTER*(*) NOMOBZ,REPKZ
      CHARACTER*32 REPK
      CHARACTER*8 NOMOB
C ----------------------------------------------------------------------
C     IN:
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOBZ : NOM D'UN OBJET DE TYPE NUM_DDL
C     OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
C
C ----------------------------------------------------------------------
C
      INTEGER K,NCARTE,IEXI
      LOGICAL ZEROBJ
      CHARACTER*8 KBID
      PARAMETER (NCARTE=13)
      CHARACTER*16 CARTES(NCARTE)
      CHARACTER*19 CART1
      DATA CARTES/
     & '.CARCOQUE',  '.CARGEOPO',  '.CARARCPO',
     & '.CARCABLE',  '.CARDISCA',  '.CARDISCK',
     & '.CARDISCM',  '.CARGENBA',  '.CARGENPO',
     & '.CARGEOBA',  '.CARMASSI',
     & '.CARORIEN',  '.CARPOUFL'/
C -------------------------------------------------------------------
      NOMOB=NOMOBZ
      REPK=' '


      IF (QUESTI.EQ.'NOM_MAILLA' ) THEN
C     ---------------------------------------------------------------
        DO 1, K=1,NCARTE
          CART1=NOMOB//CARTES(K)
          CALL DISMCA(QUESTI,CART1,REPI,REPK,IERD)
          IF (IERD.EQ.0) GOTO 2
1       CONTINUE
2       CONTINUE


      ELSE IF (QUESTI.EQ.'EXI_AMOR' ) THEN
C     ---------------------------------------------------------------
        REPK='NON'
        CALL JEEXIN(NOMOB//'.CARDISCA  .VALE',IEXI)
        IF (IEXI.NE.0) THEN
          IF (.NOT.ZEROBJ(NOMOB//'.CARDISCA  .VALE')) REPK='OUI'
        ENDIF


      ELSE
C     --------------------------------------
        IERD=1
      ENDIF
C
      REPKZ=REPK
      END
