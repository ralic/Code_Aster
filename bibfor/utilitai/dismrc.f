      SUBROUTINE DISMRC(QUESTI,NOMOBZ,REPI,REPK,IERD)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER                               REPI,     IERD
      CHARACTER*(*) QUESTI,REPK
      CHARACTER*19 NOMOB
      CHARACTER*(*) NOMOBZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C IN  : QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C IN  : NOMOBZ : NOM D'UN OBJET DE TYPE RESU_COMPO (K19)
C OUT : REPI   : REPONSE ( SI ENTIERE )
C OUT : REPK   : REPONSE ( SI CHAINE DE CARACTERES )
C OUT : IERD   : CODE RETOUR (0--> OK, 1 --> PB)
C ----------------------------------------------------------------------
      CHARACTER*1 KBID
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      NOMOB = NOMOBZ
C ----------------------------------------------------------------------
      IERD = 0
C
      IF (QUESTI.EQ.'NB_CHAMP_MAX') THEN
         CALL JELIRA(JEXNUM(NOMOB//'.TACH',1),'LONMAX',REPI,KBID)
      ELSE IF (QUESTI.EQ.'NB_CHAMP_UTI') THEN
         CALL JELIRA(NOMOB//'.ORDR','LONUTI',REPI,KBID)
      ELSE
         IERD = 1
      ENDIF
C
      REPK=' '
      END
