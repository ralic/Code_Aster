      SUBROUTINE DIMTHM(NOMTE,NDLNO,NDLNM,NDIM)
      IMPLICIT NONE
      INTEGER       NDLNO,NDLNM,NDIM
      INTEGER       IAUX
      INTEGER       LXLGUT
      CHARACTER*16  NOMTE
      LOGICAL       ELSUFM
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- BUT: CALCUL DU NOMBRE DE DDL SUR CHAQUE TYPE DE NOEUDS POUR LES --
C --- ELEMENTS DE BORD EN THM ------------------------------------------
C ======================================================================
C     ELSUFM VRAI SI ELEMENT SUSHI OU EFMH
C     NDDLNO NOMBRE DE DDL DES NOEUDS EXTREMITE DE SEGMENTS
C     NDDLM  NOMBRE DE DDL DES NOEUDS MILIEU DE SEGMENTS OU FACE
      IAUX = LXLGUT(NOMTE)
      ELSUFM= (( NOMTE(IAUX-4:IAUX).EQ.'_SUDM').OR.
     >        ( NOMTE(IAUX-4:IAUX).EQ.'_SUDA' ).OR.
     >        ( NOMTE(IAUX-3:IAUX).EQ.'_SUC' ))
      
      IF (ELSUFM) THEN
C ======================================================================
C --- SI MODELISATION = SUSHI   ----------------------------------------
C ======================================================================
         NDLNO = 0
         NDLNM = 2
      ELSE IF (NOMTE(1:5).EQ.'THHM_') THEN
C ======================================================================
C --- SI MODELISATION = THHM -------------------------------------------
C ======================================================================
         NDLNO = NDIM+3
         NDLNM = NDIM
      ELSE IF (NOMTE(1:5).EQ.'THH2M') THEN
C ======================================================================
C --- SI MODELISATION = THH2M ------------------------------------------
C ======================================================================
         NDLNO = NDIM +3
         NDLNM = NDIM
      ELSE IF (NOMTE(1:2).EQ.'HM') THEN
C ======================================================================
C --- SI MODELISATION = HM ---------------------------------------------
C ======================================================================
         NDLNO = NDIM+1
         NDLNM = NDIM
      ELSE IF (NOMTE(1:3).EQ.'HHM') THEN
C ======================================================================
C --- SI MODELISATION = HHM --------------------------------------------
C ======================================================================
         NDLNO = NDIM+2
         NDLNM = NDIM
      ELSE IF (NOMTE(1:4).EQ.'HH2M') THEN
C ======================================================================
C --- SI MODELISATION = HH2M -------------------------------------------
C ======================================================================
         NDLNO = NDIM+2
         NDLNM = NDIM
      ELSE IF (NOMTE(1:4).EQ.'THH_') THEN
C ======================================================================
C --- SI MODELISATION = THH --------------------------------------------
C ======================================================================
         NDLNO = 3
         NDLNM = 0
      ELSE IF (NOMTE(1:5).EQ.'THH2_') THEN
C ======================================================================
C --- SI MODELISATION = THH2 -------------------------------------------
C ======================================================================
         NDLNO = 3
         NDLNM = 0
      ELSE IF (NOMTE(1:3).EQ.'HH_') THEN
C ======================================================================
C --- SI MODELISATION = HH_ -------------------------------------------
C ======================================================================
         NDLNO = 2
         NDLNM = 0
      ELSE IF (NOMTE(1:4).EQ.'HH2_') THEN
C ======================================================================
C --- SI MODELISATION = HH2 -------------------------------------------
C ======================================================================
         NDLNO = 2
         NDLNM = 0
      ELSE IF (NOMTE(1:4).EQ.'THV_') THEN
C ======================================================================
C --- SI MODELISATION = THV --------------------------------------------
C ======================================================================
         NDLNO = 2
         NDLNM = 0
      ELSE IF (NOMTE(1:4).EQ.'THM_') THEN
C ======================================================================
C --- SI MODELISATION = THM --------------------------------------------
C ======================================================================
         NDLNO = NDIM+2
         NDLNM = NDIM
      ELSE
         CALL U2MESS('F','ALGORITH3_8')
      END IF
C =====================================================================
      END
