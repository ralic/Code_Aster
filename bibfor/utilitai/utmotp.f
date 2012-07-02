      INTEGER FUNCTION UTMOTP(FONREE,MOTFAC,IOCC,MOTCLE)
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
C
C     CETTE FONCTION TESTE LA PRESENCE D'UN MOTCLE
C     SOUS UN MOT-CLE FACTEUR
C     LE TEST SE FAIT EN FONCTION DE FONREE
C     SUR DES REELS, COMPLEXES OU FONCTIONS
C     UTILISE PAR EXEMPLE POUR AFFE_CHAR_MECA
C
      IMPLICIT NONE
      CHARACTER*4       FONREE
      CHARACTER*(*) MOTFAC,MOTCLE
      CHARACTER*8 KBID
      COMPLEX*16  CBID
      INTEGER      IARG
C-----------------------------------------------------------------------
      INTEGER IOCC 
      REAL*8 RBID 
C-----------------------------------------------------------------------
      IF (FONREE.EQ.'REEL') THEN
        CALL GETVR8(MOTFAC,MOTCLE,IOCC,IARG,0,RBID,UTMOTP)
      ELSE IF (FONREE.EQ.'FONC') THEN
        CALL GETVID(MOTFAC,MOTCLE,IOCC,IARG,0,KBID,UTMOTP)
      ELSE IF (FONREE.EQ.'COMP') THEN
        CALL GETVC8(MOTFAC,MOTCLE,IOCC,IARG,0,CBID,UTMOTP)
      ELSE
        CALL U2MESS('F','UTILITAI5_52')
      ENDIF
      END
