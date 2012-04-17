      SUBROUTINE IMPCOD(COLONN,ICOD  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/04/2012   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INTEGER      ICOD
      CHARACTER*9  COLONN
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (AFFICHAGE - UTILITAIRE)
C
C RETOURNE LE CODE INTERNE D'UNE COLONNE DU TABLEAU DE CONVERGENCE
C
C ----------------------------------------------------------------------
C
C
C IN  COLONN : NOM DE REFERENCE DE LA COLONNE
C                (VOIR LISTE DANS DATA MOTCLE)
C OUT ICOD   : CODE RETOUR
C                VAUT 0 LORSQU'ERREUR
C                OU LE NUMERO D'ORDRE DU NOM DE REFERENCE DE LA COLONNE
C
C ----------------------------------------------------------------------
C
      INTEGER       ZDEF
      PARAMETER    (ZDEF=40)
      INTEGER       I
      CHARACTER*9   MOTCLE(ZDEF)

      DATA MOTCLE(1)        / 'ITER_NUME'/
      DATA MOTCLE(2)        / 'INCR_INST'/
      DATA MOTCLE(3)        / 'RESI_RELA'/
      DATA MOTCLE(4)        / 'RESI_MAXI'/
      DATA MOTCLE(5)        / 'RESI_REFE'/
      DATA MOTCLE(6)        / 'RELA_NOEU'/
      DATA MOTCLE(7)        / 'MAXI_NOEU'/
      DATA MOTCLE(8)        / 'REFE_NOEU'/
      DATA MOTCLE(9)        / 'RELI_NBIT'/
      DATA MOTCLE(10)       / 'RELI_COEF'/
      DATA MOTCLE(11)       / 'PILO_COEF'/
      DATA MOTCLE(12)       / '&&&&&&&&&'/
      DATA MOTCLE(13)       / '&&&&&&&&&'/
      DATA MOTCLE(14)       / '&&&&&&&&&'/
      DATA MOTCLE(15)       / 'MATR_ASSE'/
      DATA MOTCLE(16)       / 'DEBORST  '/
      DATA MOTCLE(17)       / 'CTCD_NBIT'/
      DATA MOTCLE(18)       / '&&&&&&&&&'/
      DATA MOTCLE(19)       / '&&&&&&&&&'/
      DATA MOTCLE(20)       / 'BOUC_GEOM'/
      DATA MOTCLE(21)       / 'BOUC_FROT'/
      DATA MOTCLE(22)       / 'BOUC_CONT'/
      DATA MOTCLE(23)       / 'CONT_NEWT'/
      DATA MOTCLE(24)       / 'CTCC_INFO'/
      DATA MOTCLE(25)       / 'FROT_NEWT'/
      DATA MOTCLE(26)       / 'FROT_NOEU'/
      DATA MOTCLE(27)       / 'FETI_NBIT'/
      DATA MOTCLE(28)       / 'BOUC_VALE'/
      DATA MOTCLE(29)       / 'BOUC_NOEU'/
      DATA MOTCLE(30)       / 'RESI_COMP'/
      DATA MOTCLE(31)       / 'COMP_NOEU'/
      DATA MOTCLE(32)       / 'ITER_TIME'/
C
C ----------------------------------------------------------------------
C
      ICOD = 0
      DO 10 I = 1,ZDEF
        IF (COLONN(1:9).EQ.MOTCLE(I)) THEN
          ICOD     = I
        ENDIF
   10 CONTINUE
C
      END
