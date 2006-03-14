      SUBROUTINE IMPCOD(COLONN,ICOD)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/03/2006   AUTEUR MABBAS M.ABBAS 
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
C
      IMPLICIT NONE
      INTEGER      ICOD
      CHARACTER*9  COLONN
C
C ----------------------------------------------------------------------
C ROUTINE APPELEE PAR : IMPINI
C ----------------------------------------------------------------------
C
C RETOURNE LE CODE INTERNE D'UNE COLONNE DU TABLEAU DE CONVERGENCE
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
      PARAMETER    (ZDEF=30)
      INTEGER       I
      CHARACTER*9   MOTCLE(ZDEF)

      DATA MOTCLE(1)        / 'ITER_NEWT'/
      DATA MOTCLE(2)        / 'INCR_TPS'/
      DATA MOTCLE(3)        / 'RESI_RELA'/
      DATA MOTCLE(4)        / 'RESI_MAXI'/
      DATA MOTCLE(5)        / 'RESI_REFE'/
      DATA MOTCLE(6)        / 'RELA_NOEU'/
      DATA MOTCLE(7)        / 'MAXI_NOEU'/
      DATA MOTCLE(8)        / 'REFE_NOEU'/
      DATA MOTCLE(9)        / 'RELI_ITER'/
      DATA MOTCLE(10)       / 'RELI_COEF'/
      DATA MOTCLE(11)       / 'PILO_PARA'/
      DATA MOTCLE(12)       / 'LAGR_ECAR'/
      DATA MOTCLE(13)       / 'LAGR_INCR'/
      DATA MOTCLE(14)       / 'LAGR_ITER'/
      DATA MOTCLE(15)       / 'MATR_ASSE'/
      DATA MOTCLE(16)       / 'ITER_DEBO'/
      DATA MOTCLE(17)       / 'CTCD_ITER'/
      DATA MOTCLE(18)       / 'CTCD_INFO'/
      DATA MOTCLE(19)       / 'CTCD_GEOM'/
      DATA MOTCLE(20)       / 'CTCD_NOEU'/
      DATA MOTCLE(23)       / 'CTCC_CONT'/
      DATA MOTCLE(22)       / 'CTCC_FROT'/
      DATA MOTCLE(21)       / 'CTCC_GEOM'/
      DATA MOTCLE(24)       / 'SUIV_1'/
      DATA MOTCLE(25)       / 'SUIV_2'/
      DATA MOTCLE(26)       / 'SUIV_3'/
      DATA MOTCLE(27)       / 'SUIV_4'/
      DATA MOTCLE(28)       / 'ITER_FETI'/
      DATA MOTCLE(29)       / '&&&&&&'/
      DATA MOTCLE(30)       / '&&&&&&'/
C
C ----------------------------------------------------------------------
C
      ICOD = 0

      DO 10 I = 1,ZDEF
        IF (COLONN(1:9).EQ.MOTCLE(I)) THEN
          ICOD     = I
        ENDIF
   10 CONTINUE

      END
