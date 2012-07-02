      SUBROUTINE CALAMR(PHIB24,PHI1J,BI,NUM,J,CIJ2)
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C---------------------------------------------------------------------
      IMPLICIT NONE
C
CROUTINE STOCKANT LE VECTEUR PRESSION ISSUE D UNE RESOLUTION DE LAPLACE
C IN : VECSOL : VECTEUR SOLUTION K*
C OUT : VESTO : VECTEUR STOCKE K*
C---------------------------------------------------------------------
      INCLUDE 'jeveux.h'
      CHARACTER*3   INCR
      CHARACTER*8   K8BID
      CHARACTER*14  NUM
      CHARACTER*19  PHI1J,PH1PLO,PHIB19
      CHARACTER*24  PHIB24,BI
      REAL*8        CIJ2
C ---------------------------------------------------------------

C--------- CALCUL DE LA MASSE AJOUTEE POUR UN FLUIDE-------------
C-------------------N AYANT PAS FORCEMENT------------------------
C-----------------LA MEME DENSITE PARTOUT------------------------

C-----------PLONGEMENT DE LA PRESSION----
C---------------SUR LE MODELE THERMIQUE D INTERFACE--------------
C-----------------------------------------------------------------------
      INTEGER J 
C-----------------------------------------------------------------------
       PHIB19='PHIB19'
       INCR='BID'
       CALL CHNUCN(PHIB24(1:19),NUM,0,K8BID,'V',PHIB19)
       CALL CODENT(J,'D0',INCR)
       PH1PLO='PHPLO'//INCR
       CALL CHNUCN(PHI1J,NUM,0,K8BID,'V',PH1PLO)
C-------------------CALCUL DE L'AMORTISSEMENT AJOUTE-------------------
C---------------SUR LE MODELE THERMIQUE D INTERFACE--------------

       CALL CALCI(PHIB19,PH1PLO,BI(1:19),CIJ2)
C       CALL JEDETC('V',PH1PLO,1)
C       CALL JEDETC('V',BI(1:19),1)

      END
