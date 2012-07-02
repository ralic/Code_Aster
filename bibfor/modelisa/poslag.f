      SUBROUTINE POSLAG(TYPLAZ,ILAG1,ILAG2)
      IMPLICIT NONE
      CHARACTER*(*) TYPLAZ
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
C     BUT : INDICATEUR DE LA POSITION A AFFECTER AUX MULTIPLICATEURS
C           DE LAGRANGE ASSOCIES A UNE RELATION
C
C TYPLAG        - IN - K2  - : TYPE DES MULTIPLICATEURS DE LAGRANGE
C                              ASSOCIES A LA RELATION :
C                              SI = '12'  LE PREMIER LAGRANGE EST AVANT
C                                         LE NOEUD PHYSIQUE
C                                         LE SECOND LAGRANGE EST APRES
C                              SI = '22'  LE PREMIER LAGRANGE EST APRES
C                                         LE NOEUD PHYSIQUE
C                                         LE SECOND LAGRANGE EST APRES
C ILAG1         - OUT - I  - : INDICATEUR DE POSITION DU PREMIER
C                              LAGRANGE :
C                              SI AVANT LE NOEUD PHYSIQUE ILAG1 = +1
C                              SI APRES LE NOEUD PHYSIQUE ILAG1 = -1
C ILAG2         - OUT - I  - : INDICATEUR DE POSITION DU SECOND
C                              LAGRANGE :
C                              SI APRES LE NOEUD PHYSIQUE ILAG2 = -2
C                              (C'EST LA SEULE POSSIBILITE POUR LE
C                               MOMENT)
C-----------------------------------------------------------------------
C
      CHARACTER*2 TYPLAG
C
C-----------------------------------------------------------------------
      INTEGER ILAG1 ,ILAG2 
C-----------------------------------------------------------------------
      TYPLAG = TYPLAZ
C
      IF (TYPLAG(1:1).EQ.'1') THEN
          ILAG1 =  1
      ELSEIF (TYPLAG(1:1).EQ.'2') THEN
          ILAG1 = -1
      ELSE
         CALL U2MESK('F','MODELISA6_30',1,TYPLAG)
      ENDIF
C
      IF (TYPLAG(2:2).EQ.'2') THEN
          ILAG2 = -2
      ELSE
         CALL U2MESK('F','MODELISA6_30',1,TYPLAG)
      ENDIF
C
      END
