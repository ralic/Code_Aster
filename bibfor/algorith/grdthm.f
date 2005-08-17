      SUBROUTINE GRDTHM(NOMTE,NDIM,MECANI,PRESS1,PRESS2,TEMPE,DIMDEF,
     +                  DIMCON,NMEC,NP1,NP2)
      IMPLICIT     NONE
      INTEGER      MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),DIMDEF,DIMCON
      INTEGER      NDIM,NMEC,NP1,NP2
      CHARACTER*3  MODINT
      CHARACTER*16 NOMTE
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/08/2005   AUTEUR ROMEO R.FERNANDES 
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
C =====================================================================
C --- INITIALISATION DES GRANDEURS PRESENTES SELON LA MODELISATION ----
C --- EN THM ----------------------------------------------------------
C =====================================================================
C --- SI MODELISATION = THHM ------------------------------------------
C =====================================================================
      IF (NOMTE(1:4).EQ.'THHM') THEN
         MECANI(1) = 1
         PRESS1(1) = 1
         PRESS2(1) = 1
         TEMPE(1)  = 1
         PRESS1(2) = 2
         PRESS2(2) = 1
      END IF
C =====================================================================
C --- SI MODELISATION = THH2M -----------------------------------------
C =====================================================================
      IF (NOMTE(1:5).EQ.'THH2M') THEN
         MECANI(1) = 1
         PRESS1(1) = 1
         PRESS2(1) = 1
         TEMPE(1)  = 1
         PRESS1(2) = 2
         PRESS2(2) = 2
      END IF
C =====================================================================
C --- SI MODELISATION = HM --------------------------------------------
C =====================================================================
      IF (NOMTE(1:2).EQ.'HM') THEN
         MECANI(1) = 1
         PRESS1(1) = 1
         PRESS2(1) = 0
         TEMPE(1)  = 0
         PRESS1(2) = 1
         PRESS2(2) = 0
      END IF
C =====================================================================
C --- SI MODELISATION = HHM -------------------------------------------
C =====================================================================
C --- ON RESERVE DE LA PLACE POUR LES TROIS CONSTITUANTS. MAIS IL EST -
C --- POSSIBLE DE N'EN REMPLIR QUE DEUX EN LAISSANT LE DERNIER A ZERO -
C =====================================================================
      IF (NOMTE(1:3).EQ.'HHM') THEN
         MECANI(1) = 1
         PRESS1(1) = 1
         PRESS2(1) = 1
         TEMPE(1)  = 0
         PRESS1(2) = 2
         PRESS2(2) = 1
      END IF
C =====================================================================
C --- SI MODELISATION = HH2M ------------------------------------------
C =====================================================================
C --- ON RESERVE DE LA PLACE POUR LES TROIS CONSTITUANTS. MAIS IL EST -
C --- POSSIBLE DE N'EN REMPLIR QUE DEUX EN LAISSANT LE DERNIER A ZERO -
C =====================================================================
      IF (NOMTE(1:4).EQ.'HH2M') THEN
         MECANI(1) = 1
         PRESS1(1) = 1
         PRESS2(1) = 1
         TEMPE(1)  = 0
         PRESS1(2) = 2
         PRESS2(2) = 2
      END IF
C =====================================================================
C --- SI MODELISATION = THH -------------------------------------------
C =====================================================================
      IF (NOMTE(1:4).EQ.'THH_') THEN
         MECANI(1) = 0
         PRESS1(1) = 1
         PRESS2(1) = 1
         TEMPE(1)  = 1
         PRESS1(2) = 2
         PRESS2(2) = 1
      END IF
C =====================================================================
C --- SI MODELISATION = THH2 ------------------------------------------
C =====================================================================
      IF (NOMTE(1:5).EQ.'THH2_') THEN
         MECANI(1) = 0
         PRESS1(1) = 1
         PRESS2(1) = 1
         TEMPE(1)  = 1
         PRESS1(2) = 2
         PRESS2(2) = 2
      END IF
C =====================================================================
C --- SI MODELISATION = THV -------------------------------------------
C =====================================================================
      IF (NOMTE(1:4).EQ.'THV_') THEN
         MECANI(1) = 0
         PRESS1(1) = 1
         PRESS2(1) = 0
         TEMPE(1)  = 1
         PRESS1(2) = 2
         PRESS2(2) = 0
      END IF
C =====================================================================
C --- SI MODELISATION = THM -------------------------------------------
C =====================================================================
      IF (NOMTE(1:4).EQ.'THM_') THEN
         MECANI(1) = 1
         PRESS1(1) = 1
         PRESS2(1) = 0
         TEMPE(1)  = 1
         PRESS1(2) = 1
         PRESS2(2) = 0
      END IF
C =====================================================================
C --- LES AUTRES VALEURS DES TABLEAUX MECANI,PRESS1,PRESS2,TEMPE ------
C --- SE DEFINISSENT AUTOMATIQUEMENT : --------------------------------
C --- NOMBRE DE DEFORMATIONS ET DE CONTRAINTES DE CHAQUE PROBLEME -----
C =====================================================================
      IF (MECANI(1).EQ.1) THEN
        MECANI(4) = NDIM + 6
        MECANI(5) = 7
        NMEC = NDIM
      ELSE
        MECANI(4) = 0
        MECANI(5) = 0
        NMEC = 0
      END IF

      IF (PRESS1(1).EQ.1) THEN
        PRESS1(6) = 1 + NDIM
        PRESS1(7) = 1 + NDIM
        NP1 = 1
        IF (TEMPE(1).EQ.1) PRESS1(7) = PRESS1(7) + 1
      ELSE
        PRESS1(6) = 0
        PRESS1(7) = 0
        NP1 = 0
      END IF

      IF (PRESS2(1).EQ.1) THEN
        PRESS2(6) = 1 + NDIM
        PRESS2(7) = 1 + NDIM
        NP2 = 1
        IF (TEMPE(1).EQ.1) PRESS2(7) = PRESS2(7) + 1
      ELSE
        PRESS2(6) = 0
        PRESS2(7) = 0
        NP2 = 0
      END IF

      IF (TEMPE(1).EQ.1) THEN
        TEMPE(4) = 1 + NDIM
        TEMPE(5) = 1 + NDIM
      ELSE
        TEMPE(4) = 0
        TEMPE(5) = 0
      END IF
C =====================================================================
C --- ADRESSE DES DEFORMATIONS ET DES CONTRAINTES ---------------------
C =====================================================================
      IF (MECANI(1).EQ.1) THEN
        MECANI(2) = 1
        MECANI(3) = 1
      ELSE
        MECANI(2) = 0
        MECANI(3) = 0
      END IF

      IF (PRESS1(1).EQ.1) THEN
        PRESS1(3) = MECANI(4) + 1
        PRESS1(4) = MECANI(5) + 1
        IF (PRESS1(2).EQ.2) PRESS1(5) = PRESS1(4) + PRESS1(7)
      END IF

      IF (PRESS2(1).EQ.1) THEN
        PRESS2(3) = PRESS1(3) + PRESS1(6)
        PRESS2(4) = PRESS1(4) + PRESS1(2)*PRESS1(7)
        IF (PRESS2(2).EQ.2) PRESS2(5) = PRESS2(4) + PRESS2(7)
      END IF

      IF (TEMPE(1).EQ.1) THEN
        TEMPE(2) = MECANI(4) + PRESS1(6) + PRESS2(6) + 1
        TEMPE(3) = MECANI(5) + PRESS1(2)*PRESS1(7) +
     &             PRESS2(2)*PRESS2(7) + 1
      END IF
C =====================================================================
C --- DIMENSION DES DEFORMATIONS ET CONTRAINTES -----------------------
C =====================================================================
      DIMDEF = MECANI(4) + PRESS1(6) + PRESS2(6) + TEMPE(4)
      DIMCON = MECANI(5) + PRESS1(2)*PRESS1(7) + PRESS2(2)*PRESS2(7) +
     &         TEMPE(5)
C =====================================================================
      END
