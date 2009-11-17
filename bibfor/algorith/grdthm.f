      SUBROUTINE GRDTHM(NOMTE,PERMAN,NDIM,MECANI,PRESS1,PRESS2,TEMPE,
     >                  DIMDEP,DIMDEF,DIMCON,NMEC,NP1,NP2)
      IMPLICIT     NONE
      LOGICAL      PERMAN
      INTEGER      MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5)
      INTEGER      DIMDEP,DIMDEF,DIMCON
      INTEGER      NDIM,NMEC,NP1,NP2
      CHARACTER*16 NOMTE
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/11/2009   AUTEUR REZETTE C.REZETTE 
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
C   TABLEAU MECANI :
C   MECANI(1) = 1 : IL Y A UNE EQUATION MECANIQUE
C               0 : SINON
C   MECANI(2) = ADRESSE DANS LES TABLEAUX DES DEFORMATIONS
C               GENERALISEES AU POINT DE GAUSS DES
C               DEFORMATIONS CORRESPONDANT A LA MECANIQUE
C   MECANI(3) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
C               GENERALISEES AU POINT DE GAUSS DES
C               CONTRAINTES CORRESPONDANT A LA MECANIQUE
C   MECANI(4) = NOMBRE DE DEFORMATIONS MECANIQUES
C   MECANI(5) = NOMBRE DE CONTRAINTES MECANIQUES
C
C   TABLEAU PRESS1 :
C   PRESS1(1) = 1 : IL Y A UNE EQUATION SUR LA PREMIERE PRESSION
C               0 : SINON
C   PRESS1(2) = NOMBRE DE PHASES POUR LE CONSTITUANT 1
C   PRESS1(3) = ADRESSE DANS LES TABLEAUX DES DEFORMATIONS
C               GENERALISEES AU POINT DE GAUSS
C   PRESS1(4) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
C               GENERALISEES AU POINT DE GAUSS DES CONTRAINTES
C               CORRESPONDANT A LA PREMIERE PHASE DU 1ER CONSTITUANT
C   PRESS1(5) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
C               GENERALISEES AU POINT DE GAUSS DES CONTRAINTES
C               CORRESPONDANT A LA DEUXIEME PHASE DU 1ER CONSTITUANT
C   PRESS1(6) = NOMBRE DE DEFORMATIONS PRESSION
C   PRESS1(7) = NOMBRE DE CONTRAINTES POUR CHAQUE PHASE DU CONSTITUANT 1
C
C   TABLEAU PRESS2 :
C   PRESS2(1) = 1 : IL Y A UNE EQUATION SUR LA SECONDE PRESSION
C               0 : SINON
C   PRESS2(2) = NOMBRE DE PHASES POUR LE CONSTITUANT 2
C   PRESS2(3) = ADRESSE DANS LES TABLEAUX DES DEFORMATIONS
C               GENERALISEES AU POINT DE GAUSS
C   PRESS2(4) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
C               GENERALISEES AU POINT DE GAUSS DES CONTRAINTES
C               CORRESPONDANT A LA PREMIERE PHASE DU 2ND CONSTITUANT
C   PRESS2(5) = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
C               GENERALISEES AU POINT DE GAUSS DES CONTRAINTES
C               CORRESPONDANT A LA DEUXIEME PHASE DU 2ND CONSTITUANT
C   PRESS2(6) = NOMBRE DE DEFORMATIONS PRESSION
C   PRESS2(7) = NOMBRE DE CONTRAINTES POUR CHAQUE PHASE DU CONSTITUANT 2
C
C   TABLEAU TEMPE :
C   TEMPE(1)  = 1 : IL Y A UNE EQUATION THERMIQUE
C               0 : SINON
C   TEMPE(2)  = ADRESSE DANS LES TABLEAUX DES DEFORMATIONS
C               GENERALISEES AU POINT DE GAUSS DES
C               DEFORMATIONS CORRESPONDANT A LA THERMIQUE
C   TEMPE(3)  = ADRESSE DANS LES TABLEAUX DES CONTRAINTES
C               GENERALISEES AU POINT DE GAUSS DES
C               CONTRAINTES CORRESPONDANT A LA THERMIQUE
C   TEMPE(4)  = NOMBRE DE DEFORMATIONS THERMIQUES
C   TEMPE(5)  = NOMBRE DE CONTRAINTES THERMIQUES
C
      INTEGER IAUX
C
C====
C 1. REPERAGE DES CALCULS A FAIRE : MECANIQUE, HYDRAULIQUE, ETC.
C====
C
C =====================================================================
C --- INITIALISATION DES GRANDEURS PRESENTES SELON LA MODELISATION ----
C --- EN THM ----------------------------------------------------------
C =====================================================================
C --- SI MODELISATION = THHM ------------------------------------------
C =====================================================================
      MECANI(1)=0
      PRESS1(1)=0
      PRESS2(1)=0
      PRESS1(2)=0
      PRESS2(2)=0
      TEMPE(1) =0
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
C --- SI MODELISATION = HH -------------------------------------------
C =====================================================================
      IF (NOMTE(1:3).EQ.'HH_') THEN
         MECANI(1) = 0
         PRESS1(1) = 1
         PRESS2(1) = 1
         TEMPE(1)  = 0
         PRESS1(2) = 2
         PRESS2(2) = 1
      END IF
C =====================================================================
C --- SI MODELISATION = HH2 -------------------------------------------
C =====================================================================
      IF (NOMTE(1:4).EQ.'HH2_') THEN
         MECANI(1) = 0
         PRESS1(1) = 1
         PRESS2(1) = 1
         TEMPE(1)  = 0
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
C
C===
C 2. CALCUL PREALABLE DES ADRESSES LOCALES DES VARIABLES
C =====================================================================
C 2.1. LES AUTRES VALEURS DES TABLEAUX MECANI,PRESS1,PRESS2,TEMPE -----
C --- SE DEFINISSENT AUTOMATIQUEMENT : --------------------------------
C --- NOMBRE DE DEFORMATIONS ET DE CONTRAINTES DE CHAQUE PROBLEME -----
C =====================================================================
C====
C
      IF (MECANI(1).EQ.1) THEN
        MECANI(4) = NDIM + 6
        MECANI(5) = 7
        NMEC = NDIM
      ELSE
        MECANI(4) = 0
        MECANI(5) = 0
        NMEC = 0
      END IF
C
C  EN MODE PERMANENT POUR LES PROBLEMES HYDRAULIQUES ET/OU THERMIQUE,
C  IL N'Y A PLUS DE VARIABLES SCALAIRES. IL NE RESTE QUE LES FLUX.
C
      IF ( PERMAN ) THEN
        IAUX = 0
      ELSE
        IAUX = 1
      ENDIF

      IF (PRESS1(1).EQ.1) THEN
        PRESS1(6) = 1 + NDIM
        PRESS1(7) = IAUX + NDIM
        NP1 = 1
        IF (TEMPE(1).EQ.1) PRESS1(7) = PRESS1(7) + 1
      ELSE
        PRESS1(6) = 0
        PRESS1(7) = 0
        NP1 = 0
      END IF

      IF (PRESS2(1).EQ.1) THEN
        PRESS2(6) = 1 + NDIM
        PRESS2(7) = IAUX + NDIM
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
C
C =====================================================================
C 2.2. ADRESSE DES SOUS-TABLEAUX DANS LES DEFORMATIONS PHYSIQUES, LES -
C      DEFORMATIONS GENERALISEES ET LES CONTRAINTES GENERALISEES ------
C =====================================================================
C
C 2.2.1. ==> DEFORMATIONS ET CONTRAINTES EN MECANIQUE
C
      IF (MECANI(1).EQ.1) THEN
        MECANI(2) = 1
        MECANI(3) = 1
      ELSE
        MECANI(2) = 0
        MECANI(3) = 0
      END IF
C
C 2.2.2. ==> DEFORMATIONS ET CONTRAINTES POUR LA PREMIERE PRESSION
C
      IF (PRESS1(1).EQ.1) THEN
        PRESS1(3) = MECANI(4) + 1
        PRESS1(4) = MECANI(5) + 1
        IF (PRESS1(2).EQ.2) PRESS1(5) = PRESS1(4) + PRESS1(7)
      END IF
C
C 2.2.3. ==> DEFORMATIONS ET CONTRAINTES POUR LA SECONDE PRESSION
C
      IF (PRESS2(1).EQ.1) THEN
        PRESS2(3) = PRESS1(3) + PRESS1(6)
        PRESS2(4) = PRESS1(4) + PRESS1(2)*PRESS1(7)
        IF (PRESS2(2).EQ.2) PRESS2(5) = PRESS2(4) + PRESS2(7)
      END IF
C
C 2.2.4. ==> DEFORMATIONS ET CONTRAINTES POUR LA TEMPERATURE
C
      IF (TEMPE(1).EQ.1) THEN
        TEMPE(2) = MECANI(4) + PRESS1(6) + PRESS2(6) + 1
        TEMPE(3) = MECANI(5) +
     &             PRESS1(2)*PRESS1(7) +
     &             PRESS2(2)*PRESS2(7) +
     &             1
      END IF
C
C =====================================================================
C 2.3. DIMENSION DES DEPLACEMENTS, DEFORMATIONS ET CONTRAINTES --------
C =====================================================================
C
      DIMDEP = NDIM*MECANI(1) + PRESS1(1) + PRESS2(1) + TEMPE(1)
      DIMDEF = MECANI(4) + PRESS1(6) + PRESS2(6) + TEMPE(4)
      DIMCON = MECANI(5) + PRESS1(2)*PRESS1(7) + PRESS2(2)*PRESS2(7) +
     &         TEMPE(5)
C =====================================================================
C
      END
