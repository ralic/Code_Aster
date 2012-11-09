      SUBROUTINE FONOEI(NDIM,DT,FNOEVO,DIMDEF,DIMCON,YAMEC,YAP1,YAP2,
     &                  YATE,ADDEME,ADDEP1,ADDEP2,ADDETE,ADDLH1,
     &                  ADCOME,ADCP11,ADCP12,ADCP21,ADCP22,
     &                  ADCOTE,ADCOP1,ADCOP2,NBPHA1,NBPHA2,CONGEM,
     &                  R)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C TOLE CRP_21
C ======================================================================
C
C    BUT : CALCUL DU TERME DE CHARGEMENT EXTERIEUR AUX POINTS
C    D'INTEGRATION
C
C
C ======================================================================
C IN NDIM  : DIMENSION ESPACE
C IN DT    : INCREMENT DE TEMPS
C IN FNOEVO : APPLE DEPUIS STAT_NON_LINE
C IN DIMDEF : DIMENSION DEFORMATIONS GENERALISEES
C IN DIMCON : DIMENSION VECTEUR CONTRAINTES GENERALISEES
C IN YAMEC  : =1 S'IL Y A UNE EQUATION DE DEFORMATION MECANIQUE
C IN YAP1   : =1 S'IL Y A UNE EQUATION DE PRESSION DE FLUIDE
C IN YAP2   : =1 S'IL Y A UNE DEUXIEME EQUATION DE PRESSION DE FLUIDE
C IN YATE   : =1 S'IL YA UNE EQUATION THERMIQUE
C IN ADDEME : ADRESSE DES DEFORMATIONS MECANIQUES
C IN ADDEP1 : ADRESSE DES DEFORMATIONS CORRESPONDANT A LA PRESSION 1
C IN ADDEP2 : ADRESSE DES DEFORMATIONS CORRESPONDANT A LA PRESSION 2
C IN ADDETE : ADRESSE DES DEFORMATIONS THERMIQUES
C IN ADCOME : ADRESSE DES CONTRAINTES MECANIQUES
C IN ADCP11 : ADRESSE DES CONTRAINTES FLUIDE 1 PHASE 1
C IN ADCP12 : ADRESSE DES CONTRAINTES FLUIDE 1 PHASE 2
C IN ADCOP1 : ADRESSE DES CONTRAINTES CORRESPONDANT AU SAUT DE PRE1
C IN ADCP21 : ADRESSE DES CONTRAINTES FLUIDE 2 PHASE 1
C IN ADCP22 : ADRESSE DES CONTRAINTES FLUIDE 2 PHASE 2
C IN ADCOP2 : ADRESSE DES CONTRAINTES CORRESPONDANT AU SAUT DE PRE2
C IN ADCOTE : ADRESSE DES CONTRAINTES THERMIQUES
C IN NBPHA1 : = 1 SI DEUXIEME PHASE
C IN NBPHA2 : = 1 SI DEUXIEME PHASE
C IN CONGEM : CONTRAINTES GENERALISEES AU TEMPS MOINS
C ======================================================================
C OUT R : VECTEUR FORCES EXTERIEURES
C ======================================================================
      IMPLICIT     NONE
      LOGICAL      FNOEVO
      INTEGER      DIMDEF,DIMCON
      INTEGER      NDIM
      REAL*8       DT,CONGEM(DIMCON),R(DIMDEF)
C ======================================================================
      INTEGER      YAMEC,YAP1,YAP2,YATE,ADDEME,ADCOME,NBPHA1
      INTEGER      ADDEP1,ADCP11,ADCP12,NBPHA2,ADDEP2,ADCP21,ADCP22
      INTEGER      ADCOTE,I,ADCOP1,ADCOP2,F,ADDETE,ADDLH1

C ======================================================================
C --- CALCUL DU RESIDU R -----------------------------------------------
C ======================================================================
C ======================================================================
C -------------------------------------
C ======================================================================
            DO 6 I=1,NDIM
               R(ADDEME+I-1)= R(ADDEME+I-1)+CONGEM(ADCOME-1+I)
 6          CONTINUE
            R(ADDEME) = R(ADDEME) + CONGEM(ADCOME+NDIM)

C ======================================================================
C --- CONTRIBUTION A R DEPENDANTE DE YAP1 ------------------------------
C ======================================================================
            IF(YAP1.EQ.1) THEN
               DO 8 F=1,2
                R(ADDLH1+1+F)=R(ADDLH1+1+F)+CONGEM(ADCOP1+1+F)
 8             CONTINUE
            END IF
C ======================================================================
C --- CONTRIBUTIONS A R DEPENDANTE DE YAP2 -----------------------------
C ======================================================================
            IF(YAP2.EQ.1) THEN
               DO 11 F=1,2
                R(ADDEP2+NDIM+1+F)=R(ADDEP1+NDIM+1+F)+CONGEM(ADCOP2+1+F)
 11            CONTINUE
            END IF

C ======================================================================
         IF(FNOEVO) THEN
C ======================================================================
C --- TERMES DEPENDANT DE DT DANS FORC_NODA POUR STAT_NON_LINE ---------
C ======================================================================
            IF(YAP1.EQ.1) THEN
               DO 12 F=1,2
                 R(ADDEP1) = R(ADDEP1) + DT*CONGEM(ADCOP1-1+F)
                 R(ADDLH1-1+F) = -DT*CONGEM(ADCOP1-1+F)
 12            CONTINUE
               DO 13 I=1,NDIM-1
                 R(ADDEP1+I) = DT*CONGEM(ADCP11+I)
 13            CONTINUE
            END IF
          END IF

C ======================================================================
      END
