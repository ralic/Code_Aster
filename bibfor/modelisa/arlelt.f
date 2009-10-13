      FUNCTION ARLELT(NOMTE,MOD,CIN)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/10/2009   AUTEUR CAO B.CAO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      LOGICAL      ARLELT
      CHARACTER*16 NOMTE
      CHARACTER*16 MOD
      CHARACTER*16 CIN
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C RETOURNE .TRUE. SI LE TYPE D'ELEMENT EST RECONNU COMME ETANT VALIDE
C POUR ARLEQUIN ET DONNE LE TYPE DE MODELISATION ET DE CINEMATIQUE
C
C ----------------------------------------------------------------------
C
C
C NB: EXCLU LES BORDS/ARETES
C
C IN  NOMTE  : NOM DU TE
C OUT MOD    : TYPE DE MODELISATION
C              'DPLAN'  ELEMENT DE DEFORMATIONS PLANES
C              'CPLAN'  ELEMENT DE CONTRAINTES PLANES
C              'AXIS'   ELEMENT AXISYMETRIQUE
C              '3D'     ELEMENT 3D POUR SOLIDE
C                       DKT/DST/COQUE_3D/Q4G POUR COQUE
C OUT CIN    : TYPE DE CINEMATIQUE
C              'SOLIDE'
C              'COQUE'
C
C ----------------------------------------------------------------------
C
      ARLELT = .FALSE.
C
      IF (NOMTE(1:4).EQ.'MEDP') THEN
        MOD = 'DPLAN'
        CIN = 'SOLIDE'
        IF ((NOMTE(5:6).EQ.'TR').OR.(NOMTE(5:6).EQ.'QU')) THEN
          ARLELT = .TRUE.
        ELSE
          ARLELT = .FALSE.
        ENDIF
      ELSEIF (NOMTE(1:4).EQ.'MECP') THEN
        MOD = 'CPLAN'
        CIN = 'SOLIDE'
        IF ((NOMTE(5:6).EQ.'TR').OR.(NOMTE(5:6).EQ.'QU')) THEN
          ARLELT = .TRUE.
        ELSE
          ARLELT = .FALSE.
        ENDIF
      ELSEIF (NOMTE(1:4).EQ.'MEAX') THEN
        MOD = 'AXIS'
        CIN = 'SOLIDE'
        IF ((NOMTE(5:6).EQ.'TR').OR.(NOMTE(5:6).EQ.'QU')) THEN
          ARLELT = .TRUE.
        ELSE
          ARLELT = .FALSE.
        ENDIF
      ELSEIF (NOMTE(1:5).EQ.'MECA_') THEN
        MOD = '3D'
        CIN = 'SOLIDE'
        IF ((NOMTE(6:9).EQ.'TETR').OR.
     &      (NOMTE(6:9).EQ.'PENT').OR.
     &      (NOMTE(6:9).EQ.'HEXA')) THEN
          ARLELT = .TRUE.
        ELSE
          ARLELT = .FALSE.
        ENDIF
      ELSEIF (NOMTE(1:4).EQ.'METD') THEN
        MOD    = 'DPLAN'
        CIN    = 'COQUE'
        ARLELT = .TRUE.
      ELSEIF (NOMTE(1:4).EQ.'METC') THEN
        MOD    = 'CPLAN'
        CIN    = 'COQUE'
        ARLELT = .TRUE.
      ELSEIF (NOMTE(1:4).EQ.'MECX') THEN
        MOD    = 'AXIS'
        CIN    = 'COQUE'
        ARLELT = .TRUE.
      ELSEIF (NOMTE(1:4).EQ.'MEDK') THEN
        MOD    = '3D'
        CIN    = 'COQUE'
        ARLELT = .TRUE.
      ELSEIF (NOMTE(1:4).EQ.'MEDS') THEN
        MOD    = '3D'
        CIN    = 'COQUE'
        ARLELT = .TRUE.
      ELSEIF (NOMTE(1:4).EQ.'MEQ4') THEN
        MOD    = '3D'
        CIN    = 'COQUE'
        ARLELT = .TRUE.
      ELSEIF (NOMTE(1:4).EQ.'MEC3') THEN
        MOD    = '3D'
        CIN    = 'COQUE'
        ARLELT = .TRUE.
      ELSEIF (NOMTE(1:4).EQ.'MIPL') THEN
        MOD    = 'DPLANIN'
        CIN    = 'SOLIDEMI'
        IF ((NOMTE(5:6).EQ.'TR').OR.(NOMTE(5:6).EQ.'QU')) THEN
          ARLELT = .TRUE.
        ELSE
          ARLELT = .FALSE.
        ENDIF
      ELSE
        ARLELT = .FALSE.
      ENDIF
      END
