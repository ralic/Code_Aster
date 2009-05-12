      SUBROUTINE XMELET(NOMTE,TYPMA,ESC,MAIT,ELC,
     &                 NDIM,NDDL,NNE,NNM,NNC,MALIN)
     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/05/2009   AUTEUR MAZET S.MAZET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      CHARACTER*16 NOMTE   
      CHARACTER*8  TYPMA,ESC,MAIT,ELC
      INTEGER     NDIM,NDDL,NNE,NNM,NNC
      LOGICAL     MALIN
C ----------------------------------------------------------------------
C ROUTINE APPELLEE PAR : TE0366/TE0367
C ----------------------------------------------------------------------
C ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
C TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
C ----------------------------------------------------------------------
C RETOURNE QUELQUES INFOS SUR LES ELEMENTS DE CONTACT FORMES ENTRE
C DEUX ELEMENTS  X-FEM
C
C IN  NOMTE  : NOM DU TE DE L'ELEMENT DE CONTACT EN JEU
C OUT TYPMA  : NOM DE LA MAILLE ESCLAVE D'ORIGINE
C OUT ESC    : NOM DE LA MAILLE ESCLAVE
C OUT MAIT   : NOM DE LA MAILLE MAITRE
C OUT ELC    : NOM DE LA MAILLE DE CONTACT
C OUT NDIM   : DIMENSION DE LA MAILLE DE CONTACT
C OUT NDDL   : NOMBRE TOTAL DE DEGRES DE LIBERTE DE LA MAILLE DE CONTACT
C OUT NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C OUT NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C OUT NNC    : NOMBRE DE NOEUDS DE LA MAILLE DE CONTACT
C OUT MALIN  : INDIQUE SI LA MAILLE EST LINEAIRE
C
C ----------------------------------------------------------------------
C
      MALIN= .TRUE.     
      IF ((NOMTE.EQ.'MECPT6T3_XH').OR.(NOMTE.EQ.'MEDPT6T3_XH')) THEN
        TYPMA= 'TRIA6'
        ESC  = 'TR3'
        MAIT = 'TR3'
        ELC  = 'SE2'
        NDIM = 2
        NDDL = 36
        NNE  = 6
        NNM  = 3
        NNC  = 2
        MALIN= .FALSE.
C
      ELSE IF((NOMTE.EQ.'MECPQ8Q4_XH').OR.(NOMTE.EQ.'MEDPQ8Q4_XH')) THEN
        TYPMA= 'QUAD8'
        ESC  = 'QU4'
        MAIT = 'QU4'
        ELC  = 'SE2'
        NDIM = 2
        NDDL = 48
        NNE  = 8
        NNM  = 4
        NNC  = 2
        MALIN= .FALSE.
C
      ELSE IF((NOMTE.EQ.'MECPT3T3_XH').OR.(NOMTE.EQ.'MEDPT3T3_XH')) THEN
        TYPMA= 'TRIA3'
        ESC  = 'TR3'
        MAIT = 'TR3'
        ELC  = 'SE2'
        NDIM = 2
        NDDL = 30
        NNE  = 3
        NNM  = 3
        NNC  = 2
C
      ELSE IF((NOMTE.EQ.'MECPQ4Q4_XH').OR.(NOMTE.EQ.'MEDPQ4Q4_XH')) THEN
        TYPMA= 'QUAD4'
        ESC  = 'QU4'
        MAIT = 'QU4'
        ELC  = 'SE2'
        NDIM = 2
        NDDL = 40
        NNE  = 4
        NNM  = 4
        NNC  = 2
C
      ELSE IF((NOMTE.EQ.'MECPT3Q4_XH').OR.(NOMTE.EQ.'MEDPT3Q4_XH')) THEN
        TYPMA= 'TRIA3'
        ESC  = 'TR3'
        MAIT = 'QU4'
        ELC  = 'SE2'
        NDIM = 2
        NDDL = 34
        NNE  = 3
        NNM  = 4
        NNC  = 2
C
      ELSE IF((NOMTE.EQ.'MECPQ4T3_XH').OR.(NOMTE.EQ.'MEDPQ4T3_XH')) THEN
        TYPMA= 'QUAD4'
        ESC  = 'QU4'
        MAIT = 'TR3'
        ELC  = 'SE2'
        NDIM = 2
        NDDL = 40
        NNE  = 4
        NNM  = 3
        NNC  = 2
C
      ELSE IF (NOMTE.EQ.'MEH8H8_XH') THEN
        TYPMA= 'HEXA8'
        ESC  = 'HE8'
        MAIT = 'HE8'
        ELC  = 'TR3'
        NDIM = 3
        NDDL = 120
        NNE  = 8
        NNM  = 8
        NNC  = 3
C
      ELSE IF (NOMTE.EQ.'MEP6P6_XH') THEN
        TYPMA= 'PENTA6'
        ESC  = 'PE6'
        MAIT = 'PE6'
        ELC  = 'TR3'
        NDIM = 3
        NDDL = 90
        NNE  = 6
        NNM  = 6
        NNC  = 3
C   
      ELSE IF (NOMTE.EQ.'MET4T4_XH') THEN
        TYPMA= 'TETRA4'
        ESC  = 'TE4'
        MAIT = 'TE4'
        ELC  = 'TR3'
        NDIM = 3
        NDDL = 60
        NNE  = 4
        NNM  = 4
        NNC  = 3
C  
      ELSE
        CALL U2MESK('F','DVP_4',1,NOMTE)
      END IF
      END
