      SUBROUTINE RVOPTI(NCH19,NOMGD,TYPEGD,OPTION)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      CHARACTER*19 NCH19
      CHARACTER*16 OPTION
      CHARACTER*8  NOMGD
      CHARACTER*4  TYPEGD
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 20/09/1999   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     RECUPERATION DE L' OPTION DE CALCUL DU CHAMP_19 DE NOM NCH19
C     ------------------------------------------------------------------
C IN  NCH19  : K : NOM DU CHAMP_19
C IN  NOMGD  : K : NOM DE LA GRANDEUR
C IN  TYPEGD : K : VAUT 'CHNO' OU 'CHLM'
C OUT OPTION : K : NOM OPTION CALC_ELEM POUR CHLM OU ADAPTATION CHNO
C     ------------------------------------------------------------------
C
      INTEGER N
C
C======================================================================
C
      OPTION = '                '
C
      IF ( TYPEGD .EQ. 'CHML' ) THEN
         CALL DISMOI('F','NOM_OPTION',NCH19,'CHAMP',N,OPTION,IERR)
      ELSE IF ( TYPEGD .EQ. 'CHNO' ) THEN
         IF ( NOMGD .EQ. 'SIEF_R' ) THEN
            OPTION = 'SIEF_NOEU_DEPL  '
         ELSE IF ( NOMGD .EQ. 'EPSI_R' ) THEN
            OPTION = 'EPSI_NOEU_DEPL  '
         ELSE IF ( NOMGD .EQ. 'FLUX_R' ) THEN
            OPTION = 'FLUX_NOEU_TEMP  '
         ELSE IF ( NOMGD .EQ. 'DEPL_R' ) THEN
            OPTION = 'DEPL_NOEU_DEPL  '
         ELSE IF ( NOMGD .EQ. 'FORC_R' ) THEN
            OPTION = 'FORC_NOEU_FORC  '
         ENDIF
      ENDIF
      END
