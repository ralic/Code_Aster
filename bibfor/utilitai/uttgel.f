      SUBROUTINE UTTGEL ( NOMTE, NDIM, TYPGEO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/05/2007   AUTEUR FERNANDES R.FERNANDES 
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
C RESPONSABLE ABBAS M.ABBAS
C  UTILITAIRE - TYPE GEOMETRIQUE D'UN ELEMENT FINI
C  **           *    *                **
C =====================================================================
C IN  NOMTE  : NOM DU TYPE D'ELEMENT FINI
C IN  NDIM   : DIMENSION DE L'ELEMENT FINI
C OUT TYPGEO : TYPE GEOMETRIQUE CORRESPONDANT
C              EN 2D : 'TR', 'QU'
C              EN 3D : 'HE', 'TE', 'PE', 'PY'
C ----------------------------------------------------------------------
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      INTEGER NDIM
      CHARACTER*2 TYPGEO
      CHARACTER*16 NOMTE
C
C 0.2. ==> VARIABLES LOCALES
C
      INTEGER IAUX
      CHARACTER*8 K8BID
C
C====
C 1. EN 2D
C====
C
      IF ( NDIM.EQ.2 ) THEN
C
C 1.1. ==> CAS PARTICULIER DES ELEMENTS EN THM
C
        IF ( NOMTE(1:11).EQ.'THH2M_AXIS_' ) THEN
          IAUX = 12
        ELSEIF ( NOMTE(1:10).EQ.'THHM_AXIS_' .OR.
     >           NOMTE(1:10).EQ.'HH2M_AXIS_' .OR.
     >           NOMTE(1:10).EQ.'THH2_AXIS_' ) THEN
          IAUX = 11
        ELSEIF ( NOMTE(1:9).EQ.'THH_AXIS_' .OR.
     >           NOMTE(1:9).EQ.'HHM_AXIS_' .OR.
     >           NOMTE(1:9).EQ.'THM_AXIS_' .OR.
     >           NOMTE(1:9).EQ.'THV_AXIS_' .OR.
     >           NOMTE(1:9).EQ.'HH2_AXIS_' ) THEN
          IAUX = 10
        ELSEIF ( NOMTE(1:8).EQ.'THH2M_DP' .OR.
     >           NOMTE(1:8).EQ.'HM_AXIS_' .OR.
     >           NOMTE(1:8).EQ.'HH_AXIS_' ) THEN
          IAUX = 9
        ELSEIF ( NOMTE(1:7).EQ.'THHM_DP' .OR.
     >           NOMTE(1:7).EQ.'HH2M_DP' .OR.
     >           NOMTE(1:7).EQ.'THH2_DP' ) THEN
          IAUX = 8
        ELSEIF ( NOMTE(1:6).EQ.'THM_DP' .OR.
     >           NOMTE(1:6).EQ.'THH_DP' .OR.
     >           NOMTE(1:6).EQ.'HHM_DP' .OR.
     >           NOMTE(1:6).EQ.'THV_DP' .OR.
     >           NOMTE(1:6).EQ.'HH2_DP' ) THEN
          IAUX = 7
        ELSEIF ( NOMTE(1:5).EQ.'HM_DP' .OR.
     >           NOMTE(1:5).EQ.'HH_DP' ) THEN
          IAUX = 6
C
C 1.2. ==> CAS GENERAL
C
        ELSE
          IAUX = 5
        ENDIF
C
C====
C 2. EN 3D
C====
C
      ELSEIF ( NDIM.EQ.3 ) THEN
C
C 2.1. ==> CAS PARTICULIER DES ELEMENTS EN THM
C
        IF ( NOMTE(1:6).EQ.'THH2M_' ) THEN
          IAUX = 7
        ELSEIF ( NOMTE(1:5).EQ.'THHM_' .OR.
     >           NOMTE(1:5).EQ.'THH2_' .OR.
     >           NOMTE(1:5).EQ.'HH2M_' ) THEN
          IAUX = 6
        ELSEIF ( NOMTE(1:4).EQ.'THM_' .OR.
     >           NOMTE(1:4).EQ.'THH_' .OR.
     >           NOMTE(1:4).EQ.'HHM_' .OR.
     >           NOMTE(1:4).EQ.'THV_' .OR.
     >           NOMTE(1:4).EQ.'HH2_' ) THEN
          IAUX = 5
        ELSEIF ( NOMTE(1:3).EQ.'HM_'  .OR.
     >           NOMTE(1:3).EQ.'HH_' ) THEN
          IAUX = 4
C
C 2.2. ==> CAS GENERAL
C
        ELSE
          IAUX = 6
        ENDIF
C
C====
C 3. SINON ON NE SAIT PAS FAIRE
C====
C
      ELSE
C
        CALL CODENT(NDIM,'G',K8BID)
        CALL U2MESK('F','UTILITAI_9',1,K8BID)
C
      ENDIF
C
C====
C 4. DECODAGE
C====
C
      TYPGEO = NOMTE(IAUX:IAUX+1)
      IF ( TYPGEO.EQ.'Q8' ) THEN
        TYPGEO = 'QU'
      ENDIF
C
      END
