      SUBROUTINE TYPTHM ( NOMTE, AXI, PERMAN, TYPMOD, NDIM )
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2006   AUTEUR MEUNIER S.MEUNIER 
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
C ----------------------------------------------------------------------
C IN  NOMTE  : NOM DU TRAITEMENT ELEMENTAIRE
C OUT AXI    : .TRUE. OU .FALSE. SELON LE CARACTERE AXISYMETRIQUE DU PB
C OUT PERMAN : .TRUE. OU .FALSE. SELON LE CARACTERE AXISYMETRIQUE DE
C              LA PARTIE HYDRAULIQUE
C OUT TYPMOD : 1. TYPE DE MODELISATION : AXI/D_PLAN/3D
C OUT NDIM   : DIMENSION DU PROBLEME (2 OU 3)
C ----------------------------------------------------------------------
      IMPLICIT      NONE
C
C     --- ARGUMENTS ---
      LOGICAL       AXI, PERMAN
      INTEGER       NDIM
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  NOMTE
C
C     --- VARIABLES LOCALES ---
C
      INTEGER IAUX
      INTEGER LXLGUT
      LOGICAL LTEATT
C
C =====================================================================
C --- BUT : DETERMINER LE TYPE DE MODELISATION (AXI/D_PLAN/3D) --------
C =====================================================================
      AXI       = .FALSE.
C
      IF ( LTEATT(' ','AXIS','OUI') ) THEN
        AXI       = .TRUE.
        TYPMOD(1) = 'AXIS    '
        NDIM      = 2
C
      ELSEIF ( NOMTE(6:7) .EQ.'DP' .OR.
     >         NOMTE(7:8) .EQ.'DP' .OR.
     >         NOMTE(4:5) .EQ.'DP' .OR.
     >         NOMTE(5:6) .EQ.'DP' .OR.
     >         NOMTE(6:9) .EQ.'D_PL' .OR.
     >         NOMTE(7:10) .EQ.'D_PL' .OR.
     >         NOMTE(4:7) .EQ.'D_PL' .OR.
     >         NOMTE(5:8) .EQ.'D_PL') THEN
        TYPMOD(1) = 'D_PLAN  '
        NDIM      = 2
C
      ELSE
        TYPMOD(1) = '3D      '
        NDIM      = 3
      ENDIF
C
C =====================================================================
C --- BUT : LA PARTIE HM EST-ELLE TRANSITOIRE OU PERMANENTE EN TEMPS ?
C =====================================================================
C
      IAUX = LXLGUT(NOMTE)
      IF ( NOMTE(IAUX-1:IAUX).EQ.'_P' ) THEN
       PERMAN = .TRUE.
      ELSE
       PERMAN = .FALSE.
      ENDIF
C
      END
