      FUNCTION JEXNOM ( NOMC , NOMO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NOMC , NOMO
      CHARACTER*32     JEXNOM
C     ------------------------------------------------------------------
      INTEGER          NUMEC
      COMMON /INUMJE/  NUMEC
      REAL*8           REELC
      COMMON /REELJE/  REELC
      CHARACTER*24     NOMEC
      COMMON /KNOMJE/  NOMEC
C     ------------------------------------------------------------------
      CHARACTER*24     CH24
      CHARACTER*8      CH8
      DATA             CH8      / '$$XNOM  ' /
C     ------------------------------------------------------------------
C
C J#DEB
      NUMEC  = 0
      REELC  = 0.D0
      NOMEC  = NOMO
      CH24   = NOMC
      JEXNOM( 1:24) = CH24
      JEXNOM(25:32) = CH8
C J#FIN
      END
