      SUBROUTINE GCUTYB(NOMCO , TYPCO )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NOMCO , TYPCO
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 04/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     RETOURNE DE TYPE DE BASE D'UN IDENTIFICATEUR
C     ------------------------------------------------------------------
      CHARACTER*2  CLTYPE(6),BATYPE(0:6)
      CHARACTER*16 TYPCON
C     ------------------------------------------------------------------
      DATA        CLTYPE /       'IS', 'R8', 'NO', 'TX', 'C8', 'LS'/
      DATA        BATYPE / 'CO', 'IS', 'R8', 'CO', 'TX', 'C8', 'LS'/
C     ------------------------------------------------------------------
      CALL GETTCO(NOMCO, TYPCON )
      CALL UTREMT(TYPCON,CLTYPE,6,IPOS)
      TYPCO = BATYPE(IPOS)
      END
