      SUBROUTINE INITFI
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     ------------------------------------------------------------------
C     INITIALISATION DU GESTIONNAIRE DE FICHIERS  (GESTFI)
C     ------------------------------------------------------------------
C     --- PAS D'ARGUMENTS ---
C     ------------------------------------------------------------------
C MODIF UTILIFOR  DATE 05/12/2001   AUTEUR VABHHTS J.PELLET 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN INITFI
C     ------------------------------------------------------------------
C
      PARAMETER              (MXFILE=100)
      CHARACTER*16      DDNAME(MXFILE)
      INTEGER          FIRST, UNITES(MXFILE) , NBFILE
      COMMON/ CN01FI / FIRST, UNITES         , NBFILE
      COMMON/ CC01FI / DDNAME
C
      FIRST = 15091989
      DO 1 IFILE = 1, MXFILE
         DDNAME(IFILE) = ' '
         UNITES(IFILE) = -1
   1  CONTINUE
      NBFILE = 0
C
      END
