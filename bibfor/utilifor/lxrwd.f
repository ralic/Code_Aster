      SUBROUTINE LXRWD
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     ------------------------------------------------------------------
C     REMBOBINAGE DE L'UNITE COURANTE
C     ------------------------------------------------------------------
C      -- PAS D'ARGUMENT --
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 07/01/98   AUTEUR CIBHHLB L.BOURHRARA 
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
C FIN LXRWD
C     ------------------------------------------------------------------
C     DEFINITION DES PARAMETRES
      PARAMETER (MXCOLS=80)
C
C     VARIABLES GLOBALES DE LECTURE
      CHARACTER*(MXCOLS) CARTE
      COMMON /LXCC02/    CARTE
      INTEGER            ILECT, IECR, LRECL, IEOF, ICOL, ILIG
      COMMON /LXCN02/    ILECT, IECR, LRECL, IEOF, ICOL, ILIG
C     ------------------------------------------------------------------
C

      REWIND (UNIT=ILECT,ERR=111)
 111  CONTINUE
      IEOF = 0
      ICOL = MXCOLS
      ILIG = 0
C
      END
