      SUBROUTINE LXINFO( LIGNE , JLIG , JCOL)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)      LIGNE
      INTEGER                    JLIG, JCOL
C
C     ------------------------------------------------------------------
C     RENVOI LES INFORMATIONS SUR LA LIGNE COURANTE
C     ------------------------------------------------------------------
C OUT LIGNE :   CONTENU DE LA LIGNE COURANTE
C OUT JLIG  :   NUMERO DE LA DERNIERE LIGNE ANALYSEE
C OUT JCOL  :   NUMERO DE LA DERNIERE COLONNE ANALYSEE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C FIN LXINFO
C     ------------------------------------------------------------------
C
      PARAMETER  ( MXCOLS = 80 )
C
C     VARIABLES GLOBALES DE LECTURE
      CHARACTER*(MXCOLS) CARTE
      COMMON /LXCC02/    CARTE
      INTEGER            ILECT, IECR, LRECL, IEOF, ICOL, ILIG
      COMMON /LXCN02/    ILECT, IECR, LRECL, IEOF, ICOL, ILIG
C
      LIGNE = CARTE
      JLIG   = ILIG
      JCOL   = ICOL
C
      END
