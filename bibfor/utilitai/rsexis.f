      SUBROUTINE RSEXIS(NOMSD,IER)
      IMPLICIT NONE
      CHARACTER*(*)     NOMSD
      INTEGER                 IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     VERIFICATION DE L'EXISTENCE D'UNE STRUCTURE DE DONNEES
C                  "RESULTAT-COMPOSE".
C     ------------------------------------------------------------------
C IN  NOMSD  : NOM DE LA STRUCTURE "RESULTAT" A CREER.
C OUT IER    : CODE D'EXISTENCE
C            = 0 N'EXISTE PAS
C           /= 0 EXISTE
C     ------------------------------------------------------------------
      CHARACTER*24 DESC
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DATA         DESC/'                   .DESC'/
C     ------------------------------------------------------------------
      DESC(1:8) = NOMSD
      CALL JEEXIN(DESC,IER )
      END
