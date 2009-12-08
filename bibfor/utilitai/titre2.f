      SUBROUTINE TITRE2(NOMCON,NOMCHA,NOMOBJ,MOTFAC,IOCC)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NOMCON,NOMCHA,NOMOBJ,MOTFAC
      INTEGER                                IOCC
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     CREATION D'UN SOUS-TITRE
C     ------------------------------------------------------------------
C IN  NOMCON : K8  : NOM DU RESULTAT
C IN  NOMCHA : K19 : NOM DU CHAMP A TRAITER DANS LE CAS D'UN RESULTAT
C IN  NOMOBJ : K24 : NOM DE L'OBJET DE STOCKAGE
C IN  MOTFAC : K16 : NOM DU MOT CLE FACTEUR SOUS LEQUEL EST LE S-TITRE
C IN  IOCC   : IS  : OCCURRENCE CONCERNEE SI L'ON A UN MOT CLE FACTEUR
C     ------------------------------------------------------------------
C
      CALL TITREA('S',NOMCON,NOMCHA,NOMOBJ,'D',MOTFAC,IOCC,'V' )
      END
