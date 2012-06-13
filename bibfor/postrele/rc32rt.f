      SUBROUTINE RC32RT ( LIEU, PI, PJ, SIMPIJ )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      REAL*8              PI, PJ, SIMPIJ 
      CHARACTER*4         LIEU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     ROCHET THERMIQUE : CALCUL DE LA CONTRAINTE DE MEMBRANE DE PRESSION
C
C     ------------------------------------------------------------------
C
      INTEGER       JSIGU, ICMP
      REAL*8        PIJ, SIGMP(6)
C DEB ------------------------------------------------------------------
C
      CALL JEVEUO ( '&&RC3200.MECA_UNIT .'//LIEU, 'L', JSIGU )
C
C --- DIFFERENCE DE PRESSION ENTRE LES ETATS I ET J
C
      PIJ = PJ - PI
C
C --- CONTRAINTE DE MEMBRANE DUE A LA PRESSION  (RECUP M_0)
C
      DO 10 ICMP = 1 , 6
         SIGMP(ICMP) = PIJ * ZR(JSIGU-1+156+72+ICMP)
 10   CONTINUE
C
      CALL RCTRES ( SIGMP, SIMPIJ )
C
      END
