      SUBROUTINE JEALDY ( IACT , LGSEG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 22/02/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      INTEGER             IACT , LGSEG
C ----------------------------------------------------------------------
C ROUTINE UTILISATEUR PERMETTANT D'ACTIVER L'ALLOCATION DYNAMIQE
C DES SEGMENTS DE VALEURS
C
C IN  IACT   : =   1 ACTIVATION DE L'ALLOCATION DYNAMIQUE
C              =/= 1 DESACTIVATION DE L'ALLOCATION DYNAMIQUE
C IN  LGSEG : LONGUEUR EN ENTIER (INTEGER) A PARTIR DE LAQUELLE 
C             L'ALLOCATION EST DYNAMIQUE 
C ----------------------------------------------------------------------
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
      INTEGER          LUNDEF,IDEBUG
      COMMON /UNDFJE/  LUNDEF,IDEBUG
C     ------------------------------------------------------------------
      INTEGER          MAX
C     ------------------------------------------------------------------
       
      IF ( IACT .EQ. 1 .AND. LGSEG .GT. 0 ) THEN
         LDYN = 1
         LGDYN = MAX (1,LGSEG) 
      ELSE  
         LDYN = -100
         LGSEG = LUNDEF
      ENDIF
C     ------------------------------------------------------------------
      END
