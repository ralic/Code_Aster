      SUBROUTINE NMRECZ(NEQ,DEPL,FINT,FEXT,
     &                  FONC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2006   AUTEUR MABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MABBAS M.ABBAS
      IMPLICIT NONE  
      INTEGER  NEQ   
      REAL*8   DEPL(*)
      REAL*8   FINT(*)      
      REAL*8   FEXT(*)
      REAL*8   FONC
C ----------------------------------------------------------------------
C
C  CALCUL DE LA FONCTION POUR LA RECHERCHE LINEAIRE
C
C ----------------------------------------------------------------------
C
C IN  NEQ    : NOMBRE D'EQUATIONS
C IN  DEPL   : VECTEUR DE TOUS LES DEPLACEMENTS
C IN  FINT   : VECTEUR DES FORCES INTERNES
C IN  FEXT   : VECTEUR DES FORCES EXTERNES
C I/O FONC   : VALEUR DE LA FONCTION    
C
C ----------------------------------------------------------------------
C
      INTEGER      IEQ
C      
C-----------------------------------------------------------------------
C
      FONC = 0.D0
      DO 10 IEQ = 1, NEQ
         FONC = FONC + 
     &          DEPL(IEQ) * (FINT(IEQ)-FEXT(IEQ))   
  10  CONTINUE
  
      END
