      SUBROUTINE ZBARCH(X,F,MEM)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/04/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C           
      IMPLICIT NONE
      REAL*8   X,F,MEM(2,*)
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE MIXTE)
C
C ARCHIVAGE DES SOLUTIONS
C      
C ----------------------------------------------------------------------
C 
C IN  X      : SOLUTION COURANTE
C IN  F      : VALEUR DE LA FONCTION EN X
C I/O MEM    : COUPLES ARCHIVES
C
C ----------------------------------------------------------------------
C    
      REAL*8  XEXCLU,XNEG  ,XPOS  ,XOPT
      REAL*8  PARMUL,FNEG  ,FPOS  ,FOPT
      INTEGER ITER  ,DIMCPL,NBCPL
      LOGICAL BPOS  ,OPTI      
      COMMON /ZBPAR/ XEXCLU,XNEG  ,XPOS  ,XOPT,
     &               PARMUL,FNEG  ,FPOS  ,FOPT,
     &               ITER  ,DIMCPL,NBCPL ,BPOS,
     &               OPTI
C  
      INTEGER I
C         
C ----------------------------------------------------------------------
C                       
      NBCPL = MIN(NBCPL+1,DIMCPL)
       
      DO 10 I = NBCPL,2,-1
        MEM(1,I) = MEM(1,I-1)
        MEM(2,I) = MEM(2,I-1)
 10   CONTINUE
 
      MEM(1,1) = X
      MEM(2,1) = F
               
      END 
