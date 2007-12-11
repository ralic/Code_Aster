      SUBROUTINE ZBPROJ(X)
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
      REAL*8   X
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE MIXTE)
C
C PROJECTION DE LA SOLUTION SUR LES BORNES ADMISSIBLES 
C REACTUALISATION DES BORNES ADMISSIBLES
C      
C ----------------------------------------------------------------------
C 
C  I/O X      : SOLUTION COURANTE
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
C ----------------------------------------------------------------------
C          
C
C --- BORNE MIN
C       
       IF (X.LT.XNEG) THEN
         IF (BPOS) THEN
           X = (XNEG+XPOS)/2
         ELSE
           CALL U2MESS('F','MECANONLINE_81')
         END IF
       END IF
C
C --- BORNE MAX
C       
       IF (BPOS) THEN
         IF (X.GT.XPOS) THEN
           X = (XNEG+XPOS)/2
         END IF
       END IF
C       
       END 
