      SUBROUTINE ZBITER(X,F,MEM,OPT,Y)
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
      LOGICAL  OPT
      REAL*8   X,F,MEM,Y
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE MIXTE)
C
C RESOLUTION F(X) = 0 : ITERATION COURANTE
C      
C ----------------------------------------------------------------------
C 
C  IN  X      : SOLUTION COURANTE
C  IN  F      : VALEUR DE LA FONCTION EN X
C  I/O MEM    : COUPLES (X,F) ARCHIVES - GESTION INTERNE PAR ZEITER
C  OUT OPTI   : .TRUE. SI X EST UNE SOLUTION OPTIMALE
C  OUT Y      : NOUVEL ITERE
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
      LOGICAL ZBOPTI
      REAL*8  ZBROOT
C      
C ----------------------------------------------------------------------
C
C       
C --- ARCHIVAGE DU NOUVEAU COUPLE (X,F)
C   
       CALL ZBARCH(X,F,MEM)
C
C -- ACTUALISATION DES BORNES 
C
       CALL ZBBORN(X,F)
C
C --- DETECTION S'IL S'AGIT DE LA SOLUTION OPTIMALE JUSQU'A PRESENT
C
       OPT = ZBOPTI(X,F)
C
C --- RECHERCHE DE LA BORNE MAX
C
       IF (BPOS) THEN
         Y = ZBROOT(MEM)
       ELSE
         Y = X * PARMUL
       END IF
C             
C --- PROJECTION DE LA NOUVELLE SOLUTION SUR LES BORNES
C 
       CALL ZBPROJ(Y)
C
       END 
