      SUBROUTINE NMCHCP(TYCHAP,VACHIN,VACHOU)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/01/2010   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE GRANET S.GRANET
C
      IMPLICIT NONE
      CHARACTER*19  VACHIN(*),VACHOU(*)
      CHARACTER*6   TYCHAP 
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
C
C RECOPIE D'UNE VARIABLE CHAPEAU
C
C ----------------------------------------------------------------------
C
C
C IN  VACHIN : VARIABLE CHAPEAU EN ENTREE
C IN  TYCHAP : TYPE DE VARIABLE CHAPEAU
C                MEELEM - NOMS DES MATR_ELEM
C                MEASSE - NOMS DES MATR_ASSE
C                VEELEM - NOMS DES VECT_ELEM
C                VEASSE - NOMS DES VECT_ASSE
C                SOLALG - NOMS DES CHAM_NO SOLUTIONS
C                VALINC - VALEURS SOLUTION INCREMENTALE
C OUT VACHOU : VARIABLE CHAPEAU EN SORTIE
C      
C ----------------------------------------------------------------------
C
      INTEGER      I,NBVAR
C      
C ----------------------------------------------------------------------
C        
      CALL NMCHAI(TYCHAP,'LONMAX',NBVAR )
C 
      DO 12 I = 1,NBVAR
        VACHOU(I) = VACHIN(I)          
   12 CONTINUE
C
      END
