      SUBROUTINE EIINIT(NOMTE,IU,IL,IT)
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/07/2008   AUTEUR LAVERNE J.LAVERNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*16 NOMTE
      INTEGER      IU(3,18),IL(3,9),IT(18)
C ----------------------------------------------------------------------
C            DECALAGE D'INDICE POUR LES ELEMENTS D'INTERFACE
C ----------------------------------------------------------------------
C IN  NOMTE  NOM DE L'ELEMENT FINI
C OUT IU     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
C OUT IL     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE LAGRANGE
C OUT IT     DECALAGE D'INDICE POUR ACCEDER A LA TEMPERATURE
C ----------------------------------------------------------------------
      INTEGER N
      INTEGER UH20(16),LH20(4)
      INTEGER UP15(12),LP15(3)
      INTEGER UQ8(6),LQ8(2)
C ----------------------------------------------------------------------
      DATA UH20 /1,2,3,4,9,10,11,12,5,6,7,8,17,18,19,20/
      DATA LH20 /13,14,15,16/
      DATA UP15 /1,2,3,7,8,9,4,5,6,13,14,15/
      DATA LP15 /10,11,12/
      DATA UQ8  /1,2,5,4,3,7/
      DATA LQ8  /8,6/
C ----------------------------------------------------------------------
      
      IF (NOMTE.EQ.'MEEI_HEXA20') THEN
        DO 10 N = 1,16
          IU(1,N) = 1 + (UH20(N)-1)*3
          IU(2,N) = 2 + (UH20(N)-1)*3
          IU(3,N) = 3 + (UH20(N)-1)*3
 10     CONTINUE
        
        DO 20 N = 1,4
          IL(1,N) = 1 + (LH20(N)-1)*3          
          IL(2,N) = 2 + (LH20(N)-1)*3          
          IL(3,N) = 3 + (LH20(N)-1)*3          
 20     CONTINUE
 
        DO 30 N = 1,16
          IT(N) = UH20(N)          
 30     CONTINUE
 
 
      ELSE IF (NOMTE.EQ.'MEEI_PENTA15') THEN
        DO 110 N = 1,12
          IU(1,N) = 1 + (UP15(N)-1)*3
          IU(2,N) = 2 + (UP15(N)-1)*3
          IU(3,N) = 3 + (UP15(N)-1)*3
 110    CONTINUE
        
        DO 120 N = 1,3
          IL(1,N) = 1 + (LP15(N)-1)*3          
          IL(2,N) = 2 + (LP15(N)-1)*3          
          IL(3,N) = 3 + (LP15(N)-1)*3          
 120    CONTINUE
 
        DO 130 N = 1,12
          IT(N) = UP15(N)          
 130    CONTINUE
 

      ELSE IF ((NOMTE.EQ.'EIPLQU8') .OR. (NOMTE.EQ.'EIAXQU8')) THEN
      
        DO 210 N = 1,6
          IU(1,N) = 1 + (UQ8(N)-1)*2
          IU(2,N) = 2 + (UQ8(N)-1)*2
 210    CONTINUE
        
        DO 220 N = 1,2
          IL(1,N) = 1 + (LQ8(N)-1)*2          
          IL(2,N) = 2 + (LQ8(N)-1)*2          
 220    CONTINUE
 
        DO 230 N = 1,6
          IT(N) = UQ8(N)          
 230    CONTINUE

      END IF
      
      END
