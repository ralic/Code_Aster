      SUBROUTINE EJINIT(NOMTE,IU,IP)
      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/12/2010   AUTEUR PROIX J-M.PROIX 
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
C RESPONSABLE LAVERNE J.LAVERNE
   
      IMPLICIT NONE
      CHARACTER*16 NOMTE
      INTEGER      IU(3,16),IP(4)
C ----------------------------------------------------------------------
C            DECALAGE D'INDICE POUR LES ELEMENTS DE JOINT HM
C ----------------------------------------------------------------------
C IN  NOMTE  NOM DE L'ELEMENT FINI
C OUT IU     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE DEPLACEMENT
C OUT IP     DECALAGE D'INDICE POUR ACCEDER AUX DDL DE PRESSION
C ----------------------------------------------------------------------
      INTEGER N
      INTEGER UH20(16),PH20(4)
      INTEGER UP15(12),PP15(3)
      INTEGER UQ8(6),PQ8(2)
C ----------------------------------------------------------------------
      DATA UH20 /1,2,3,4,9,10,11,12,5,6,7,8,17,18,19,20/
      DATA PH20 /13,14,15,16/
      
      DATA UP15 /1,2,3,7,8,9,4,5,6,13,14,15/
      
      DATA PP15 /10,11,12/
      
      DATA UQ8  /1,2,5,4,3,7/
      DATA PQ8  /8,6/
C ----------------------------------------------------------------------
      
      IF (NOMTE.EQ.'EJHYME_HEXA20') THEN
      
        DO 10 N = 1,12
          IU(1,N) = 1 + (UH20(N)-1)*3
          IU(2,N) = 2 + (UH20(N)-1)*3
          IU(3,N) = 3 + (UH20(N)-1)*3
 10     CONTINUE
 
        DO 20 N = 13,16
          IU(1,N) = 1 + (UH20(N)-1)*3 - 8
          IU(2,N) = 2 + (UH20(N)-1)*3 - 8
          IU(3,N) = 3 + (UH20(N)-1)*3 - 8
 20     CONTINUE

        IP(1) = 1 + (PH20(1)-1)*3        
        IP(2) = 1 + (PH20(2)-1)*3 - 2      
        IP(3) = 1 + (PH20(3)-1)*3 - 4       
        IP(4) = 1 + (PH20(4)-1)*3 - 6       
 
      ELSEIF (NOMTE.EQ.'EJHYME_PENTA15') THEN
      
        DO 30 N = 1,9
          IU(1,N) = 1 + (UP15(N)-1)*3
          IU(2,N) = 2 + (UP15(N)-1)*3
          IU(3,N) = 3 + (UP15(N)-1)*3
 30     CONTINUE
 
        DO 40 N = 10,12
          IU(1,N) = 1 + (UP15(N)-1)*3 - 6
          IU(2,N) = 2 + (UP15(N)-1)*3 - 6
          IU(3,N) = 3 + (UP15(N)-1)*3 - 6
 40     CONTINUE
 
        IP(1) = 1 + (PP15(1)-1)*3        
        IP(2) = 1 + (PP15(2)-1)*3 - 2      
        IP(3) = 1 + (PP15(3)-1)*3 - 4       
 
      ELSEIF ((NOMTE.EQ.'EJHYME_PLQU8') .OR. 
     &        (NOMTE.EQ.'EJHYME_AXQU8')) THEN
      
        DO 50 N = 1,5
          IU(1,N) = 1 + (UQ8(N)-1)*2
          IU(2,N) = 2 + (UQ8(N)-1)*2
 50    CONTINUE
        IU(1,6) = 1 + (UQ8(6)-1)*2 - 1
        IU(2,6) = 2 + (UQ8(6)-1)*2 - 1
        
        IP(1)   = 1 + (PQ8(1)-1)*2 - 1          
        IP(2)   = 1 + (PQ8(2)-1)*2          
 
      ENDIF
      
      END
