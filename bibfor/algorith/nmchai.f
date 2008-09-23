      INTEGER FUNCTION NMCHAI(TYCHAP,TYVARI)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*6  TYCHAP
      CHARACTER*6  TYVARI   
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
C
C INDEX OU EST STOCKE LE NOM DE LA VARIABLE DANS UNE VARIABLE CHAPEAU
C
C ----------------------------------------------------------------------
C
C
C IN  TYCHAP : TYPE DE VARIABLE CHAPEAU
C                MEELEM - NOMS DES MATR_ELEM
C                MEASSE - NOMS DES MATR_ASSE
C                VEELEM - NOMS DES VECT_ELEM
C                VEASSE - NOMS DES VECT_ASSE
C                SOLALG - NOMS DES CHAM_NO SOLUTIONS
C IN  TYVARI : TYPE DE LA VARIABLE
C OUT NMCHAI : INDEX OU EST STOCKE LE NOM DE LA VARIABLE DANS UNE 
C              VARIABLE CHAPEAU
C      
C ----------------------------------------------------------------------
C

C      
C ----------------------------------------------------------------------
C 
      NMCHAI = -1
C
C ---
C
      IF (TYCHAP.EQ.'MEELEM') THEN
        IF (TYVARI.EQ.'MERIGI') THEN
          NMCHAI = 1
        ELSEIF (TYVARI.EQ.'MEDIRI') THEN
          NMCHAI = 2
        ELSEIF (TYVARI.EQ.'MEMASS') THEN
          NMCHAI = 3  
        ELSEIF (TYVARI.EQ.'MEAMOR') THEN
          NMCHAI = 4
        ELSEIF (TYVARI.EQ.'MESUIV') THEN
          NMCHAI = 5           
        ELSEIF (TYVARI.EQ.'MESSTR') THEN
          NMCHAI = 6
        ELSEIF (TYVARI.EQ.'MEGEOM') THEN
          NMCHAI = 7  
        ELSEIF (TYVARI.EQ.'MECTCC') THEN
          NMCHAI = 8
        ELSEIF (TYVARI.EQ.'MECTCF') THEN
          NMCHAI = 9 
        ELSEIF (TYVARI.EQ.'MEXFEC') THEN
          NMCHAI = 10
        ELSEIF (TYVARI.EQ.'MEXFEF') THEN
          NMCHAI = 11   
        ELSEIF (TYVARI.EQ.'MEXFTC') THEN
          NMCHAI = 12
        ELSEIF (TYVARI.EQ.'MEXFTF') THEN
          NMCHAI = 13                              
        ELSE    
          CALL ASSERT(.FALSE.)
        ENDIF     
      ELSEIF (TYCHAP.EQ.'MEASSE') THEN
        IF (TYVARI.EQ.'MERIGI') THEN
          NMCHAI = 1
        ELSEIF (TYVARI.EQ.'MEMASS') THEN
          NMCHAI = 2  
        ELSEIF (TYVARI.EQ.'MEAMOR') THEN
          NMCHAI = 3          
        ELSEIF (TYVARI.EQ.'MESSTR') THEN
          NMCHAI = 4           
        ELSE    
          CALL ASSERT(.FALSE.)
        ENDIF          
      ELSEIF (TYCHAP.EQ.'VEELEM') THEN
        IF (TYVARI.EQ.'CNFINT') THEN
          NMCHAI = 1
        ELSEIF (TYVARI.EQ.'CNDIRI') THEN
          NMCHAI = 2
        ELSEIF (TYVARI.EQ.'CNBUDI') THEN
          NMCHAI = 3
        ELSEIF (TYVARI.EQ.'CNFNOD') THEN
          NMCHAI = 4
        ELSEIF (TYVARI.EQ.'CNDIDO') THEN
          NMCHAI = 5
        ELSEIF (TYVARI.EQ.'CNDIPI') THEN
          NMCHAI = 6
        ELSEIF (TYVARI.EQ.'CNFEDO') THEN
          NMCHAI = 7
        ELSEIF (TYVARI.EQ.'CNFEPI') THEN
          NMCHAI = 8
        ELSEIF (TYVARI.EQ.'CNLAPL') THEN
          NMCHAI = 9
        ELSEIF (TYVARI.EQ.'CNONDP') THEN
          NMCHAI = 10
        ELSEIF (TYVARI.EQ.'CNFSDO') THEN
          NMCHAI = 11
        ELSEIF (TYVARI.EQ.'CNIMPE') THEN
          NMCHAI = 12
        ELSEIF (TYVARI.EQ.'CNMSME') THEN
          NMCHAI = 13
        ELSEIF (TYVARI.EQ.'CNDIDI') THEN
          NMCHAI = 14
        ELSEIF (TYVARI.EQ.'CNSSTF') THEN
          NMCHAI = 15
        ELSEIF (TYVARI.EQ.'CNCTCC') THEN
          NMCHAI = 16
        ELSEIF (TYVARI.EQ.'CNCTCF') THEN
          NMCHAI = 17
        ELSEIF (TYVARI.EQ.'CNXFEC') THEN
          NMCHAI = 18
        ELSEIF (TYVARI.EQ.'CNXFEF') THEN
          NMCHAI = 19
        ELSEIF (TYVARI.EQ.'CNXFTC') THEN
          NMCHAI = 20
        ELSEIF (TYVARI.EQ.'CNXFTF') THEN
          NMCHAI = 21
        ELSEIF (TYVARI.EQ.'CNREFE') THEN
          NMCHAI = 22 
        ELSEIF (TYVARI.EQ.'CNVCF1') THEN
          NMCHAI = 23
        ELSEIF (TYVARI.EQ.'CNVCF0') THEN
          NMCHAI = 24                                  
        ELSE    
          CALL ASSERT(.FALSE.)
        ENDIF      
      ELSEIF (TYCHAP.EQ.'VEASSE') THEN
        IF (TYVARI.EQ.'CNFINT') THEN
          NMCHAI = 1 
        ELSEIF (TYVARI.EQ.'CNDIRI') THEN
          NMCHAI = 2
        ELSEIF (TYVARI.EQ.'CNBUDI') THEN
          NMCHAI = 3
        ELSEIF (TYVARI.EQ.'CNFNOD') THEN
          NMCHAI = 4
        ELSEIF (TYVARI.EQ.'CNDIDO') THEN
          NMCHAI = 5
        ELSEIF (TYVARI.EQ.'CNDIPI') THEN
          NMCHAI = 6
        ELSEIF (TYVARI.EQ.'CNFEDO') THEN
          NMCHAI = 7
        ELSEIF (TYVARI.EQ.'CNFEPI') THEN
          NMCHAI = 8
        ELSEIF (TYVARI.EQ.'CNLAPL') THEN
          NMCHAI = 9
        ELSEIF (TYVARI.EQ.'CNONDP') THEN
          NMCHAI = 10
        ELSEIF (TYVARI.EQ.'CNFSDO') THEN
          NMCHAI = 11
        ELSEIF (TYVARI.EQ.'CNIMPE') THEN
          NMCHAI = 12
        ELSEIF (TYVARI.EQ.'CNMSME') THEN
          NMCHAI = 13
        ELSEIF (TYVARI.EQ.'CNDIDI') THEN
          NMCHAI = 14
        ELSEIF (TYVARI.EQ.'CNSSTF') THEN
          NMCHAI = 15
        ELSEIF (TYVARI.EQ.'CNCTCC') THEN
          NMCHAI = 16
        ELSEIF (TYVARI.EQ.'CNCTCF') THEN
          NMCHAI = 17
        ELSEIF (TYVARI.EQ.'CNXFEC') THEN
          NMCHAI = 18
        ELSEIF (TYVARI.EQ.'CNXFEF') THEN
          NMCHAI = 19
        ELSEIF (TYVARI.EQ.'CNXFTC') THEN
          NMCHAI = 20
        ELSEIF (TYVARI.EQ.'CNXFTF') THEN
          NMCHAI = 21
        ELSEIF (TYVARI.EQ.'CNREFE') THEN
          NMCHAI = 22 
        ELSEIF (TYVARI.EQ.'CNVCF1') THEN
          NMCHAI = 23
        ELSEIF (TYVARI.EQ.'CNVCF0') THEN
          NMCHAI = 24 
        ELSEIF (TYVARI.EQ.'CNCINE') THEN
          NMCHAI = 25        
        ELSEIF (TYVARI.EQ.'CNSSTR') THEN
          NMCHAI = 26        
        ELSEIF (TYVARI.EQ.'CNCTDF') THEN
          NMCHAI = 27        
        ELSEIF (TYVARI.EQ.'CNGRFL') THEN
          NMCHAI = 28  
        ELSEIF (TYVARI.EQ.'CNVCPR') THEN
          NMCHAI = 29        
        ELSEIF (TYVARI.EQ.'CNDYNA') THEN
          NMCHAI = 30         
        ELSEIF (TYVARI.EQ.'CNMODP') THEN
          NMCHAI = 32          
        ELSEIF (TYVARI.EQ.'CNMODC') THEN
          NMCHAI = 33    
        ELSEIF (TYVARI.EQ.'CNCTDC') THEN
          NMCHAI = 34 
        ELSEIF (TYVARI.EQ.'CNUNIL') THEN
          NMCHAI = 35    
        ELSEIF (TYVARI.EQ.'CNFEXT') THEN
          NMCHAI = 36  
        ELSE    
          CALL ASSERT(.FALSE.)
        ENDIF 
      ELSEIF (TYCHAP.EQ.'SOLALG') THEN
        IF (TYVARI.EQ.'DDEPLA') THEN
          NMCHAI = 1 
        ELSEIF (TYVARI.EQ.'DEPDEL') THEN
          NMCHAI = 2 
        ELSEIF (TYVARI.EQ.'DEPOLD') THEN
          NMCHAI = 3   
        ELSEIF (TYVARI.EQ.'DEPPR1') THEN
          NMCHAI = 4   
        ELSEIF (TYVARI.EQ.'DEPPR2') THEN
          NMCHAI = 5   
        ELSEIF (TYVARI.EQ.'DVITLA') THEN
          NMCHAI = 6 
        ELSEIF (TYVARI.EQ.'VITDEL') THEN
          NMCHAI = 7 
        ELSEIF (TYVARI.EQ.'VITOLD') THEN
          NMCHAI = 8   
        ELSEIF (TYVARI.EQ.'VITPR1') THEN
          NMCHAI = 9   
        ELSEIF (TYVARI.EQ.'VITPR2') THEN
          NMCHAI = 10  
        ELSEIF (TYVARI.EQ.'DACCLA') THEN
          NMCHAI = 11 
        ELSEIF (TYVARI.EQ.'ACCDEL') THEN
          NMCHAI = 12 
        ELSEIF (TYVARI.EQ.'ACCOLD') THEN
          NMCHAI = 13   
        ELSEIF (TYVARI.EQ.'ACCPR1') THEN
          NMCHAI = 14   
        ELSEIF (TYVARI.EQ.'ACCPR2') THEN
          NMCHAI = 15 
        ELSEIF (TYVARI.EQ.'DEPSO1') THEN
          NMCHAI = 16   
        ELSEIF (TYVARI.EQ.'DEPSO2') THEN
          NMCHAI = 17                          
        ELSE 
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF           
C
      END
