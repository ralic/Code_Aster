      SUBROUTINE CFTYPN(DEFICO,POSNO ,TYPNO )
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'
      CHARACTER*24 DEFICO
      INTEGER      POSNO
      CHARACTER*4  TYPNO
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES MAILLEES - UTILITAIRE)
C
C TYPE D'UN NOEUD
C
C ----------------------------------------------------------------------
C
C
C IN  DEFICO : SD DE CONTACT (DEFINITION)
C IN  POSNO  : INDICE DANS CONTNO DU NOEUD 
C OUT TYPNO  : TYPE DU NOEUD 'MAIT' OU 'ESCL'
C
C
C
C
      CHARACTER*24 TYPENO
      INTEGER      JTYPNO  
      INTEGER      CFMMVD,ZTYPN
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ ()
C
C --- ACCES SD DE CONTACT
C          
      TYPENO = DEFICO(1:16)//'.TYPENO' 
      CALL JEVEUO(TYPENO,'L',JTYPNO)   
      ZTYPN  = CFMMVD('ZTYPN')
C  
C --- REPONSE
C    
      IF (ZI(JTYPNO+ZTYPN*(POSNO -1)+1-1).EQ.1) THEN
        TYPNO  = 'MAIT'
      ELSEIF (ZI(JTYPNO+ZTYPN*(POSNO -1)+1-1).EQ.-1) THEN
        TYPNO  = 'ESCL'
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF       
C
      CALL JEDEMA()
C 
      END
