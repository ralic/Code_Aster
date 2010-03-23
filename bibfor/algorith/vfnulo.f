      SUBROUTINE VFNULO(MAXFA,MAXAR,NDIM,NNOS,NFACE,
     >                 NBNOFA,NOSAR,NOSFA,NARFA)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
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
C ======================================================================
C DONNE LA NUMEROTATION LOCALE DES SOMMETS DES FACES DE VF
C LA FACE EST UN ELEMENT DE BORD DE DIMENSION DIM-1      
C
C IN NDIM DIMENSION D ESPACE
C IN NFACE NOMBRE DE FACES
C IN NNOS NOMBRE DE SOMMETS
C MAXFA NOMBRE MAX DE FACES
C MAXAR NOMBRE MAX DE ARETES
C NOSAR(IAR ,1:2)  LES DESDUS SOMMETS DE L ARETE IAR
C OUT NBNOFA(1:NFACE) : NOMBRE DE SOMMETS DE LA FACE
C OUT NOSFA(IFA :1,NFACE,J : 1,NBNOFA(IFA)) J EME SOMMET DE LA FACE IFA
C     (EN NUMEROTATION LOCALE)
C OUT NARFA(IFA :1,NFACE,J : 1,NBNOFA(IFA)) J EME ARETE DE LA FACE IFA
C     (EN NUMEROTATION LOCALE)
C
C
      INTEGER MAXFA,MAXAR,NDIM,NNOS,NFACE
      INTEGER NBNOFA(1:NFACE)
      INTEGER NOSAR(1:MAXAR,2)
      INTEGER NOSFA(1:MAXFA,*)
      INTEGER NARFA(1:MAXFA,*)
C      
      CHARACTER*8  ELREFE
      INTEGER IFA
C      
      CALL ELREF1(ELREFE)
C      
      IF(NDIM.EQ.2) THEN
       CALL ASSERT(NNOS.EQ.NFACE)
       DO 1 IFA = 1 , NFACE
        NBNOFA(IFA)=2 
        NOSFA(IFA,1)=IFA
        IF((IFA+1).LE.NNOS) THEN
         NOSFA(IFA,2)=IFA+1
        ELSE
         NOSFA(IFA,2)=IFA+1-NNOS
        ENDIF
    1  CONTINUE
      ELSE
        IF(ELREFE.EQ.'H27') THEN
         CALL ASSERT(NFACE.EQ.6)
         CALL ASSERT(NNOS.EQ.8)
         DO 2 IFA = 1 , 6
          NBNOFA(IFA)=4
    2    CONTINUE
C SOMMETS DE ARETE
         NOSAR(1,1)=1
         NOSAR(1,2)=2        
         NOSAR(2,1)=2        
         NOSAR(2,2)=3         
         NOSAR(3,1)=3        
         NOSAR(3,2)=4         
         NOSAR(4,1)=4        
         NOSAR(4,2)=1       
         NOSAR(5,1)=1        
         NOSAR(5,2)=5         
         NOSAR(6,1)=2        
         NOSAR(6,2)=6         
         NOSAR(7,1)=3
         NOSAR(7,2)=7         
         NOSAR(8,1)=4       
         NOSAR(8,2)=8         
         NOSAR(9,1)=5        
         NOSAR(9,2)=6
         NOSAR(10,1)=6       
         NOSAR(10,2)=7
         NOSAR(11,1)=7       
         NOSAR(11,2)=8 
C SOMMETS DE FACE          
         NOSAR(12,1)=8        
         NOSAR(12,2)=5
         NOSFA(1,1)=1         
         NOSFA(1,2)=2         
         NOSFA(1,3)=3         
         NOSFA(1,4)=4
         NOSFA(2,1)=1         
         NOSFA(2,2)=5         
         NOSFA(2,3)=6         
         NOSFA(2,4)=2
         NOSFA(3,1)=2         
         NOSFA(3,2)=3         
         NOSFA(3,3)=7         
         NOSFA(3,4)=6 
         NOSFA(4,1)=7         
         NOSFA(4,2)=3         
         NOSFA(4,3)=4         
         NOSFA(4,4)=8
         NOSFA(5,1)=1         
         NOSFA(5,2)=4         
         NOSFA(5,3)=8         
         NOSFA(5,4)=5
         NOSFA(6,1)=5         
         NOSFA(6,2)=6         
         NOSFA(6,3)=7         
         NOSFA(6,4)=8
C ARETES DE FACE
         NARFA(1,1)=1         
         NARFA(1,2)=2         
         NARFA(1,3)=3         
         NARFA(1,4)=4
         NARFA(2,1)=5         
         NARFA(2,2)=9         
         NARFA(2,3)=6         
         NARFA(2,4)=1
         NARFA(3,1)=2         
         NARFA(3,2)=7         
         NARFA(3,3)=10         
         NARFA(3,4)=6
         NARFA(4,1)=7         
         NARFA(4,2)=3         
         NARFA(4,3)=8         
         NARFA(4,4)=11
         NARFA(5,1)=4         
         NARFA(5,2)=8         
         NARFA(5,3)=12         
         NARFA(5,4)=5
         NARFA(6,1)=9         
         NARFA(6,2)=10         
         NARFA(6,3)=11         
         NARFA(6,4)=12
        ELSE IF(ELREFE.EQ.'T9') THEN
         CALL ASSERT(NFACE.EQ.4)
         CALL ASSERT(NNOS.EQ.4)
         DO 3 IFA = 1 , 4
          NBNOFA(IFA)=3
    3    CONTINUE
C SOMMETS DE ARETE
         NOSAR(1,1)=1
         NOSAR(1,2)=2
         NOSAR(2,1)=2        
         NOSAR(2,2)=3
         NOSAR(3,1)=3        
         NOSAR(3,2)=1
         NOSAR(4,1)=1        
         NOSAR(4,2)=4 
         NOSAR(5,1)=2        
         NOSAR(5,2)=4 
         NOSAR(6,1)=3        
         NOSAR(6,2)=4
C SOMMETS DE FACE 
         NOSFA(1,1)=2
         NOSFA(1,2)=3
         NOSFA(1,3)=4
         NOSFA(2,1)=3        
         NOSFA(2,2)=4        
         NOSFA(2,3)=1
         NOSFA(3,1)=4        
         NOSFA(3,2)=1        
         NOSFA(3,3)=2
         NOSFA(4,1)=1        
         NOSFA(4,2)=2        
         NOSFA(4,3)=3
C ARETES DE FACE          
         NARFA(1,1)=2
         NARFA(1,2)=6
         NARFA(1,3)=5         
         NARFA(2,1)=6        
         NARFA(2,2)=4        
         NARFA(2,3)=3         
         NARFA(3,1)=4        
         NARFA(3,2)=1        
         NARFA(3,3)=5         
         NARFA(4,1)=1        
         NARFA(4,2)=2        
         NARFA(4,3)=3
        ELSE
          CALL U2MESK('F','VOLUFINI_12', 1 ,ELREFE)
        ENDIF
      ENDIF
      END
