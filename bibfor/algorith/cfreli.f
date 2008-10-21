      SUBROUTINE CFRELI(NOMA  ,NUMMAI,NBNOM ,KSI1  ,KSI2,
     &                  COEFNO)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/10/2008   AUTEUR DESOZA T.DESOZA 
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
      IMPLICIT     NONE
      CHARACTER*8  NOMA
      REAL*8       KSI1,KSI2
      REAL*8       COEFNO(9)
      INTEGER      NUMMAI,NBNOM    
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
C
C COEFFICIENTS DE LA RELATION SUR NOEUDS MAITRES
C
C ----------------------------------------------------------------------
C
C
C IN  NOMA   : MAILLAGE
C IN  NBNOM  : NOIMBRE NOEUDS MAILLE MAITRE PORTANT DES DX/DY/DZ
C IN  NUMMAI : NUMERO ABSOLU DE LA MAILLE
C IN  KSIx   : COORDONNEES PARAMETRIQUES SUR LA MAILLE MAITRE
C                 DE LA "PROJECTION" M 
C OUT COEFNO : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
C               MAITRES
C
C ----------------------------------------------------------------------
C
      REAL*8       ZERO,UN
      PARAMETER  ( ZERO   =  0.0D0  )      
      PARAMETER  ( UN     =  1.0D0  )
      REAL*8       KSI(3)
      INTEGER      K,IBID
      REAL*8       FF(9)
      CHARACTER*8  ALIAS               
C
C ----------------------------------------------------------------------
C
C --- INITIALISATIONS
C
      DO 10 K = 1,9
        COEFNO(K) = ZERO
 10   CONTINUE
C
C --- CARACTERISTIQUE DE LA MAILLE 
C     
      CALL MMELTY(NOMA  ,NUMMAI,ALIAS ,IBID  ,IBID)
C
C --- COEFFICIENTS SUR NOEUD MAITRE SUIVANT TYPE APPARIEMENT/ELEMENT
C
      KSI(1) = KSI1
      KSI(2) = KSI2
      KSI(3) = UN - KSI1 - KSI2
C      
      IF (ALIAS.EQ.'SE2') THEN
        CALL ELRFVF('SE2',KSI,2,FF,IBID)
        COEFNO(1) = - FF(1)
        COEFNO(2) = - FF(2)
        COEFNO(3) = ZERO        
      ELSE IF (ALIAS.EQ.'SE3') THEN
        CALL ELRFVF('SE3',KSI,3,FF,IBID)
        COEFNO(1) = - FF(1)
        COEFNO(2) = - FF(2)
        COEFNO(3) = - FF(3)        
      ELSE IF (ALIAS(1:4).EQ.'TR3') THEN              
        CALL ELRFVF('TR3',KSI,3,FF,IBID)
        COEFNO(1) = - FF(1)
        COEFNO(2) = - FF(2)
        COEFNO(3) = - FF(3)        
      ELSE IF (ALIAS(1:4).EQ.'TR6') THEN             
        CALL ELRFVF('TR6',KSI,6,FF,IBID)
        COEFNO(1) = - FF(1)
        COEFNO(2) = - FF(2)
        COEFNO(3) = - FF(3)
        COEFNO(4) = - FF(4)
        COEFNO(5) = - FF(5)
        COEFNO(6) = - FF(6)      
      ELSE IF (ALIAS(1:4).EQ.'TR7') THEN
        IF (NBNOM.EQ.7) THEN           
          CALL ELRFVF('TR7',KSI,7,FF,IBID)        
          COEFNO(1) = - FF(1)
          COEFNO(2) = - FF(2)
          COEFNO(3) = - FF(3)
          COEFNO(4) = - FF(4)
          COEFNO(5) = - FF(5)
          COEFNO(6) = - FF(6)
          COEFNO(7) = - FF(7)
        ELSEIF (NBNOM.EQ.6) THEN           
          CALL ELRFVF('TR6',KSI,6,FF,IBID)        
          COEFNO(1) = - FF(1)
          COEFNO(2) = - FF(2)
          COEFNO(3) = - FF(3)
          COEFNO(4) = - FF(4)
          COEFNO(5) = - FF(5)
          COEFNO(6) = - FF(6)                           
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF                               
      ELSE IF (ALIAS(1:2).EQ.'QU') THEN
        IF (ALIAS.EQ.'QU4') THEN
          CALL ELRFVF('QU4',KSI,4,FF,IBID)
          COEFNO(1) = - FF(1)
          COEFNO(2) = - FF(2)
          COEFNO(3) = - FF(3)
          COEFNO(4) = - FF(4) 
        ELSEIF (ALIAS.EQ.'QU8') THEN
          CALL ELRFVF('QU4',KSI,4,FF,IBID)
          COEFNO(1) = - FF(1)
          COEFNO(2) = - FF(2)
          COEFNO(3) = - FF(3)
          COEFNO(4) = - FF(4)             
        ELSEIF (ALIAS.EQ.'QU9') THEN
          IF (NBNOM.EQ.9) THEN         
            CALL ELRFVF('QU9',KSI,9,FF,IBID)
            COEFNO(1) = - FF(1)
            COEFNO(2) = - FF(2)
            COEFNO(3) = - FF(3)
            COEFNO(4) = - FF(4) 
            COEFNO(5) = - FF(5) 
            COEFNO(6) = - FF(6) 
            COEFNO(7) = - FF(7) 
            COEFNO(8) = - FF(8)          
            COEFNO(9) = - FF(9)
          ELSEIF (NBNOM.EQ.8) THEN   
            CALL ELRFVF('QU8',KSI,8,FF,IBID)
            COEFNO(1) = - FF(1)
            COEFNO(2) = - FF(2)
            COEFNO(3) = - FF(3)
            COEFNO(4) = - FF(4) 
            COEFNO(5) = - FF(5) 
            COEFNO(6) = - FF(6) 
            COEFNO(7) = - FF(7) 
            COEFNO(8) = - FF(8)                   
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF          
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF        
      ELSE  
        CALL ASSERT(.FALSE.)
      END IF    
C

      END
