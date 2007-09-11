      SUBROUTINE CFRELI(MATYP ,PROJ  ,ITRIA,LAMBDA,COORDM,
     &                  COORMA,COEFNO)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/09/2007   AUTEUR ABBAS M.ABBAS 
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
      CHARACTER*4  MATYP
      REAL*8       LAMBDA(3)
      REAL*8       COEFNO(9)
      INTEGER      PROJ
      INTEGER      ITRIA
      REAL*8       COORDM(3),COORMA(27)      
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
C IN  MATYP  : TYPE DE LA MAILLE MAITRE
C                -> SEG2,SEG3,TRI3,TRI6,QUA4,QUA8,QUA9,NODE
C                'NODE' EST POUR L'APPARIEMENT NODAL PUR
C IN  PROJ   : TYPE DE PROJECTION
C               1 PROJECTION LINEAIRE
C               2 PROJECTION QUADRATIQUE
C IN  LAMBDA : COORDONNEES PARAMETRIQUES SUR LA MAILLE MAITRE
C                 DE LA "PROJECTION" M 
C IN  ITRIA  : INDICE DU TRIANGLE CHOISI DANS DECOUPE QUADRANGLE
C OUT COEFNO : VALEURS EN M DES FONCTIONS DE FORME ASSOCIEES AUX NOEUDS
C               MAITRES
C              POUR APPARIEMENT NODAL: -1.D0 SUR LE NOEUD MAITRE
C
C ----------------------------------------------------------------------
C
      REAL*8       ZERO,UN, DEUX
      PARAMETER  ( ZERO   =  0.0D0  )      
      PARAMETER  ( UN     =  1.0D0  )
      PARAMETER  ( DEUX   =  2.0D0  )
      REAL*8       KSI1,KSI2,KSI3
      REAL*8       KSIR(3)
      INTEGER      K,IBID
      REAL*8       FF(9)
C
C ----------------------------------------------------------------------
C
C --- INITIALISATIONS
C
      DO 10 K = 1,9
        COEFNO(K) = ZERO
 10   CONTINUE 
C
C --- COEFFICIENTS SUR NOEUD MAITRE SUIVANT TYPE APPARIEMENT/ELEMENT
C
      KSI1 = LAMBDA(1)
      KSI2 = LAMBDA(2)
      KSI3 = LAMBDA(3)       
      IF (MATYP.EQ.'NODE') THEN  
        COEFNO(1) = - UN                    
      ELSEIF (MATYP.EQ.'SEG2') THEN
        KSIR(1)   = DEUX*KSI1 - UN
        KSIR(2)   = KSI2 
        KSIR(3)   = KSI3 
        CALL ELRFVF('SE2',KSIR,2,FF,IBID)
        COEFNO(1) = - FF(1)
        COEFNO(2) = - FF(2)
        COEFNO(3) = ZERO        
      ELSE IF (MATYP.EQ.'SEG3') THEN
C --- ATTENTION ! FCTIONS DE FORME AUSSI DANS PROJSQ !!  
        KSIR(1)   = DEUX*KSI1 - UN
        KSIR(2)   = KSI2 
        KSIR(3)   = KSI3 
        CALL ELRFVF('SE3',KSIR,3,FF,IBID)
        COEFNO(1) = - FF(1)
        COEFNO(2) = - FF(2)
        COEFNO(3) = - FF(3)  
               
      ELSE IF (MATYP(1:4).EQ.'TRI3') THEN 
        KSIR(1)   = KSI1 
        KSIR(2)   = KSI2 
        KSIR(3)   = KSI3              
        COEFNO(1) = - KSI3
        COEFNO(2) = - KSI1
        COEFNO(3) = - KSI2
        CALL ELRFVF('TR3',KSIR,3,FF,IBID)
        COEFNO(1) = - FF(1)
        COEFNO(2) = - FF(2)
        COEFNO(3) = - FF(3)        
      ELSE IF (MATYP(1:4).EQ.'TRI6') THEN 
        KSIR(1)   = KSI1 
        KSIR(2)   = KSI2 
        KSIR(3)   = KSI3              
        CALL ELRFVF('TR6',KSIR,6,FF,IBID)
        COEFNO(1) = - FF(1)
        COEFNO(2) = - FF(2)
        COEFNO(3) = - FF(3)
        COEFNO(4) = - FF(4)
        COEFNO(5) = - FF(5)
        COEFNO(6) = - FF(6)      
      ELSE IF (MATYP(1:4).EQ.'TRI7') THEN 
        KSIR(1)   = KSI1 
        KSIR(2)   = KSI2 
        KSIR(3)   = KSI3              
        CALL ELRFVF('TR7',KSIR,7,FF,IBID)        
        COEFNO(1) = - FF(1)
        COEFNO(2) = - FF(2)
        COEFNO(3) = - FF(3)
        COEFNO(4) = - FF(4)
        COEFNO(5) = - FF(5)
        COEFNO(6) = - FF(6)
        COEFNO(7) = - FF(7)                             
      ELSE IF (MATYP(1:3).EQ.'QUA') THEN
        IF (ITRIA.EQ.1) THEN
          KSIR(1)   = DEUX*(KSI1+KSI2) - UN
          KSIR(2)   = DEUX*KSI2 - UN
          KSIR(3)   = KSI3 
        ELSEIF (ITRIA.EQ.2) THEN
          KSIR(1)   = DEUX*(KSI1) - UN
          KSIR(2)   = DEUX*(KSI1+KSI2) - UN
          KSIR(3)   = KSI3 
        ELSEIF (ITRIA.EQ.3) THEN
          KSIR(1)   = DEUX*KSI1 - UN
          KSIR(2)   = DEUX*KSI2 - UN
          KSIR(3)   = KSI3    
        ELSEIF (ITRIA.EQ.4) THEN
          KSIR(1)   = -DEUX*KSI2 + UN
          KSIR(2)   = DEUX*(KSI1+KSI2) - UN
          KSIR(3)   = KSI3                                    
        ELSE
          CALL CFIMPA('CFRELI',1)
        ENDIF
        IF (MATYP.EQ.'QUA4') THEN
          CALL ELRFVF('QU4',KSIR,4,FF,IBID)
          COEFNO(1) = - FF(1)
          COEFNO(2) = - FF(2)
          COEFNO(3) = - FF(3)
          COEFNO(4) = - FF(4) 
        ELSEIF (MATYP.EQ.'QUA8') THEN
          CALL ELRFVF('QU4',KSIR,4,FF,IBID)
          COEFNO(1) = - FF(1)
          COEFNO(2) = - FF(2)
          COEFNO(3) = - FF(3)
          COEFNO(4) = - FF(4)         
       
        ELSEIF (MATYP.EQ.'QUA9') THEN
          CALL ELRFVF('QU9',KSIR,9,FF,IBID)
          COEFNO(1) = - FF(1)
          COEFNO(2) = - FF(2)
          COEFNO(3) = - FF(3)
          COEFNO(4) = - FF(4) 
          COEFNO(5) = - FF(5) 
          COEFNO(6) = - FF(6) 
          COEFNO(7) = - FF(7) 
          COEFNO(8) = - FF(8)          
          COEFNO(9) = - FF(9)           
        ELSE
          CALL U2MESS('F','CONTACT_30')
        ENDIF        
      ELSE  
        CALL U2MESS('F','CONTACT_30')
      END IF    
C

      END
