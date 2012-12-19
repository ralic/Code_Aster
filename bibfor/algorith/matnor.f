      SUBROUTINE MATNOR(FAMI,KPG,KSP,IMAT,NMAT,POUM,COEFEL,COEFPL,NVI)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2012   AUTEUR PELLET J.PELLET 
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
C       ----------------------------------------------------------------
C     NORTON   : RECUPERATION DU MATERIAU A T ET T+DT
C                  NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                  MATER(*,1) = E , NU , ALPHA
C                  MATER(*,2) = EPS0 , K , H1 , H2 , DELTA1 , DELTA2 ,
C                               H1ST , H2ST , KC , BIGS , SMALLS ,
C                               EPSC
C                  VARIABLES INTERNES : 
C     ----------------------------------------------------------------
C     IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C         KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
C         IMAT   :  ADRESSE DU MATERIAU CODE
C         NMAT   :  DIMENSION  DE MATER
C         POUM   :  '+' OU '-'
C     OUT COEFEL :  COEFFICIENTS MATERIAU POUR LA PARTIE ELASTIQUE
C         COEFPL :  COEFFICIENTS MATERIAU POUR LA PARTIE NON LINEAIRE
C         NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C         NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C         NR     :  NB DE COMPOSANTES SYSTEME NL
C         NVI    :  NB DE VARIABLES INTERNES
C     ----------------------------------------------------------------
      INTEGER       KPG,KSP,NMAT,NVI,IMAT,CERR(5)
      REAL*8        COEFEL(NMAT),COEFPL(NMAT)
      CHARACTER*(*) FAMI,POUM
      CHARACTER*8   NOMC(5)
C
C -   RECUPERATION MATERIAU -----------------------------------------
C
        NOMC(1)  = 'E'
        NOMC(2)  = 'NU'
        NOMC(3)  = 'ALPHA'
        NOMC(4)  = 'N'
        NOMC(5)  = 'UN_SUR_K'
C
C
C -   RECUPERATION MATERIAU A (T)
C
        CALL RCVALB(FAMI,KPG,KSP,POUM,IMAT,' ', 'ELAS',0,' ',
     1              0.D0, 3,NOMC(1),  COEFEL,  CERR(1), 0 )
     
        IF ( CERR(3) .NE. 0 ) COEFEL(3) = 0.D0
        
        CALL RCVALB(FAMI,KPG,KSP,POUM,IMAT,' ', 'LEMAITRE',0,' ',
     1              0.D0,2,NOMC(4),  COEFPL,  CERR(4), 1 )
C
C     NOMBRE DE COEF MATERIAU      
      COEFPL(NMAT)=2
      NVI=7
      
      END
