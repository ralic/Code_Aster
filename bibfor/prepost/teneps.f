      SUBROUTINE TENEPS( JRWORK,ADR, C1, C2, SIG,EPS,EPSE,EPSP  )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/09/2011   AUTEUR TRAN V-X.TRAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT     NONE
      INTEGER      JRWORK, ADR
      REAL*8       C1, C2, SIG(6), EPS(6),EPSE(6),EPSP(6) 


C ---------------------------------------------------------------------
C BUT: POUR UN NUMERO D'ORDE, RECUPERER LES TENSEURS DE CONTRAINTE ET 
C                      DEFORMATION
C ---------------------------------------------------------------------
C ARGUMENTS:
C    JRWORK : IN : ADRESSE DE VECTEUR DE TRAVAIL ACTUEL
C    ARD    : IN : DECALGE DU NUMRO D'ORDE EN COURS
C    C1     : IN : COEEFS D'ELASTICITE
C    C2     : IN : COEEFS D'ELASTICITE
C    SIG    : OUT : CONTRAINTE (6 COMPOSANTES)
C    EPS    : OUT : DEFORMATION TOTALE (6 COMPOSANTES)
C    EPSE   : OUT : DEFORMATION ELASTIQUE (6 COMPOSANTES)
C    SIG    : OUT : DEFORMATION PLASTIQUE (6 COMPOSANTES)
C-----------------------------------------------------------------------
C---- COMMUNS NORMALISES  JEVEUX
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------
      INTEGER       I

      DO 25 I = 1, 6
         SIG(I) = 0.0D0
         EPS(I) = 0.0D0
         EPSE(I)= 0.0D0
         EPSP(I)= 0.0D0
25    CONTINUE 

      DO 35 I = 1, 6 
         SIG(I) = ZR(JRWORK + ADR + I - 1)        
         EPS(I) = ZR(JRWORK + ADR + I - 1 + 6)
35    CONTINUE 

C ON SUPPOSE QUE EPS_TOT = EPS_ELAS + EPSPLAS
         
      EPSE(1) = C1*SIG(1) - C2*(SIG(1) + SIG(2) + SIG(3))
      EPSE(2) = C1*SIG(2) - C2*(SIG(1) + SIG(2) + SIG(3))
      EPSE(3) = C1*SIG(3) - C2*(SIG(1) + SIG(2) + SIG(3))
      EPSE(4) = C1*SIG(4)
      EPSE(5) = C1*SIG(5)
      EPSE(6) = C1*SIG(6)

      DO 45 I = 1, 6 
        EPSP(I) =  EPS(I) - EPSE(I)
45    CONTINUE 

      END
