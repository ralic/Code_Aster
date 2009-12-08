      SUBROUTINE CALET(NDIM,FM,FMA,FMP,EDPN1,FMAM,FTA,
     &                 ETDPN1,JM,JP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/12/2009   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER NDIM,I,J
      REAL*8  FMA(3,3),FMP(3,3)
      REAL*8 EDPN1(3,3)
      REAL*8 FM(3,3),WORK(9),DET
      
C ------------------------------------------------------------------
C   NDIM    : DIMENSION DE L'ESPACE
C   FM      : TENSEUR DE DEFORMATION ENTRE CONFIGURATION INITIALE ET 
C             CONFIGURATION A T-
C   FMA     : TENSEUR DE DEFORMATION ENTRE CONFIGURATION T- ET 
C             CONFIGURATION A T_(N+ALPHA)
C   FMP     : TENSEUR DE DEFORMATION ENTRE CONFIGURATION T- ET 
C             CONFIGURATION A T+
C   EDPN1   : TENSEUR 

C   SORTIE:
C   FMAM    : INVERSE DU FMA
C   FTA     : TENSEUR PRODUIT DE FMP ET FMAM
C   ETDPN1  : 
C   JM      : DETERMINANT DE FM 
C   JP      : DETERMINANT DE FMP 

C---------------------------------------------------------------
C
C       CALCUL DE ETDPN1
C
C---------------------------------------------------------------
      REAL*8 FMAM(3,3),FTA(3,3)
      REAL*8 ETDPN1(3,3),JM,JP
      
C ---------------------------INITIALISATION---------------------
      CALL R8INIR(9,0.D0,FMAM,1)
      CALL R8INIR(9,0.D0,FTA,1)
      CALL R8INIR(9,0.D0,ETDPN1,1)
      JM=0.D0
      JP=0.D0
      
C--------------------------------CALCUL DE e~_(n+alpha)---------
C      CALCUL DE f~_(n+alpha)

C      CALCUL DE L'INVERSE DE FMA = F_(N+ALPHA) NOTE FMAM
       CALL MATINV('S',3,FMA,FMAM,DET)
       
C      CALCUL DE F_(n+1)* FMAM= f~_(n+alpha) = FTA
       CALL PMAT(3,FMP,FMAM,FTA)
                     
C      CALCUL DE e~_(n+alpha) = ETDPN1
       CALL UTBTAB('ZERO',3,3,EDPN1,FTA,WORK,ETDPN1)        
                        
C      DETERMINANT DES MATRICE F A L INSTANT T-(JM) ET T+(JP)
       JM = FM(1,1)*(FM(2,2)*FM(3,3)-FM(2,3)*FM(3,2))
     &     - FM(2,1)*(FM(1,2)*FM(3,3)-FM(1,3)*FM(3,2))
     &     + FM(3,1)*(FM(1,2)*FM(2,3)-FM(1,3)*FM(2,2))
       JP = FMP(1,1)*(FMP(2,2)*FMP(3,3)-FMP(2,3)*FMP(3,2))
     &     - FMP(2,1)*(FMP(1,2)*FMP(3,3)-FMP(1,3)*FMP(3,2))
     &     + FMP(3,1)*(FMP(1,2)*FMP(2,3)-FMP(1,3)*FMP(2,2))

       END
