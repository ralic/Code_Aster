      SUBROUTINE HSAME ( VECTT , DUDX , HSM1 , HSM2 )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 27/01/99   AUTEUR D6BHHMA M.ALMIKDAD 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C
      IMPLICIT NONE
C
      REAL * 8 VECTT ( 3 , 3 )
C
      REAL * 8 DUDX ( 9 )
C
      REAL * 8 HSM1 ( 3 , 9 )
C
      REAL * 8 HSM2 ( 3 , 9 )
C
      REAL * 8 HFM ( 3 , 6 )
C
      REAL * 8 SA1 ( 6 , 9 )
C
      REAL * 8 SA2 ( 6 , 9 )
C
CDEB
C
C
C     CONSTRUCTION DE  HFM  ( 3 , 6 ) MEMBRANE-FLEXION
C                                     ( ROUTINE HFMSS )
C
C
      HFM(1,1)=VECTT(1,1)*VECTT(1,1)
      HFM(1,2)=VECTT(1,2)*VECTT(1,2)
      HFM(1,3)=VECTT(1,3)*VECTT(1,3)
      HFM(1,4)=VECTT(1,1)*VECTT(1,2)
      HFM(1,5)=VECTT(1,1)*VECTT(1,3)
      HFM(1,6)=VECTT(1,2)*VECTT(1,3)
C
      HFM(2,1)=VECTT(2,1)*VECTT(2,1)
      HFM(2,2)=VECTT(2,2)*VECTT(2,2)
      HFM(2,3)=VECTT(2,3)*VECTT(2,3)
      HFM(2,4)=VECTT(2,1)*VECTT(2,2)
      HFM(2,5)=VECTT(2,1)*VECTT(2,3)
      HFM(2,6)=VECTT(2,2)*VECTT(2,3)
C
      HFM(3,1)=2*VECTT(1,1)*VECTT(2,1)
      HFM(3,2)=2*VECTT(1,2)*VECTT(2,2)
      HFM(3,3)=2*VECTT(1,3)*VECTT(2,3)
      HFM(3,4)=VECTT(2,1)*VECTT(1,2)+VECTT(1,1)*VECTT(2,2)
      HFM(3,5)=VECTT(2,1)*VECTT(1,3)+VECTT(1,1)*VECTT(2,3)
      HFM(3,6)=VECTT(2,2)*VECTT(1,3)+VECTT(1,2)*VECTT(2,3)
C
C
C---- MATRICES 
C      SA1 ( 6 , 9 ) = ( S ) + 1 / 2 ( A ( DUDX ) ) POUR DEF TOT
C      SA2 ( 6 , 9 ) = ( S ) +       ( A ( DUDX ) ) POUR DEF DIF
C
      CALL MATSA ( DUDX , SA1 , SA2 )
C
C---- MATRICE 
                HSM1 ( 3 , 9 ) = HFM(3,6) * SA1 ( 6 , 9 )
C
      CALL PROMAT ( HFM   , 3 , 3 , 6 ,
     &              SA1   , 6 , 6 , 9 , 
     &              HSM1  )
C
C---- MATRICE 
                HSM2 ( 3 , 9 ) = HFM(3,6) * SA2 ( 6 , 9 )
C
      CALL PROMAT ( HFM   , 3 , 3 , 6 ,
     &              SA2   , 6 , 6 , 9 , 
     &              HSM2  )
C
C
C
CFIN
C
      END
