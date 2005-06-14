      SUBROUTINE CALCVH(NBMAT,MATERF,ETA,VP,SIGEQE,VH,VG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/06/2005   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        VH,VG,MATERF(NBMAT,2),ETA,VP(3),SIGEQE
C ======================================================================
C --- LOI DE HOEK BROWN :CALCUL DE TERMES DE LA FONCTION DE CHARGE (H/G)
C ======================================================================
C IN     NBMAT  NOMBRE DE DONNEES MATERIAU -----------------------------
C IN     MATERF DONNES MATERIAU ----------------------------------------
C IN     VP     VALEURS PROPRES DU DEVIATEUR ELASTIQUE -----------------
C IN/OUT VH     VALEUR DE LA FONCTION H --------------------------------
C IN/OUT VG     VALEUR DE LA FONCTION G --------------------------------
C ======================================================================
      REAL*8       AUX4,K,MU,UN,TROIS
C ======================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
C ======================================================================
      MU     = MATERF(4,1)
      K      = MATERF(5,1)
      AUX4   = TROIS*MU/SIGEQE
      VH     = UN/(ETA+UN)
      VG     = VH*(TROIS*K*ETA+AUX4*VP(3))      
C ======================================================================
      END
