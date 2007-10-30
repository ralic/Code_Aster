      SUBROUTINE LKCALG(DFDSIG,VECN,G,DEVGII)

      IMPLICIT      NONE
      REAL*8        DFDSIG(6), VECN(6), G(6), DEVGII
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2007   AUTEUR ELGHARIB J.EL-GHARIB 
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
C =================================================================
C --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
C =================================================================
C --- BUT : CALCUL DE G=df/dsig-(df/dsig*n)*n----------------------
C =================================================================
C IN  : DFDSIG : df/dsig ------------------------------------------
C        VECN  : --------------------------------------------------
C OUT : G : G=df/dsig-(df/dsig*n)*n -------------------------------
C      DEVGII : SECOND INVARIANT DE G -----------------------------
C =================================================================
      COMMON /TDIM/   NDT , NDI
      INTEGER NDI, NDT, I
      REAL*8  DEVG(6), FACT1
      
C =================================================================
C --- CALCUL DE G -------------------------------------------------
C =================================================================
      CALL R8INIR(6,0.D0,G,1)
     
      CALL LCPRSC(DFDSIG, VECN, FACT1)

      DO 10 I = 1, NDT
      G(I) = DFDSIG(I) - FACT1 * VECN(I)
 10   CONTINUE      
C =================================================================
C --- CALCUL DU DEVIATEUR DE G ET DE SA NORME ---------------------
C =================================================================
      CALL     LCDEVI(G,DEVG)
      CALL LCPRSC(DEVG,DEVG,DEVGII)
      DEVGII = SQRT(DEVGII)
C =================================================================
      END
