      FUNCTION DPVPDF(X,N,CONST,FONC1,FONC2, FONC3, FONC4) 
      IMPLICIT      NONE      
      REAL*8        X, N, CONST,CONST1
      REAL*8        FONC1, FONC2, FONC3, FONC4
      REAL*8        ZERO, UN, DEUX, TROIS
      REAL*8        DPVPDF, FONC, FONCP, DFDP
C =====================================================================
C --- LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER VISCOPLASTIQUE -------
C ---- VISC_DRUC_PRAG -------------------------------------------------
C --- EQUATION NON LINEAIRE EN DP -------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/11/2009   AUTEUR ELGHARIB J.EL-GHARIB 
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
C =====================================================================
      PARAMETER ( ZERO    =  0.0D0 )
      PARAMETER ( UN      =  1.0D0 )
      PARAMETER ( DEUX    =  2.0D0 )
      PARAMETER ( TROIS   =  3.0D0 )
C =====================================================================
      FONC = FONC1 - FONC2 *X  -
     &       FONC3 * X**2 - FONC4 * X**3
      FONCP = -FONC2 -DEUX*X*FONC3-TROIS*X**2*FONC4   
         IF (FONC .LT. ZERO) THEN
                FONC = ZERO
             ELSE
                FONC = FONC
         ENDIF
      CONST1 = N * CONST *  FONC**(N-UN)
      DFDP = CONST1 * FONCP - UN

      DPVPDF =  DFDP   
C =====================================================================
      END      
