      SUBROUTINE REDRPR( MOD, IMATE, SIGP, VIM, VIP, DSDE, ICODE )
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER       IMATE, ICODE
      REAL*8        VIM(*), VIP(*), SIGP(*), DSDE(6,6)
      CHARACTER*8   MOD
C =====================================================================
C --- ROUTINE POUR LES RECUPERATIONS DES DONNES -----------------------
C --- POUR LE CALCUL DU TENSEUR TANGENT -------------------------------
C --- ICODE = 0 CORRESPONDT AU CAS ELASTIQUE --------------------------
C --- ICODE = 1 SINON -------------------------------------------------
C =====================================================================
      INTEGER      NPG, NDT, NDI, NVI, TYPEDP
      REAL*8       PMOINS, PPLUS, MATERF(4,2), HOOKF(6,6), DPDENO, DP
      REAL*8       SE(6), SEQ, PLAS, ALPHA, DPLITG, DPPATG, PHI, VALEUR
      REAL*8       SS(6), SII, SQ, YOUNG, NU, DEUXMU, UN, DEUX, TROIS
      CHARACTER*8  TYPMOD(2)
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C =====================================================================
C --- INITIALISATION --------------------------------------------------
C =====================================================================
      PARAMETER  ( UN    = 1.0D0 )
      PARAMETER  ( DEUX  = 2.0D0 )
      PARAMETER  ( TROIS = 3.0D0 )
C =====================================================================
      CALL LCINVE ( 0.0D0, SE   )
      CALL LCINVE ( 0.0D0, SS   )
      CALL LCINMA ( 0.0D0, DSDE )
C =====================================================================
C --- RECUPERATION DES DONNEES MATERIAUX ------------------------------
C =====================================================================
      CALL DPMATE(MOD, IMATE, MATERF, NDT, NDI, NVI, TYPEDP)
      YOUNG  = MATERF(1,1)
      NU     = MATERF(2,1)
      DEUXMU = YOUNG / (UN+NU)
      PMOINS = VIM(1)
      PPLUS  = VIP(1)
      DP     = PPLUS - PMOINS
      PLAS   = VIP(NVI)
      IF (PLAS.EQ.0.0D0) THEN
         ICODE = 0
         GO TO 999
      ENDIF
      ICODE = 1
C =====================================================================
C --- OPERATEUR ELASTIQUE LINEAIRE ISOTROPE ---------------------------
C =====================================================================
      CALL LCOPLI ( 'ISOTROPE', MOD, MATERF(1,1), HOOKF )
C =====================================================================
C --- INTEGRATION ELASTIQUE : SIGF = HOOKF EPSP -----------------------
C =====================================================================
      CALL     LCDEVI(SIGP,SS)
      CALL     PSCAL (NDT,SS,SS,SII)
      SQ     = SQRT  (TROIS*SII/DEUX)
      SEQ    = SQ + TROIS*DEUXMU*DP/DEUX
      VALEUR = UN / (UN - TROIS*DEUXMU*DP/DEUX/SEQ)
      CALL     LCPRSV( VALEUR, SS, SE )
C =====================================================================
C --- CALCUL DES CONTRAINTES ------------------------------------------
C =====================================================================
      IF (TYPEDP.EQ.1) THEN
C =====================================================================
C --- RESOLUTION DU SYSTEME -------------------------------------------
C =====================================================================
         ALPHA  = MATERF(3,2)
         DPDENO = DPLITG( MATERF, PPLUS )
      ELSE IF (TYPEDP.EQ.2) THEN
C =====================================================================
C --- RESOLUTION DU SYSTEME -------------------------------------------
C =====================================================================
         PHI    = MATERF(2,2)
         ALPHA  = DEUX * SIN(PHI) / (TROIS - SIN(PHI))
         DPDENO = DPPATG( MATERF, PPLUS )
      ENDIF
      CALL DPMATA( HOOKF, MATERF, ALPHA, DP, DPDENO,
     +                                             SE, SEQ, PLAS, DSDE)
C =====================================================================
 999  CONTINUE
C =====================================================================
      END
