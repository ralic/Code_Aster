      SUBROUTINE DPMATA( MOD, MATER, ALPHA, DP, DPDENO, PPLUS,
     +                                            SE, SEQ, PLAS, DSDE )
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/03/2008   AUTEUR MAHFOUZ D.MAHFOUZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8        MATER(5,2), DP, DPDENO, SE(6), SEQ, DSDE(6,6)
      REAL*8        PLAS, ALPHA, PPLUS
      CHARACTER*8   MOD
C =====================================================================
C --- MISE A JOUR DES CONTRAINTES -------------------------------------
C =====================================================================
      INTEGER    II, JJ, NDT, NDI
      REAL*8     UN, DEUX, TROIS, YOUNG, NU, TROISK, DEUXMU, DSEDE(6,6)
      REAL*8     BIDON(6,6), PMAT1(6,6), PMAT2(6,6), PMAT3(6,6), PARAM1
      REAL*8     PMAT4(6,6), VUNITE(6), VECT1(6), VECT2(6), VECT3(6)
      REAL*8     PULT, QUATRE, NEUF, MATER2(5,2)
      PARAMETER ( NEUF   =  9.0D0 )
      PARAMETER ( QUATRE =  4.0D0 )
      PARAMETER ( TROIS  =  3.0D0 )
      PARAMETER ( DEUX   =  2.0D0 )
      PARAMETER ( UN     =  1.0D0 )
C =====================================================================
      COMMON /TDIM/   NDT, NDI
C =====================================================================
C --- AFFECTATION DES VARIABLES ---------------------------------------
C =====================================================================
      YOUNG  = MATER(1,1)
      NU     = MATER(2,1)
      TROISK = YOUNG / (UN-DEUX*NU)
      DEUXMU = YOUNG / (UN+NU)
      PULT   = MATER(4,2)
      CALL     LCINMA ( 0.0D0, DSDE   )
C =====================================================================
C --- CAS ELASTIQUE ---------------------------------------------------
C =====================================================================
      IF (PLAS.EQ.0.0D0) THEN
         CALL LCOPLI ( 'ISOTROPE', MOD, MATER(1,1), DSDE )
         GOTO 9999
      ELSE
         IF ( PLAS .NE. 2.0D0   .OR.
     +        PPLUS  .LT. PULT         ) THEN
C =====================================================================
C --- INITIALISATIONS DE MATRICES ET VECTEURS UTILES ------------------
C =====================================================================
            CALL     LCINMA ( 0.0D0, DSEDE  )
            CALL     LCINMA ( 0.0D0, BIDON  )
            CALL     LCINMA ( 0.0D0, PMAT1  )
            CALL     LCINMA ( 0.0D0, PMAT2  )
            CALL     LCINMA ( 0.0D0, PMAT3  )
            CALL     LCINMA ( 0.0D0, PMAT4  )
            CALL     LCINVE ( 0.0D0, VUNITE )
            CALL     LCINVE ( 0.0D0, VECT1  )
            CALL     LCINVE ( 0.0D0, VECT2  )
            CALL     LCINVE ( 0.0D0, VECT3  )
C =====================================================================
C --- CALCUL DU VECTEUR UNITE -----------------------------------------
C =====================================================================
            DO 120 II = 1, NDI
               VUNITE(II) = UN
 120        CONTINUE
            IF (PLAS.EQ.1.0D0) THEN
C =====================================================================
C --- CAS PLASTIQUE ---------------------------------------------------
C =====================================================================
C --- CALCUL DE DSEDE -------------------------------------------------
C =====================================================================
               DO 30 II = 1, NDI
                  DO 40 JJ = 1, NDI
                     DSEDE(II,JJ) = - DEUXMU/TROIS
 40               CONTINUE
 30            CONTINUE
               DO 50 II = 1, NDT
                  DSEDE(II,II) = DSEDE(II,II) + DEUXMU
 50            CONTINUE
C =====================================================================
C --- CALCUL DE PMAT1 -------------------------------------------------
C =====================================================================
               PARAM1 = UN - TROIS * DEUXMU * DP / DEUX / SEQ
               CALL     LCPRSM( PARAM1, DSEDE,  PMAT1 )
C =====================================================================
C --- CALCUL DE PMAT2 -------------------------------------------------
C =====================================================================
               PARAM1 = TROISK / TROIS
               CALL     LCPRTE( VUNITE, VUNITE, BIDON )
               CALL     LCPRSM( PARAM1, BIDON,  PMAT2 )
C =====================================================================
C --- CALCUL DE PMAT3 -------------------------------------------------
C =====================================================================
               PARAM1 = NEUF*DEUXMU*DEUXMU*DP/QUATRE/SEQ/SEQ/SEQ
               CALL     LCPRTE( SE, SE, BIDON )
               CALL     LCPRSM( PARAM1, BIDON,  PMAT3 )
C =====================================================================
C --- CALCUL DE PMAT4 -------------------------------------------------
C =====================================================================
               PARAM1 = TROIS * DEUXMU / DEUX / SEQ
               CALL     LCPRSV( PARAM1, SE, VECT1 )
               PARAM1 = TROISK * ALPHA
               CALL     LCPRSV( PARAM1, VUNITE, VECT2 )
               CALL     LCSOVE( VECT1, VECT2, VECT3 )
               PARAM1 = - UN / DPDENO
               CALL     LCPRTE( VECT3, VECT3, BIDON )
               CALL     LCPRSM( PARAM1, BIDON,  PMAT4 )
C =====================================================================
C --- CALCUL DE L OPERATEUR TANGENT -----------------------------------
C =====================================================================
               CALL LCSOMA(PMAT1, PMAT2, BIDON)
               CALL LCSOMA(BIDON, PMAT3, PMAT1)
               CALL LCSOMA(PMAT1, PMAT4, DSDE )
            ELSE IF (PLAS.EQ.2.0D0) THEN
C =====================================================================
C --- CAS DE LA PROJECTION AU SOMMET ----------------------------------
C =====================================================================
               PARAM1 = TROISK/TROIS - TROISK*TROISK*ALPHA*ALPHA/DPDENO
               CALL     LCPRTE( VUNITE, VUNITE, BIDON )
               CALL     LCPRSM( PARAM1, BIDON,  DSDE  )
            ENDIF
         ELSE
C =====================================================================
C --- CAS DE LA PROJECTION AU SOMMET AVEC P > P_ULT -------------------
C --- DANS CE CAS ON PROPOSE DE CONSIDERER L'OPERATEUR TANGENT A UN ---
C --- FACTEUR MULTIPLICATIF PRES, QUE L'ON PREND ARBITRAIREMENT EGAL --
C --- A YOUNG/10E6 ----------------------------------------------------
C =====================================================================
            MATER2(1,1) = MATER(1,1)/1.0D6
            MATER2(2,1) = MATER(2,1)
            MATER2(3,1) = MATER(3,1)
            MATER2(1,2) = MATER(1,2)
            MATER2(2,2) = MATER(2,2)
            MATER2(3,2) = MATER(3,2)
            MATER2(3,2) = MATER(3,2)
            CALL LCOPLI ( 'ISOTROPE', MOD, MATER2(1,1), DSDE )
         ENDIF
      ENDIF
C =====================================================================
 9999 CONTINUE
C =====================================================================
      END
