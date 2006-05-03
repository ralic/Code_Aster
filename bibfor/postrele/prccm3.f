      SUBROUTINE PRCCM3 ( NOMMAT, PARA, SM, SN, SP, KE, SALT, NADM )
      IMPLICIT   NONE
      REAL*8              PARA(*), SM, SN, SP, KE, SALT, NADM
      CHARACTER*(*)       NOMMAT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 02/05/2006   AUTEUR MCOURTOI M.COURTOIS 
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
C     OPERATEUR POST_RCCM: CALCUL DU KE, SALT, NADM
C
C     ------------------------------------------------------------------
      REAL*8       UN, XM, XN, SNS3, TROISM,R8MAEM
      CHARACTER*2  CODRET
      LOGICAL       ENDUR
C     ------------------------------------------------------------------
C
      UN     =  1.0D0
      TROISM = 3.0D0 * SM
C
C --- CALCUL DU COEFFICIENT DE CONCENTRATION ELASTO-PLASTIQUE KE :
C     ----------------------------------------------------------
C
C --- SI SN < 3*SM  KE = 1 :
C     --------------------
      IF ( SN .LT. TROISM ) THEN
         KE = UN
C
C --  SI 3*SM < SN < 3*M*SM 
C --- KE = 1 +((1-N)/(N*(M-1)))*((SN/(3*SM))-1) :
C             ------------------------------------- ---            
      ELSEIF ( SN .LT. 3.D0*PARA(1)*SM ) THEN
         XM = PARA(1)
         XN = PARA(2)
         SNS3 = SN / 3.D0
         KE = UN+((UN-XN)/(XN*(XM-UN)))*((SNS3/SM)-UN)
C
C --- SI 3*M*SM < SN   KE = 1/N :
C     -------------------------
      ELSE
         KE = UN / PARA(2)
      ENDIF
C
C
C --- CALCUL DE LA CONTRAINTE EQUIVALENTE ALTERNEE SALT 
C --- PAR DEFINITION SALT = 0.5*EC/E*KE*SP(TEMP1,TEMP2) :
C     -------------------------------------------------
      SALT = 0.5D0 * PARA(3) * KE * SP
C
C
C --- CALCUL DU NOMBRE DE CYCLES ADMISSIBLE NADM EN UTILISANT
C --- LA COURBE DE WOHLER AUX EXTREMITES DU CHEMIN :
C     --------------------------------------------
      CALL LIMEND( NOMMAT,SALT,'WOHLER',ENDUR)
      IF (ENDUR) THEN
         NADM=R8MAEM()
      ELSE
         CALL RCVALE ( NOMMAT, 'FATIGUE', 1, 'SIGM    ', SALT, 1,
     +                      'WOHLER  ', NADM, CODRET, 'F ' )
         IF ( NADM .LT. 0 ) THEN
            CALL UTDEBM ('A','WOHLER','NOMBRE DE CYCLES ADMISSIBLES'//
     +                       ' NEGATIF, VERIFIER LA COURBE DE WOHLER')
            CALL UTIMPR ('L','   CONTRAINTE CALCULEE = ',1,SALT)
            CALL UTIMPR ('L','   NADM = ',1,NADM)
            CALL UTFINM ()
         ENDIF
      ENDIF
C
      END
