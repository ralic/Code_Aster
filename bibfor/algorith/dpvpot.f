      SUBROUTINE DPVPOT (MOD,VIM,VIP,NBMAT,MATER,SIG,DT,DP,PLAS,DSIDEP)
C
      IMPLICIT      NONE
      INTEGER       NDT, NDI
      INTEGER       NBMAT
      REAL*8        DT, DP, PLAS
      REAL*8        MATER(NBMAT,2), VIM(4), VIP(4), SIG(6), DSIDEP(6,6)
      REAL*8        MATER2(3,2)
      CHARACTER*8   MOD
C =====================================================================
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
C --- BUT   OPERATEUR TANGENT COHERENT POUR LA LOI --------------------
C --- VISC_DRUC_PRAG --------------------------------------------------
C =====================================================================
      INTEGER  II, JJ
      REAL*8   ZERO,UN, DEUX, TROIS, NEUF,UNSTR
      REAL*8   TROISK, DEUXMU, K, MU
      REAL*8   PREF, A, N
      REAL*8   FONC1, FONC2, FONC3, FONC4, FONC, FONCP
      REAL*8   BETA
      REAL*8   ALPHAM, BETAM, RM
      REAL*8   DALPDP, DBETDP, DRDP
      REAL*8   FONECM(3),FONECP(3), FONDER(3)
      REAL*8   TRACE, SII, SEQ, I1
      REAL*8   SCAL1, SCAL2, SCAL3, SCAL4, SCAL5, SCAL6, SCAL7, SCAL8
      REAL*8   SCAL11, SCAL12
      REAL*8   DENOM, INT, DFDP, CONST, CONST1
      REAL*8   KRON(6)
      REAL*8   DSEDE(6,6)
      REAL*8   S(6)
      REAL*8   DSDSIG(6,6),DQDSIG(6) ,DFDSIG(6),DPDSIG(6)
      REAL*8   ADIDSI(6),BDIDSI(6),CDIDSI(6)
      REAL*8   DGDSIG(6)
      REAL*8   DQDEPS(6,6),DSDEPS(6,6), DPDEPS(6)
      REAL*8   DI1EDE(6)  , DI1DE(6)
      REAL*8   VECT1(6),VECT2(6),VECT3(6), VECT4(6)
      REAL*8   MATR1(6,6) , MATR2(6,6),MATR3(6,6)
      REAL*8   MATR1A(6,6), MATR1B(6,6)
      REAL*8   PART1(6,6) , PART2(6,6) , PART3(6,6), PART4(6,6)
      REAL*8   INTER1(6,6), INTER2(6,6), INTER3(6,6)
      REAL*8   INT2A(6,6), INT2B(6,6), DSDEPT(6,6)
      REAL*8   TOL
C =====================================================================
      PARAMETER  ( ZERO  = 0.0D0 )
      PARAMETER  ( UN    = 1.0D0 )
      PARAMETER  ( DEUX  = 2.0D0 )
      PARAMETER  ( TROIS = 3.0D0 )
      PARAMETER  ( NEUF  = 9.0D0 )
      PARAMETER  ( UNSTR = 1.D0/3.0D0)
      PARAMETER  ( TOL = 1.D-12)
      
C =====================================================================
      COMMON /TDIM/   NDT , NDI
C =================================================================
      DATA   KRON /UN , UN , UN , ZERO ,ZERO ,ZERO/
C =================================================================
C ---- RECUPERATION DES PARAMETRES MATERIAUX ----------------------
C =================================================================
      
          MU        = MATER(4,1)
          K         = MATER(5,1)
          PREF      = MATER(1,2)
          A         = MATER(2,2)
          N         = MATER(3,2)
          TROISK    = TROIS*K
          DEUXMU    = DEUX*MU
C =====================================================================
C --- INITIALISATIONS DES VECTEURS ------------------------------------
C =====================================================================
          CALL     LCINVE ( 0.0D0, VECT1   )
          CALL     LCINVE ( 0.0D0, VECT2   )
          CALL     LCINVE ( 0.0D0, VECT3   )
          CALL     LCINVE ( 0.0D0, VECT4   )      
          CALL     LCINVE ( 0.0D0, DQDSIG  )
          CALL     LCINVE ( 0.0D0, DFDSIG  )
          CALL     LCINVE ( 0.0D0, DPDSIG  )
          CALL     LCINVE ( 0.0D0, DGDSIG  )
          CALL     LCINVE ( 0.0D0, DPDEPS  )
          CALL     LCINVE ( 0.0D0, DI1EDE  )
          CALL     LCINVE ( 0.0D0, DI1DE   )
          CALL     LCINVE ( 0.0D0, ADIDSI  )
          CALL     LCINVE ( 0.0D0, BDIDSI  )
          CALL     LCINVE ( 0.0D0, CDIDSI  )
          CALL     LCINVE ( 0.0D0, S       )
C =====================================================================
C --- INITIALISATIONS DES MATRICES ------------------------------------
C =====================================================================
          CALL     LCINMA ( 0.0D0, MATR1   )
          CALL     LCINMA ( 0.0D0, MATR1A  )
          CALL     LCINMA ( 0.0D0, MATR1B  )
          CALL     LCINMA ( 0.0D0, MATR2   )
          CALL     LCINMA ( 0.0D0, MATR3   )
          CALL     LCINMA ( 0.0D0, PART1   )
          CALL     LCINMA ( 0.0D0, PART2   )
          CALL     LCINMA ( 0.0D0, PART3   )
          CALL     LCINMA ( 0.0D0, PART4   )
          CALL     LCINMA ( 0.0D0, INTER1  )
          CALL     LCINMA ( 0.0D0, INTER2  )
          CALL     LCINMA ( 0.0D0, INT2A )
          CALL     LCINMA ( 0.0D0, INT2B )
          CALL     LCINMA ( 0.0D0, INTER3  )
          CALL     LCINMA ( 0.0D0, DSDSIG  )
          CALL     LCINMA ( 0.0D0, DSDEPS  )
          CALL     LCINMA ( 0.0D0, DSDEPT  )
          CALL     LCINMA ( 0.0D0, DQDEPS  )
          CALL     LCINMA ( 0.0D0, DSIDEP  )
          CALL     LCINMA ( 0.0D0, DSEDE   )

          CALL LCOPLI ( 'ISOTROPE', MOD, MATER(1,1), DSEDE )
C =====================================================================
C --- CAS ELASTIQUE ---------------------------------------------------
      IF ((PLAS.EQ.0.0D0).OR.(DP.EQ.0.D0).OR. (ABS(DP).LT.TOL)) THEN
         CALL LCEQMA(DSEDE, DSIDEP)
          GOTO 9999
      ELSE
C =================================================================
C ----  CALCUL DU DEVIATEUR - DE LA CONTRAINTE EQUIVALENTE  -------
C ----  ET DE LA TRACE --------------------------------------------
C =================================================================
         CALL LCDEVI(SIG,S)
         CALL LCPRSC(S,S,SII)
         SEQ  = SQRT(TROIS*SII/DEUX)
         I1      = TRACE (NDI,SIG)

C =====================================================================
C --- FONCTIONS D ECROUISSAGE ET LEURS DERIVEES------------------------
C =====================================================================
         CALL DPVPVA (VIM, NBMAT, MATER, FONECM)
         CALL DPVPVA (VIP, NBMAT, MATER, FONECP)
         CALL DPVPDV (VIP, NBMAT, MATER, FONDER)

         ALPHAM  = FONECM(1)
         RM      = FONECM(2)
         BETAM   = FONECM(3)   

         BETA       = FONECP(3)   

         DALPDP = FONDER(1)
         DRDP     = FONDER(2)   
         DBETDP = FONDER(3)
         
         CONST = A*DT/(PREF)**N
C =====================================================================
C --- CALCUL DE DSIDEP ------------------------------------------------
C =====================================================================
               DO 30 II = 1, NDI
                  DO 40 JJ = 1, NDI
                     DSDSIG(II,JJ) = - UN/TROIS
 40               CONTINUE
 30            CONTINUE
               DO 50 II = 1, NDT
                  DSDSIG(II,II) = DSDSIG(II,II) + UN
 50            CONTINUE


C =====================================================================
C --- CALCUL DE LA TROISIEME PARTIE DU TERME DS/DEPS ------------------
C =====================================================================
C --- CALCUL DE DSIEQ/ DSIG -------------------------------------------
C =====================================================================
            
             SCAL1 = TROIS/DEUX/SEQ
             
             CALL LCPRMV(DSDSIG,S,DQDSIG)
             CALL LCPRSV(SCAL1,DQDSIG,DQDSIG)
C =====================================================================
C --- CALCUL DE ALPHA * DI1/ DSIG -------------------------------------
C =====================================================================
             CALL LCPRSV(ALPHAM,KRON,ADIDSI)
C =====================================================================
C --- CALCUL DE ALPHA_CONS * DI1/ DSIG * DP ---------------------------
C =====================================================================
             SCAL12 = DALPDP * DP
             CALL LCPRSV(SCAL12,KRON,BDIDSI)
C =====================================================================
C --- CALCUL DE Df/ DSIG ----------------------------------------------
C =====================================================================
             CALL LCSOVE(DQDSIG,ADIDSI,CDIDSI)
             CALL LCSOVE(CDIDSI,BDIDSI,DFDSIG)
C =====================================================================
C --- CALCUL DE DfDp --------------------------------------------------
C =====================================================================
             FONC1 = SEQ + ALPHAM * I1 - RM
        
             FONC2 = TROIS*MU + DRDP  - DALPDP*I1
     &         +NEUF*K *ALPHAM*BETAM
 
             FONC3 = NEUF*K*(ALPHAM*DBETDP+BETAM*DALPDP)
 
             FONC4 = NEUF*K*DALPDP*DBETDP
        
         
             FONC  = FONC1 - FONC2*DP - FONC3*DP**2 - FONC4*DP**3
             FONCP = -FONC2 -DEUX*DP*FONC3-TROIS*DP**2*FONC4
             
              IF (FONC .GT. ZERO) THEN
                  FONC = FONC
                ELSE
                  CALL LCEQMA(DSEDE, DSIDEP)
                  GOTO 9999
              ENDIF

             CONST1 = N * CONST *  FONC**(N-UN)
             DFDP = CONST1 * FONCP - UN

              IF (DFDP . EQ. ZERO) THEN
               CALL LCEQMA(DSEDE, DSIDEP)
               GOTO 9999
              ELSE
               DENOM = -UN / DFDP
              ENDIF 
C =====================================================================
C --- CALCUL DE Df/ DSIG ----------------------------------------------
C =====================================================================
             CALL LCPRSV(CONST1,DFDSIG,DFDSIG)
C =====================================================================
C --- CALCUL DE d deltap/dSIG -----------------------------------------
C =====================================================================
             CALL LCPRSV(DENOM,DFDSIG,DPDSIG)
C =====================================================================
C --- CALCUL DE d deltap/dEPS -----------------------------------------
C =====================================================================
             CALL LCPRMV(DSEDE,DPDSIG,DPDEPS)
C =====================================================================
C --- CALCUL DE 3GDT/SEQ *se * deltap/dEPS ----------------------------
C =====================================================================
             CALL LCPRTE(DPDEPS,S,MATR1A)
C =====================================================================
C --- TRANSPOSEE ------------------------------------------------------
C =====================================================================
             CALL LCTRMA(MATR1A,MATR1B)
C =====================================================================
C --- SYMETRISATION  --------------------------------------------------
C =====================================================================
                  DO 68 II = 1, NDT
                  DO 67 JJ = 1, NDT
                  MATR1(II,JJ) = UN/DEUX*(MATR1A(II,JJ)+MATR1B(II,JJ))
   67     CONTINUE
   68     CONTINUE         
           
             SCAL3 = -TROIS*MU/SEQ
      
             CALL LCPRSM(SCAL3, MATR1, PART3)

C =====================================================================
C --- CALCUL DE LA  PREMIERE PARTIE DU TERME DS/DEPS ------------------
C =====================================================================
C --- CALCUL DE dse / deps *(1-3GDP/SEQ) ----------------------------
C =====================================================================
             SCAL4 = DEUXMU * (UN - TROIS*MU * DP / SEQ)
             CALL LCPRSM(SCAL4, DSDSIG, PART1)
C =====================================================================
C --- CALCUL DE LA  DEUXIEME PARTIE DU TERME DS/DEPS ------------------
C =====================================================================
C --- CALCUL DE 3GDP/SEQ**2 *(se * dSEQ/dEPS ------------------------
C =====================================================================
             SCAL5 = NEUF * MU *MU* DP/SEQ/ SEQ/ SEQ         
             CALL LCPRTE(S,S,MATR3)
           
             CALL LCPRSM(SCAL5, MATR3 ,PART2)
C =====================================================================
C --- SOMMATION DES PARTIES DE ds/dEPS --------------------------------
C =====================================================================
             CALL LCSOMA(PART1,PART2,INTER1)
         
             CALL LCSOMA(INTER1,PART3,DSDEPS)
             
             CALL LCTRMA(DSDEPS,DSDEPT)
             DO 980 II   = 1, NDT
             DO 990 JJ = 1, NDT
               DSDEPS(II,JJ) = UN/DEUX*(DSDEPS(II,JJ)+DSDEPT(II,JJ))
 990      CONTINUE
 980      CONTINUE
C =====================================================================
C --- CALCUL  DU TERME DI/DEPS ----------------------------------------
C =====================================================================
C --- CALCUL DE dI1E/dEPS ---------------------------------------------
C =====================================================================
             CALL LCPRSV(TROISK,KRON,DI1EDE)
C =====================================================================
C --- CALCUL DE 9KBETAdp/dEPS -----------------------------------------
C =====================================================================
             SCAL6 = -NEUF*K*BETA
             CALL LCPRSV(SCAL6, DPDEPS, VECT2)
C =====================================================================
C --- CALCUL DE dI1/dEPS ----------------------------------------------
C =====================================================================
              CALL LCSOVE(DI1EDE,VECT2,DI1DE)
C =====================================================================
C --- CALCUL DE I * dI/dEPS -------------------------------------------
C =====================================================================
             CALL LCPRTE(KRON,DI1DE,INT2A)
C =====================================================================
C --- TRANSPOSEE DE I * dI/dEPS ---------------------------------------
C =====================================================================
             CALL LCTRMA(INT2A,INT2B)
C =====================================================================
C --- SYMETRISATION  --------------------------------------------------
C =====================================================================
             DO 98 II   = 1, NDT
             DO 99 JJ = 1, NDT
               INTER2(II,JJ) = UN/DEUX*(INT2A(II,JJ)+INT2B(II,JJ))
   99      CONTINUE
   98      CONTINUE
           CALL LCPRSM(UNSTR,INTER2,INTER2)
           CALL LCSOMA(DSDEPS, INTER2, DSIDEP)
           ENDIF     
C =====================================================================
 9999 CONTINUE
C =====================================================================
      END
