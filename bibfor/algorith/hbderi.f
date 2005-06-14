      SUBROUTINE HBDERI(GAMMA,NBMAT,MATERF,VG,ETA,PARAM2,PARAME)
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
      REAL*8        GAMMA,MATERF(NBMAT,2),PARAME(5),VG,ETA,PARAM2(4)
C ======================================================================
C --- HOEK BROWN : CALCUL DES DERIVEES DES FONCTIONS DE LA VARIABLES ---
C --- D ECROUISSAGE PAR RAPPORT A LA VARIABLE D ECROUISSAGE GAMMA ------
C --- DERIVE : S*SIG_C**2, M*SIG_C, B, VH, VG --------------------------
C ======================================================================
C IN  GAMMA  VALEUR DE LA VARIABLE D ECROUISSAGE -----------------------
C IN  NBMAT  NOMBRE DE DONNEES MATERIAU --------------------------------
C IN  MATERF DONNEES MATERIAU ------------------------------------------
C IN  VG     VALEUR DE LA FONCTION G DE GAMMA --------------------------
C IN  PARAM2 VALEUR DES PARAMETRES D ECROUISSAGE S*SIGC2,M*SIGC,B,PHI---
C OUT PARAME DERIVEES DES PARAMETRES D ECROUISSAGE S*SIGC2,M*SIGC,B,H,G 
C ======================================================================
      REAL*8       AUX1,AUX2,AUX3,AUX4,AUX5,AUX6,AUX7,TOTO
      REAL*8       GRUP,GRES,SEND,SRUP,MEND,MRUP
      REAL*8       AP,DP,PPHI1,K,PPHI2,PI,R8PI,PPHI0
C ======================================================================
C --- RECUPERATION DES DONNEES MATERIAU --------------------------------
C ======================================================================
      PI     = R8PI()/180.0D0        
      GRUP   = MATERF(1,2)
      GRES   = MATERF(2,2)
      MEND   = MATERF(5,2)
      MRUP   = MATERF(6,2)
      SEND   = MATERF(3,2)
      SRUP   = MATERF(4,2)
      AP     = MATERF(11,2)
      DP     = MATERF(12,2)
      PPHI1  = MATERF(9,2)
      K      = MATERF(5,1)
      PPHI2  = MATERF(15,2)
      PPHI0  = MATERF(16,2)      
C ======================================================================
      IF (GAMMA.LT.GRUP) THEN
         AUX2 = (SRUP-SEND)/GRUP
         AUX3 = (MRUP-MEND)/GRUP
         AUX4 = 0.D0
         AUX5 =-2.0D0*(PPHI1-PPHI0)*PI*COS(PARAM2(4)*PI)/
     &                 (GRUP*3.0D0*(1.0D0+SIN(PARAM2(4)*PI))**2)
         AUX6 = AUX5*VG*(ETA+1.0D0) 
     &      +6.0D0*K*(PPHI1-PPHI0)*PI*COS(PARAM2(4)*PI)/
     &      (GRUP*(3.0D0+SIN(PARAM2(4)*PI))*(1.0D0+SIN(PARAM2(4)*PI)))
C ======================================================================
      ELSE IF (GAMMA.LT.GRES) THEN
         AUX2 = 0.D0
         AUX3 = 0.D0
         AUX4 = 2.D0*AP*GAMMA + DP
         AUX5 =-2.0D0*(PPHI2-PPHI1)*PI*COS(PARAM2(4)*PI)/
     &             ((GRES-GRUP)*3.0D0*(1.0D0+SIN(PARAM2(4)*PI))**2)
         AUX6 = AUX5*VG*(ETA+1.0D0) 
     &      +6.0D0*K*(PPHI2-PPHI1)*PI*COS(PARAM2(4)*PI)/
     &           ((GRES-GRUP)*(3.0D0+SIN(PARAM2(4)*PI))
     &     *(1.0D0+SIN(PARAM2(4)*PI)))
C ======================================================================
      ELSE
         AUX2 = 0.D0
         AUX3 = 0.D0
         AUX4 = 0.D0
        AUX5 = 0.0D0
        AUX6 = 0.0D0
      ENDIF
C ======================================================================
C --- STOCKAGE ---------------------------------------------------------
C ======================================================================
      PARAME(1) = AUX2
      PARAME(2) = AUX3
      PARAME(3) = AUX4
      PARAME(4) = AUX5
      PARAME(5) = AUX6
C ======================================================================
      END
