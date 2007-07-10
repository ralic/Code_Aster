      SUBROUTINE FCWEIB(NRUPT,CALS,SK,SIGW,NUR,NT,NBRES,INDTP,NBTP,
     +                  M,FC,DFC)        
      IMPLICIT NONE
      INTEGER       NRUPT,NUR(*),NT(*),NBRES,INDTP(*),NBTP
      REAL*8        SIGW(*),M,FC,DFC,S1,S2,SK(*)
      LOGICAL       CALS
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
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
C     ----------------------------------------------------------------
C     AUTEUR : M. BONNAMY
C     ----------------------------------------------------------------
C
C     BUT: CALCUL DE RECALAGE DES PARAMETRES DE WEIBULL PAR LA
C          METHODE DU MAXIMUM DE VRAISSEMBLANCE
C
C     ----------------------------------------------------------------
C
C     NRUPT        /IN/:NOMBRE DE CONTRAINTES
C     CALS         /IN/:TRUE SI SIGMA_U EST FIXE
C     SK           /IN/:PARAMETRE SIGMA-U(K) DE WEIBULL
C     SIGW         /IN/:CONTRAINTES DE WEIBULL AUX INSTANTS DE RUPTURE
C     NUR          /IN/:NUMERO DE RESULTAT ASSOCIEE A 
C                       LA CONTRAINTE SIGW(I)
C     NT           /IN/:DIMENSION DE LA SOUS-BASE CORRESPONDANT A LA 
C                       TEMPERATURE T
C     NBRES        /IN/:NOMBRE DE BASES DE RESULTATS
C     INDTP        /IN/:INDICE DE TEMPERATURE POUR CHAQUE RESULTAT
C     NBTP         /IN/:NOMBRE DE TEMPERATURE DIFFERENTES
C
C     M            /OUT/:PARAMETRE M(K+1)DE WEIBULL
C     FC           /OUT/:FONCTION F(M) DE WEIBULL
C     DFC          /OUT/:DERIVEE DE LA FONCTION DF(M) DE WEIBULL
C     
C
C     ----------------------------------------------------------------
C
      REAL*8  SWM,SLW,SLWM,SL2WM,SL2BWM,SNT,R8MAEM,MAXR,MAXM
      REAL*8 VALR
      INTEGER I,ITP,IR
     
C     ----------------------------------------------------------------
C
         SLW   = 0.D0  
         SL2BWM = 0.D0
         MAXR =  R8MAEM()
         MAXM = LOG (MAXR) / LOG ( NRUPT*SIGW(NRUPT) )
         IF (M.GE.MAXM) THEN
           VALR = MAXM
           CALL U2MESG('S','UTILITAI8_22',0,' ',0,0,1,VALR)
         END IF           
         IF (M.LE.0.D0) THEN
           VALR = M
           CALL U2MESG('S','UTILITAI8_23',0,' ',0,0,1,VALR)
         END IF
C
         DO 10 I=1,NRUPT
C
           IF (CALS) THEN
             SLW = SLW + ( LOG ( SIGW(I)/SK(1) ) ) 
     +                     * ( 1.D0-(SIGW(I)/SK(1))**M ) 
             SL2BWM = SL2BWM + ( (SIGW(I)/SK(1)) ** M ) 
     +                * ( LOG (SIGW(I)/SK(1)) * LOG (SIGW(I)/SK(1)) )
           ELSE
             SLW = SLW + LOG ( SIGW(I) )
           END IF
C
 10      CONTINUE
C
         S1 = 0.D0 
         S2 = 0.D0
         DO 210 ITP=1,NBTP
C   
          SNT = 0.D0

          DO 200 IR=1 , NBRES
C
            IF (INDTP(IR).EQ.ITP) SNT = SNT + NT(IR)

 200      CONTINUE

            SWM   = 0.D0
            SLWM  = 0.D0
            SL2WM = 0.D0
            DO 300 I=1,NRUPT
C  
               IF (INDTP(NUR(I)).EQ.ITP) THEN
                SWM = SWM +  SIGW(I) ** M
                SLWM = SLWM + (SIGW(I) ** M )* ( LOG ( SIGW(I) ) )
                SL2WM = SL2WM + (SIGW(I) ** M )* ( LOG ( SIGW(I) ) * 
     +                                             LOG ( SIGW(I) ) )
               END IF
C
300         CONTINUE 
C     
            S1 = S1 +  SNT * SLWM/SWM  
            S2 = S2 +  SNT * ( (SL2WM/SWM)*SWM - 
     +                                  (SLWM/SWM)*SLWM ) /SWM 
C
 210      CONTINUE
C
         IF (CALS) THEN
           FC = NRUPT
           FC = FC / M + SLW 
          DFC = NRUPT
          DFC = - DFC * ( 1.D0/(M*M) ) - SL2BWM
         ELSE
           FC = NRUPT
           FC = FC / M + SLW - S1
          DFC = NRUPT
          DFC = - DFC * ( 1.D0/(M*M) ) - S2
         END IF
C
C
      END
