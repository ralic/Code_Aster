      SUBROUTINE OPTIMW(METHOD,NRUPT,X,Y,PROB,SIGW,NT,NUR,NBRES,CALM,
     +                  CALS,MK,SK,MKP,SKP,IMPR,IFM,DEPT,INDTP,NBTP)
      IMPLICIT NONE
      INTEGER       NRUPT,NT(*),NBRES,NUR(*),IR,INDTP(*),NBTP,IFM
      REAL*8        X(*),Y(*),SIGW(*),MK,SK(*),MKP,SKP(*),PROB(*)
      CHARACTER*16  METHOD
      LOGICAL       CALM,CALS,IMPR,DEPT
C     ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 20/06/2001   AUTEUR T2BAXJM R.MASSON 
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
C     BUT: CALCUL DE RECALAGE DES PARAMETRES DE WEIBULL 
C
C     ----------------------------------------------------------------
C
C     METHOD       /IN/:METHODE DE CALAGE
C     NRUPT        /IN/:NOMBRE TOTAL DE CONTRAINTES DE WEIBULL
C     SIGW         /IN/:CONTRAINTES DE WEIBULL AUX INSTANTS DE RUPTURE
C     NT           /IN/:DIMENSION DE LA SOUS-BASE CORRESPONDANT A LA 
C                       TEMPERATURE T
C     NUR          /IN/:NUMERO DE RESULTAT ASSOCIEE A 
C                       LA CONTRAINTE SIGW(I)
C     NBRES        /IN/:NOMBRE DE BASES DE RESULTATS
C     MK           /IN/:PARAMETRE M(K)DE WEIBULL
C     SK           /IN/:PARAMETRE SIGMA-U(K) DE WEIBULL
C     CALM         /IN/:TRUE SI M EST FIXE
C     CALS         /IN/:TRUE SI SIGMA_U EST FIXE
C     IMPR         /IN/:IMPRESSION DETAILLEE
C     DEPT         /IN/:DEPENDANCE EN TEMPERATURE POUR SIGMA-U
C     INDTP        /IN/:INDICE DE TEMPERATURE POUR CHAQUE RESULTAT
C     NBTP         /IN/:NOMBRE DE TEMPERATURE DIFFERENTES
C
C     X,Y          /OUT/:VALEUR DES FONCTIONS LOG(SIGMAW)
C                        ET LOG(LOG(1/(1-PF)))
C     PROB         /OUT/:PROBABILITE THEORIQUE POUR REGRESSION LINEAIRE
C     MKP          /OUT/:PARAMETRE M(K+1)DE WEIBULL
C     SKP          /OUT/:PARAMETRE SIGMA-U(K+1) DE WEIBULL
C
C     ----------------------------------------------------------------
C
      REAL*8  SYI,SXI,SXIXI,SXIYI,SXIYJ,SXIXJ,UNSURN,UNSURM
      REAL*8  SWM,PREC,MG,MD,PROV,SNT,S1,S2
      INTEGER I,J,K,ITP,IRG
C     ----------------------------------------------------------------
C
      IF (METHOD(1:9).EQ.'REGR_LINE') THEN
C
C        METHODE DE REGRESSION LINEAIRE
C
C
        IF (.NOT.DEPT) THEN
C
C       UN SEUL RESU : PAS DE DEPENDANCE EN TEMPERATURE
C
          SYI = 0.D0
          SXI = 0.D0
          SXIXI = 0.D0
          SXIYI = 0.D0
C
          DO 10 I=1,NRUPT
C
           PROB(I) = I
           S1 = NRUPT
           PROB(I) = PROB(I) / (S1 + 1.D0)
           Y(I) = LOG ( LOG ( 1.D0 / ( 1.D0-PROB(I)) ) )
           X(I) = LOG ( SIGW(I) )
           SYI = SYI + Y(I)
           SXI = SXI + X(I)
           SXIXI = SXIXI + X(I)*X(I)
           SXIYI = SXIYI + X(I)*Y(I)
C
 10       CONTINUE
C
          SXIYJ = 0.D0
          SXIXJ = 0.D0
C
          DO 20 I=1,NRUPT
C        
           DO 30 J=1,NRUPT
C  
             SXIYJ = SXIYJ + X(I)*Y(J)
             SXIXJ = SXIXJ + X(I)*X(J)
C
30         CONTINUE
C
20        CONTINUE
C
          UNSURN = NRUPT
          UNSURN = 1.D0/UNSURN
C
          IF ((.NOT.CALM).AND.(.NOT.CALS)) THEN
            MKP = (UNSURN*SXIYJ-SXIYI) /
     +               ( UNSURN*SXIXJ-SXIXI )
            SKP(1) = EXP ( UNSURN*(SXI- (1.D0/MKP)*SYI) )
          ELSE IF (CALM) THEN
            MKP = MK
            SKP(1) = EXP ( UNSURN*(SXI- (1.D0/MKP)*SYI) )
          ELSE IF (CALS) THEN
            SKP(1) = SK(1)
            MKP = SXIYI/(SXIXI - LOG(SKP(1))*SXI)
          END IF
          IF (IMPR) WRITE(IFM,*) 'M(K) =',MKP,'SIGU(K) = ',SKP(1)
C
          SXI =0.D0
          DO 40 J=1,NRUPT
C  
             PROV = (1.D0-EXP(-(SIGW(J)/SK(1))**MK))
             IF (PROV.NE.1.D0) PROV = LOG ( LOG (1.D0/(1.D0-PROV) ) )
             SXI = SXI + (Y(J)- PROV)**2
C
40        CONTINUE
          IF (IMPR) WRITE(IFM,*)
     +     'ECART THEORIE-EXPERIENCE AU DEBUT DE L''ITERATION : ',SXI
C
        ELSE
C
C       DEPENDANCE EN TEMPERATURE SIGMA_U(T)
C
          SXIXI = 0.D0
          SXIYI = 0.D0

          DO 110 ITP=1,NBTP
C
          SNT = 0.D0
C
          DO 120 IR=1 , NBRES
C
            IF (INDTP(IR).EQ.ITP) SNT = SNT + NT(IR)
C
 120      CONTINUE
C                 
            DO 130 I=1,NRUPT
C      
            IRG = 1
            DO 140 K=1,I-1
               IF (INDTP(NUR(K)).EQ.ITP) THEN
                  IRG = IRG+1
               END IF
140         CONTINUE

               IF (INDTP(NUR(I)).EQ.ITP) THEN
C 
                 PROB(I) = IRG               
                 PROB(I) = PROB(I) / (SNT+1.D0)
                 Y(I) = LOG ( LOG ( 1.D0 / ( 1.D0-PROB(I)) ) )
                 X(I) = LOG ( SIGW(I) )     
                 SXIXI = SXIXI + X(I)*X(I)
                 SXIYI = SXIYI + X(I)*Y(I)

               END IF
C
130         CONTINUE
C  
 110      CONTINUE    
C
          S1 = 0.D0
          S2 = 0.D0
C
          DO 210 ITP=1,NBTP
C
          SXIYJ = 0.D0
          SXIXJ = 0.D0
          SNT = 0.D0
C
          DO 200 IR=1 , NBRES
C
            IF (INDTP(IR).EQ.ITP) SNT = SNT + NT(IR)
C
 200      CONTINUE
C
          DO 300 I=1,NRUPT
C                 
            DO 400 J=1,NRUPT
C  
               IF (INDTP(NUR(I)).EQ.ITP.AND.INDTP(NUR(J)).EQ.ITP) THEN
                 SXIYJ = SXIYJ + X(I)*Y(J)
                 SXIXJ = SXIXJ + X(I)*X(J)
               END IF
C
400         CONTINUE
C
300       CONTINUE      
          S1 = S1 + SXIYJ/SNT 
          S2 = S2 + SXIXJ/SNT
C  
 210      CONTINUE               
C
          IF ((.NOT.CALM)) THEN
           MKP = (S1-SXIYI) / ( S2-SXIXI )
          ELSE IF (CALM) THEN
           MKP = MK
          END IF
C
          IF (IMPR) WRITE(IFM,*) 'M(K) =',MKP
C
          IF ( ((.NOT.CALM).AND.(.NOT.CALS)) .OR. CALM) THEN
C 
C          (M ET SIGMA-U) OU (SIGMA-U) SONT A RECALER 
C
           DO 211 ITP=1,NBTP
C
              SNT = 0.D0
C
              DO 101 IR=1 , NBRES
C
               IF (INDTP(IR).EQ.ITP) SNT = SNT + NT(IR)
C
 101          CONTINUE
C
              SXI = 0.D0
              SYI = 0.D0
C
              DO 201 I=1,NRUPT
C
                  IF (INDTP(NUR(I)).EQ.ITP) THEN
                   SYI = SYI + Y(I)
                   SXI = SXI + X(I)
                  END IF
C
 201          CONTINUE
C
              SKP(ITP) = EXP ( (SXI-(1.D0/MKP)*SYI)/SNT )
              IF (IMPR) WRITE(IFM,*) 'S(K) (',ITP,')=',SKP(ITP)
C
 211       CONTINUE
C
          ELSE IF (CALS) THEN

           DO 301 IR = 1, NBTP

             SKP(IR) = SK(IR)       
             IF (IMPR) WRITE(IFM,*) 'S(K) (',IR,')=',SKP(IR)

 301       CONTINUE

          END IF
C
        END IF
C
      ELSE IF (METHOD(1:9).EQ.'MAXI_VRAI') THEN
C
C        METHODE DU MAXIMUM DE VRAISSEMBLANCE
C
         IF ((.NOT.CALM).AND.(.NOT.CALS)) THEN
C 
C        M ET SIGMA-U SONT A RECALER 
C
           PREC = 1.D-8
           MG = 1.D0
           MD = MK
           SWM = 0.D0
           UNSURN = NRUPT
           UNSURN = 1.D0/UNSURN
           IF (IMPR) WRITE(IFM,*) 'RESOLUTION F(M)=0 PAR NEWTON'
C
C          RESOLUTION DE L'EQUATION NON LINEAIRE F(M)=0
C
           CALL NTWEIB(NRUPT,CALS,SK,SIGW,NUR,NT,NBRES,MG,MD,PREC,MKP,
     +                 IMPR,IFM,INDTP,NBTP)
C
           UNSURM = 1.D0/MKP
C
C          CALCUL DU SIGMA-U
C
           DO 12 ITP=1,NBTP
C
             SNT = 0.D0
             DO 11 IR=1 , NBRES
C     
             IF (INDTP(IR).EQ.ITP) SNT = SNT + NT(IR)
C
 11          CONTINUE
C
             SWM   = 0.D0
             DO 31 I=1,NRUPT
C  
               IF (INDTP(NUR(I)).EQ.ITP) THEN
                SWM = SWM +  SIGW(I) ** MKP
               END IF
C
 31         CONTINUE 
C       
            SKP(ITP) = ( SWM / SNT ) ** ( UNSURM )
C
 12        CONTINUE
C
         ELSE IF (CALM) THEN
C
C        M EST CALE 
C
           MKP = MK
           UNSURM = 1.D0/MKP
C
           DO 52 ITP=1,NBTP
C
             SNT = 0.D0
             DO 51 IR=1 , NBRES
               IF (INDTP(IR).EQ.ITP) SNT = SNT + NT(IR)
 51          CONTINUE
C
             SWM   = 0.D0
             DO 41 I=1,NRUPT
C  
               IF (INDTP(NUR(I)).EQ.ITP) THEN
                SWM = SWM +  SIGW(I) ** MKP
               END IF
C
 41          CONTINUE 
C       
             SKP(ITP) = ( SWM / SNT ) ** ( UNSURM )
C
 52        CONTINUE
C 
         ELSE IF (CALS) THEN
C
C        SIGMA-U EST CALE 
C
          DO 71 IR = 1, NBTP
             SKP(IR) = SK(IR)       
 71        CONTINUE
           PREC = 1.D-8
           MG = 1.D0
           MD = MK
C
C          RESOLUTION DE L'EQUATION NON LINEAIRE F(M)=0
C
           IF (IMPR) WRITE(IFM,*) 'RESOLUTION F(M)=0 PAR NEWTON'
           CALL NTWEIB(NRUPT,CALS,SK,SIGW,NUR,NT,NBRES,
     +                 MG,MD,PREC,MKP,IMPR,IFM,INDTP,NBTP)
C
         END IF
C
         IF (IMPR) THEN
           WRITE(IFM,*) 'M(K) =',MKP
           DO 61 IR = 1, NBTP
              WRITE(IFM,*) 'S(K) (',IR,')=',SKP(IR)
 61        CONTINUE
         END IF
C
      END IF
C
      IF (MKP.LT.1.D0) THEN
        CALL UTMESS('S','OPTIMW','ARRET DE LA PROCEDURE DE RECALAGE : '
     +   // 'LE PARAMETRE M EST DEVENU TROP PETIT (M<1) , VERIFIEZ '
     +   // 'VOS LISTES D''INSTANTS DE RUPTURE')
      END IF            
C 
      END
