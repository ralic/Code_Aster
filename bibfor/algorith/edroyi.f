      REAL*8 FUNCTION EDROYI ()

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/07/2003   AUTEUR ADBHHVV V.CANO 
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
C RESPONSABLE ADBHHVV V.CANO

      IMPLICIT NONE

C *******************************************************************
C *       INTEGRATION DE LA LOI DE ROUSSELIER NON LOCAL             *
C *  CALCUL DES BORNES INF ET SUP DE LA FONCTION S(Y) QUAND S(0)<0  *
C *  ET RESOLUTION DE S(Y)=0                                        *
C *  PAR UNE METHODE DE NEWTON AVEC BORNES CONTROLEES ET DICHOTOMIE *
C *  - LA BORNE SUPERIEURE EST TELLE QUE                            *
C *    YSUP=LOG(SIG1*FONC/(RPM+C1*PM-C2))                           *
C *  - LA BORNE INFERIEURE EST TELLE QUE                            *
C *    YINF =LOG(SIG1*G/(R(PSUP)+C1*PSUP-C2)                        *
C *******************************************************************

      INTEGER ITEMAX, JPROLP, JVALEP, NBVALP
      REAL*8  YOUNG,NU,MU,K,SIGY
      REAL*8  SIG1,D,F0,FCR,ACCE
      REAL*8  FONC,EQETR,PM,RPM,PENTEM,PENTE,PREC
      REAL*8  C1,C2,C3,C4(3),LB,LC
      COMMON /EDROU/ YOUNG,NU,MU,K,SIGY,
     &               SIG1,D,F0,FCR,ACCE,
     &               FONC,EQETR,PM,RPM,PENTEM,PENTE,PREC,
     &               C1,C2,C3,C4,LB,LC,
     &               ITEMAX, JPROLP, JVALEP, NBVALP
      
      INTEGER ITER
      REAL*8  Y,E,DP,RP,S,DS,YINF,YSUP,R8BID,AIRE
      REAL*8  SIEQ,YOUN,LCROTY
      
      IF (RPM.GT.0.D0) THEN

C 1 - CALCUL DU MAJORANT
       
       E  = SIG1*FONC/RPM
       YSUP = LOG(E)
       Y  = YSUP
       CALL EDROFS(Y, DP, S, DS)

C 2 - CALCUL DU MINORANT

       CALL RCFONC('V','TRACTION',JPROLP,JVALEP,NBVALP,R8BID,
     &           R8BID,R8BID,PM+DP,RP,PENTE,AIRE,R8BID,R8BID)
       E  = SIG1*FONC / (RP+C1*(PM+DP)-C2)
       IF (LOG(E).GT.0.D0) THEN
        YINF=LOG(E)
       ELSE
        YINF=0.D0
       ENDIF
      ELSE

C 1 - CALCUL DU MAJORANT

       SIEQ=SIG1*FONC-C1*PM+C2
       YOUN =4.D0*C1/3.D0
        
       CALL RCFONC('E','TRACTION',JPROLP,JVALEP,NBVALP,R8BID,YOUN,
     &             1.D0,PM,RP,PENTE,AIRE,SIEQ,DP)
     
       YSUP = LCROTY(DP*K*FONC/SIG1, PREC, ITEMAX)
       Y  = YSUP
       CALL EDROFS(Y, DP, S, DS)
      
       YINF=0.D0
      ENDIF
                
C 3 - RESOLUTION PAR UNE METHODE DE NEWTON ENTRE LES BORNES
      
      DO 10 ITER = 1, ITEMAX        
       IF (ABS(S)/SIGY .LE. PREC) GOTO 100
       Y = Y - S/DS
       IF (Y.LE.YINF .OR. Y.GE.YSUP)  Y=(YINF+YSUP)/2

       CALL EDROFS(Y, DP, S, DS)
       IF (S.LE.0) YINF = Y
       IF (S.GE.0) YSUP = Y
 10   CONTINUE
      CALL UTMESS('F','ROUSSELIER EDROYI','ITER_INTE_MAXI INSUFFISANT')

 100  CONTINUE
      EDROYI = Y            
      END 
