      SUBROUTINE GFDASH ( IT, Z,DZ,D2Z, DT, FPTG2, FFTG2, FRTG2, FLUID,
     +                GEOM1, CFPCD1, RUGOSI, X, YY, ITDASH, Z0, L2, L3 )
      IMPLICIT NONE
      INTEGER  IT, ITDASH
      REAL*8   FLUID(8), GEOM1(15), CFPCD1(6), RUGOSI(8), Z, DZ, D2Z,
     &         DT, X(5), YY(2), Z0, L2, L3, FPTG2, FFTG2, FRTG2
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2003   AUTEUR BOYERE E.BOYERE 
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
C    
C     CALCUL DE LA FORCE FLUIDE EXERCEE SUR UN CRAYON EN CHUTE 
C     DANS LE DASHPOT EN FONCTION 
C     DE SA PROFONDEUR D'ENFONCEMENT Z DANS LE DASHPOT 
C     DE SA VITESSE DE CHUTE DZ
C
C-----------------------------------------------------------------------
C  
      INTEGER N2,LWA,SGU1,IRET,I
      PARAMETER (N2=6,LWA=30)
C
      REAL*8  A, A1, AA0, AC, ROC, CD0, CD1, CD2, DH, NUC, PI, S
      REAL*8  A0, CDELG, CDRET, URM1
      REAL*8  AR, DHR, GAMMA, P0, P1, P2, CF1, CF2
      REAL*8  L0, L1, LAMR, LAMF, LAM, THETA
      REAL*8  TOL, KCF1, KCF2, LEQ, ZZ, L22, L33
      REAL*8  DCRAY, DTG, DR, LF
      REAL*8  LAMEQ, LAMREQ, LAMFEQ, DH0, GFCORR
      REAL*8  HRUGC, HRUGTG
      REAL*8  R8PREM, R8PI, ZERO, DEMI, UN
      REAL*8  FVEC(2), WA(LWA), F(N2), Y(6), COEF1(5)
C     ------------------------------------------------------------------
C
      ROC = FLUID(1)
      NUC = FLUID(4)
      P0  = FLUID(5)
      P1  = FLUID(6)
      P2  = FLUID(7)
      
      DCRAY = GEOM1(1)
      DTG   = GEOM1(2)
      DR    = GEOM1(3)
      A     = GEOM1(4)
      A0    = GEOM1(5)
      A1    = GEOM1(6)
      AA0   = GEOM1(7)
      AC    = GEOM1(8)
      AR    = GEOM1(9)
      DH    = GEOM1(10)
      DH0   = GEOM1(11)
      DHR   = GEOM1(12)
      L0    = GEOM1(13)
      L1    = GEOM1(14)
C
      CD0   = CFPCD1(1)
      CD1   = CFPCD1(2)
      CD2   = CFPCD1(3)
      CDELG = CFPCD1(4)
      CDRET = CFPCD1(5)
C
      HRUGC  = RUGOSI(1)
      HRUGTG = RUGOSI(3)
C
      TOL  = SQRT(R8PREM())
      PI   = R8PI()
      ZERO = 0.0D0
      DEMI = 0.5D0
      UN   = 1.0D0
      IRET = 0
C
      ITDASH = ITDASH + 1
C
      IF ( ITDASH .EQ. 1 ) THEN
         YY(1) = X(2)
         YY(2) = (AC*DZ-A0*X(3))/AR
      ENDIF
C
      URM1 = YY(2)
C
C     CALCUL D'UN COEFFICIENT LAM EQUIVALENT PRENANT EN COMPTE
C     LES CONTRAINTES DE CISAILLEMENT A LA PAROI INTERNE DU RETREINT
C     ET A LA SURFACE EXTERNE DU CRAYON
C
      CALL GFCFRV ( (YY(2)+DZ)*DHR/NUC,  HRUGC/DHR, CF1 )
      CALL GFCFRV (      YY(2)*DHR/NUC, HRUGTG/DHR, CF2 )
      KCF1 = GFCORR ( (YY(2)+DZ)*DHR/NUC )      
      KCF2 = GFCORR (      YY(2)*DHR/NUC )
      LAMREQ = 4*( KCF2*CF2/(UN+DCRAY/DR) +
     &             KCF1*CF1/(UN+DR/DCRAY)*(UN+DZ/YY(2))**2 )
C
      CALL GFCFRV ( (AR/A*YY(2)+DZ)*DH/NUC,  HRUGC/DH, CF1 )
      CALL GFCFRV (      AR/A*YY(2)*DH/NUC, HRUGTG/DH, CF2 )
      KCF1 = GFCORR ( (AR/A*YY(2)+DZ)*DH/NUC )
      KCF2 = GFCORR (      AR/A*YY(2)*DH/NUC )
      LAMFEQ = 4*( KCF2*CF2/(UN+DCRAY/DTG) +
     &             KCF1*CF1/(UN+DTG/DCRAY)*(UN+DZ/(AR/A*YY(2)))**2 ) 
C
      CALL GFCFRV ( ((AR*YY(2)-A1*YY(1))/A+DZ)*DH/NUC,  HRUGC/DH, CF1 )
      CALL GFCFRV (      (AR*YY(2)-A1*YY(1))/A*DH/NUC, HRUGTG/DH, CF2 )
      KCF1 = GFCORR ( ((AR*YY(2)-A1*YY(1))/A+DZ)*DH/NUC )
      KCF2 = GFCORR (      (AR*YY(2)-A1*YY(1))/A*DH/NUC )
      LAMEQ = 4*( KCF2*CF2/(UN+DCRAY/DTG) +
     &            KCF1*CF1/(UN+DTG/DCRAY)*
     &                     (UN+DZ/((AR*YY(2)-A1*YY(1))/A))**2 )
C
      L22 = ZERO
      L33 = ZERO
      ZZ = Z + Z0 - L1
      THETA = ZERO
C
         L22 = L22+L2
         IF (THETA.EQ.ZERO) THEN
            IF ((ZZ.GT.ZERO).AND.(ZZ.LE.L2)) THEN
               THETA = UN
               GAMMA = LAMREQ/DHR*(Z+Z0-L1-L33)+CDELG
     &                         +(AR/A)**2*(LAMFEQ/DH*L33)
               LEQ = Z+Z0-L1-L33
            ELSEIF ((ZZ.GT.L2).AND.(ZZ.LE.(L2+L3))) THEN
               THETA = (AR/A)**2
               GAMMA = UN+(CDRET+CDELG)+LAMREQ/DHR*L22
     &                     +(AR/A)**2*(LAMFEQ/DH*(Z+Z0-L1-L22)-UN)
               LEQ = L22
            ELSE
               THETA = ZERO
               GAMMA = ZERO
               LEQ   = ZERO
            ENDIF
         ENDIF
         ZZ = ZZ-(L2+L3)
         L33 = L33+L3
C
      COEF1(1) = 0.0D0
      COEF1(2) = LAMEQ
      COEF1(3) = GAMMA
      COEF1(4) = THETA
      COEF1(5) = LEQ
C
C---------------------------------------------------------------------
C     RESOLUTON du SYSTEME NON LINEAIRE 2-2 PAR LA METHODE DE BROYDEN
C---------------------------------------------------------------------
C
      CALL GFRESD ( DZ, D2Z, DT, X, FVEC, COEF1 ,FLUID, GEOM1, CFPCD1,
     +                                 ITDASH, URM1 )
C
      CALL HYBRD1 ( 2, YY, FVEC, TOL, WA, LWA, IRET, COEF1 , FLUID,
     +              GEOM1, CFPCD1,  DZ, D2Z, DT, ITDASH, URM1 ) 
      IF ( IRET .NE. 1 ) THEN
         CALL UTDEBM('A','DASHPOT','RESOLUTION MAL TERMINEE')
         CALL UTIMPI('S',' CODE RETOUR ', 1, IRET)
         CALL UTIMPI('L','   POUR L''ITERATION ', 1, IT)
         CALL UTFINM()
      ENDIF
C
      Y(1) = YY(1)
      Y(2) = YY(2)
C
      IF ( Y(1) .GT. 0.D0 ) THEN
         SGU1 = +1
      ELSE
         SGU1 = -1
      ENDIF
C
C---------------------------------------------------------------------
C                 CALCUL DES AUTRES INCONNUES DU SYSTEME
C---------------------------------------------------------------------
C     Y(1) = U1 : VITESSE DU FLUIDE A NIVEAU DES ORIFICES DE SORTIE DU
C                 FLUIDE
C     Y(2) = UR : VITESSE ANNULAIRE DU FLUIDE DANS LE RETTREINT
C     Y(3) = U0 : VITESSE DE SORTIE DU FLUIDE AU NIVEAU DE
C                 L'ECHAPPEMENT DE LA VIS EPAULEE
C     Y(4) = U  : VITESSE ANNULAIRE DU FLUIDE HORS RETREINT 
C     Y(5) = PC : PRESSION A L'EXTREMITE INFERIEURE DU CRAYON
C     Y(6) = P  : PRESSION DU FLUIDE A L'ALTITUDE Z
C
      Y(3) = ( AC*DZ - AR*Y(2) ) / A0
      Y(4) = ( AR*Y(2) - A1*Y(1) ) / A 
      Y(6) = P2 + DEMI*ROC*Y(4)**2*(UN+LAMEQ*L1/DH+CD2) +
     +            DEMI*ROC*Y(2)**2*(-UN+GAMMA)+ROC*LEQ*(Y(2)-URM1)/DT
      Y(5) = Y(6) + DEMI*ROC*Y(2)**2*THETA - DEMI*ROC*DZ**2
C
C--------------------------------------------------------------------
C      CALCUL DE L'ERREUR RESIDUELLE DANS LA RESOLUTION DU SYSTEME
C--------------------------------------------------------------------
C
      F(1) = AC*DZ - A0*Y(3) - AR*Y(2)
C
      F(2) = AR*Y(2) - A1*Y(1) - A*Y(4)
C
      F(3) = Y(5)+DEMI*ROC*DZ**2-P0-0.5D0*ROC*((A0/AA0)**2+CD0)*Y(3)**2
     &                - ROC*L0*AC/A0*D2Z + ROC*L0*AR/A0*(Y(2)-URM1)/DT
C
      F(4) = Y(5) + DEMI*ROC*DZ**2 - Y(6) - DEMI*ROC*Y(2)**2*THETA
C
      F(5) = Y(6) - P1 - DEMI*ROC*Y(1)**2*(UN+SGU1*CD1) - 
     +        DEMI*ROC*Y(2)**2*(-UN+GAMMA) - ROC*LEQ*(Y(2)-URM1)/DT
C
      F(6) = Y(6) - P2 - DEMI*ROC*Y(4)**2*(UN+LAMEQ*L1/DH+CD2) -
     +        DEMI*ROC*Y(2)**2*(-UN+GAMMA) - ROC*LEQ*(Y(2)-URM1)/DT
C
      S = 0.D0
      DO 10 I = 1 , 6
         S = S + ABS(F(I))
 10   CONTINUE
C
      IF ( S .GT. 1.0D-3 )  THEN
         CALL UTDEBM('A','DASHPOT','CALCUL DE L''ERREUR RESIDUELLE'//
     +   ' DANS LA RESOLUTION DU MODELE DANS LE DASHPOT')
         CALL UTIMPR('L',' SOMME(F) > 1.0D-3 , SOMME(F) = ', 1, S )
         CALL UTIMPR('L',' F = ', 6, F )
         CALL UTFINM
      ENDIF
C
C
C-------------------------------------------------------------------
C          CALCUL DE LA FORCE FLUIDE APPLIQUEE SUR LE CRAYON     
C-------------------------------------------------------------------
C
C     CALCUL DU COEF LAM PRENANT JUSTE EN COMPTE LA CONTRAINTE A LA
C     SURFACE EXTERNE DU CRAYON
C
      CALL GFCFRV ( (Y(2)+DZ)*DHR/NUC, HRUGC/DHR, CF1 )
      KCF1 = GFCORR ( (Y(2)+DZ)*DHR/NUC )
      LAMR = 4*KCF1*CF1
C
      CALL GFCFRV ( (AR/A*Y(2)+DZ)*DH/NUC, HRUGC/DH, CF1 )
      KCF1 = GFCORR ( (AR/A*Y(2)+DZ)*DH/NUC )
      LAMF = 4*KCF1*CF1      
C
      CALL GFCFRV ( (Y(4)+DZ)*DH/NUC, HRUGC/DH, CF1 )
      KCF1 = GFCORR( (Y(4)+DZ)*DH/NUC )
      LAM = 4*KCF1*CF1  
C
C     CALCUL DU NOUVEAU COEFFICIENT CD0
      CALL GFCFRV ( Y(3)*DH0/NUC, HRUGTG/DH0, CF1 )
C
C --- FORCE DE :
C     --------
      FPTG2 = (Y(6)+2*Y(5)-3*P2)/3*AC
C
C --- FORCE DANS LE TUBE GUIDE :
C     ------------------------
      FFTG2 = LAM*ROC*L1*SQRT(PI*AC)/4*(Y(4)+DZ)**2
C
C --- FORCE DANS LE RETREINT :
C     ----------------------
      LF = MAX(0.D0,Z+Z0-L1-LEQ)
C
      FRTG2 = LAMR*ROC*LEQ*SQRT(PI*AC)/4*(Y(2)+DZ)**2
     &            +LAMF*ROC*LF*SQRT(PI*AC)/4*(AR/A*Y(2)+DZ)**2
C
      END
