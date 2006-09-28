      SUBROUTINE USUKWU ( NBPT, FN, VG, PARA, W, IRET )
      IMPLICIT REAL*8 (A-H,O-Z)
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C----------------------------------------------------------------------
C
      REAL*8  FN(*), VG(*), PARA(*)
C
      IF   = IUNIFI('RESULTAT')
      IRET = 0
      ZERO = 0.D0
      UN   = 1.D0
      DE   = 2.D0
      XK1 = PARA(2)
      XK2 = PARA(3)
      XK  = PARA(4)
      XC  = PARA(5)
C
C     --- RECHERCHE DES MAX ---
C
      CALL GETVR8(' ','FNOR_MAXI',1,1,1,FNM,N1)
      CALL GETVR8(' ','VTAN_MAXI',1,1,1,VGM,N2)
      IF ( (N1+N2) .EQ. 0 ) THEN
         FNM = ZERO
         VGM = ZERO
         DO 10 I = 1,NBPT
            FNM = MAX(FNM,FN(I))
            VGM = MAX(VGM,VG(I))
 10      CONTINUE
      ELSEIF ( N1 .EQ. 0 ) THEN
         FNM = ZERO
         DO 12 I = 1,NBPT
            FNM = MAX(FNM,FN(I))
 12      CONTINUE
      ELSEIF ( N2 .EQ. 0 ) THEN
         VGM = ZERO
         DO 14 I = 1,NBPT
            VGM = MAX(VGM,VG(I))
 14      CONTINUE
      ENDIF
      WRITE(IF,2000) FNM
      WRITE(IF,2010) VGM
C
C     --- DETERMINATION DES BORNES DES INTERVALLES ---
C
      IFN0 = NINT ( FNM  )
      IF ( IFN0 .EQ. 0 ) THEN
         CALL U2MESS('A','ALGORITH11_5')
         IRET = 10
         GOTO 9999
      ENDIF
      IFN0 = IFN0 / 5
      IF ( IFN0 .EQ. 0 ) THEN
         CALL U2MESS('A','ALGORITH11_5')
         IRET = 10
         GOTO 9999
      ENDIF
      VG0 = VGM / 5.D0
      FN1 = DBLE ( IFN0 )
      FN2 = FN1 + IFN0
      FN3 = FN2 + IFN0
      FN4 = FN3 + IFN0
      FN5 = FN4 + IFN0
      XVG1 = VG0
      XVG2 = XVG1 + VG0
      XVG3 = XVG2 + VG0
      XVG4 = XVG3 + VG0
      XVG5 = XVG4 + VG0
C
C     --- REMPLISSAGE DES CLASSES ---
C
      XI11 = ZERO
      XI12 = ZERO
      XI13 = ZERO
      XI14 = ZERO
      XI15 = ZERO
      XI21 = ZERO
      XI22 = ZERO
      XI23 = ZERO
      XI24 = ZERO
      XI25 = ZERO
      XI31 = ZERO
      XI32 = ZERO
      XI33 = ZERO
      XI34 = ZERO
      XI35 = ZERO
      XI41 = ZERO
      XI42 = ZERO
      XI43 = ZERO
      XI44 = ZERO
      XI45 = ZERO
      XI51 = ZERO
      XI52 = ZERO
      XI53 = ZERO
      XI54 = ZERO
      XI55 = ZERO
      DO 30 I = 1,NBPT
         XVG = VG(I)
         XFN = FN(I)
         IF (XFN.GT.ZERO) THEN
           IF (XFN.GT.FN4) THEN
             IF (XVG.GT.XVG4) THEN
               XI55 = XI55 + UN
             ELSEIF (XVG.GT.XVG3) THEN
               XI45 = XI45 + UN
             ELSEIF (XVG.GT.XVG2) THEN
               XI35 = XI35 + UN
             ELSEIF (XVG.GT.XVG1) THEN
               XI25 = XI25 + UN
             ELSE
               XI15 = XI15 + UN
             ENDIF
           ELSEIF (XFN.GT.FN3) THEN
             IF (XVG.GT.XVG4) THEN
               XI54 = XI54 + UN
             ELSEIF (XVG.GT.XVG3) THEN
               XI44 = XI44 + UN
             ELSEIF (XVG.GT.XVG2) THEN
               XI34 = XI34 + UN
             ELSEIF (XVG.GT.XVG1) THEN
               XI24 = XI24 + UN
             ELSE
               XI14 = XI14 + UN
             ENDIF
           ELSEIF (XFN.GT.FN2) THEN
             IF (XVG.GT.XVG4) THEN
               XI53 = XI53 + UN
             ELSEIF (XVG.GT.XVG3) THEN
               XI43 = XI43 + UN
             ELSEIF (XVG.GT.XVG2) THEN
               XI33 = XI33 + UN
             ELSEIF (XVG.GT.XVG1) THEN
               XI23 = XI23 + UN
             ELSE
               XI13 = XI13 + UN
             ENDIF
           ELSEIF (XFN.GT.FN1) THEN
             IF (XVG.GT.XVG4) THEN
               XI52 = XI52 + UN
             ELSEIF (XVG.GT.XVG3) THEN
               XI42 = XI42 + UN
             ELSEIF (XVG.GT.XVG2) THEN
               XI32 = XI32 + UN
             ELSEIF (XVG.GT.XVG1) THEN
               XI22 = XI22 + UN
             ELSE
               XI12 = XI12 + UN
             ENDIF
           ELSE
             IF (XVG.GT.XVG4) THEN
               XI51 = XI51 + UN
             ELSEIF (XVG.GT.XVG3) THEN
               XI41 = XI41 + UN
             ELSEIF (XVG.GT.XVG2) THEN
               XI31 = XI31 + UN
             ELSEIF (XVG.GT.XVG1) THEN
               XI21 = XI21 + UN
             ELSE
               XI11 = XI11 + UN
             ENDIF
           ENDIF
         ENDIF
 30   CONTINUE
      XIT1 = XI11 + XI12 + XI13 + XI14 + XI15
      XIT2 = XI21 + XI22 + XI23 + XI24 + XI25
      XIT3 = XI31 + XI32 + XI33 + XI34 + XI35
      XIT4 = XI41 + XI42 + XI43 + XI44 + XI45
      XIT5 = XI51 + XI52 + XI53 + XI54 + XI55
      XITT = XIT1 + XIT2 + XIT3 + XIT4 + XIT5
      WRITE(IF,2020) NINT(XITT)
      XXI11 = XI11 / XITT
      XXI12 = XI12 / XITT
      XXI13 = XI13 / XITT
      XXI14 = XI14 / XITT
      XXI15 = XI15 / XITT
      XXI21 = XI21 / XITT
      XXI22 = XI22 / XITT
      XXI23 = XI23 / XITT
      XXI24 = XI24 / XITT
      XXI25 = XI25 / XITT
      XXI31 = XI31 / XITT
      XXI32 = XI32 / XITT
      XXI33 = XI33 / XITT
      XXI34 = XI34 / XITT
      XXI35 = XI35 / XITT
      XXI41 = XI41 / XITT
      XXI42 = XI42 / XITT
      XXI43 = XI43 / XITT
      XXI44 = XI44 / XITT
      XXI45 = XI45 / XITT
      XXI51 = XI51 / XITT
      XXI52 = XI52 / XITT
      XXI53 = XI53 / XITT
      XXI54 = XI54 / XITT
      XXI55 = XI55 / XITT
      WRITE(IF,2030)
      WRITE(IF,1020) ZERO,FN1,FN2,FN3,FN4
      WRITE(IF,1022)  FN1,FN2,FN3,FN4,FN5
      WRITE(IF,1024)
      WRITE(IF,1000) ZERO,XXI11,XXI12,XXI13,XXI14,XXI15
      WRITE(IF,1010) XVG1
      WRITE(IF,1026)
      WRITE(IF,1000) XVG1,XXI21,XXI22,XXI23,XXI24,XXI25
      WRITE(IF,1010) XVG2
      WRITE(IF,1026)
      WRITE(IF,1000) XVG2,XXI31,XXI32,XXI33,XXI34,XXI35
      WRITE(IF,1010) XVG3
      WRITE(IF,1026)
      WRITE(IF,1000) XVG3,XXI41,XXI42,XXI43,XXI44,XXI45
      WRITE(IF,1010) XVG4
      WRITE(IF,1026)
      WRITE(IF,1000) XVG4,XXI51,XXI52,XXI53,XXI54,XXI55
      WRITE(IF,1010) XVG5
      WRITE(IF,1026)
C
C     --- CALCUL DES FONCTEURS DE PONDERATION ---
C
      YYI11 =  XK1 * XK * ( (  FN1      /DE ) / XC ) ** 3
      YYI12 =  XK1 * XK * ( ( (FN2+FN1) /DE ) / XC ) ** 3
      YYI13 =  XK1 * XK * ( ( (FN3+FN2) /DE ) / XC ) ** 3
      YYI14 =  XK1 * XK * ( ( (FN4+FN3) /DE ) / XC ) ** 3
      YYI15 =  XK1 * XK * ( ( (FN5+FN4) /DE ) / XC ) ** 3
      YY    =  XVG1 / DE
      YYI20 =  XK2 * ( FN1     /DE) * YY * YY
      YY    =  ( XVG2 + XVG1 ) / DE
      YYI21 =  XK2 * ( FN1     /DE) * YY * YY
      YYI22 =  XK2 * ((FN2+FN1)/DE) * YY * YY
      YYI23 =  XK2 * ((FN3+FN2)/DE) * YY * YY
      YYI24 =  XK2 * ((FN4+FN3)/DE) * YY * YY
      YYI25 =  XK2 * ((FN5+FN4)/DE) * YY * YY
      YY    =  ( XVG3 + XVG2 ) / DE
      YYI31 =  XK2 * ( FN1     /DE) * YY * YY
      YYI32 =  XK2 * ((FN2+FN1)/DE) * YY * YY
      YYI33 =  XK2 * ((FN3+FN2)/DE) * YY * YY
      YYI34 =  XK2 * ((FN4+FN3)/DE) * YY * YY
      YYI35 =  XK2 * ((FN5+FN4)/DE) * YY * YY
      YY    =  ( XVG4 + XVG3 ) / DE
      YYI41 =  XK2 * ( FN1     /DE) * YY * YY
      YYI42 =  XK2 * ((FN2+FN1)/DE) * YY * YY
      YYI43 =  XK2 * ((FN3+FN2)/DE) * YY * YY
      YYI44 =  XK2 * ((FN4+FN3)/DE) * YY * YY
      YYI45 =  XK2 * ((FN5+FN4)/DE) * YY * YY
      YY    =  ( XVG5 + XVG4 ) / DE
      YYI51 =  XK2 * ( FN1     /DE) * YY * YY
      YYI52 =  XK2 * ((FN2+FN1)/DE) * YY * YY
      YYI53 =  XK2 * ((FN3+FN2)/DE) * YY * YY
      YYI54 =  XK2 * ((FN4+FN3)/DE) * YY * YY
      YYI55 =  XK2 * ((FN5+FN4)/DE) * YY * YY
      WRITE(IF,2040)
      WRITE(IF,1020) ZERO,FN1,FN2,FN3,FN4
      WRITE(IF,1022)  FN1,FN2,FN3,FN4,FN5
      WRITE(IF,1024)
      WRITE(IF,1000) ZERO,YYI11,YYI12,YYI13,YYI14,YYI15
      WRITE(IF,1030) XVG1,YYI20
      WRITE(IF,1026)
      WRITE(IF,1000) XVG1,YYI21,YYI22,YYI23,YYI24,YYI25
      WRITE(IF,1010) XVG2
      WRITE(IF,1026)
      WRITE(IF,1000) XVG2,YYI31,YYI32,YYI33,YYI34,YYI35
      WRITE(IF,1010) XVG3
      WRITE(IF,1026)
      WRITE(IF,1000) XVG3,YYI41,YYI42,YYI43,YYI44,YYI45
      WRITE(IF,1010) XVG4
      WRITE(IF,1026)
      WRITE(IF,1000) XVG4,YYI51,YYI52,YYI53,YYI54,YYI55
      WRITE(IF,1010) XVG5
      WRITE(IF,1026)
C
C     --- CALCUL DU FACTEUR GLOBAL D'INTENSITE ---
C
      SPHI = XXI15*YYI15 + XXI14*YYI14 + XXI13*YYI13 +
     &       XXI12*YYI12 + XXI11*YYI11
      SPWI = XXI25*YYI25 + XXI24*YYI24 + XXI23*YYI23 +
     &       XXI22*YYI22 + XXI21*YYI21 + XXI11*YYI20 +
     &       XXI35*YYI35 + XXI34*YYI34 + XXI33*YYI33 +
     &       XXI32*YYI32 + XXI31*YYI31 +
     &       XXI45*YYI45 + XXI44*YYI44 + XXI43*YYI43 +
     &       XXI42*YYI42 + XXI41*YYI41 +
     &       XXI55*YYI55 + XXI54*YYI54 + XXI53*YYI53 +
     &       XXI52*YYI52 + XXI51*YYI51
      W = SPWI * SPWI / ( SPWI + SPHI )
      WRITE(IF,2050) SPHI
      WRITE(IF,2060) SPWI
      WRITE(IF,2070) W
      IF ( SPHI .LT. SPWI ) THEN
         IRET = 10
         CALL U2MESS('A','ALGORITH11_6')
      ENDIF
C
 1000 FORMAT(1P,1X,E12.5,' !',5(1X,E12.5))
 1010 FORMAT(1P,1X,E12.5,' !')
 1020 FORMAT(1P,'   FORCE_NORM  ',5(1X,E12.5))
 1022 FORMAT(1P,15X,5(1X,E12.5))
 1024 FORMAT('   VITE_GLIS  ',66('-'))
 1026 FORMAT(14X,66('-'))
 1030 FORMAT(1P,1X,E12.5,' !',1X,E12.5)
 2000 FORMAT(/,1P,'===> FORCE NORMALE MAXIMUM      : ',E12.5)
 2010 FORMAT(  1P,'     VITESSE GLISSEMENT MAXIMUM : ',E12.5)
 2020 FORMAT(/,'===> NOMBRE TOTAL DE COUPLES : ',I8)
 2030 FORMAT(/,'===> POURCENTAGES OBTENUS POUR CHAQUE CLASSE :')
 2040 FORMAT(/,'===> FACTEUR DE PONDERATION :')
 2050 FORMAT(/,1P,'===> SOMME DE LA CATEGORIE "IMPACTS-ECROUISSAGE" : '
     &           ,E12.5)
 2060 FORMAT(1P,5X,'SOMME DE LA CATEGORIE "GLISSEMENT"          : '
     &         ,E12.5)
 2070 FORMAT(1P,5X,'FACTEUR GLOBAL INTENSITE D''USURE            : '
     &         ,E12.5)
C
 9999 CONTINUE
      END
