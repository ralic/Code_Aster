      SUBROUTINE I2ISGT (EPSI,XA,YA,XB,YB,XC,YC,XD,YD,NPI,S1,S2,R1,R2)
      IMPLICIT  NONE
C
      INTEGER NPI
      REAL*8  EPSI,XA,YA,XB,YB,XC,YC,XD,YD,S1,S2,R1,R2
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 28/01/97   AUTEUR CIBHHLV L.VIVAN 
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
C**********************************************************************
C
C           CALCUL DE L' INTERSECTION DES SGMENTS (AB) ET (CD)
C
C           3 CAS DE FIGURE :
C
C                1. INTERSECTION VIDE
C                2. INTERSECTION REDUITE A UN UNIQUE POINT
C                3. INTERSECTION EGALE A UN SEGMENT CONTENU
C                   DANS UNE FACE
C
C           ENTREES :
C
C               COORDONNEES DES POINTS DEFINISSANT LES SEGMENTS
C
C           SORTIES :
C
C               NPI : NOMBRE DE POINTS DEFINISSANT L' INTERSECTION
C                     NPI VAUT 0 OU 1 OU 2
C
C               S1, S2 : ABSCISSES CURVILIGNES DES POINTS PRECEDANTS
C                        SUIVANT AB (S(A)=0 ET S(B)=1)
C
C               R1, R2 : ABSCISSES CURVILIGNES DE CES POINTS
C                        SUIVANT LA FACE (CD)
C
C*********************************************************************
C
      REAL*8  D1, D2, D3, D4, AB2, CD2, A, B, C, D, AUX
      REAL*8  XBA, XCD, XCA, YBA, YCD, YCA, COND
C
      NPI = 0
      S1  = 0.0D0
      S2  = 0.0D0
      R1  = 0.0D0
      R2  = 0.0D0
C
      XBA  = XB - XA
      XCA  = XC - XA
      XCD  = XC - XD
      YBA  = YB - YA
      YCA  = YC - YA
      YCD  = YC - YD
      COND = ABS(XBA) + ABS(YBA) + ABS(XCD) + ABS(YCD)
      XBA  = XBA / COND
      XCA  = XCA / COND
      XCD  = XCD / COND
      YBA  = YBA / COND
      YCA  = YCA / COND
      YCD  = YCD / COND
C
      CALL RVDET2 ( XBA, YBA, XCD, YCD, D1 )
C
      IF ( ABS( D1 ) .GT. EPSI ) THEN
C
C----------LES DROITES AB ET CD SE COUPENT---------------
C
         CALL RVDET2 ( XCA, YCA, XCD, YCD, D2 )
         CALL RVDET2 ( XBA, YBA, XCA, YCA, D3 )
         A = D2 / D1
         B = D3 / D1
C
         IF ( ABS(A)       .LT. EPSI )  A = 0.0D0
C
         IF ( ABS(A-1.0D0) .LT. EPSI )  A = 1.0D0
C
         IF ( ABS(B)       .LT. EPSI )  B = 0.0D0
C
         IF ( ABS(B-1.0D0) .LT. EPSI )  B = 1.0D0
C
         IF ( (A .LE. 1.0D0) .AND. (A .GE. 0.0D0) .AND.
     +        (B .LE. 1.0D0) .AND. (B .GE. 0.0D0) ) THEN
C
C----------LEUR POINT D' INTERSECTION SE SITUE ENTRE A ET B -----
C----------ET ENTRE C ET D                                  -----
C
                 NPI = 1
                 S1  = A
                 R1  = B
C
         ENDIF
C
      ELSE
C
C----------LES DROITES AB ET CD SONT PARALLELES-----------------
C
         CALL RVDET2 ( XCA, YCA, XBA, YBA, D4 )
C
         IF ( ABS( D4 ) .LE. EPSI ) THEN
C
C----------LES DROITES AB ET CD SONT, DE PLUS, CONFONDUES----------
C
                 CALL RVDET2 ( XB-XA, YB-YA, YA-YB, XB-XA, AB2 )
                 CALL RVDET2 ( XD-XC, YD-YC, YC-YD, XD-XC, CD2 )
                 CALL RVDET2 ( XB-XA, YB-YA, YA-YC, XC-XA, C   )
                 CALL RVDET2 ( XB-XA, YB-YA, YA-YD, XD-XA, D   )
                 CALL RVDET2 ( XD-XC, YD-YC, YC-YA, XA-XC, A   )
                 CALL RVDET2 ( XD-XC, YD-YC, YC-YB, XB-XC, B   )
C
                 S1 = C / AB2
                 S2 = D / AB2
                 R1 = A / CD2
                 R2 = B / CD2
C
                 IF ( ABS(S1)       .LT. EPSI )  S1 = 0.0D0
C
                 IF ( ABS(S1-1.0D0) .LT. EPSI )  S1 = 1.0D0
C
                 IF ( ABS(S2)       .LT. EPSI )  S2 = 0.0D0
C
                 IF ( ABS(S2-1.0D0) .LT. EPSI )  S2 = 1.0D0
C
                 IF ( ABS(R1)       .LT. EPSI )  R1 = 0.0D0
C
                 IF ( ABS(R1-1.0D0) .LT. EPSI )  R1 = 1.0D0
C
                 IF ( ABS(R2)       .LT. EPSI )  R2 = 0.0D0
C
                 IF ( ABS(R2-1.0D0) .LT. EPSI )  R2 = 1.0D0
C
                 IF ( S1 .LT. 0.0D0) THEN
C
                     IF (S2 .LT. 0.0D0) THEN
C
                              NPI = 0
C
                     ELSE IF (S2 .LE. 1.0D0) THEN
C
                              NPI = 2
                              S1  = 0.0D0
                              R2  = 1.0D0
C
                     ELSE
C
                              NPI = 2
                              S1  = 0.0D0
                              S2  = 1.0D0
C
                     ENDIF
C
                 ELSE IF ( S1 .LE. 1.0D0) THEN
C
                     IF (S2 .LT. 0.0D0) THEN
C
                              NPI = 2
                              S2  = S1
                              S1  = 0.0D0
                              R2  = 0.0D0
C
                     ELSE IF (S2 .LE. 1.0D0) THEN
C
                              NPI  = 2
C
                              IF ( S1 .LT. S2 ) THEN
C
                                 R1 = 0.0D0
                                 R2 = 1.0D0
C
                              ELSE
C
                                 AUX = S1
                                 S1  = S2
                                 S2  = AUX
                                 R1  = 1.0D0
                                 R2  = 0.0D0
C
                              ENDIF
C
                     ELSE
C
                              NPI = 2
                              S2  = 1.0D0
                              R1  = 0.0D0
C
                     ENDIF
C
                 ELSE
C
                     IF (S2 .LT. 0.0D0) THEN
C
                              NPI = 2
                              S1  = 0.0D0
                              S2  = 1.0D0
C
                     ELSE IF (S2 .LE. 1.0D0) THEN
C
                              NPI = 2
                              S1  = S2
                              S2  = 1.0D0
                              R1  = 1.0D0
C
                     ELSE
C
                              NPI = 0
C
                     ENDIF
C
                 ENDIF
C
C---------LES DROITES AB ET CD SONT PARALLELES NON CONFONDUES----
C
         ENDIF
C
      ENDIF
C
      END
