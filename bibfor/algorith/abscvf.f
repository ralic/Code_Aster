      SUBROUTINE ABSCVF(NDIM,JTABAR,XE,S)
      IMPLICIT NONE 

      REAL*8     XE,S
      INTEGER    NDIM,JTABAR

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/12/2010   AUTEUR MASSIN P.MASSIN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRS_1404
C 
C                      TROUVER L'ABSCISSE CURVILIGNE D'UN POINT
C                      SUR UNE ARETE QUADRATIQUE A PARTIR DE SES
C                      COORDONNEES DANS L'ELEMENT DE REFERENCE
C                    
C     ENTREE
C       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
C       JTABAR  : COORDONNEES DES 3 NOEUDS QUI DEFINISSENT L'ARETE
C       XE     : COORDONNEES DU POINT DANS L'ELEMENT DE REFERENCE
C
C     SORTIE
C       S        : ABSCISSE CURVILIGNE DU POINT SUR L'ARETE
C......................................................................
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  ------------------------

      REAL*8        COEF1,COEF2,COEF3,COEF4,EPS
      REAL*8        PT1(NDIM),PT2(NDIM),PT3(NDIM)
      REAL*8        D,MU,R8PREM
      REAL*8        BORSUP, BORINF
      INTEGER       I
      CHARACTER*8   TYPFCT
C
C......................................................................

      CALL JEMARQ()

C     TABAR : XE2=-1  /  XE1= 1  /  XE3= 0
C     XE2 XENT LE POINT D'ORIGINE

C     CALCUL DE COEF1, COEF2, COEF3, D
      COEF1=0.D0
      COEF2=0.D0
      COEF3=0.D0
      CALL VECINI(NDIM,0.D0,PT1)
      CALL VECINI(NDIM,0.D0,PT2)
      CALL VECINI(NDIM,0.D0,PT3)
      EPS=1.D-7

      DO 10 I=1, NDIM
      PT1(I)=ZR(JTABAR-1+I)
      PT2(I)=ZR(JTABAR-1+NDIM+I)
      PT3(I)=ZR(JTABAR-1+2*NDIM+I)
 10   CONTINUE

      DO 101 I=1,NDIM
        COEF1 = COEF1 + (PT1(I)-2*PT3(I)+PT2(I))*
     &                                  (PT1(I)-2*PT3(I)+PT2(I))
 101  CONTINUE  

      DO 102 I=1,NDIM
        COEF2 = COEF2 + (PT2(I)-PT1(I))*(PT1(I)-2*PT3(I)+PT2(I))
 102  CONTINUE

      DO 103 I=1,NDIM
        COEF3 = COEF3 + (PT2(I)-PT1(I))*(PT2(I)-PT1(I))/4
 103  CONTINUE

      D = COEF2*COEF2 - 4*COEF1*COEF3

C     CALCUL ABSCISSE CURVILIGNE DU POINT

      IF (ABS(COEF1).LE.EPS)     THEN
        S = (XE+1)*SQRT(COEF3)
      ELSEIF (ABS(COEF1).GT.R8PREM()) THEN     
        IF (ABS(D).LE.R8PREM()) THEN
          S = (COEF1*XE*XE + COEF2*XE + COEF2 - COEF1)
     &        /(2*SQRT(COEF1))
        ELSEIF (D.GT.EPS) THEN
          MU = SQRT(D/(4*COEF1*COEF1))
          COEF4 = MU*MU*SQRT(COEF1)/4  
          TYPFCT = 'ACOSH'                
          CALL FCTHYP(TYPFCT,(2*COEF1*XE+COEF2)/(2*COEF1*MU),BORSUP)
          CALL FCTHYP(TYPFCT,(COEF2-2*COEF1)/(2*COEF1*MU),BORINF)
          S =  COEF4*(SINH(2*BORSUP)-2*BORSUP)
     &            - COEF4*(SINH(2*BORINF)-2*BORINF)
        ELSEIF (D.LT.EPS) THEN
          MU = SQRT(-D/(4*COEF1*COEF1))
          COEF4 = MU*MU*SQRT(COEF1)/4
          TYPFCT = 'ASINH'
          CALL FCTHYP(TYPFCT,(2*COEF1*XE+COEF2)/(2*COEF1*MU),BORSUP)
          CALL FCTHYP(TYPFCT,(COEF2-2*COEF1)/(2*COEF1*MU),BORINF)
          S =  COEF4*(SINH(2*BORSUP)+2*BORSUP) 
     &            - COEF4*(SINH(2*BORINF)+2*BORINF)
       
        ENDIF
      ENDIF

      IF (S.LT.0.D0) THEN
        CALL ASSERT(2.EQ.3)
      ENDIF

      CALL JEDEMA()
      END
