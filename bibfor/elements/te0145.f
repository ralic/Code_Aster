      SUBROUTINE TE0145(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 01/09/1999   AUTEUR ACBHHCD G.DEVESA 
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
C     CALCUL FORCES ELEMENTAIRES ELECTRIQUES POUR LES ELEMENTS DE POUTRE
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'CHAR_MECA' : CALCUL DE LA MATRICE DE MASSE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
C        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
C        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
C     ------------------------------------------------------------------
C
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*16 CH16
      CHARACTER*24 MOMINI
      CHARACTER*32 JEXNUM,JEXNOM
      REAL*8       ZERO, DEUX, ZCOD
      REAL*8 XL,E1,E2,E3,F1,F2,F3,R1,R2,R3,Q1,Q2,Q3
      REAL*8 B1,B2,B3,U(3),S,ALP,D,UM,AN1,AN2,AN3,AN4
      REAL*8 V1,V2,V3,V(3),W(3),WM,PF1,PF2
      REAL*8 RAD,ANG,ANGARC,ANGS2,XFL
      ZERO = 0.D0
      DEUX = 2.D0
C     ------------------------------------------------------------------
C
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER','L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2
     +  + (ZR(LX+5)-ZR(LX+2))**2 + (ZR(LX+6)-ZR(LX+3))**2 )
      IF( XL .EQ. ZERO ) THEN
         CH16 = ' ?????????'
         CALL UTMESS('F','ELEMENTS DE POUTRE (40.2)',
     +               'NOEUDS CONFONDUS POUR UN ELEMENT: '//CH16(:8))
      ENDIF
      IF (NOMTE.EQ.'MECA_POU_C_T') THEN
         CALL UTMESS('F','ELEMENTS DE POUTRE (TE0145)',
     +        'FORCE ELEMENTAIRE ELECTRIQUE NON DEFINIE POUR'//
     +        'LES ELEMENTS COURBES.')
         CALL JEVECH ('PCAARPO','L',LRCOU)
         RAD    = ZR(LRCOU)
         ANGS2  = ASIN( XL / ( DEUX * RAD ) )
         ANG    = ANGS2 * DEUX
         XL     = RAD * ANG
      ENDIF
C
C     ------------------- CALCUL DES VECTEURS ELEMENTAIRES ------------
C
      CALL JEVECH ('PFRELEC','L',IFORC)
      ZCOD=ZR(IFORC+6)
      CALL JEVECH ('PVECTUR','E',IVECT)
CC    BARRES INFINIES PARALLELES MULTIPLES (CAS VIBRAPORTIC)
      IF (ZCOD.EQ.10.D0) THEN
       U(1)=(ZR(LX+4)-ZR(LX+1))/XL
       U(2)=(ZR(LX+5)-ZR(LX+2))/XL
       U(3)=(ZR(LX+6)-ZR(LX+3))/XL
       W(1)=U(2)*ZR(IFORC-1+3)-U(3)*ZR(IFORC-1+2)
       W(2)=U(3)*ZR(IFORC-1+1)-U(1)*ZR(IFORC-1+3)
       W(3)=U(1)*ZR(IFORC-1+2)-U(2)*ZR(IFORC-1+1)
       WM=SQRT(W(1)**2+W(2)**2+W(3)**2)
       IF (WM.LT.1.D-3) THEN
CCDIR$ IVDEP
        DO 10 J = 1,3
         ZR(IVECT-1+J)=ZR(IVECT-1+J)+XL*ZR(IFORC-1+J)/DEUX
         ZR(IVECT-1+J+6)=ZR(IVECT-1+J+6)+XL*ZR(IFORC-1+J)/DEUX
   10   CONTINUE
        GO TO 1000
       END IF
       W(1)=W(1)/WM
       W(2)=W(2)/WM
       W(3)=W(3)/WM
       V(1)=W(2)*U(3)-W(3)*U(2)
       V(2)=W(3)*U(1)-W(1)*U(3)
       V(3)=W(1)*U(2)-W(2)*U(1)
       PF1=ZR(IFORC)*U(1)+ZR(IFORC+1)*U(2)+ZR(IFORC+2)*U(3)
       PF2=ZR(IFORC)*V(1)+ZR(IFORC+1)*V(2)+ZR(IFORC+2)*V(3)
       DO 20 J = 1,3
        ZR(IVECT-1+J)=ZR(IVECT-1+J)+XL*(PF1*U(J)+PF2*V(J))/2.D0
        ZR(IVECT-1+J+3)=ZR(IVECT-1+J+3)+PF2*W(J)*XL**2/12.D0
        ZR(IVECT-1+J+6)=ZR(IVECT-1+J+6)+XL*(PF1*U(J)+PF2*V(J))/2.D0
        ZR(IVECT-1+J+9)=ZR(IVECT-1+J+9)-PF2*W(J)*XL**2/12.D0
   20  CONTINUE
C
       GO TO 1000
      END IF
C
      E1=ZR(LX+4)-ZR(LX+1)
      E2=ZR(LX+5)-ZR(LX+2)
      E3=ZR(LX+6)-ZR(LX+3)
      S=SQRT(E1**2+E2**2+E3**2)
      E1=E1/S
      E2=E2/S
      E3=E3/S
CC    2 BARRES INFINIES PARALLELES DEFINIES PAR UNE TRANSLATION
      IF (ZCOD.EQ.11.D0) THEN
         R1=-ZR(IFORC)
         R2=-ZR(IFORC+1)
         R3=-ZR(IFORC+2)
         B1=E2*R3-E3*R2
         B2=E3*R1-E1*R3
         B3=E1*R2-E2*R1
         D=SQRT(B1**2+B2**2+B3**2)
         U(1)=E2*B3-E3*B2
         U(2)=E3*B1-E1*B3
         U(3)=E1*B2-E2*B1
         S=SQRT(U(1)**2+U(2)**2+U(3)**2)
         U(1)=U(1)/S
         U(2)=U(2)/S
         U(3)=U(3)/S
         V1=U(1)
         V2=U(2)
         V3=U(3)
         W(1)=E2*V3-E3*V2
         W(2)=E3*V1-E1*V3
         W(3)=E1*V2-E2*V1
         DO 11 J = 1,3
         ZR(IVECT-1+J)=ZR(IVECT-1+J)+XL*U(J)/D/DEUX
         ZR(IVECT-1+J+3)=ZR(IVECT-1+J+3)+XL**2*W(J)/D/12.D0
         ZR(IVECT-1+J+6)=ZR(IVECT-1+J+6)+XL*U(J)/D/DEUX
         ZR(IVECT-1+J+9)=ZR(IVECT-1+J+9)-XL**2*W(J)/D/12.D0
   11    CONTINUE
         GO TO 1000
      END IF
CC    2 BARRES INFINIES PARALLELES DEFINIES PAR UNE DISTANCE ET 1 POINT
      IF (ZCOD.EQ.12.D0) THEN
         D=ZR(IFORC)
         R1=ZR(LX+4)/2.D0+ZR(LX+1)/2.D0-ZR(IFORC+3)
         R2=ZR(LX+5)/2.D0+ZR(LX+2)/2.D0-ZR(IFORC+4)
         R3=ZR(LX+6)/2.D0+ZR(LX+3)/2.D0-ZR(IFORC+5)
         B1=E2*R3-E3*R2
         B2=E3*R1-E1*R3
         B3=E1*R2-E2*R1
         U(1)=E2*B3-E3*B2
         U(2)=E3*B1-E1*B3
         U(3)=E1*B2-E2*B1
         S=SQRT(U(1)**2+U(2)**2+U(3)**2)
         U(1)=U(1)/S
         U(2)=U(2)/S
         U(3)=U(3)/S
         V1=U(1)
         V2=U(2)
         V3=U(3)
         W(1)=E2*V3-E3*V2
         W(2)=E3*V1-E1*V3
         W(3)=E1*V2-E2*V1
         DO 12 J = 1,3
         ZR(IVECT-1+J)=ZR(IVECT-1+J)+XL*U(J)/D/DEUX
         ZR(IVECT-1+J+3)=ZR(IVECT-1+J+3)+XL**2*W(J)/D/12.D0
         ZR(IVECT-1+J+6)=ZR(IVECT-1+J+6)+XL*U(J)/D/DEUX
         ZR(IVECT-1+J+9)=ZR(IVECT-1+J+9)-XL**2*W(J)/D/12.D0
   12    CONTINUE
         GO TO 1000
      END IF
CC    2 BARRES EN POSITION QUELCONQUE
      IF (ZCOD.EQ.2.D0.OR.ZCOD.EQ.3.D0) THEN
         DO 30 I = 1,100
            ALP=(DBLE(I)-0.5D0)/100.D0
            AN1=1.D0-3.D0*ALP**2+2.D0*ALP**3
            AN2=(ALP-2.D0*ALP**2+ALP**3)*XL
            AN3=3.D0*ALP**2-2.D0*ALP**3
            AN4=(-ALP**2+ALP**3)*XL
            R1=ZR(LX+4)*(1.D0-ALP)+ZR(LX+1)*ALP-ZR(IFORC+3)
            R2=ZR(LX+5)*(1.D0-ALP)+ZR(LX+2)*ALP-ZR(IFORC+4)
            R3=ZR(LX+6)*(1.D0-ALP)+ZR(LX+3)*ALP-ZR(IFORC+5)
            Q1=ZR(LX+4)*(1.D0-ALP)+ZR(LX+1)*ALP-ZR(IFORC)
            Q2=ZR(LX+5)*(1.D0-ALP)+ZR(LX+2)*ALP-ZR(IFORC+1)
            Q3=ZR(LX+6)*(1.D0-ALP)+ZR(LX+3)*ALP-ZR(IFORC+2)
            F1=ZR(IFORC+3)-ZR(IFORC)
            F2=ZR(IFORC+4)-ZR(IFORC+1)
            F3=ZR(IFORC+5)-ZR(IFORC+2)
            S=SQRT(F1**2+F2**2+F3**2)
            F1=F1/S
            F2=F2/S
            F3=F3/S
            B1=F2*Q3-F3*Q2
            B2=F3*Q1-F1*Q3
            B3=F1*Q2-F2*Q1
            D=SQRT(B1**2+B2**2+B3**2)
            DD=D/SQRT(Q1**2+Q2**2+Q3**2)
            IF (DD.LT.1.D-8) GO TO 30
            B1=B1/D
            B2=B2/D
            B3=B3/D
            U(1)=E2*B3-E3*B2
            U(2)=E3*B1-E1*B3
            U(3)=E1*B2-E2*B1
            UM=SQRT(U(1)**2+U(2)**2+U(3)**2)
            V1=U(1)/UM
            V2=U(2)/UM
            V3=U(3)/UM
            W(1)=E2*V3-E3*V2
            W(2)=E3*V1-E1*V3
            W(3)=E1*V2-E2*V1
CC    2 BARRES EN POSITION QUELCONQUE INFINIES
            IF (ZCOD.EQ.2.D0) S=0.01D0*XL/D
CC    2 BARRES EN POSITION QUELCONQUE FINIES
            IF (ZCOD.EQ.3.D0) THEN
               S=SQRT(Q1**2+Q2**2+Q3**2)
               Q1=Q1/S
               Q2=Q2/S
               Q3=Q3/S
               S=SQRT(R1**2+R2**2+R3**2)
               R1=R1/S
               R2=R2/S
               R3=R3/S
               S=F1*(Q1-R1)+F2*(Q2-R2)+F3*(Q3-R3)
               S=S*5.D-3*XL/D
            END IF
            DO 2 J = 1,3
            ZR(IVECT-1+J)=ZR(IVECT-1+J)+S*U(J)*AN1
            ZR(IVECT-1+J+3)=ZR(IVECT-1+J+3)+S*W(J)*AN2
            ZR(IVECT-1+J+6)=ZR(IVECT-1+J+6)+S*U(J)*AN3
            ZR(IVECT-1+J+9)=ZR(IVECT-1+J+9)+S*W(J)*AN4
    2       CONTINUE
   30    CONTINUE
      END IF
C
 1000 CONTINUE
C
      END
