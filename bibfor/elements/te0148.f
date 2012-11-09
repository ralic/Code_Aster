      SUBROUTINE TE0148(OPTION,NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*(*)     OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL FORCES ELEMENTAIRES DE LAPLACE
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'CHAR_MECA' : CALCUL DE LA FORCE DE LAPLACE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECA_POU_D_T' : POUTRE DROITE DE TYMOSHENKO (SECTION VARIABLE)
C        'MECA_POU_D_E' : POUTRE DROITE D  EULER      (SECTION VARIABLE)
C     ------------------------------------------------------------------
C
C
C
      CHARACTER*8  NOMAIL
      CHARACTER*16 LISTMA,LTRANS
      CHARACTER*19 CHGEOM
      REAL*8       ZERO
      REAL*8 XL,E1,E2,E3,F1,F2,F3,R1,R2,R3,Q1,Q2,Q3,DD
      REAL*8 B1,B2,B3,U(3),S,D,UM
      REAL*8 ALP(20),AN1(20),AN2(20),AN3(20),AN4(20)
      REAL*8 V1,V2,V3,W(3)
      REAL*8 X0,Y0,Z0,X1,Y1,Z1,X2,Y2,Z2,A,B,C,LAM1,LAM2
      REAL*8 FORCE(12)
      INTEGER IPT
      CHARACTER*1 K1BID
      INTEGER IADZI,IAZK24
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,ILAPL ,ILIST ,IMA ,IVECT ,J ,JGEOM
      INTEGER JLIMA ,JTRAN ,LX ,NBMA ,NBMA2 ,NO1 ,NO2

C-----------------------------------------------------------------------
      CALL JEMARQ()
      ZERO = 0.D0
C
C     --- INITIALISATION
C
      DO 40 I=1,12
         FORCE(I) = ZERO
40    CONTINUE
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER','L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2
     &  + (ZR(LX+5)-ZR(LX+2))**2 + (ZR(LX+6)-ZR(LX+3))**2 )
      IF( XL .EQ. ZERO ) THEN
        CALL TECAEL(IADZI,IAZK24)
        NOMAIL = ZK24(IAZK24-1+3)(1:8)
        CALL U2MESK('F','ELEMENTS2_43',1,NOMAIL)
      ENDIF
C
C     ------------------- CALCUL DES VECTEURS ELEMENTAIRES ------------
C
      CALL JEVECH ('PFLAPLA','L',ILAPL)
      CALL JEVECH ('PLISTMA','L',ILIST)
      CALL JEVECH ('PVECTUR','E',IVECT)
      LISTMA=ZK16(ILIST)
      LTRANS=ZK16(ILIST+1)
      CHGEOM=ZK24(ILAPL+1)(1:19)
      CALL JEVEUO(CHGEOM//'.VALE','L',JGEOM)
C
      E1=ZR(LX+4)-ZR(LX+1)
      E2=ZR(LX+5)-ZR(LX+2)
      E3=ZR(LX+6)-ZR(LX+3)
      S=SQRT(E1**2+E2**2+E3**2)
      E1=E1/S
      E2=E2/S
      E3=E3/S
      IF (LISTMA.EQ.' '.OR.LTRANS.EQ.' ') GO TO 1000
      CALL JEVEUO(LISTMA,'L',JLIMA)
      CALL JELIRA(LISTMA,'LONMAX',NBMA2,K1BID)
      NBMA=NBMA2/2
      CALL JEVEUO(LTRANS,'L',JTRAN)
      X0=ZR(JTRAN)
      Y0=ZR(JTRAN+1)
      Z0=ZR(JTRAN+2)
      A=ZR(JTRAN+3)
      B=ZR(JTRAN+4)
      C=ZR(JTRAN+5)
CC    2 BARRES EN POSITION QUELCONQUE
      DO 1 IMA=1,NBMA
         NO1=ZI(JLIMA+2*IMA-2)
         NO2=ZI(JLIMA+2*IMA-1)
         IF (A.NE.0.0D0.OR.B.NE.0.0D0.OR.C.NE.0.0D0) THEN
           X1=ZR(JGEOM+3*NO1-3)
           Y1=ZR(JGEOM+3*NO1-2)
           Z1=ZR(JGEOM+3*NO1-1)
           X2=ZR(JGEOM+3*NO2-3)
           Y2=ZR(JGEOM+3*NO2-2)
           Z2=ZR(JGEOM+3*NO2-1)
           LAM1=(A*(X1-X0)+B*(Y1-Y0)+C*(Z1-Z0))/(A**2+B**2+C**2)
           LAM2=(A*(X2-X0)+B*(Y2-Y0)+C*(Z2-Z0))/(A**2+B**2+C**2)
           F1=X2-X1-2.D0*(LAM2-LAM1)*A
           F2=Y2-Y1-2.D0*(LAM2-LAM1)*B
           F3=Z2-Z1-2.D0*(LAM2-LAM1)*C
           S=SQRT(F1**2+F2**2+F3**2)
           F1=F1/S
           F2=F2/S
           F3=F3/S
         ELSE
           F1=ZR(JGEOM+3*NO2-3)-ZR(JGEOM+3*NO1-3)
           F2=ZR(JGEOM+3*NO2-2)-ZR(JGEOM+3*NO1-2)
           F3=ZR(JGEOM+3*NO2-1)-ZR(JGEOM+3*NO1-1)
           S=SQRT(F1**2+F2**2+F3**2)
           F1=F1/S
           F2=F2/S
           F3=F3/S
         END IF
         DO 30 I = 1,5
            IF (IMA.GT.1) GO TO 31
            ALP(I)=(DBLE(I)-0.5D0)/5.D0
            AN1(I)=1.D0-3.D0*ALP(I)**2+2.D0*ALP(I)**3
            AN2(I)=(ALP(I)-2.D0*ALP(I)**2+ALP(I)**3)*XL
            AN3(I)=3.D0*ALP(I)**2-2.D0*ALP(I)**3
            AN4(I)=(-ALP(I)**2+ALP(I)**3)*XL
   31       CONTINUE
            IF (A.EQ.0.0D0.AND.B.EQ.0.0D0.AND.C.EQ.0.0D0) THEN
CC           CAS DES TRANSLATIONS
             R1=ZR(LX+4)*(1.D0-ALP(I))+ZR(LX+1)*ALP(I)-ZR(JGEOM+3*NO2-3)
     &         -ZR(JTRAN)
             R2=ZR(LX+5)*(1.D0-ALP(I))+ZR(LX+2)*ALP(I)-ZR(JGEOM+3*NO2-2)
     &         -ZR(JTRAN+1)
             R3=ZR(LX+6)*(1.D0-ALP(I))+ZR(LX+3)*ALP(I)-ZR(JGEOM+3*NO2-1)
     &         -ZR(JTRAN+2)
             Q1=ZR(LX+4)*(1.D0-ALP(I))+ZR(LX+1)*ALP(I)-ZR(JGEOM+3*NO1-3)
     &         -ZR(JTRAN)
             Q2=ZR(LX+5)*(1.D0-ALP(I))+ZR(LX+2)*ALP(I)-ZR(JGEOM+3*NO1-2)
     &         -ZR(JTRAN+1)
             Q3=ZR(LX+6)*(1.D0-ALP(I))+ZR(LX+3)*ALP(I)-ZR(JGEOM+3*NO1-1)
     &         -ZR(JTRAN+2)
            ELSE
CC           CAS DES SYMETRIES
             R1=ZR(LX+4)*(1.D0-ALP(I))+ZR(LX+1)*ALP(I)-X2+2.D0*LAM2*A
             R2=ZR(LX+5)*(1.D0-ALP(I))+ZR(LX+2)*ALP(I)-Y2+2.D0*LAM2*B
             R3=ZR(LX+6)*(1.D0-ALP(I))+ZR(LX+3)*ALP(I)-Z2+2.D0*LAM2*C
             Q1=ZR(LX+4)*(1.D0-ALP(I))+ZR(LX+1)*ALP(I)-X1+2.D0*LAM1*A
             Q2=ZR(LX+5)*(1.D0-ALP(I))+ZR(LX+2)*ALP(I)-Y1+2.D0*LAM1*B
             Q3=ZR(LX+6)*(1.D0-ALP(I))+ZR(LX+3)*ALP(I)-Z1+2.D0*LAM1*C
            END IF
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
            S=SQRT(Q1**2+Q2**2+Q3**2)
            Q1=Q1/S
            Q2=Q2/S
            Q3=Q3/S
            S=SQRT(R1**2+R2**2+R3**2)
            R1=R1/S
            R2=R2/S
            R3=R3/S
            S=F1*(Q1-R1)+F2*(Q2-R2)+F3*(Q3-R3)
            S=S*.10D0*XL/D
            DO 2 J = 1,3
            FORCE(J)   = FORCE(J)+S*U(J)*AN1(I)
            FORCE(J+3) = FORCE(J+3)+S*W(J)*AN2(I)
            FORCE(J+6) = FORCE(J+6)+S*U(J)*AN3(I)
            FORCE(J+9) = FORCE(+J+9)+S*W(J)*AN4(I)
    2       CONTINUE
   30    CONTINUE
    1 CONTINUE
C
 1000 CONTINUE
C
C   --- STOCKAGE
C
      IPT = 6
      IF(NOMTE.EQ.'MECA_POU_D_EM' .OR.
     &   NOMTE.EQ.'MECA_POU_D_TG' .OR.
     &   NOMTE.EQ.'MECA_POU_D_TGM'     ) IPT=7

      DO 50 I=1,6
          ZR(IVECT-1+I)  = FORCE(I)
          ZR(IVECT-1+I+IPT)= FORCE(I+6)
 50   CONTINUE
C
      CALL JEDEMA()
      END
