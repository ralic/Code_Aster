      SUBROUTINE TE0163(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16      OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C     CALCUL FORCES ELEMENTAIRES DE LAPLACE DES CABLES
C     ------------------------------------------------------------------
C IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
C        'CHAR_MECA' : CALCUL DE LA FORCE DE LAPLACE
C IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
C        'MECABL2'     : CABLE2
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
      CHARACTER*8  NOMA,FONCT,ELREFE
      CHARACTER*16 CH16,LISTMA,LTRANS
      CHARACTER*19 CHGEOM
      CHARACTER*24 CARAC,FF
      CHARACTER*32 JEXNUM,JEXNOM
      REAL*8       ZERO, DEUX, ZCOD
      REAL*8 XL,E1,E2,E3,F1,F2,F3,G1,G2,G3,R1,R2,R3,Q1,Q2,Q3,DD
      REAL*8 B1,B2,B3,U(3),S,D,UM
      REAL*8 POIDS(20)
      REAL*8 V1,V2,V3,V(3),W(3),WM
      CHARACTER*1 K1BID
C     ------------------------------------------------------------------
      CALL ELREF1(ELREFE)
      CALL JEMARQ()
      ZERO = 0.D0
C
C
      IF (NOMTE(1:15).EQ.'MECA_POU_D_T_GD') THEN
        NDDL = 6
      ELSE
        NDDL = 3
      ENDIF
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,' ',ICARAC)
      NNO=ZI(ICARAC)
      NPG=ZI(ICARAC+2)
      FF='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,' ',IFF)
      IPOIDS=IFF
      IVF=IPOIDS+NPG
      IDFDE=IVF+NPG*NNO
C
C     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
      CALL JEVECH ('PGEOMER','L',LX)
      LX = LX - 1
      XL = SQRT( (ZR(LX+4)-ZR(LX+1))**2
     +  + (ZR(LX+5)-ZR(LX+2))**2 + (ZR(LX+6)-ZR(LX+3))**2 )
      IF( XL .EQ. ZERO ) THEN
         CH16 = ' ?????????'
         CALL UTMESS('F','ELEMENTS DE CABLE (40.2)',
     +               'NOEUDS CONFONDUS POUR UN ELEMENT: '//CH16(:8))
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
CC    2 BARRES EN POSITION QUELCONQUE
      DO 1 IMA=1,NBMA
         NO1=ZI(JLIMA+2*IMA-2)
         NO2=ZI(JLIMA+2*IMA-1)
         G1=ZR(JGEOM+3*NO2-3)-ZR(JGEOM+3*NO1-3)
         G2=ZR(JGEOM+3*NO2-2)-ZR(JGEOM+3*NO1-2)
         G3=ZR(JGEOM+3*NO2-1)-ZR(JGEOM+3*NO1-1)
         S=SQRT(G1**2+G2**2+G3**2)
         F1=G1/S
         F2=G2/S
         F3=G3/S
         DO 30 KP = 1,NPG
            K=(KP-1)*NNO
            IF (IMA.EQ.1)
     +       CALL VFF3D(NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(LX+1)
     +                                                 ,POIDS(KP))
            R1=-ZR(JGEOM+3*NO2-3)
            R2=-ZR(JGEOM+3*NO2-2)
            R3=-ZR(JGEOM+3*NO2-1)
            DO 31 I = 1,NNO
               R1=R1+ZR(LX+1+3*(I-1))*ZR(IVF+K+I-1)
               R2=R2+ZR(LX+2+3*(I-1))*ZR(IVF+K+I-1)
               R3=R3+ZR(LX+3+3*(I-1))*ZR(IVF+K+I-1)
   31       CONTINUE
            Q1=R1+G1
            Q2=R2+G2
            Q3=R3+G3
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
            S=SQRT(Q1**2+Q2**2+Q3**2)
            Q1=Q1/S
            Q2=Q2/S
            Q3=Q3/S
            S=SQRT(R1**2+R2**2+R3**2)
            R1=R1/S
            R2=R2/S
            R3=R3/S
            S=F1*(Q1-R1)+F2*(Q2-R2)+F3*(Q3-R3)
            S=S/D/2.D0
            DO 2 I = 1,NNO
            DO 2 J = 1,3
             ZR(IVECT-1+J+NDDL*(I-1)) =
     +       ZR(IVECT-1+J+NDDL*(I-1))+S*U(J)*POIDS(KP)*ZR(IVF+K+I-1)
    2       CONTINUE
   30    CONTINUE
    1 CONTINUE
C
 1000 CONTINUE
C
      CALL JEDEMA()
      END
