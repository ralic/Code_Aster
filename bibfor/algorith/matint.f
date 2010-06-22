      SUBROUTINE MATINT(KR,MR,DIREC,VTEST,RAYON)
      
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/06/2010   AUTEUR CORUS M.CORUS 
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
C-----------------------------------------------------------------------
C    M. CORUS     DATE 11/03/10
C-----------------------------------------------------------------------
C  BUT:      < CONSTRUIRE LES MATRICES ELEMENTAIRES D'INTERFACE >
C
C  ON CONSTRUIT LES MATRICES ELEMENTAIRES DE RAIDEUR ET DE MASSE
C   D'UNE POUTRE DROITE D'EULER BERNOULLI
C
C-----------------------------------------------------------------------
C  KR        /O/ : MATRICE DE RAIDEUR
C  MR        /O/ : MATRICE DE MASSE
C  DIREC     /I/ : VECTEUR DIRECTEUR DE LA POUTRE
C  VTEST     /I/ : VECTEUR DEFINISSANT L'ORIENTATION DE LA POUTRE
C  RAYON     /I/ : RAYON DE LA POUTRE
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      CHARACTER*32 JEXNUM, JEXNOM

C-- VARIABLES EN ENTREES / SORTIE
      REAL*8       KR(12,12),MR(12,12),DIREC(3),VTEST(3),RAYON

C-- VARIABLES DE LA ROUTINE      
      INTEGER      I1,J1,K1,L1
      REAL*8       E,I,S,G,RHO,PI,L,T(12,12),V1(3),V2(3),V3(3),
     &             TEMP,KINI(12,12),MINI(12,12)
      PARAMETER    (E=2.1D11,G=0.8D11,RHO=7.8D3,
     &              PI=3.141592653589793238462643D0)

C-----------C
C--       --C      
C-- DEBUT --C      
C--       --C
C-----------C

      CALL JEMARQ()
      
      DO 10 I1=1,12
        DO 20 J1=1,12
         T(I1,J1)=0.D0
         KR(I1,J1)=0.D0
         MR(I1,J1)=0.D0
         KINI(I1,J1)=0.D0
         MINI(I1,J1)=0.D0
  20    CONTINUE       
  10  CONTINUE  
              
      S=PI*(RAYON**2)
      I=PI*(RAYON**4)/2
      L=SQRT(DIREC(1)**2+DIREC(2)**2+DIREC(3)**2)
      
      V1(1)=DIREC(1)/L
      V1(2)=DIREC(2)/L
      V1(3)=DIREC(3)/L
      
      TEMP=V1(1)*VTEST(1)+V1(2)*VTEST(2)+V1(3)*VTEST(3)
      
      V2(1)=VTEST(1)-V1(1)*TEMP
      V2(2)=VTEST(2)-V1(2)*TEMP
      V2(3)=VTEST(3)-V1(3)*TEMP
      
      TEMP=SQRT(V2(1)**2+V2(2)**2+V2(3)**2)
      
      V2(1)=V2(1)/TEMP
      V2(2)=V2(2)/TEMP
      V2(3)=V2(3)/TEMP
      
      V3(1)=V1(2)*V2(3)-V1(3)*V2(2)
      V3(2)=V1(3)*V2(1)-V1(1)*V2(3)
      V3(3)=V1(1)*V2(2)-V1(2)*V2(1)
        
      DO 30 I1=1,4
        
        T(1+(I1-1)*3,1+(I1-1)*3)=V1(1)
        T(2+(I1-1)*3,1+(I1-1)*3)=V1(2)
        T(3+(I1-1)*3,1+(I1-1)*3)=V1(3)
          
        T(1+(I1-1)*3,2+(I1-1)*3)=V2(1)
        T(2+(I1-1)*3,2+(I1-1)*3)=V2(2)
        T(3+(I1-1)*3,2+(I1-1)*3)=V2(3)
          
        T(1+(I1-1)*3,3+(I1-1)*3)=V3(1)
        T(2+(I1-1)*3,3+(I1-1)*3)=V3(2)
        T(3+(I1-1)*3,3+(I1-1)*3)=V3(3)
        
  30  CONTINUE      

C-- poutre d'axe +X, DDL :
C-- X1 Y1 Z1 RX1 RY1 RZ1 X2 Y2 Z2 RX2 RY2 RZ2

C------------------------C
C--                    --C
C-- MATRICE DE RAIDEUR --C
C--                    --C
C------------------------C

      KINI(1,1)=E*S/L
      KINI(1,7)=-E*S/L
       
      KINI(2,2)=12.D0*E*I/(L**3)
      KINI(2,6)= 6.D0*E*I/(L**2)
      KINI(2,8)= -12.D0*E*I/(L**3)
      KINI(2,12)= 6.D0*E*I/(L**2)
       
      KINI(3,3)=12.D0*E*I/(L**3)
      KINI(3,5)=-6.D0*E*I/(L**2)
      KINI(3,9)=-12.D0*E*I/(L**3)
      KINI(3,11)=-6.D0*E*I/(L**2)
       
      KINI(4,4)=G*I/L
      KINI(4,10)=-G*I/L
      
      KINI(5,3)=-6.D0*E*I/(L**2) 
      KINI(5,5)=4.D0*E*I/L 
      KINI(5,9)=6.D0*E*I/(L**2) 
      KINI(5,11)=2.D0*E*I/L
       
      KINI(6,2)=6.D0*E*I/(L**2) 
      KINI(6,6)= 4.D0*E*I/L
      KINI(6,8)= -6.D0*E*I/(L**2)
      KINI(6,12)=2.D0*E*I/L
       
      DO 40 I1=1,4
        DO 50 J1=1,12
          KINI(I1+6,J1)=-KINI(I1,J1)
  50    CONTINUE      
  40  CONTINUE
      
      KINI(11,3)=-6.D0*E*I/(L**2)
      KINI(11,5)= 2.D0*E*I/L
      KINI(11,9)= 6.D0*E*I/(L**2)
      KINI(11,11)= 4.D0*E*I/L
      
      KINI(12,2)=6.D0*E*I/(L**2)
      KINI(12,6)= 2.D0*E*I/L
      KINI(12,8)= -6.D0*E*I/(L**2)
      KINI(12,12)= 4.D0*E*I/L

C----------------------C
C--                  --C
C-- MATRICE DE MASSE --C
C--                  --C
C----------------------C

      MINI(1,1)=1.D0/3
      MINI(2,2)= 13.D0/35
      MINI(3,3)= 13.D0/35 
      MINI(4,4)=RAYON**2/3 
      MINI(5,5)=L**2/105 
      MINI(6,6)=L**2/105 
      MINI(7,7)=1.D0/3 
      MINI(8,8)=13.D0/35 
      MINI(9,9)=13.D0/35
      MINI(10,10)=RAYON**2/3 
      MINI(11,11)=L**2/105 
      MINI(12,12)=L**2/105
      
      MINI(1,7)=1.D0/6
      MINI(7,1)=1.D0/6
      
      MINI(2,6)=11.D0*L/210
      MINI(6,2)=11.D0*L/210
      MINI(3,5)=-11.D0*L/210
      MINI(5,3)=-11.D0*L/210
      
      MINI(2,8)=9.D0/70
      MINI(8,2)=9.D0/70
      MINI(3,9)=9.D0/70
      MINI(9,3)=9.D0/70

      MINI(2,12)=-13.D0*L/420
      MINI(12,2)=-13.D0*L/420
      MINI(3,11)=13.D0*L/420
      MINI(11,3)=13.D0*L/420
      
      MINI(4,10)=RAYON**2/6
      MINI(10,4)=RAYON**2/6
      
      MINI(5,9)=-13.D0*L/420
      MINI(9,5)=-13.D0*L/420
      MINI(6,8)=13.D0*L/420
      MINI(8,6)=13.D0*L/420
      
      MINI(5,11)=-(L**2)/140
      MINI(11,5)=-(L**2)/140
      MINI(6,12)=-(L**2)/140
      MINI(12,6)=-(L**2)/140
      
      MINI(8,12)=-11.D0*L/210
      MINI(12,8)=-11.D0*L/210
      MINI(9,11)=11.D0*L/210
      MINI(11,9)=11.D0*L/210
      
      DO 110 I1=1,12
        DO 120 J1=1,12
         MINI(I1,J1)=MINI(I1,J1)*RHO*S
  120   CONTINUE       
  110 CONTINUE       

C-- DANS LA BASE GLOBALE, KR=T*KINI*T^T 
      DO 130 J1=1,12
        DO 140 K1=1,12
          DO 150 L1=1,12
            DO 160 I1=1,12
              MR(I1,J1)=MR(I1,J1)+T(I1,K1)*MINI(K1,L1)*T(J1,L1)
              KR(I1,J1)=KR(I1,J1)+T(I1,K1)*KINI(K1,L1)*T(J1,L1)
  160       CONTINUE
  150     CONTINUE
  140   CONTINUE
  130 CONTINUE

C---------C
C--     --C
C-- FIN --C
C--     --C
C---------C
      CALL JEDEMA()
      END
