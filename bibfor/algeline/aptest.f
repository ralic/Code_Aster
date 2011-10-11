      SUBROUTINE APTEST(NK,IMATA,ITEST,CBD)
      IMPLICIT NONE
      INTEGER      NK,IMATA,ITEST
      COMPLEX*16   CBD(100)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 10/10/2011   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     SUBROUTINE THAT IMPLEMENTS CANONICAL TESTS FOR APM012 (IMPR_STURM)
C     THIS IS ONLY SEP (STANDARD EIGENVALUE PROBLEM) FOR DEBUGGING.
C     ------------------------------------------------------------------
C IN NK     : IN  : SIZE OF THE CANONICAL SEP
C IN IMATA  : IN  : JEVEUX ADRESS OF THE MATRIX OF THE SEP IN ZC
C IN ITEST  : IN  : NUMBER OF THE CANONICAL TEST
C OUT CBD   : C16 : EIGENVALUES OF THE SEP
C     ------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU

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

      INTEGER      IFM,NIV,NK2,N1,I
      REAL*8       VALM,VALKK,VALA,VALB,VALOME,RAUX1,RAUX2,RAUXX,RAUXY,
     &             R8PI,PI
      COMPLEX*16   CZERO,CUN

C   --- MISCELLANEOUS ---
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
      CZERO=DCMPLX(0.D0,0.D0)
      CUN=DCMPLX(1.D0,0.D0)
      PI=R8PI()

C   --- DATA OF THE CANONICAL SEP
      NK2=NK*NK      
      DO 5 I=1,NK2
        ZC(IMATA+I-1)=CZERO
    5 CONTINUE
      WRITE(IFM,*)'*** WARNING WARNING WARNING WARNING WARNING ***'
      IF (ITEST.EQ.0) THEN
C   --- TEST 1: MATRIX WITH EIGENVALUES (-3/2;+/- SQRT(61)/2) (NK=2) ---
        CBD(1)=DCMPLX(-3.D0/2.D0,SQRT(61.D0)/2.D0)
        CBD(2)=DCMPLX(-3.D0/2.D0,-SQRT(61.D0)/2.D0)
        ZC(IMATA+(1-1)*NK+1-1)=1.D0*CUN
        ZC(IMATA+(1-1)*NK+2-1)=3.D0*CUN

        ZC(IMATA+(2-1)*NK+1-1)=3.D0*CUN
        ZC(IMATA+(2-1)*NK+2-1)=-4.D0*CUN
        WRITE(IFM,*)'BASIC TEST 0'
      ELSE IF (ITEST.EQ.1) THEN
C   --- TEST 1: DIAGONAL MATRIX WITH EIGENVALUES 0.00, -0.2, 30 ---
C   --- AND -400 (NK=4)                                         ---
        CBD(1)=0.0D0*CUN
        CBD(2)=-0.2D0*CUN
        CBD(3)=30.0D0*CUN
        CBD(4)=-400.D0*CUN
        ZC(IMATA+(1-1)*NK+1-1)=CBD(1)
        ZC(IMATA+(2-1)*NK+2-1)=CBD(2)
        ZC(IMATA+(3-1)*NK+3-1)=CBD(3)
        ZC(IMATA+(4-1)*NK+4-1)=CBD(4)                 
        WRITE(IFM,*)'BASIC TEST 1'
      ELSE IF (ITEST.EQ.2) THEN
C   --- TEST 2 MATRIX (FOR NK=4) WITH EIGENVALUES: 4, 4, 2 AND 1 ---
C   --- NON SYMETRICAL AND REAL (CF. THEODOR/LASCAUX P606, NK=4) ---
        CBD(1)=1.D0*CUN
        CBD(2)=2.D0*CUN
        CBD(3)=4.D0*CUN
        CBD(4)=4.D0*CUN
        ZC(IMATA+(1-1)*NK+1-1)=-11.98D0*CUN
        ZC(IMATA+(1-1)*NK+2-1)=20.61D0*CUN  
        ZC(IMATA+(1-1)*NK+3-1)=-10.63D0*CUN
        ZC(IMATA+(1-1)*NK+4-1)=14.95D0*CUN

        ZC(IMATA+(2-1)*NK+1-1)=-8.521D0*CUN
        ZC(IMATA+(2-1)*NK+2-1)=15.44D0*CUN  
        ZC(IMATA+(2-1)*NK+3-1)=-6.15D0*CUN
        ZC(IMATA+(2-1)*NK+4-1)=7.885D0*CUN
                
        ZC(IMATA+(3-1)*NK+1-1)=-5.483D0*CUN
        ZC(IMATA+(3-1)*NK+2-1)=9.465D0*CUN  
        ZC(IMATA+(3-1)*NK+3-1)=-2.088D0*CUN
        ZC(IMATA+(3-1)*NK+4-1)=4.590D0*CUN  

        ZC(IMATA+(4-1)*NK+1-1)=-6.298D0*CUN
        ZC(IMATA+(4-1)*NK+2-1)=9.351D0*CUN  
        ZC(IMATA+(4-1)*NK+3-1)=-5.390D0*CUN     
        ZC(IMATA+(4-1)*NK+4-1)=9.573D0*CUN
        WRITE(IFM,*)'BASIC TEST 2'
      ELSE IF (ITEST.EQ.3) THEN
C   --- TEST 3 MATRIX OF SIMPLE SPRING-MASS-DAMPER SYSTEM ---
C   --- (CF. JUNG/KIM/LEE 2001, NK=20)                    ---
        VALM=1.D0
        VALKK=1.D0
        VALA=0.05D0
        VALB=0.5D0
        VALOME=2.D0*SQRT(VALM/VALKK)
        N1=NK/2
C MODIFIER LA TAILLE FIXEE A 100 DU VECTEUR CBD (NE PAS OUBLIER APM012)
        IF (N1.GT.100) CALL ASSERT(.FALSE.)
        DO 7 I=1,N1       
          RAUX1=VALOME*SIN(PI*0.5D0*(2*I-1)/(2*N1+1))
          RAUX2=0.5D0*((VALA/RAUX1)+VALB*RAUX1)
          RAUXX=-RAUX2*RAUX1
          RAUXY=RAUX1*SQRT(1-(RAUX2*RAUX2))
          ZC(IMATA+(2*I-2)*NK+2*I-2)=DCMPLX(RAUXX,RAUXY)
          ZC(IMATA+(2*I-1)*NK+2*I-1)=DCMPLX(RAUXX,-RAUXY)
          CBD(2*I-1)=DCMPLX(RAUXX,RAUXY)
          CBD(2*I)  =DCMPLX(RAUXX,-RAUXY)
    7   CONTINUE 
        WRITE(IFM,*)'BASIC TEST 3'
      ELSE IF (ITEST.EQ.4) THEN
C   --- TEST 4 MATRIX OF SIMPLE SPRING-MASS SYSTEM         ---
C   --- (CF. ASTER TEST SDLD02A, NK=10)                    ---
        CBD(1)=40000.D0*CUN
        CBD(2)=20000.D0*CUN
        CBD(3)=10000.D0*CUN
        CBD(4)=1000.D0*CUN
        CBD(5)=1.D0*CUN
        CBD(6)=0.01D0*CUN
        CBD(7)=-0.001D0*CUN
        CBD(8)=100000.D0*CUN
        CBD(9)=100000.D0*CUN
        CBD(10)=100000.D0*CUN
        DO 8 I=1,NK
          ZC(IMATA+(I-1)*NK+I-1)=CBD(I)
    8   CONTINUE             
        WRITE(IFM,*)'BASIC TEST 4'
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      WRITE(IFM,*)'*** WARNING WARNING WARNING WARNING WARNING ***'
      CALL JEDEMA()
      END
