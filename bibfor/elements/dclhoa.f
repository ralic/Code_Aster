      SUBROUTINE DCLHOA(N,TAB,IRAN)
      
      IMPLICIT NONE
      
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 25/09/2006   AUTEUR MARKOVIC D.MARKOVIC 
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
      INTEGER I,J,IA,IB,IR,IQ,IRA,IRB,IAUX
      REAL*8  PIVOT
      INTEGER   LGPILE
      INTEGER   KOPF, IPIL1(100),IPIL2(100)
      REAL*8      TAB(*)
      INTEGER     N
      INTEGER     IRAN(*)
      DATA      LGPILE /100/  

      DO 10, I=1,N
         IRAN(I)=I
10    CONTINUE

      IF(N.LE.1) GOTO 100

      J=0
      IPIL1(1)=1                  
      IPIL2(1)=N
      KOPF=1
 20   CONTINUE

      IA=IPIL1(KOPF)
      IB=IPIL2(KOPF)
      KOPF=KOPF-1

 30   CONTINUE
      IF(IA.LT.IB) THEN
        PIVOT=TAB(IRAN(IA))

        IQ=IA+1
        IR=IB

  40    CONTINUE
        IF(IQ.LE.IR) THEN
          J=IR-1
          DO 45, I=IQ,J
            IF(TAB(IRAN(I)).GT.PIVOT) GOTO 50
  45      CONTINUE

  50      CONTINUE
          IQ=I+1
          DO 60, J=IR,I+1,-1
            IF(TAB(IRAN(J)).LT.PIVOT) GOTO 70
  60      CONTINUE
  70      CONTINUE
          IAUX=IRAN(I)
          IRAN(I)=IRAN(J)
          IRAN(J)=IAUX
          IR=J-1
          GOTO 40
        ENDIF

        IF(I.EQ.J .AND. TAB(IRAN(J)).LT.PIVOT) IR=J
          IAUX=IRAN(IA)
          IRAN(IA)=IRAN(IR)
          IRAN(IR)=IAUX

          IRA=IR-1
          IRB=IR+1
          IF(IR-IA .GT. IB-IR) THEN
            IF(IA.LT.IRA) THEN
              IF(KOPF .GE. LGPILE ) THEN
                CALL UTMESS('F','DCLASS','PILES SATUREES')
              ENDIF                            
              KOPF=KOPF+1
              IPIL1(KOPF)=IA
              IPIL2(KOPF)=IRA
            ENDIF
            IA=IRB
          ELSE
            IF(IRB.LT.IB) THEN
              IF(KOPF .GE. LGPILE ) THEN
                CALL UTMESS('F','DCLASS','PILES SATUREES')
              ENDIF                            
              KOPF=KOPF+1
              IPIL1(KOPF)=IRB
              IPIL2(KOPF)=IB
            ENDIF
            IB=IRA
          ENDIF
          GOTO 30
      ENDIF

      IF(KOPF .GT. 0 ) GOTO 20
 100  CONTINUE
      END 
