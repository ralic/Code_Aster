      SUBROUTINE PACOUC(TYPFLU,VECR1,VECR2,VITE,VECR3,MASG,FREQ,
     +                  AMOR,NBNO,INDIC,NBPV,W,VECI1,VECR4,VECR5,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*8  TYPFLU
      INTEGER      NBNO,INDIC,NBPV,VECI1(*),IER,JTRAV1,JTRAV2
      REAL*8       VECR1(*),VECR2(*),VITE(*),VECR3(*),MASG(*),FREQ(*)
      REAL*8       AMOR(*),W(*),VECR4(*),VECR5(*)
      CHARACTER*24 NOM1,NOM2
C
      LOGICAL      CHECK,VERIU0
      REAL*8       KSI0,KCAJ,VGAP
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOM1 = '&&COEFMO.COMPT'
      NOM2 = '&&COEFMO.EXTR'
C
      VERIU0 = .FALSE.
      KCAJ = 0.D0
      CALL JEVEUO(TYPFLU//'           .FSIC','L',LFSIC)
      ITYPFL = ZI(LFSIC)
      IF (ITYPFL.EQ.4) THEN
        IF (INDIC.EQ.1) THEN
          VERIU0 = .TRUE.
          CALL JEVEUO(TYPFLU//'           .FSVR','L',LFSVR)
          VISC = ZR(LFSVR+1)
          HMOY = VECR4(1)
          KCAJ = 12.D0*VISC/(HMOY*HMOY)
        ENDIF
      ENDIF
C
      PI = R8PI()
      NT = 2
      K1 = 1 + NT
      K2 = K1 + NT
      K3 = K2 + NT*NT
      K4 = K3 + NT*NT
      K5 = K4 + NT
      K6 = K5 + NT
      K7 = K6 + NT
      K8 = K7 + NT
      K9 = K8 + NT
      K10 = K9 + NT
      K11 = K10 + NT
      K12 = K11 + NT
C      
      IF (ITYPFL .EQ. 1) THEN
         CALL JEVEUO('&&MDCONF.TEMPO','L',JZONE)               
         NZONE = ZI(JZONE-1+1)
         NB = NBNO*NBPV*NZONE
         CALL WKVECT('&&PACOUC.TRAV1','V V R',2*NB,JTRAV1)
         CALL WKVECT('&&PACOUC.TRAV2','V V I',3*NB,JTRAV2) 
      ENDIF
C          
      DO 10 I = 1,NBPV
        VGAP = VITE(I)
        DO 20 J = 1,NBNO
          IF (VERIU0 .AND. DBLE(ABS(VGAP)).LT.1.D-5) THEN
            KSI0 = (AMOR(J) + KCAJ*VECR1(J))
     &           / (MASG(J)*4.D0*PI*AMOR(NBNO+J))
          ELSE
            KSI0 = AMOR(J) / (MASG(J)*4.D0*PI*AMOR(NBNO+J))
          ENDIF
          DELTA = -2.D0*PI*AMOR(NBNO+J)*KSI0
          PULSAM = 2.D0*PI*AMOR(NBNO+J)*SQRT(1.D0-KSI0*KSI0)
          W(1) = DELTA
          W(2) = PULSAM
C
          CALL PACOU0(W(1),W(K1),W(K2),W(K3),W(K4),W(K5),W(K6),W(K7),
     +                W(K8),W(K9),W(K10),W(K11),W(K12),W(13),CHECK,
     +                VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,VECR5,
     +                VECI1,VGAP,INDIC,NBNO,J,NT)
C
          IF (CHECK) THEN
            W(1) = -DELTA
            W(2) = PULSAM
            CALL PACOU0(W(1),W(K1),W(K2),W(K3),W(K4),W(K5),W(K6),W(K7),
     +                  W(K8),W(K9),W(K10),W(K11),W(K12),W(13),CHECK,
     +                  VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,VECR5,
     +                  VECI1,VGAP,INDIC,NBNO,J,NT)
          END IF
C
          IF (CHECK) THEN
            W(1) = 0.D0
            W(2) = PULSAM
            CALL PACOU0(W(1),W(K1),W(K2),W(K3),W(K4),W(K5),W(K6),W(K7),
     +                  W(K8),W(K9),W(K10),W(K11),W(K12),W(13),CHECK,
     +                  VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,VECR5,
     +                  VECI1,VGAP,INDIC,NBNO,J,NT)
          END IF
C
          IF (CHECK) THEN
            W(1) = 0.5D0*DELTA
            W(2) = PULSAM
            CALL PACOU0(W(1),W(K1),W(K2),W(K3),W(K4),W(K5),W(K6),W(K7),
     +                  W(K8),W(K9),W(K10),W(K11),W(K12),W(13),CHECK,
     +                  VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,VECR5,
     +                  VECI1,VGAP,INDIC,NBNO,J,NT)
          END IF
C
          IF (CHECK) THEN
            W(1) = 2.D0*DELTA
            W(2) = PULSAM
            CALL PACOU0(W(1),W(K1),W(K2),W(K3),W(K4),W(K5),W(K6),W(K7),
     +                  W(K8),W(K9),W(K10),W(K11),W(K12),W(13),CHECK,
     +                  VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,VECR5,
     +                  VECI1,VGAP,INDIC,NBNO,J,NT)
          END IF
C
          IF (CHECK) THEN
            W(1) = 5.D0*DELTA
            W(2) = PULSAM
            CALL PACOU0(W(1),W(K1),W(K2),W(K3),W(K4),W(K5),W(K6),W(K7),
     +                  W(K8),W(K9),W(K10),W(K11),W(K12),W(13),CHECK,
     +                  VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,VECR5,
     +                  VECI1,VGAP,INDIC,NBNO,J,NT)
          END IF
C
          IF (CHECK) THEN
            W(1) = 10.D0*DELTA
            W(2) = PULSAM
            CALL PACOU0(W(1),W(K1),W(K2),W(K3),W(K4),W(K5),W(K6),W(K7),
     +                  W(K8),W(K9),W(K10),W(K11),W(K12),W(13),CHECK,
     +                  VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,VECR5,
     +                  VECI1,VGAP,INDIC,NBNO,J,NT)
          END IF
C
          IF (CHECK) THEN
            W(1) = 20.D0*DELTA
            W(2) = PULSAM
            CALL PACOU0(W(1),W(K1),W(K2),W(K3),W(K4),W(K5),W(K6),W(K7),
     +                  W(K8),W(K9),W(K10),W(K11),W(K12),W(13),CHECK,
     +                  VECR1,VECR2,TYPFLU,VECR3,AMOR,MASG,VECR4,VECR5,
     +                  VECI1,VGAP,INDIC,NBNO,J,NT)
          END IF
C
          IF (CHECK) THEN
            I1 = (I-1)*2*NBNO + (J-1)*2 + 1
            I2 = (I-1)*2*NBNO + (J-1)*2 + 2
            FREQ(I1) = -1.D0
            FREQ(I2) = -1.D0
          ELSE
            I1 = (I-1)*2*NBNO + (J-1)*2 + 1
            I2 = (I-1)*2*NBNO + (J-1)*2 + 2
            FREQ(I1) = SQRT(W(1)*W(1)+W(2)*W(2))/ (2.D0*PI)
            FREQ(I2) = -W(1)/ (2.D0*PI*FREQ(I1))
C
C           ON STOCKE EN FIN DE BOUCLE ET POUR CHAQUE ZONE LES VALEURS 
C           DE VITESSES REDUITES MIN ET MAX QUI SORTENT DE LA PLAGE 
C           EXPERIMENTALE         
            IF (ITYPFL .EQ. 1) THEN 
               CALL JEVEUO(NOM1,'L',JCOMPT)
               CALL JEVEUO(NOM2,'L',JEXTR)                          
               DO 30 K = 1,NZONE
                 L1 = ZI(JCOMPT+3*(K-1))
                 L2 = ZI(JCOMPT+3*(K-1)+1)
                 L3 = ZI(JCOMPT+3*(K-1)+2)
                 BMIN = ZR(JEXTR+2*(K-1))
                 BMAX = ZR(JEXTR+2*(K-1)+1)
                 ZR(JTRAV1 + 2*NZONE*NBPV*(J-1) + 2*(I-1)*NZONE + 
     &           2*(K-1)) = BMIN
                 ZR(JTRAV1 + 2*NZONE*NBPV*(J-1) + 2*(I-1)*NZONE + 
     &           2*(K-1) + 1) = BMAX               
                 ZI(JTRAV2 + 3*NZONE*NBPV*(J-1) + 3*(I-1)*NZONE +
     &           3*(K-1)) = L1            
                 ZI(JTRAV2 + 3*NZONE*NBPV*(J-1) + 3*(I-1)*NZONE +
     &           3*(K-1) + 1) = L2            
                 ZI(JTRAV2 + 3*NZONE*NBPV*(J-1) + 3*(I-1)*NZONE +
     &           3*(K-1) + 2) = L3            
   30          CONTINUE
            ENDIF             
          END IF
   20   CONTINUE
   10 CONTINUE
C
      IF (.NOT.CHECK) IER = 0
C
      CALL JEDEMA()
      END
