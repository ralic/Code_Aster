      SUBROUTINE TE0452(OPTION,NOMTE)
      IMPLICIT  NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/10/2011   AUTEUR DELMAS J.DELMAS 
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
C     OPTION : EFGE_ELNO_EXCENT
C     IN   K16   OPTION : NOM DE L'OPTION A CALCULER
C     IN   K16   NOMTE  : NOM DU TYPE_ELEMENT
C     ------------------------------------------------------------------
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER ITAB1(7),ITAB2(7),IRET,JIN,JOUT,LGCATA
      INTEGER NBNO,NBCMP,JCARA,K,I,IBID
      LOGICAL LREEL
      REAL*8 EXCEN
C     ------------------------------------------------------------------

      CALL ASSERT(OPTION.EQ.'EFGE_ELNO_EXCENT')

      CALL TECACH('ONO','PEFFONR',7,ITAB1,IRET)
      IF (IRET.EQ.0) THEN
        LREEL=.TRUE.
        CALL TECACH('OOO','PEFFOER',7,ITAB2,IBID)
      ELSE
        LREEL=.FALSE.
        CALL TECACH('OOO','PEFFONC',7,ITAB1,IBID)
        CALL TECACH('OOO','PEFFOEC',7,ITAB2,IBID)
      ENDIF

      JIN=ITAB1(1)
      NBNO=ITAB1(3)
      LGCATA=ITAB1(2)
      NBCMP=LGCATA/NBNO
      CALL ASSERT(LGCATA.EQ.NBNO*NBCMP)
      CALL ASSERT(NBNO.LE.4)
      CALL ASSERT(NBCMP.LE.8)

      JOUT=ITAB2(1)
      CALL ASSERT(ITAB2(2).EQ.LGCATA)

      CALL JEVECH('PCACOQU','L',JCARA)
      EXCEN=ZR(JCARA+5-1)


      IF (LREEL) THEN
        DO 10,K=1,NBNO*NBCMP
          ZR(JOUT-1+K)=ZR(JIN-1+K)
   10   CONTINUE

        DO 20 I=1,NBNO
          ZR(JOUT-1+(I-1)*NBCMP+4)=ZR(JOUT-1+(I-1)*NBCMP+4)-
     &                             EXCEN*ZR(JOUT-1+(I-1)*NBCMP+1)
          ZR(JOUT-1+(I-1)*NBCMP+5)=ZR(JOUT-1+(I-1)*NBCMP+5)-
     &                             EXCEN*ZR(JOUT-1+(I-1)*NBCMP+2)
          ZR(JOUT-1+(I-1)*NBCMP+6)=ZR(JOUT-1+(I-1)*NBCMP+6)-
     &                             EXCEN*ZR(JOUT-1+(I-1)*NBCMP+3)
   20   CONTINUE

      ELSE
        DO 30,K=1,NBNO*NBCMP
          ZC(JOUT-1+K)=ZC(JIN-1+K)
   30   CONTINUE

        DO 40 I=1,NBNO
          ZC(JOUT-1+(I-1)*NBCMP+4)=ZC(JOUT-1+(I-1)*NBCMP+4)-
     &                             EXCEN*ZC(JOUT-1+(I-1)*NBCMP+1)
          ZC(JOUT-1+(I-1)*NBCMP+5)=ZC(JOUT-1+(I-1)*NBCMP+5)-
     &                             EXCEN*ZC(JOUT-1+(I-1)*NBCMP+2)
          ZC(JOUT-1+(I-1)*NBCMP+6)=ZC(JOUT-1+(I-1)*NBCMP+6)-
     &                             EXCEN*ZC(JOUT-1+(I-1)*NBCMP+3)
   40   CONTINUE

      ENDIF


      END
