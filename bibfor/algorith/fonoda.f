      SUBROUTINE FONODA(IMATE,PERMAN,MECANI,PRESS1,PRESS2,TEMPE,
     &                  DIMDEF,DIMCON,NDIM,DT,FNOEVO,
     &                  CONGEM,R)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C ======================================================================
      IMPLICIT     NONE
      LOGICAL      FNOEVO
      LOGICAL      PERMAN
      INTEGER      MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5),DIMDEF,DIMCON
      INTEGER      NDIM,IMATE
      REAL*8       DT,CONGEM(DIMCON),R(DIMDEF+1)
C ======================================================================
      INTEGER      NHOM,YAMEC,YAP1,YAP2,YATE,ADDEME,ADCOME,NBPHA1,ADDETE
      INTEGER      ADDEP1,ADCP11,ADCP12,NBPHA2,ADDEP2,ADCP21,ADCP22
      INTEGER      ADCOTE,I
      PARAMETER   (NHOM=3)
      REAL*8       HOM(NHOM),PESA(3),RAC2
      INTEGER ICODRE(NHOM)
      CHARACTER*8  NCRA5(NHOM)
      DATA NCRA5 / 'PESA_X','PESA_Y','PESA_Z' /
C ======================================================================
C --- RECUPERATION DE LA PESANTEUR DANS DEFI_MATERIAU ------------------
C ======================================================================
       CALL RCVALA(IMATE,' ','THM_DIFFU',0,' ',0.D0,
     &                                       NHOM,NCRA5,HOM,ICODRE,1)
       PESA(1)=HOM(1)
       PESA(2)=HOM(2)
       PESA(3)=HOM(3)
       RAC2 = SQRT(2.0D0)
C ======================================================================
C --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU --------------
C ======================================================================
      YAMEC  = MECANI(1)
      ADDEME = MECANI(2)
      YAP1   = PRESS1(1)
      NBPHA1 = PRESS1(2)
      ADDEP1 = PRESS1(3)
      IF ( PERMAN ) THEN
        I = 1
      ELSE
        I = 0
      ENDIF
      ADCP11 = PRESS1(4) - I
      ADCP12 = PRESS1(5) - I
      ADCOME = MECANI(3)
      YAP2   = PRESS2(1)
      NBPHA2 = PRESS2(2)
      ADDEP2 = PRESS2(3)
      ADCP21 = PRESS2(4) - I
      ADCP22 = PRESS2(5) - I
      YATE   = TEMPE(1)
      ADDETE = TEMPE(2)
      ADCOTE = TEMPE(3)
C ======================================================================
C --- COMME CONGEM CONTIENT LES VRAIES CONTRAINTES ET ------------------
C --- COMME PAR LA SUITE ON TRAVAILLE AVEC SQRT(2)*SXY -----------------
C --- ON COMMENCE PAR MODIFIER LES CONGEM EN CONSEQUENCE ---------------
C ======================================================================
         IF(YAMEC.EQ.1) THEN
            DO 100 I = 4 , 6
               CONGEM(ADCOME+I-1) = CONGEM(ADCOME+I-1)*RAC2
 100        CONTINUE
         ENDIF
C ======================================================================
C --- CALCUL DU RESIDU R -----------------------------------------------
C ======================================================================
         IF(YAMEC.EQ.1) THEN
C ======================================================================
C --- CONTRIBUTIONS A R2 INDEPENDANTE DE YAP1, YAP2 ET YATE ------------
C --- CONTRAINTES SIGPRIMPLUS PAGE 33 ----------------------------------
C ======================================================================
            DO 6 I=1,6
               R(ADDEME+NDIM+I-1)= R(ADDEME+NDIM+I-1)+CONGEM(ADCOME-1+I)
 6          CONTINUE
C ======================================================================
C --- SCALAIRE SIGPPLUS MULTIPLIE PAR LE TENSEUR UNITE -----------------
C ======================================================================
            DO 7 I=1,3
               R(ADDEME+NDIM-1+I)=R(ADDEME+NDIM-1+I)+CONGEM(ADCOME+6)
 7          CONTINUE
C ======================================================================
C --- CONTRIBUTION A R DEPENDANTE DE YAP1 ------------------------------
C ======================================================================
            IF(YAP1.EQ.1) THEN
               DO 8 I=1,NDIM
                  R(ADDEME+I-1)=R(ADDEME+I-1) - PESA(I)*CONGEM(ADCP11)
 8             CONTINUE
               IF(NBPHA1.GT.1) THEN
                  DO 9 I=1,NDIM
                     R(ADDEME+I-1)=R(ADDEME+I-1)- PESA(I)*CONGEM(ADCP12)
 9                CONTINUE
               ENDIF
            ENDIF
C ======================================================================
C --- CONTRIBUTIONS A R DEPENDANTE DE YAP2 -----------------------------
C ======================================================================
            IF(YAP2.EQ.1) THEN
               DO 11 I=1,NDIM
                  R(ADDEME+I-1)=R(ADDEME+I-1)- PESA(I)*CONGEM(ADCP21)
 11            CONTINUE
               IF(NBPHA2.GT.1) THEN
                  DO 12 I=1,NDIM
                     R(ADDEME+I-1)=R(ADDEME+I-1)- PESA(I)*CONGEM(ADCP22)
 12               CONTINUE
               ENDIF
            ENDIF
         ENDIF
C ======================================================================
         IF(FNOEVO) THEN
C ======================================================================
C --- TERMES DEPENDANT DE DT DANS FORC_NODA POUR STAT_NON_LINE ---------
C ======================================================================
            IF(YAP1.EQ.1) THEN

               DO 112 I=1,NDIM
                  R(ADDEP1+I)=R(ADDEP1+I)+DT*CONGEM(ADCP11+I)
 112           CONTINUE

               IF(NBPHA1.GT.1) THEN
                  DO 13 I=1,NDIM
                     R(ADDEP1+I)=R(ADDEP1+I)+DT*CONGEM(ADCP12+I)
 13               CONTINUE
               ENDIF

               IF(YATE.EQ.1) THEN

                  DO 14 I=1,NDIM
                     R(ADDETE)=R(ADDETE)+DT*CONGEM(ADCP11+I)*PESA(I)
 14               CONTINUE

                  IF(NBPHA1.GT.1) THEN
                     DO 15 I=1,NDIM
                        R(ADDETE)=R(ADDETE) +DT*CONGEM(ADCP12+I)*PESA(I)
 15                  CONTINUE
                  ENDIF

                  DO 16 I=1,NDIM
                     R(ADDETE+I)=R(ADDETE+I)+
     &                         DT*CONGEM(ADCP11+NDIM+1)*CONGEM(ADCP11+I)
 16               CONTINUE

                  IF(NBPHA1.GT.1) THEN
                     DO 17 I=1,NDIM
                        R(ADDETE+I)=R(ADDETE+I)+
     &                         DT*CONGEM(ADCP12+NDIM+1)*CONGEM(ADCP12+I)
 17                  CONTINUE
                  ENDIF

               ENDIF
            ENDIF
C
            IF(YAP2.EQ.1) THEN
               DO 18 I=1,NDIM
                  R(ADDEP2+I)=R(ADDEP2+I)+DT*CONGEM(ADCP21+I)
 18            CONTINUE
               IF(NBPHA2.GT.1) THEN
                  DO 19 I=1,NDIM
                     R(ADDEP2+I)=R(ADDEP2+I)+DT*CONGEM(ADCP22+I)
 19               CONTINUE
               ENDIF
C
               IF(YATE.EQ.1) THEN
                  DO 20 I=1,NDIM
                     R(ADDETE)=R(ADDETE)+DT*CONGEM(ADCP21+I)*PESA(I)
 20               CONTINUE
                  IF(NBPHA2.GT.1) THEN
                     DO 21 I=1,NDIM
                        R(ADDETE)=R(ADDETE)+DT*CONGEM(ADCP22+I)*PESA(I)
 21                  CONTINUE
                  ENDIF
                  DO 122 I=1,NDIM
                     R(ADDETE+I)=R(ADDETE+I)+
     &                        DT*CONGEM(ADCP21+NDIM+1)*CONGEM(ADCP21+I)
 122              CONTINUE
                  IF(NBPHA2.GT.1) THEN
                     DO 23 I=1,NDIM
                        R(ADDETE+I)=R(ADDETE+I)+
     &                       DT*CONGEM(ADCP22+NDIM+1)*CONGEM(ADCP22+I)
 23                  CONTINUE
                  ENDIF
               ENDIF
            ENDIF
C
            IF(YATE.EQ.1) THEN
               DO 24 I=1,NDIM
                  R(ADDETE+I)=R(ADDETE+I)+DT*CONGEM(ADCOTE+I)
 24            CONTINUE
            ENDIF
         ENDIF
C ======================================================================
      END
