      SUBROUTINE TE0358(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16 OPTION,NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_MECA_META_Z  '
C                          POUR LES ELEMENTS 3D
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................


C-----------------------------------------------------------------------
      INTEGER ICOMPO ,ICONTR ,ICPG ,IVARI ,J ,LGPG ,MATER
      INTEGER NBRES ,NNOS
      REAL*8 DELTAZ ,SIGMO ,ZALPHA ,ZVARIM ,ZVARIP
C-----------------------------------------------------------------------
      PARAMETER (NBRES=21)
      CHARACTER*16 COMPOR
      CHARACTER*8 NOMRES(NBRES),NOMCLE(5),ACIER(4),ZIRC(2)
      CHARACTER*4 FAMI
      INTEGER ICODRE(NBRES)
      INTEGER TEST
      REAL*8 VALRES(NBRES),E,NU
      REAL*8 ZFBM,SIG(6),KPT(5),SIGDV(6),DEUXMU
      REAL*8 DFDX(27),DFDY(27),DFDZ(27),TPG,POIDS
      REAL*8 KRON(6),R8BID,PHAS(5),RPRIM,COEF,R0(5),TRANS,VI(5)
      REAL*8 PHASM(4),PHASP(4)

      INTEGER JGANO,NNO,KP,L,NPG1,I,IVECTU,NDIM
      INTEGER IPOIDS,IVF,IDFDE,JPROL,JVALE,NBVAL
      INTEGER IGEOM,IMATE,JTAB(7),IRET,IRET1

      DATA KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA ACIER /'PFERRITE','PPERLITE','PBAINITE','PMARTENS'/
      DATA ZIRC /'ALPHPUR','ALPHBETA'/

C ---------------------------------------------------------------------
C --- FONCTIONS DE FORMES ET POINTS DE GAUSS
      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)

C PARAMETRES EN ENTREE
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      MATER = ZI(IMATE)
      COMPOR = ZK16(ICOMPO+7)

      CALL JEVECH('PCONTMR','L',ICONTR)
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      COMPOR = ZK16(ICOMPO+7)
      CALL TECACH('OON','PVARIPR',7,JTAB,IRET)
      LGPG = MAX(JTAB(6),1)*JTAB(7)
      CALL JEVECH('PVARIPR','L',IVARI)
      CALL JEVECH('PVECTUR','E',IVECTU)

      IF (COMPOR(1:5).EQ.'ACIER') THEN

        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'F1_K'
        NOMRES(4) = 'F2_K'
        NOMRES(5) = 'F3_K'
        NOMRES(6) = 'F4_K'
        NOMRES(7) = 'F1_D_F_M'
        NOMRES(8) = 'F2_D_F_M'
        NOMRES(9) = 'F3_D_F_M'
        NOMRES(10) = 'F4_D_F_M'
        NOMRES(11) = 'SY_MELAN'

        IF (ZK16(ICOMPO) (1:9).EQ.'META_P_IL' .OR.
     &      ZK16(ICOMPO) (1:9).EQ.'META_V_IL' .OR.
     &      ZK16(ICOMPO) (1:9).EQ.'META_P_CL' .OR.
     &      ZK16(ICOMPO) (1:9).EQ.'META_V_CL') THEN
          NOMRES(12) = 'F1_D_SIGM'
          NOMRES(13) = 'F2_D_SIGM'
          NOMRES(14) = 'F3_D_SIGM'
          NOMRES(15) = 'F4_D_SIGM'
          NOMRES(16) = 'C_D_SIGM'
        END IF
        IF (ZK16(ICOMPO) (1:10).EQ.'META_P_INL' .OR.
     &      ZK16(ICOMPO) (1:10).EQ.'META_V_INL') THEN
          NOMCLE(1) = 'SIGM_F1'
          NOMCLE(2) = 'SIGM_F2'
          NOMCLE(3) = 'SIGM_F3'
          NOMCLE(4) = 'SIGM_F4'
          NOMCLE(5) = 'SIGM_C'
        END IF
        NOMRES(17) = 'F1_ETA'
        NOMRES(18) = 'F2_ETA'
        NOMRES(19) = 'F3_ETA'
        NOMRES(20) = 'F4_ETA'
        NOMRES(21) = 'C_ETA'

      ELSE IF (COMPOR(1:4).EQ.'ZIRC') THEN
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'F1_K'
        NOMRES(4) = 'F2_K'
        NOMRES(5) = 'F1_D_F_ME'
        NOMRES(6) = 'F2_D_F_ME'
        NOMRES(7) = 'SY_MELAN'

        IF (ZK16(ICOMPO) (1:9).EQ.'META_P_IL' .OR.
     &      ZK16(ICOMPO) (1:9).EQ.'META_V_IL') THEN
          NOMRES(8) = 'F1_D_SIGM'
          NOMRES(9) = 'F2_D_SIGM'
          NOMRES(10) = 'C_D_SIGM'
        END IF
        IF (ZK16(ICOMPO) (1:10).EQ.'META_P_INL' .OR.
     &      ZK16(ICOMPO) (1:10).EQ.'META_V_INL') THEN
          NOMCLE(1) = 'SIGM_F1'
          NOMCLE(2) = 'SIGM_F2'
          NOMCLE(3) = 'SIGM_C'
        END IF
        NOMRES(14) = 'F1_ETA'
        NOMRES(15) = 'F2_ETA'
        NOMRES(16) = 'C_ETA'

      END IF
      DO 10 I = 1,NBRES
   10 CONTINUE

      DO 140 KP = 1,NPG1
        ICPG = 6* (KP-1)
        CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )
        CALL RCVARC(' ','TEMP','+','RIGI',KP,1,TPG,IRET1)

        IF (COMPOR(1:5) .EQ. 'ACIER') THEN
          DO 7 L=1,4
             CALL RCVARC(' ',ACIER(L),'-','RIGI',KP,1,
     &            PHASM(L),IRET)
             IF (IRET.EQ.1) PHASM(L)=0.D0
             CALL RCVARC(' ',ACIER(L),'+','RIGI',KP,1,
     &            PHASP(L),IRET)
             IF (IRET.EQ.1) PHASP(L)=0.D0
    7     CONTINUE
        ELSEIF (COMPOR(1:4) .EQ. 'ZIRC') THEN
          DO 9 L=1,2
             CALL RCVARC(' ',ZIRC(L),'-','RIGI',KP,1,
     &            PHASM(L),IRET)
             IF (IRET.EQ.1) PHASM(L)=0.D0
             CALL RCVARC(' ',ZIRC(L),'+','RIGI',KP,1,
     &            PHASP(L),IRET)
             IF (IRET.EQ.1) PHASP(L)=0.D0
    9     CONTINUE
        ENDIF

        CALL RCVALB(FAMI,KP,1,'+',MATER,' ','ELAS_META',0,' ',
     &              0.D0,2,NOMRES,VALRES,ICODRE,1)
        E = VALRES(1)
        NU = VALRES(2)
        DEUXMU = E/ (1.D0+NU)

        IF (COMPOR(1:5).EQ.'ACIER') THEN

          CALL RCVALB(FAMI,KP,1,'+',MATER,' ','META_PT',0,' ',
     &                0.D0,4,NOMRES(3),VALRES(3),ICODRE(3),0)

          DO 30 I = 3,6
            IF (ICODRE(I).NE.0) THEN
              KPT(I-2) = 0.D0
            ELSE
              KPT(I-2) = VALRES(I)
            END IF
   30     CONTINUE

          ZFBM = PHASP(1)
          DO 40 I = 1,3
             ZFBM = ZFBM + PHASP(1+I)
   40     CONTINUE

          TRANS = 0.D0
          DO 50 I = 1,4
            ZVARIM = PHASM(I)
            ZVARIP = PHASP(I)
            DELTAZ = (ZVARIP-ZVARIM)
            IF (DELTAZ.GT.0) THEN
              J = 6 + I
              CALL RCVALB('RIGI',1,1,'+',MATER,' ','META_PT',
     &                    1,'META',ZFBM,1,NOMRES(J),
     &                    VALRES(J),ICODRE(J),0)
              IF (ICODRE(J).NE.0) VALRES(J) = 0.D0

              TRANS = TRANS + KPT(I)*VALRES(J)*DELTAZ
            END IF
   50     CONTINUE

          CALL RCVALB(FAMI,KP,1,'+',MATER,' ','META_VISC',0,' ',0.D0,
     &                5,NOMRES(17),VALRES(17),ICODRE(17),0)
          TEST = 1
          DO 60 I = 17,21
            IF (ICODRE(I).EQ.0) TEST = 0
   60     CONTINUE
          IF ((ZR(IVARI+ (KP-1)*LGPG+5).GT.0.5D0) .AND.
     &        (TEST.EQ.1)) THEN

            CALL RCVALB('RIGI',1,1,'+',MATER,' ','ELAS_META',
     &                1,'META',ZFBM,1,
     &                NOMRES(11),  VALRES(11),ICODRE(11),0)
            IF (ICODRE(11).NE.0) THEN
              VALRES(11) = ZFBM
            END IF


            PHAS(1) = PHASP(1)
            PHAS(2) = PHASP(2)
            PHAS(3) = PHASP(3)
            PHAS(4) = PHASP(4)
            PHAS(5) = 1.D0 - (PHAS(1)+PHAS(2)+PHAS(3)+PHAS(4))

            IF (ZK16(ICOMPO) (1:9).EQ.'META_P_IL' .OR.
     &          ZK16(ICOMPO) (1:9).EQ.'META_V_IL') THEN

              CALL RCVALB(FAMI,KP,1,'+',MATER,' ','META_ECRO_LINE',
     &                    0,' ',0.D0,5,
     &                    NOMRES(12),VALRES(12),ICODRE(12),1)
              R0(1) = VALRES(12)*E/ (E-VALRES(12))
              R0(2) = VALRES(13)*E/ (E-VALRES(13))
              R0(3) = VALRES(14)*E/ (E-VALRES(14))
              R0(4) = VALRES(15)*E/ (E-VALRES(15))
              R0(5) = VALRES(16)*E/ (E-VALRES(16))
            END IF
            IF (ZK16(ICOMPO) (1:9).EQ.'META_P_CL' .OR.
     &          ZK16(ICOMPO) (1:9).EQ.'META_V_CL') THEN
              CALL RCVALB(FAMI,KP,1,'+',MATER,' ','META_ECRO_LINE',0,
     &                    ' ',0.D0,5,
     &                    NOMRES(12),VALRES(12),ICODRE(12),1)
              R0(1) = (2.D0/3.D0)*VALRES(12)*E/ (E-VALRES(12))
              R0(2) = (2.D0/3.D0)*VALRES(13)*E/ (E-VALRES(13))
              R0(3) = (2.D0/3.D0)*VALRES(14)*E/ (E-VALRES(14))
              R0(4) = (2.D0/3.D0)*VALRES(15)*E/ (E-VALRES(15))
              R0(5) = (2.D0/3.D0)*VALRES(16)*E/ (E-VALRES(16))
            END IF

            IF (ZK16(ICOMPO) (1:10).EQ.'META_P_INL' .OR.
     &          ZK16(ICOMPO) (1:10).EQ.'META_V_INL') THEN
              VI(1) = ZR(IVARI+ (KP-1)*LGPG)
              VI(2) = ZR(IVARI+ (KP-1)*LGPG+1)
              VI(3) = ZR(IVARI+ (KP-1)*LGPG+2)
              VI(4) = ZR(IVARI+ (KP-1)*LGPG+3)
              VI(5) = ZR(IVARI+ (KP-1)*LGPG+4)

              IF (IRET1.EQ.1) CALL U2MESS('F','CALCULEL_31')
              DO 70 I = 1,5
                CALL RCTRAC(MATER,2,NOMCLE(I),TPG,JPROL,
     &                      JVALE,NBVAL,R8BID)
                CALL RCFONC('V',2,JPROL,JVALE,NBVAL,R8BID,
     &                      R8BID,R8BID,VI(I),R8BID,R0(I),R8BID,R8BID,
     &                      R8BID)
   70         CONTINUE
            END IF

            IF (ZFBM.GT.0.D0) THEN
              RPRIM = PHAS(1)*R0(1) + PHAS(2)*R0(2) + PHAS(3)*R0(3) +
     &                PHAS(4)*R0(4)
              RPRIM = RPRIM/ZFBM
            ELSE
              RPRIM = 0.D0
            END IF
            RPRIM = (1.D0-VALRES(11))*R0(5) + VALRES(11)*RPRIM
            COEF = 1.D0 - (1.5D0*DEUXMU)/ (1.5D0*DEUXMU+RPRIM)

          ELSE
            COEF = 1.D0
          END IF

        ELSE IF (COMPOR(1:4).EQ.'ZIRC') THEN
          CALL RCVALB(FAMI,KP,1,'+',MATER,' ','META_PT',0,' ',
     &                0.D0,2,NOMRES(3),
     &                VALRES(3),ICODRE(3),0)

          DO 80 I = 3,4
            IF (ICODRE(I).NE.0) THEN
              KPT(I-2) = 0.D0
            ELSE
              KPT(I-2) = VALRES(I)
            END IF
   80     CONTINUE

          ZALPHA = PHASP(1) + PHASP(2)


          ZVARIM = PHASM(1)
          ZVARIP = PHASP(1)
          DELTAZ = (ZVARIP-ZVARIM)

          TRANS = 0.D0
          IF (DELTAZ.GT.0) THEN
            CALL RCVALB('RIGI',1,1,'+',MATER,' ','META_PT',
     &                  1,'META',ZFBM,1,NOMRES(5),
     &                  VALRES(5),ICODRE(5),0)
            IF (ICODRE(5).NE.0) VALRES(5) = 0.D0

            TRANS = TRANS + KPT(1)*VALRES(5)*DELTAZ
          END IF


          ZVARIM = PHASM(2)
          ZVARIP = PHASP(2)
          DELTAZ = (ZVARIP-ZVARIM)
          IF (DELTAZ.GT.0) THEN

            CALL RCVALB('RIGI',1,1,'+',MATER,' ','META_PT',
     &                  1,'META',ZFBM,1,NOMRES(6),
     &                  VALRES(6),ICODRE(6),0)
            IF (ICODRE(6).NE.0) VALRES(6) = 0.D0

            TRANS = TRANS + KPT(2)*VALRES(6)*DELTAZ
          END IF


          CALL RCVALB(FAMI,KP,1,'+',MATER,' ','META_VISC',0,' ',0.D0,
     &                3,NOMRES(14),VALRES(14),ICODRE(14),0)
          TEST = 1
          DO 90 I = 14,16
            IF (ICODRE(14).EQ.0) TEST = 0
   90     CONTINUE
          IF ((ZR(IVARI+ (KP-1)*LGPG+3).GT.0.5D0) .AND.
     &        (TEST.EQ.1)) THEN

            CALL RCVALB('RIGI',1,1,'+',MATER,' ','ELAS_META',
     &                  1,'META',ZALPHA,1,
     &                 NOMRES(7), VALRES(7),ICODRE(7),0)
            IF (ICODRE(7).NE.0) THEN
              VALRES(7) = ZALPHA
            END IF

            PHAS(1) = PHASP(1)
            PHAS(2) = PHASP(2)

            PHAS(3) = 1.D0 - (PHAS(1)+PHAS(2))

            IF (ZK16(ICOMPO) (1:9).EQ.'META_P_IL' .OR.
     &          ZK16(ICOMPO) (1:9).EQ.'META_V_IL') THEN
              CALL RCVALB(FAMI,KP,1,'+',MATER,' ','META_ECRO_LINE',
     &                    0,' ',0.D0,3,
     &                    NOMRES(8),VALRES(8),ICODRE(8),1)
              R0(1) = VALRES(8)*E/ (E-VALRES(8))
              R0(2) = VALRES(9)*E/ (E-VALRES(9))
              R0(3) = VALRES(10)*E/ (E-VALRES(10))
            END IF
            IF (ZK16(ICOMPO) (1:9).EQ.'META_P_CL' .OR.
     &          ZK16(ICOMPO) (1:9).EQ.'META_V_CL') THEN
              CALL RCVALB(FAMI,KP,1,'+',MATER,' ','META_ECRO_LINE',
     &                    0,' ',0.D0,5,
     &                    NOMRES(8),VALRES(8),ICODRE(8),1)
              R0(1) = (2.D0/3.D0)*VALRES(8)*E/ (E-VALRES(8))
              R0(2) = (2.D0/3.D0)*VALRES(9)*E/ (E-VALRES(9))
              R0(3) = (2.D0/3.D0)*VALRES(10)*E/ (E-VALRES(10))

            END IF

            IF (ZK16(ICOMPO) (1:10).EQ.'META_P_INL' .OR.
     &          ZK16(ICOMPO) (1:10).EQ.'META_V_INL') THEN
              VI(1) = ZR(IVARI+ (KP-1)*LGPG)
              VI(2) = ZR(IVARI+ (KP-1)*LGPG+1)
              VI(3) = ZR(IVARI+ (KP-1)*LGPG+2)

              DO 100 I = 1,3
                CALL RCTRAC(MATER,3,NOMCLE(I),TPG,JPROL,
     &                      JVALE,NBVAL,R8BID)
                CALL RCFONC('V',3,JPROL,JVALE,NBVAL,
     &                      R8BID,R8BID,R8BID,VI(I),R8BID,R0(I),R8BID,
     &                      R8BID,R8BID)
  100         CONTINUE
            END IF

            IF (ZALPHA.GT.0.D0) THEN
              RPRIM = PHAS(1)*R0(1) + PHAS(2)*R0(2)
              RPRIM = RPRIM/ZALPHA
            ELSE
              RPRIM = 0.D0
            END IF
            RPRIM = (1.D0-VALRES(7))*R0(3) + VALRES(7)*RPRIM
            COEF = 1.D0 - (1.5D0*DEUXMU)/ (1.5D0*DEUXMU+RPRIM)

          ELSE
            COEF = 1.D0
          END IF
        END IF


        SIGMO = 0.D0
        DO 110 I = 1,3
          SIGMO = SIGMO + ZR(ICONTR+ICPG+I-1)
  110   CONTINUE
        SIGMO = SIGMO/3.D0
        DO 120 I = 1,6
          SIGDV(I) = ZR(ICONTR+ICPG+I-1) - SIGMO*KRON(I)
          SIG(I) = COEF* (+1.5D0*TRANS*SIGDV(I))
          SIG(I) = DEUXMU*SIG(I)
  120   CONTINUE
        DO 130 I = 1,NNO
          ZR(IVECTU+3* (I-1)) = ZR(IVECTU+3* (I-1)) +
     &                          POIDS* (SIG(1)*DFDX(I)+SIG(4)*DFDY(I)+
     &                          SIG(5)*DFDZ(I))
          ZR(IVECTU+3* (I-1)+1) = ZR(IVECTU+3* (I-1)+1) +
     &                            POIDS* (SIG(2)*DFDY(I)+SIG(4)*DFDX(I)+
     &                            SIG(6)*DFDZ(I))
          ZR(IVECTU+3* (I-1)+2) = ZR(IVECTU+3* (I-1)+2) +
     &                            POIDS* (SIG(3)*DFDZ(I)+SIG(5)*DFDX(I)+
     &                            SIG(6)*DFDY(I))
  130   CONTINUE
  140 CONTINUE
      END
