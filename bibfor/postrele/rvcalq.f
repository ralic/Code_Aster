      SUBROUTINE RVCALQ(IOCC,SDEVAL,VEC1,VEC2,REPERE,NOMCP,NBCPNC,
     &                  NBCPCD,OPTION,QUANT,SDLIEU,CODIR,VALDIR,SDCALQ,
     &                  COURBE)
      IMPLICIT   NONE
C
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      CHARACTER*24 SDEVAL,QUANT,SDLIEU
      CHARACTER*19 SDCALQ
      CHARACTER*16 OPTION
      CHARACTER*8 NOMCP(*),REPERE,COURBE
      INTEGER NBCPNC,NBCPCD,CODIR
      REAL*8 VALDIR(*),VEC1(*),VEC2(*)
      REAL*8 VTV,VAX,VAY,VAZ,VBX,VBY,VBZ
      LOGICAL TRIDIM
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C TOLE CRP_20
C     ------------------------------------------------------------------
C     CALCUL DE LA QUANTITE POST-TRAITEE
C     ------------------------------------------------------------------
C IN  IOCC   : I : NUMERO DE L' OCCURENCE TRAITEE
C IN  SDEVAL : K : SS_CHP_GD DE L' EVALUATION DES CMP NECESSAIRE
C IN  VEC1   : R : VECTEUR 1 DU NOUVEAU REPERE (TANGEANTE)
C IN  VEC2   : R : VECTEUR 2 DU NOUVEAU REPERE (NORMALE)
C IN  SDLIEU : K : NOM DE LA SD LIEU DU LIEU TRAITE
C IN  REPERE : K : NOM DU REPERE DE POST_TRAITEMENT
C IN  NOMCP  : K : TABLEAU DES NOMS DE CMP CANDIDATES
C IN  OPTION : K : NOM DE L' OPTION DEMANDEE
C IN  NBCPCD : I : NOMBRE DE CMP CANDIDATE
C IN  NBCPNC : I : NOMBRE DE CMP NECESSAIRE
C IN  QUANT  : K : NOM DE LA QUANTITE A POST-TRAITER
C IN  CODIR  : I : CODE DES DIRECTIONS ACTIVES (TRACE DIRECTION)
C IN  VALDIR : R : VALEUR DES DIRECTIONS ACTIVES (TRACE DIRECTION)
C OUT SDCALQ : K : NOM DE LA SD DE CALCUL DE LA QUANTITE
C                    .VALE : S V R8 <-- VALEUR DES CMP DE LA QUANTITE
C                                       DOCU = 'CHNO' OU 'CHLM'
C                    .PADR : S V I  <-- TABLE D' INDIRECTION SUR .VALE
C
C                    .PNBN : S V I  <-- TABLE NB_NOEUD PAR MAILLES
C                                       N' EXISTE QUE POUR 'CHLM'
C                    .PNCO : S V I  <-- TABLE DES NB_COUCHE PAR MAILLES
C                                       N' EXISTE QUE POUR 'CHLM'
C                    .PNSP : S V I  <-- TABLE DES NB_SS-PT PAR MAILLES
C                                       N' EXISTE QUE POUR 'CHLM'
C     ------------------------------------------------------------------



      CHARACTER*80 KBID
      CHARACTER*24 NVALEE,NPADRE,NPNBNE,NVALEQ,NPADRQ,NPNBNQ,NPCMPE
      CHARACTER*24 NNOCPQ,NPNCOE,NPNSPE
      CHARACTER*4 DOCU,DOCUL,K4
      CHARACTER*2 TQ
      CHARACTER*1 K1

      INTEGER NBPT,LPADR,LPNBN,APNBNQ,AVALEQ,APADRQ,IBID,ANOCPQ,APNCOE
      INTEGER APNBNE,AVALEE,APADRE,IDECQ,IDECE,NVP,N,NBOC
      INTEGER ADRE,ADRQ,LL,I,J,K,IOC,APCMPE,ANUMCP,IADR,ACPGD,APNSPE
      INTEGER AV1X,AV2X,AV1Y,AV2Y,IPADR,APNSPQ
      INTEGER KD1,KD2,KD3,KD4,KD5,KD6,IDEC,NY,IL
      INTEGER L,LNQ,LNE,M,MDER,NBADRQ,NBND,NC,NUM,PT,APNCOQ
      INTEGER NPERM,ITYPE,IORDRE, IFM, NIV
      INTEGER ICO,ICOEF,IND,INDICE,IOCC,ISP,NBCO,NBSP,NITJAC

      REAL*8 VP1,VP2,TR,DET,A,B,C,TXX,TXY,TYY,TXZ,TYZ,SXX,SXY,SYY
      REAL*8 TX,TY,TZ,TZZ,VAL,VM,AUX,ZERO,VECTY(3)
      REAL*8 V1X,V2X,V3X,V1Y,V2Y,V3Y,V1Z,V2Z,V3Z,V1N,V2N,V2P
      REAL*8 AR(6),BR(6),VECPRO(3,3),VALPRO(3),V2(2)
      REAL*8 JACAUX(3),TOL,TOLDYN,R8VIDE
      INTEGER TCOE,TCOQ,TNDE,TNDQ

      INTEGER ASGTU
      REAL*8 SGTU
      INTEGER      IARG
      SGTU(I) = ZR(ASGTU+I-1)
C     FORME BILINEAIRE ASSOCIEE AU TENSEUR
      VTV(VAX,VAY,VAZ,VBX,VBY,VBZ) = VAX* (VBX*TXX+VBY*TXY+VBZ*TXZ) +
     &                               VAY* (VBX*TXY+VBY*TYY+VBZ*TYZ) +
     &                               VAZ* (VBX*TXZ+VBY*TYZ+VBZ*TZZ)

C======================================================================

      CALL JEMARQ()
      ZERO = 0.0D0
      NVP = 3
      IF (COURBE.EQ.' ') THEN
        TRIDIM = .FALSE.
      ELSE
        CALL GETVR8('ACTION','VECT_Y',IOCC,IARG,3,VECTY,NY)
        TRIDIM = NY .NE. 0
      END IF
      CALL INFNIV ( IFM, NIV )

      NVALEE = SDEVAL(1:19)//'.VALE'
      NPADRE = SDEVAL(1:19)//'.PADR'
      NPNBNE = SDEVAL(1:19)//'.PNBN'
      NPCMPE = SDEVAL(1:19)//'.PCMP'
      NPNCOE = SDEVAL(1:19)//'.PNCO'
      NPNSPE = SDEVAL(1:19)//'.PNSP'
      NNOCPQ = SDCALQ(1:19)//'.NOCP'
      NVALEQ = SDCALQ(1:19)//'.VALE'
      NPADRQ = SDCALQ(1:19)//'.PADR'
      NPNBNQ = SDCALQ(1:19)//'.PNBN'

      CALL JELIRA(SDLIEU(1:19)//'.REFE','DOCU',IBID,DOCUL)
      CALL JELIRA(NVALEE,'DOCU',IBID,DOCU)
      CALL JELIRA(NPADRE,'LONMAX',LPADR,KBID)
      CALL JEVEUO(NVALEE,'L',AVALEE)
      CALL JEVEUO(NPADRE,'L',APADRE)

      K4 = OPTION(1:4)
      K1 = QUANT(1:1)
      IF ((K4.EQ.'SIGM') .OR. (K4.EQ.'EPSI') .OR. (K4.EQ.'SIEF')) THEN
        TQ = 'T3'
      ELSE IF (K4.EQ.'EFGE') THEN
        TQ = 'T2'
      ELSE IF ((K4.EQ.'FLUX') .OR. (K4.EQ.'DEPL') .OR.
     &         (K4.EQ.'FORC')) THEN
        TQ = 'V3'
      ELSE
        TQ = 'AS'
      END IF

      IF (K1.EQ.'I') THEN
        LNE = 6
        IF (TQ.EQ.'T3') THEN
          LNQ = 4
        ELSE
          LNQ = 8
        END IF
        CALL WKVECT(NNOCPQ,'V V K8',4,ANOCPQ)
        ZK8(ANOCPQ+1-1) = 'VMIS'
        ZK8(ANOCPQ+2-1) = 'TRESCA'
        ZK8(ANOCPQ+3-1) = 'TRACE'
        ZK8(ANOCPQ+4-1) = 'DETER'

      ELSE IF (K1.EQ.'E') THEN
        LNE = 6
        IF (TQ.EQ.'T3') THEN
          LNQ = 12
          CALL WKVECT(NNOCPQ,'V V K8',LNQ,ANOCPQ)
          ZK8(ANOCPQ+1-1) = 'PRIN_1'
          ZK8(ANOCPQ+2-1) = 'PRIN_2'
          ZK8(ANOCPQ+3-1) = 'PRIN_3'
          ZK8(ANOCPQ+4-1) = 'VECT_1_X'
          ZK8(ANOCPQ+5-1) = 'VECT_1_Y'
          ZK8(ANOCPQ+6-1) = 'VECT_1_Z'
          ZK8(ANOCPQ+7-1) = 'VECT_2_X'
          ZK8(ANOCPQ+8-1) = 'VECT_2_Y'
          ZK8(ANOCPQ+9-1) = 'VECT_2_Z'
          ZK8(ANOCPQ+10-1) = 'VECT_3_X'
          ZK8(ANOCPQ+11-1) = 'VECT_3_Y'
          ZK8(ANOCPQ+12-1) = 'VECT_3_Z'
        ELSE
          LNQ = 8
          CALL WKVECT(NNOCPQ,'V V K8',LNQ,ANOCPQ)
          ZK8(ANOCPQ+1-1) = 'PRIN_1'
          ZK8(ANOCPQ+2-1) = 'PRIN_2'
          ZK8(ANOCPQ+3-1) = 'VECT_1_X'
          ZK8(ANOCPQ+4-1) = 'VECT_1_Y'
          ZK8(ANOCPQ+5-1) = 'PRIN_1'
          ZK8(ANOCPQ+6-1) = 'PRIN_2'
          ZK8(ANOCPQ+7-1) = 'VECT_2_X'
          ZK8(ANOCPQ+8-1) = 'VECT_2_Y'
        END IF

      ELSE IF (QUANT(1:7).EQ.'TRACE_D') THEN
        IF (TQ.EQ.'T3') THEN
          LNQ = 3
          CALL WKVECT(NNOCPQ,'V V K8',3,ANOCPQ)
          ZK8(ANOCPQ+1-1) = 'TR_DIR_1'
          ZK8(ANOCPQ+2-1) = 'TR_DIR_2'
          ZK8(ANOCPQ+3-1) = 'TR_DIR_3'
          IF (CODIR.LE.3) THEN
            LNE = 3
          ELSE IF (CODIR.LE.6) THEN
            LNE = 5
          ELSE
            LNE = 6
          END IF
        ELSE IF (TQ.EQ.'T2') THEN
          LNQ = 4
          CALL WKVECT(NNOCPQ,'V V K8',4,ANOCPQ)
          ZK8(ANOCPQ+1-1) = 'TR_DIR_1'
          ZK8(ANOCPQ+2-1) = 'TR_DIR_2'
          ZK8(ANOCPQ+3-1) = 'TR_DIR_1'
          ZK8(ANOCPQ+4-1) = 'TR_DIR_2'
          IF (CODIR.LE.3) THEN
            LNE = 4
          ELSE
            LNE = 6
          END IF
        ELSE
          LNQ = 1
          CALL WKVECT(NNOCPQ,'V V K8',1,ANOCPQ)
          ZK8(ANOCPQ+1-1) = 'TRAC_DIR'
          IF (CODIR.LE.3) THEN
            LNE = 1
          ELSE IF (CODIR.LE.6) THEN
            LNE = 2
          ELSE
            LNE = 3
          END IF
        END IF

      ELSE IF (QUANT(1:7).EQ.'TRACE_N') THEN
        IF (TQ.EQ.'T3') THEN
          LNE = 5
          LNQ = 3
          CALL WKVECT(NNOCPQ,'V V K8',3,ANOCPQ)
          ZK8(ANOCPQ+1-1) = 'TR_NOR_1'
          ZK8(ANOCPQ+2-1) = 'TR_NOR_2'
          ZK8(ANOCPQ+3-1) = 'TR_NOR_3'
        ELSE IF (TQ.EQ.'T2') THEN
          LNE = 6
          LNQ = 4
          CALL WKVECT(NNOCPQ,'V V K8',4,ANOCPQ)
          ZK8(ANOCPQ+1-1) = 'TR_NOR_1'
          ZK8(ANOCPQ+2-1) = 'TR_NOR_2'
          ZK8(ANOCPQ+3-1) = 'TR_NOR_1'
          ZK8(ANOCPQ+4-1) = 'TR_NOR_2'
        ELSE
          LNE = 3
          LNQ = 1
          CALL WKVECT(NNOCPQ,'V V K8',1,ANOCPQ)
          ZK8(ANOCPQ+1-1) = 'TRAC_NOR'
        END IF
      ELSE
        LNE = NBCPNC
        LNQ = NBCPCD
      END IF

      NBPT = 0
      ICOEF = 1
      IF (DOCU.EQ.'CHNO') THEN
        NBPT = LPADR
        LPNBN = LPADR
      ELSE
        CALL JELIRA(NPNBNE,'LONMAX',LPNBN,KBID)
        CALL JEVEUO(NPNBNE,'L',APNBNE)
        DO 10,I = 1,LPNBN,1
          NBPT = NBPT + ZI(APNBNE+I-1)
   10   CONTINUE
        CALL JEVEUO(NPNCOE,'L',APNCOE)
        CALL JEVEUO(NPNSPE,'L',APNSPE)
        ICOEF = ZI(APNCOE+1-1)*ZI(APNSPE+1-1)
      END IF
      CALL WKVECT(NVALEQ,'V V R',LNQ*NBPT*ICOEF,AVALEQ)
      CALL WKVECT(NPADRQ,'V V I',LPADR,APADRQ)
      CALL WKVECT(NPNBNQ,'V V I',LPNBN,APNBNQ)
      CALL WKVECT(SDCALQ(1:19)//'.PNCO','V V I',LPNBN,APNCOQ)
      CALL WKVECT(SDCALQ(1:19)//'.PNSP','V V I',LPNBN,APNSPQ)
      IF (DOCU.EQ.'CHNO') THEN
        CALL WKVECT('&&RVCALQ.CHNO.PNSP','V V I',LPADR,APNSPE)
        CALL WKVECT('&&RVCALQ.CHNO.PNCO','V V I',LPADR,APNCOE)
        CALL WKVECT('&&RVCALQ.CHNO.PNBN','V V I',LPADR,APNBNE)
        DO 20,I = 1,LPADR,1
          ZI(APADRQ+I-1) = 1 + (I-1)*LNQ
          ZI(APNBNQ+I-1) = 1
          ZI(APNCOQ+I-1) = 1
          ZI(APNSPQ+I-1) = 1
          ZI(APNBNE+I-1) = 1
          ZI(APNCOE+I-1) = 1
          ZI(APNSPE+I-1) = 1
   20   CONTINUE
      ELSE
        ZI(APADRQ+1-1) = 1
        DO 30,I = 1,LPNBN - 1,1
          NBCO = ZI(APNCOE+I-1)
          NBSP = ZI(APNSPE+I-1)
          NBND = ZI(APNBNE+I-1)
          ZI(APNBNQ+I-1) = NBND
          ZI(APADRQ+I) = ZI(APADRQ+I-1) + LNQ*NBND*NBSP*NBCO
          ZI(APNCOQ+I-1) = NBCO
          ZI(APNSPQ+I-1) = NBSP
   30   CONTINUE
        ZI(APNBNQ+LPNBN-1) = ZI(APNBNE+LPNBN-1)
        ZI(APNCOQ+LPNBN-1) = ZI(APNCOE+LPNBN-1)
        ZI(APNSPQ+LPNBN-1) = ZI(APNSPE+LPNBN-1)
      END IF

      K1 = QUANT(1:1)
      IF (K1.EQ.'I') THEN
        IF (TQ.EQ.'T3') THEN
          DO 70,IPADR = 1,LPADR,1
            IDECE = ZI(APADRE+IPADR-1)
            IDECQ = ZI(APADRQ+IPADR-1)
            NBSP = ZI(APNSPE+IPADR-1)
            NBCO = ZI(APNCOE+IPADR-1)
            NBND = ZI(APNBNE+IPADR-1)
            TNDE = 6*NBSP
            TNDQ = 4*NBSP
            TCOQ = TNDQ*NBND
            TCOE = TNDE*NBND
            DO 60,ICO = 1,NBCO,1
              ADRE = (ICO-1)*TCOE
              ADRQ = (ICO-1)*TCOQ
              DO 50,IND = 1,NBND,1
                DO 40,ISP = 1,NBSP,1
                  CALL RVINVT(ZR(AVALEE+IDECE-1+ADRE+ (ISP-1)*6),
     &                        ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*4),
     &                        ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*4+1),
     &                        ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*4+2),
     &                        ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*4+3))
   40           CONTINUE
                ADRE = ADRE + TNDE
                ADRQ = ADRQ + TNDQ
   50         CONTINUE
   60       CONTINUE
   70     CONTINUE
        ELSE
          DO 110,IPADR = 1,LPADR,1
            IDECE = ZI(APADRE+IPADR-1)
            IDECQ = ZI(APADRQ+IPADR-1)
            NBSP = ZI(APNSPE+IPADR-1)
            NBCO = ZI(APNCOE+IPADR-1)
            NBND = ZI(APNBNE+IPADR-1)
            TNDE = LNE*NBSP
            TNDQ = LNQ*NBSP
            TCOQ = TNDQ*NBND
            TCOE = TNDE*NBND
            DO 100,ICO = 1,NBCO,1
              ADRE = (ICO-1)*TCOE
              ADRQ = (ICO-1)*TCOQ
              DO 90,IND = 1,ZI(APNBNQ+IPADR-1),1
                DO 80,ISP = 1,NBSP,1
                IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6).EQ.R8VIDE()) THEN
                  A = 0.D0
                ELSE
                  A = ZR(AVALEE+IDECE-1+ADRE+ (ISP-1)*6)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+1).EQ.R8VIDE()) THEN
                  B = 0.D0
                ELSE
                  B = ZR(AVALEE+IDECE-1+ADRE+ (ISP-1)*6+1)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+2).EQ.R8VIDE()) THEN
                  C = 0.D0
                ELSE
                  C = ZR(AVALEE+IDECE-1+ADRE+ (ISP-1)*6+2)
                ENDIF
                  TR = A + B
                  DET = A*B - C*C
                  TX = MAX(ABS(A),ABS(B),ABS(C))
                  IF (TX.EQ.ZERO) THEN
                    VP1 = ZERO
                    VP2 = ZERO
                    VM = ZERO
                  ELSE
                    TX = 0.5D0* (A-B)
                    IF (ABS(TX).GT.ABS(C)) THEN
                      AUX = C/TX
                      AUX = TX*SQRT(1.0D0+AUX*AUX)
                    ELSE
                      AUX = TX/C
                      AUX = C*SQRT(1.0D0+AUX*AUX)
                    END IF
                    VP1 = 0.5D0*TR + AUX
                    VP2 = 0.5D0*TR - AUX
                    A = A - TR/3.0D0
                    B = B - TR/3.0D0
                    C = SQRT(2.0D0)*C
                    IF (ABS(A).GT.MAX(ABS(B),ABS(C))) THEN
                      AUX = ABS(A)
                      TX = B/A
                      TY = C/A
                    ELSE IF (ABS(B).GT.MAX(ABS(A),ABS(C))) THEN
                      AUX = ABS(B)
                      TX = A/B
                      TY = C/B
                    ELSE
                      AUX = ABS(C)
                      TX = A/C
                      TY = B/C
                    END IF
                    VM = SQRT((1.0D0+TX*TX+TY*TY)*3.0D0/2.0D0)
                    VM = AUX*VM
                  END IF
                  C = ABS(VP1-VP2)
                  ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*8) = VM
                  ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*8+1) = C
                  ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*8+2) = TR
                  ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*8+3) = DET
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+3).EQ.R8VIDE()) THEN
                  A = 0.D0
                ELSE
                  A = ZR(AVALEE+IDECE-1+ADRE+ (ISP-1)*6+3)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+4).EQ.R8VIDE()) THEN
                  B = 0.D0
                ELSE
                  B = ZR(AVALEE+IDECE-1+ADRE+ (ISP-1)*6+4)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+5).EQ.R8VIDE()) THEN
                  C = 0.D0
                ELSE
                  C = ZR(AVALEE+IDECE-1+ADRE+ (ISP-1)*6+5)
                ENDIF
                  TR = A + B
                  DET = A*B - C*C
                  TX = MAX(ABS(A),ABS(B),ABS(C))
                  IF (TX.EQ.ZERO) THEN
                    VP1 = ZERO
                    VP2 = ZERO
                    VM = ZERO
                  ELSE
                    TX = 0.5D0* (A-B)
                    IF (ABS(TX).GT.ABS(C)) THEN
                      AUX = C/TX
                      AUX = TX*SQRT(1.0D0+AUX*AUX)
                    ELSE
                      AUX = TX/C
                      AUX = C*SQRT(1.0D0+AUX*AUX)
                    END IF
                    VP1 = 0.5D0*TR + AUX
                    VP2 = 0.5D0*TR - AUX
                    A = A - TR/3.0D0
                    B = B - TR/3.0D0
                    C = SQRT(2.0D0)*C
                    IF (ABS(A).GT.MAX(ABS(B),ABS(C))) THEN
                      AUX = ABS(A)
                      TX = B/A
                      TY = C/A
                    ELSE IF (ABS(B).GT.MAX(ABS(A),ABS(C))) THEN
                      AUX = ABS(B)
                      TX = A/B
                      TY = C/B
                    ELSE
                      AUX = ABS(C)
                      TX = A/C
                      TY = B/C
                    END IF
                    VM = SQRT((1.0D0+TX*TX+TY*TY)*3.0D0/2.0D0)
                    VM = AUX*VM
                  END IF
                  C = ABS(VP1-VP2)
                  ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*8+4) = VM
                  ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*8+5) = C
                  ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*8+6) = TR
                  ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-1)*8+7) = DET
   80           CONTINUE
                ADRE = ADRE + TNDE
                ADRQ = ADRQ + TNDQ
   90         CONTINUE
  100       CONTINUE
  110     CONTINUE
        END IF
      ELSE IF (K1.EQ.'E') THEN
        IF (TQ.EQ.'T3') THEN
          NPERM = 12
          TOL = 1.D-10
          TOLDYN = 1.D-2
          ITYPE = 0
          IORDRE = 0
          DO 150,IPADR = 1,LPADR,1
            IDECE = ZI(APADRE+IPADR-1)
            IDECQ = ZI(APADRQ+IPADR-1)
            NBSP = ZI(APNSPE+IPADR-1)
            NBCO = ZI(APNCOE+IPADR-1)
            NBND = ZI(APNBNE+IPADR-1)
            TNDE = LNE*NBSP
            TNDQ = LNQ*NBSP
            TCOQ = TNDQ*NBND
            TCOE = TNDE*NBND
            DO 140,ICO = 1,NBCO,1
              ADRE = (ICO-1)*TCOE
              ADRQ = (ICO-1)*TCOQ
              DO 130,IND = 1,ZI(APNBNQ+IPADR-1),1
                DO 120,ISP = 1,NBSP,1
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6).EQ.R8VIDE()) THEN
                  AR(1) = 0.D0
                ELSE
                  AR(1) = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+3).EQ.R8VIDE()) THEN
                  AR(2) = 0.D0
                ELSE
                  AR(2) = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+3)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+4).EQ.R8VIDE()) THEN
                  AR(3) = 0.D0
                ELSE
                  AR(3) = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+4)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+1).EQ.R8VIDE()) THEN
                  AR(4) = 0.D0
                ELSE
                  AR(4) = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+1)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+5).EQ.R8VIDE()) THEN
                  AR(5) = 0.D0
                ELSE
                  AR(5) = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+5)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+2).EQ.R8VIDE()) THEN
                  AR(6) = 0.D0
                ELSE
                  AR(6) = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+2)
                ENDIF
                  BR(1) = 1.D0
                  BR(2) = 0.D0
                  BR(3) = 0.D0
                  BR(4) = 1.D0
                  BR(5) = 0.D0
                  BR(6) = 1.D0
                  CALL JACOBI(NVP,NPERM,TOL,TOLDYN,AR,BR,VECPRO,VALPRO,
     &                        JACAUX,NITJAC,ITYPE,IORDRE)
                  INDICE = AVALEQ + IDECQ - 1 + ADRQ + (ISP-1)*LNQ
                  ZR(INDICE) = VALPRO(1)
                  ZR(INDICE+1) = VALPRO(2)
                  ZR(INDICE+2) = VALPRO(3)
                  ZR(INDICE+3) = VECPRO(1,1)
                  ZR(INDICE+4) = VECPRO(2,1)
                  ZR(INDICE+5) = VECPRO(3,1)
                  ZR(INDICE+6) = VECPRO(1,2)
                  ZR(INDICE+7) = VECPRO(2,2)
                  ZR(INDICE+8) = VECPRO(3,2)
                  ZR(INDICE+9) = VECPRO(1,3)
                  ZR(INDICE+10) = VECPRO(2,3)
                  ZR(INDICE+11) = VECPRO(3,3)
  120           CONTINUE
                ADRE = ADRE + TNDE
                ADRQ = ADRQ + TNDQ
  130         CONTINUE
  140       CONTINUE
  150     CONTINUE
        ELSE
          DO 190,IPADR = 1,LPADR,1
            IDECE = ZI(APADRE+IPADR-1)
            IDECQ = ZI(APADRQ+IPADR-1)
            NBSP = ZI(APNSPE+IPADR-1)
            NBCO = ZI(APNCOE+IPADR-1)
            NBND = ZI(APNBNE+IPADR-1)
            TNDE = LNE*NBSP
            TNDQ = LNQ*NBSP
            TCOQ = TNDQ*NBND
            TCOE = TNDE*NBND
            DO 180,ICO = 1,NBCO,1
              ADRE = (ICO-1)*TCOE
              ADRQ = (ICO-1)*TCOQ
              DO 170,IND = 1,ZI(APNBNQ+IPADR-1),1
                DO 160,ISP = 1,NBSP
                IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6).EQ.R8VIDE()) THEN
                  A = 0.D0
                ELSE
                  A = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+1).EQ.R8VIDE()) THEN
                  B = 0.D0
                ELSE
                  B = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+1)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+2).EQ.R8VIDE()) THEN
                  C = 0.D0
                ELSE
                  C = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+2)
                ENDIF
                  TR = MAX(ABS(A),ABS(B),ABS(C))
                  IF (TR.EQ.ZERO) THEN
                    VP1 = ZERO
                    VP2 = ZERO
                  ELSE
                    TR = 0.5D0* (A-B)
                    IF (ABS(TR).GT.ABS(C)) THEN
                      AUX = C/TR
                      AUX = TR*SQRT(1.0D0+AUX*AUX)
                    ELSE
                      AUX = TR/C
                      AUX = C*SQRT(1.0D0+AUX*AUX)
                    END IF
                    VP1 = 0.5D0* (A+B) + AUX
                    VP2 = 0.5D0* (A+B) - AUX
                  END IF
                  INDICE = AVALEQ + IDECQ - 1 + ADRQ + (ISP-1)*LNQ
                  ZR(INDICE) = VP1
                  ZR(INDICE+1) = VP2
                  ZR(INDICE+2) = 1.D0
                  ZR(INDICE+3) = 0.D0
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+3).EQ.R8VIDE()) THEN
                  A = 0.D0
                ELSE
                  A = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+3)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+4).EQ.R8VIDE()) THEN
                  B = 0.D0
                ELSE
                  B = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+4)
                ENDIF
              IF (ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+5).EQ.R8VIDE()) THEN
                  C = 0.D0
                ELSE
                  C = ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*6+5)
                ENDIF
                  TR = MAX(ABS(A),ABS(B),ABS(C))
                  IF (TR.EQ.ZERO) THEN
                    VP1 = ZERO
                    VP2 = ZERO
                  ELSE
                    TR = 0.5D0* (A-B)
                    IF (ABS(TR).GT.ABS(C)) THEN
                      AUX = C/TR
                      AUX = TR*SQRT(1.0D0+AUX*AUX)
                    ELSE
                      AUX = TR/C
                      AUX = C*SQRT(1.0D0+AUX*AUX)
                    END IF
                    VP1 = 0.5D0* (A+B) + AUX
                    VP2 = 0.5D0* (A+B) - AUX
                  END IF
                  ZR(INDICE+4) = VP1
                  ZR(INDICE+5) = VP2
                  ZR(INDICE+6) = 0.D0
                  ZR(INDICE+7) = 1.D0
  160           CONTINUE
                ADRE = ADRE + TNDE
                ADRQ = ADRQ + TNDQ
  170         CONTINUE
  180       CONTINUE
  190     CONTINUE
        END IF
      ELSE IF (QUANT(1:7).EQ.'TRACE_D') THEN
        IF (DOCU.EQ.'CHLM') THEN
          DO 230,IPADR = 1,LPADR,1
            IDECE = ZI(APADRE+IPADR-1)
            IDECQ = ZI(APADRQ+IPADR-1)
            NBSP = ZI(APNSPE+IPADR-1)
            NBCO = ZI(APNCOE+IPADR-1)
            NBND = ZI(APNBNE+IPADR-1)
            TNDE = LNE*NBSP
            TNDQ = LNQ*NBSP
            TCOQ = TNDQ*NBND
            TCOE = TNDE*NBND
            DO 220,ICO = 1,NBCO,1
              ADRE = (ICO-1)*TCOE
              ADRQ = (ICO-1)*TCOQ
              DO 210,IND = 1,ZI(APNBNQ+IPADR-1),1
                DO 200,ISP = 1,NBSP,1
                  CALL RVPSTD(ZR(AVALEE+IDECE-1+ADRE+ (ISP-1)*LNE),TQ,
     &                        CODIR,VALDIR,ZR(AVALEQ+IDECQ-1+ADRQ+ (ISP-
     &                        1)*LNQ))
  200           CONTINUE
                ADRE = ADRE + TNDE
                ADRQ = ADRQ + TNDQ
  210         CONTINUE
  220       CONTINUE
  230     CONTINUE
        ELSE
          DO 240,IPADR = 1,LPADR,1
            IDECE = ZI(APADRE+IPADR-1)
            IDECQ = ZI(APADRQ+IPADR-1)
            NBND = ZI(APNBNE+IPADR-1)
            CALL RVPSTD(ZR(AVALEE+IDECE-1),TQ,CODIR,VALDIR,
     &                  ZR(AVALEQ+IDECQ-1))
  240     CONTINUE
        END IF

      ELSE IF (QUANT(1:7).EQ.'TRACE_N') THEN
        CALL JELIRA(SDLIEU(1:19)//'.ABSC','NMAXOC',NBOC,KBID)
        IF (DOCU.EQ.'CHNO') THEN
          CALL JELIRA(NPADRQ,'LONMAX',L,KBID)
          DO 250,N = 1,L,1
            IDECE = ZI(APADRE+N-1)
            IDECQ = ZI(APADRQ+N-1)
            V2(1) = VEC2(2*(N-1)+1)
            V2(2) = VEC2(2*(N-1)+2)
            CALL RVPSTD(ZR(AVALEE+IDECE-1),TQ,4,V2,ZR(AVALEQ+IDECQ-1))
  250     CONTINUE
        ELSE
          IF (DOCUL.EQ.'LSTN') THEN
            CALL JELIRA(JEXNUM(SDLIEU(1:19)//'.ABSC',1),'LONMAX',L,KBID)
            DO 290,M = 1,L,1
              IDECE = ZI(APADRE+M-1)
              IDECQ = ZI(APADRQ+M-1)
              NBSP = ZI(APNSPE+M-1)
              NBCO = ZI(APNCOE+M-1)
              NBND = ZI(APNBNE+M-1)
              TNDE = LNE*NBSP
              TNDQ = LNQ*NBSP
              TCOQ = TNDQ*NBND
              TCOE = TNDE*NBND
              V2(1) = VEC2(2*(M-1)+1)
              V2(2) = VEC2(2*(M-1)+2)
              DO 280,ICO = 1,NBCO,1
                ADRE = (ICO-1)*TCOE
                ADRQ = (ICO-1)*TCOQ
                DO 270,IND = 1,ZI(APNBNQ+M-1),1
                  DO 260,ISP = 1,NBSP,1
                    CALL RVPSTD(ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*LNE),TQ,
     &                     4,V2,ZR(AVALEQ+IDECQ-1+ADRQ+(ISP-1)*LNQ))
  260             CONTINUE
                  ADRE = ADRE + TNDE
                  ADRQ = ADRQ + TNDQ
  270           CONTINUE
  280         CONTINUE
  290       CONTINUE
          ELSE
            MDER = 0
            DO 330,IOC = 1,NBOC,1
              CALL JELIRA(JEXNUM(SDLIEU(1:19)//'.ABSC',IOC),'LONMAX',L,
     &                    KBID)
              DO 320,M = MDER + 1,MDER + L - 1,1
                N = M + IOC - 1
                IDECE = ZI(APADRE+M-1)
                IDECQ = ZI(APADRQ+M-1)
                NBSP = ZI(APNSPE+M-1)
                NBCO = ZI(APNCOE+M-1)
                NBND = ZI(APNBNE+M-1)
                TNDE = LNE*NBSP
                TNDQ = LNQ*NBSP
                TCOQ = TNDQ*NBND
                TCOE = TNDE*NBND
                V2(1) = VEC2(2*(M-1)+1)
                V2(2) = VEC2(2*(M-1)+2)
                DO 310,ICO = 1,NBCO,1
                  ADRE = (ICO-1)*TCOE
                  ADRQ = (ICO-1)*TCOQ
                  DO 300,ISP = 1,NBSP,1
                    CALL RVPSTD(ZR(AVALEE+IDECE-1+ADRE+(ISP-1)*LNE),TQ,
     &                     4,V2,ZR(AVALEQ+IDECQ-1+ADRQ+(ISP-1)*LNQ))
               CALL RVPSTD(ZR(AVALEE+IDECE-1+ADRE+(ISP-1+NBSP)*LNE),TQ,
     &                4,V2,ZR(AVALEQ+IDECQ-1+ADRQ+(ISP-1+NBSP)*LNQ))
  300             CONTINUE
  310           CONTINUE
  320         CONTINUE
              MDER = MDER + L - 1
  330       CONTINUE
          END IF
        END IF
      ELSE IF ( REPERE(1:1).EQ.'L' .AND. TRIDIM ) THEN
C               -------------------------------
        CALL WKVECT(NNOCPQ,'V V K8',NBCPCD,ANOCPQ)
        CALL JEVEUO(COURBE//'S1   '//'.DESC','L',ASGTU)
        DO 340,I = 1,NBCPCD,1
          ZK8(ANOCPQ+I-1) = NOMCP(I)
  340   CONTINUE
        CALL JELIRA(JEXNUM(SDLIEU(1:19)//'.ABSC',1),'LONMAX',L,KBID)
        CALL JELIRA(NPADRQ,'LONMAX',NBADRQ,KBID)
        CALL JELIRA(SDLIEU(1:19)//'.ABSC','NMAXOC',NBOC,KBID)
        CALL JEVEUO(NPCMPE,'L',APCMPE)
        CALL JEVEUO(SDEVAL(1:19)//'.NUGD','L',IADR)
        CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',ZI(IADR)),'L',ACPGD)
        CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',ZI(IADR)),'LONMAX',NC,KBID)
        CALL WKVECT('&&RVCALQ.NUM.CP.CD','V V I',NBCPCD,ANUMCP)
        CALL NUMEK8(ZK8(ACPGD),NOMCP,NC,NBCPCD,ZI(ANUMCP))
        IOC = 1
        DO 400,I = 1,NBADRQ,1
          ADRE = ZI(APADRE+I-1)
          ADRQ = ZI(APADRQ+I-1)
          NBSP = ZI(APNSPE+I-1)
          NBCO = ZI(APNCOE+I-1)
          NBND = ZI(APNBNE+I-1)
          TNDE = LNE*NBSP
          TNDQ = LNQ*NBSP
          TCOQ = TNDQ*NBND
          TCOE = TNDE*NBND
C         -- VECTEUR COLINEAIRE AU CHEMIN
          V1X = SGTU(4) - SGTU(1)
          V1Y = SGTU(5) - SGTU(2)
          V1Z = SGTU(6) - SGTU(3)
          V1N = SQRT(V1X**2+V1Y**2+V1Z**2)
          V1X = V1X/V1N
          V1Y = V1Y/V1N
          V1Z = V1Z/V1N
C         -- VECTEUR VECT_Y FOURNI
          V2X = VECTY(1)
          V2Y = VECTY(2)
          V2Z = VECTY(3)
C         -- PROJECTION / NORMALISATION
          V2P = V1X*V2X + V1Y*V2Y + V1Z*V2Z
          V2X = V2X - V2P*V1X
          V2Y = V2Y - V2P*V1Y
          V2Z = V2Z - V2P*V1Z
          V2N = SQRT(V2X**2+V2Y**2+V2Z**2)
          V2X = V2X/V2N
          V2Y = V2Y/V2N
          V2Z = V2Z/V2N
C         -- VECTEUR TANGENT
          V3X = V1Y*V2Z - V2Y*V1Z
          V3Y = V1Z*V2X - V2Z*V1X
          V3Z = V1X*V2Y - V2X*V1Y
          IF ( NIV .GE. 2 ) THEN
             WRITE(IFM,1000) IOCC
             WRITE(IFM,1002) 'V1 : ', V1X, V1Y, V1Z
             WRITE(IFM,1002) 'V2 : ', V2X, V2Y, V2Z
             WRITE(IFM,1002) 'V3 : ', V3X, V3Y, V3Z
 1000        FORMAT( 'OCCURRENCE ', I4 )
 1002        FORMAT( 1P, A5, E12.5, 2X, E12.5, 2X, E12.5 )
          ENDIF
          DO 390,ICO = 1,NBCO,1
            DO 380,J = 1,NBND,1
              DO 370,ISP = 1,NBSP,1
                IDECE = (ICO-1)*TCOE + (J-1)*TNDE + (ISP-1)*LNE
                IDECQ = (ICO-1)*TCOQ + (J-1)*TNDQ + (ISP-1)*LNQ
                PT = 1
                IF (TQ.EQ.'V3') THEN
                  DO 350,K = 1,NBCPCD,1
                    NUM = ZI(ANUMCP+K-1)
                    IF (NUM.GE.1 .AND. NUM.LE.3) THEN
                      IDEC = 0
                    ELSE
                      IDEC = 3
                    END IF
                    KD1 = ZI(APCMPE+IDEC+1-1)
                    KD2 = ZI(APCMPE+IDEC+2-1)
                    KD3 = ZI(APCMPE+IDEC+3-1)
                    IF (ZR(AVALEE+ADRE+IDECE+KD1-2).EQ.R8VIDE()) THEN
                      TX = 0.D0
                    ELSE
                      TX = ZR(AVALEE+ADRE+IDECE+KD1-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD2-2).EQ.R8VIDE()) THEN
                      TY = 0.D0
                    ELSE
                      TY = ZR(AVALEE+ADRE+IDECE+KD2-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD3-2).EQ.R8VIDE()) THEN
                      TZ = 0.D0
                    ELSE
                      TZ = ZR(AVALEE+ADRE+IDECE+KD3-2)
                    ENDIF
                    IF (NUM.EQ.1 .OR. NUM.EQ.4) THEN
                      VAL = V1X*TX + V1Y*TY + V1Z*TZ
                    ELSE IF (NUM.EQ.2 .OR. NUM.EQ.5) THEN
                      VAL = V2X*TX + V2Y*TY + V2Z*TZ
                    ELSE IF (NUM.EQ.3 .OR. NUM.EQ.6) THEN
                      VAL = V3X*TX + V3Y*TY + V3Z*TZ
                    END IF
                    ZR(AVALEQ+ADRQ+IDECQ+PT-2) = VAL
                    PT = PT + 1
  350             CONTINUE
                ELSE IF (TQ.EQ.'T3') THEN
                  DO 360,K = 1,NBCPCD,1
                    NUM = ZI(ANUMCP+K-1)
                    KD1 = ZI(APCMPE+1-1)
                    KD2 = ZI(APCMPE+2-1)
                    KD3 = ZI(APCMPE+3-1)
                    KD4 = ZI(APCMPE+4-1)
                    KD5 = ZI(APCMPE+5-1)
                    KD6 = ZI(APCMPE+6-1)
                    IF (ZR(AVALEE+ADRE+IDECE+KD1-2).EQ.R8VIDE()) THEN
                      TXX = 0.D0
                    ELSE
                      TXX = ZR(AVALEE+ADRE+IDECE+KD1-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD2-2).EQ.R8VIDE()) THEN
                      TYY = 0.D0
                    ELSE
                      TYY = ZR(AVALEE+ADRE+IDECE+KD2-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD3-2).EQ.R8VIDE()) THEN
                      TZZ = 0.D0
                    ELSE
                      TZZ = ZR(AVALEE+ADRE+IDECE+KD3-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD4-2).EQ.R8VIDE()) THEN
                      TXY = 0.D0
                    ELSE
                      TXY = ZR(AVALEE+ADRE+IDECE+KD4-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD5-2).EQ.R8VIDE()) THEN
                      TXZ = 0.D0
                    ELSE
                      TXZ = ZR(AVALEE+ADRE+IDECE+KD5-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD6-2).EQ.R8VIDE()) THEN
                      TYZ = 0.D0
                    ELSE
                      TYZ = ZR(AVALEE+ADRE+IDECE+KD6-2)
                    ENDIF
                    IF (NUM.EQ.1) THEN
                      VAL = VTV(V1X,V1Y,V1Z,V1X,V1Y,V1Z)
                    ELSE IF (NUM.EQ.2) THEN
                      VAL = VTV(V2X,V2Y,V2Z,V2X,V2Y,V2Z)
                    ELSE IF (NUM.EQ.3) THEN
                      VAL = VTV(V3X,V3Y,V3Z,V3X,V3Y,V3Z)
                    ELSE IF (NUM.EQ.4) THEN
                      VAL = VTV(V1X,V1Y,V1Z,V2X,V2Y,V2Z)
                    ELSE IF (NUM.EQ.5) THEN
                      VAL = VTV(V1X,V1Y,V1Z,V3X,V3Y,V3Z)
                    ELSE
                      VAL = VTV(V2X,V2Y,V2Z,V3X,V3Y,V3Z)
                    END IF
                    ZR(AVALEQ+ADRQ+IDECQ+PT-2) = VAL
                    PT = PT + 1
  360             CONTINUE
                ELSE IF (TQ.EQ.'AS') THEN
                  CALL JELIRA(NVALEE,'LONMAX',L,KBID)
                  DO 362,IL = 1,L,1
                     ZR(AVALEQ+IL-1) = ZR(AVALEE+IL-1)
  362             CONTINUE
                ELSE
                  CALL U2MESK('F','POSTRELE_14',1,K4)
                END IF
  370         CONTINUE
  380       CONTINUE
  390     CONTINUE
  400   CONTINUE
        CALL JEDETR('&&RVCALQ.NUM.CP.CD')

      ELSE
        CALL WKVECT(NNOCPQ,'V V K8',NBCPCD,ANOCPQ)
        DO 410,I = 1,NBCPCD,1
          ZK8(ANOCPQ+I-1) = NOMCP(I)
  410   CONTINUE
        IF (REPERE(1:1).EQ.'G') THEN
          CALL JELIRA(NVALEE,'LONMAX',L,KBID)
          DO 420,I = 1,L,1
            ZR(AVALEQ+I-1) = ZR(AVALEE+I-1)
  420     CONTINUE
        ELSE
          CALL JELIRA(JEXNUM(SDLIEU(1:19)//'.ABSC',1),'LONMAX',L,KBID)

          CALL JELIRA(NPADRQ,'LONMAX',NBADRQ,KBID)
          CALL JELIRA(SDLIEU(1:19)//'.ABSC','NMAXOC',NBOC,KBID)
          CALL JEVEUO(NPCMPE,'L',APCMPE)
          CALL JEVEUO(SDEVAL(1:19)//'.NUGD','L',IADR)
          CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',ZI(IADR)),'L',ACPGD)
          CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',ZI(IADR)),'LONMAX',NC,
     &                KBID)
          CALL WKVECT('&&RVCALQ.NUM.CP.CD','V V I',NBCPCD,ANUMCP)
          CALL NUMEK8(ZK8(ACPGD),NOMCP,NC,NBCPCD,ZI(ANUMCP))
          IOC = 1
          DO 500,I = 1,NBADRQ,1
            ADRE = ZI(APADRE+I-1)
            ADRQ = ZI(APADRQ+I-1)
            NBSP = ZI(APNSPE+I-1)
            NBCO = ZI(APNCOE+I-1)
            NBND = ZI(APNBNE+I-1)
            TNDE = LNE*NBSP
            TNDQ = LNQ*NBSP
            TCOQ = TNDQ*NBND
            TCOE = TNDE*NBND
            CALL WKVECT('&&RVCALQ.V1X','V V R',NBND,AV1X)
            CALL WKVECT('&&RVCALQ.V1Y','V V R',NBND,AV1Y)
            CALL WKVECT('&&RVCALQ.V2X','V V R',NBND,AV2X)
            CALL WKVECT('&&RVCALQ.V2Y','V V R',NBND,AV2Y)
            IF ((DOCU.EQ.'CHNO') .OR. (DOCUL.EQ.'LSTN')) THEN
              IF (REPERE.EQ.'LOCAL') THEN
                V1X = VEC1(2* (I-1)+1)
                V1Y = VEC1(2* (I-1)+2)
                V2X = VEC2(2* (I-1)+1)
                V2Y = VEC2(2* (I-1)+2)
              ELSE IF (REPERE.EQ.'POLAIRE') THEN
                V1X = VEC1(2* (I-1)+1)
                V1Y = VEC1(2* (I-1)+2)
                V2X = -VEC2(2* (I-1)+1)
                V2Y = -VEC2(2* (I-1)+2)
              ELSE
                V1X = VEC2(2* (I-1)+1)
                V1Y = VEC2(2* (I-1)+2)
                V2X = VEC1(2* (I-1)+1)
                V2Y = VEC1(2* (I-1)+2)
              END IF
              DO 430,J = 1,NBND,1
                ZR(AV1X+J-1) = V1X
                ZR(AV1Y+J-1) = V1Y
                ZR(AV2X+J-1) = V2X
                ZR(AV2Y+J-1) = V2Y
  430         CONTINUE
            ELSE
              IF (I.LT.L) THEN
                N = I + IOC - 1
              ELSE
                N = N + 2
                IOC = IOC + 1
                IF (IOC.LE.NBOC) THEN
                  CALL JELIRA(JEXNUM(SDLIEU(1:19)//'.ABSC',IOC),
     &                        'LONMAX',LL,KBID)
                  L = L + LL - 1
                END IF
              END IF
              IF (REPERE.EQ.'LOCAL') THEN
                ZR(AV1X+1-1) = VEC1(2* (N-1)+1)
                ZR(AV1Y+1-1) = VEC1(2* (N-1)+2)
                ZR(AV2X+1-1) = VEC2(2* (N-1)+1)
                ZR(AV2Y+1-1) = VEC2(2* (N-1)+2)
                ZR(AV1X+2-1) = VEC1(2*N+1)
                ZR(AV1Y+2-1) = VEC1(2*N+2)
                ZR(AV2X+2-1) = VEC2(2*N+1)
                ZR(AV2Y+2-1) = VEC2(2*N+2)
              ELSE
                ZR(AV1X+1-1) = VEC1(2* (N-1)+1)
                ZR(AV1Y+1-1) = VEC1(2* (N-1)+2)
                ZR(AV2X+1-1) = -VEC2(2* (N-1)+1)
                ZR(AV2Y+1-1) = -VEC2(2* (N-1)+2)
                ZR(AV1X+2-1) = VEC1(2*N+1)
                ZR(AV1Y+2-1) = VEC1(2*N+2)
                ZR(AV2X+2-1) = -VEC2(2*N+1)
                ZR(AV2Y+2-1) = -VEC2(2*N+2)
              END IF
            END IF
            DO 490,ICO = 1,NBCO,1
              DO 480,J = 1,NBND,1
                V1X = ZR(AV1X+J-1)
                V1Y = ZR(AV1Y+J-1)
                V2X = ZR(AV2X+J-1)
                V2Y = ZR(AV2Y+J-1)
                DO 470,ISP = 1,NBSP,1
                  IDECE = (ICO-1)*TCOE + (J-1)*TNDE + (ISP-1)*LNE
                  IDECQ = (ICO-1)*TCOQ + (J-1)*TNDQ + (ISP-1)*LNQ
                  PT = 1
                  IF (TQ.EQ.'V3') THEN
                    DO 440,K = 1,NBCPCD,1
                      NUM = ZI(ANUMCP+K-1)
                      IF (NUM.EQ.1) THEN
                        KD1 = ZI(APCMPE+1-1)
                        KD2 = ZI(APCMPE+2-1)
                    IF (ZR(AVALEE+ADRE+IDECE+KD1-2).EQ.R8VIDE()) THEN
                        TX = 0.D0
                    ELSE
                        TX = ZR(AVALEE+ADRE+IDECE+KD1-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD2-2).EQ.R8VIDE()) THEN
                        TY = 0.D0
                    ELSE
                        TY = ZR(AVALEE+ADRE+IDECE+KD2-2)
                    ENDIF
                        VAL = V1X*TX + V1Y*TY
                      ELSE IF (NUM.EQ.2) THEN
                        KD1 = ZI(APCMPE+1-1)
                        KD2 = ZI(APCMPE+2-1)
                    IF (ZR(AVALEE+ADRE+IDECE+KD1-2).EQ.R8VIDE()) THEN
                        TX = 0.D0
                    ELSE
                        TX = ZR(AVALEE+ADRE+IDECE+KD1-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD2-2).EQ.R8VIDE()) THEN
                        TY = 0.D0
                    ELSE
                        TY = ZR(AVALEE+ADRE+IDECE+KD2-2)
                    ENDIF
                        VAL = V2X*TX + V2Y*TY
                      ELSE IF (NUM.EQ.3) THEN
                        KD1 = ZI(APCMPE+3-1)
                        VAL = ZR(AVALEE+ADRE+IDECE+KD1-2)
                      ELSE IF (NUM.EQ.4) THEN
                        KD1 = ZI(APCMPE+4-1)
                        KD2 = ZI(APCMPE+5-1)
                    IF (ZR(AVALEE+ADRE+IDECE+KD1-2).EQ.R8VIDE()) THEN
                        TX = 0.D0
                    ELSE
                        TX = ZR(AVALEE+ADRE+IDECE+KD1-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD2-2).EQ.R8VIDE()) THEN
                        TY = 0.D0
                    ELSE
                        TY = ZR(AVALEE+ADRE+IDECE+KD2-2)
                    ENDIF
                        VAL = V1X*TX + V1Y*TY
                      ELSE IF (NUM.EQ.5) THEN
                        KD1 = ZI(APCMPE+4-1)
                        KD2 = ZI(APCMPE+5-1)
                    IF (ZR(AVALEE+ADRE+IDECE+KD1-2).EQ.R8VIDE()) THEN
                        TX = 0.D0
                    ELSE
                        TX = ZR(AVALEE+ADRE+IDECE+KD1-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD2-2).EQ.R8VIDE()) THEN
                        TY = 0.D0
                    ELSE
                        TY = ZR(AVALEE+ADRE+IDECE+KD2-2)
                    ENDIF
                        VAL = V2X*TX + V2Y*TY
                      ELSE
                        KD1 = ZI(APCMPE+6-1)
                        VAL = ZR(AVALEE+ADRE+IDECE+KD1-2)
                      END IF
                      ZR(AVALEQ+ADRQ+IDECQ+PT-2) = VAL
                      PT = PT + 1
  440               CONTINUE
                  ELSE IF (TQ.EQ.'T3') THEN
                    DO 450,K = 1,NBCPCD,1
                      NUM = ZI(ANUMCP+K-1)
                      IF (NUM.EQ.1) THEN
                        KD1 = ZI(APCMPE+1-1)
                        KD2 = ZI(APCMPE+2-1)
                        KD3 = ZI(APCMPE+4-1)
                    IF (ZR(AVALEE+ADRE+IDECE+KD1-2).EQ.R8VIDE()) THEN
                        TXX = 0.D0
                    ELSE
                        TXX = ZR(AVALEE+ADRE+IDECE+KD1-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD2-2).EQ.R8VIDE()) THEN
                        TYY = 0.D0
                    ELSE
                        TYY = ZR(AVALEE+ADRE+IDECE+KD2-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD3-2).EQ.R8VIDE()) THEN
                        TXY = 0.D0
                    ELSE
                        TXY = ZR(AVALEE+ADRE+IDECE+KD3-2)
                    ENDIF
                        VAL = V1X* (V1X*TXX+V1Y*TXY) +
     &                        V1Y* (V1X*TXY+V1Y*TYY)
                      ELSE IF (NUM.EQ.2) THEN
                        KD1 = ZI(APCMPE+1-1)
                        KD2 = ZI(APCMPE+2-1)
                        KD3 = ZI(APCMPE+4-1)
                    IF (ZR(AVALEE+ADRE+IDECE+KD1-2).EQ.R8VIDE()) THEN
                        TXX = 0.D0
                    ELSE
                        TXX = ZR(AVALEE+ADRE+IDECE+KD1-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD2-2).EQ.R8VIDE()) THEN
                        TYY = 0.D0
                    ELSE
                        TYY = ZR(AVALEE+ADRE+IDECE+KD2-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD3-2).EQ.R8VIDE()) THEN
                        TXY = 0.D0
                    ELSE
                        TXY = ZR(AVALEE+ADRE+IDECE+KD3-2)
                    ENDIF
                        VAL = V2X* (V2X*TXX+V2Y*TXY) +
     &                        V2Y* (V2X*TXY+V2Y*TYY)
                      ELSE IF (NUM.EQ.3) THEN
                        KD1 = ZI(APCMPE+3-1)
                        VAL = ZR(AVALEE+ADRE+IDECE+KD1-2)
                      ELSE IF (NUM.EQ.4) THEN
                        KD1 = ZI(APCMPE+1-1)
                        KD2 = ZI(APCMPE+2-1)
                        KD3 = ZI(APCMPE+4-1)
                    IF (ZR(AVALEE+ADRE+IDECE+KD1-2).EQ.R8VIDE()) THEN
                        TXX = 0.D0
                    ELSE
                        TXX = ZR(AVALEE+ADRE+IDECE+KD1-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD2-2).EQ.R8VIDE()) THEN
                        TYY = 0.D0
                    ELSE
                        TYY = ZR(AVALEE+ADRE+IDECE+KD2-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD3-2).EQ.R8VIDE()) THEN
                        TXY = 0.D0
                    ELSE
                        TXY = ZR(AVALEE+ADRE+IDECE+KD3-2)
                    ENDIF
                        VAL = V1X* (V2X*TXX+V2Y*TXY) +
     &                        V1Y* (V2X*TXY+V2Y*TYY)
                      ELSE IF (NUM.EQ.5) THEN
                        KD1 = ZI(APCMPE+5-1)
                        KD2 = ZI(APCMPE+6-1)
                    IF (ZR(AVALEE+ADRE+IDECE+KD1-2).EQ.R8VIDE()) THEN
                        TXZ = 0.D0
                    ELSE
                        TXZ = ZR(AVALEE+ADRE+IDECE+KD1-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD2-2).EQ.R8VIDE()) THEN
                        TYZ = 0.D0
                    ELSE
                        TYZ = ZR(AVALEE+ADRE+IDECE+KD2-2)
                    ENDIF
                        VAL = V1X*TXZ + V1Y*TYZ
                      ELSE
                        KD1 = ZI(APCMPE+5-1)
                        KD2 = ZI(APCMPE+6-1)
                    IF (ZR(AVALEE+ADRE+IDECE+KD1-2).EQ.R8VIDE()) THEN
                        TXZ = 0.D0
                    ELSE
                        TXZ = ZR(AVALEE+ADRE+IDECE+KD1-2)
                    ENDIF
                    IF (ZR(AVALEE+ADRE+IDECE+KD2-2).EQ.R8VIDE()) THEN
                        TYZ = 0.D0
                    ELSE
                        TYZ = ZR(AVALEE+ADRE+IDECE+KD2-2)
                    ENDIF
                        VAL = V2X*TXZ + V2Y*TYZ
                      END IF
                      ZR(AVALEQ+ADRQ+IDECQ+PT-2) = VAL
                      PT = PT + 1
  450               CONTINUE
                  ELSE
                    KD1 = ZI(APCMPE+7-1)
                    KD2 = ZI(APCMPE+8-1)
                    KD3 = ZI(APCMPE+9-1)
            IF (ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD1-2).EQ.R8VIDE()) THEN
                      TXX = 0.D0
                    ELSE
                      TXX = ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD1-2)
                    ENDIF
            IF (ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD2-2).EQ.R8VIDE()) THEN
                      TYY = 0.D0
                    ELSE
                      TYY = ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD2-2)
                    ENDIF
            IF (ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD3-2).EQ.R8VIDE()) THEN
                      TXY = 0.D0
                    ELSE
                      TXY = ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD3-2)
                    ENDIF
                    KD1 = ZI(APCMPE+10-1)
                    KD2 = ZI(APCMPE+11-1)
                    KD3 = ZI(APCMPE+12-1)
            IF (ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD1-2).EQ.R8VIDE()) THEN
                      SXX = 0.D0
                    ELSE
                      SXX = ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD1-2)
                    ENDIF
            IF (ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD2-2).EQ.R8VIDE()) THEN
                      SYY = 0.D0
                    ELSE
                      SYY = ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD2-2)
                    ENDIF
            IF (ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD3-2).EQ.R8VIDE()) THEN
                      SXY = 0.D0
                    ELSE
                      SXY = ZR(AVALEE+ADRE+IDECE+(J-1)*LNE+KD3-2)
                    ENDIF
                    DO 460,K = 1,NBCPCD,1
                      NUM = ZI(ANUMCP+K-1)
                      IF (NUM.EQ.7) THEN
                        VAL = V1X* (V1X*TXX+V1Y*TXY) +
     &                        V1X* (V1X*TXY+V1Y*TYY)
                      ELSE IF (NUM.EQ.8) THEN
                        VAL = V2X* (V2X*TXX+V2Y*TXY) +
     &                        V2X* (V2X*TXY+V2Y*TYY)
                      ELSE IF (NUM.EQ.9) THEN
                        VAL = V1X* (V2X*TXX+V2Y*TXY) +
     &                        V1X* (V2X*TXY+V2Y*TYY)
                      ELSE IF (NUM.EQ.10) THEN
                        VAL = V1X* (V1X*SXX+V1Y*SXY) +
     &                        V1X* (V1X*SXY+V1Y*SYY)
                      ELSE IF (NUM.EQ.11) THEN
                        VAL = V2X* (V2X*SXX+V2Y*SXY) +
     &                        V2X* (V2X*SXY+V2Y*SYY)
                      ELSE
                        VAL = V1X* (V2X*SXX+V2Y*SXY) +
     &                        V1X* (V2X*SXY+V2Y*SYY)
                      END IF
                      ZR(AVALEQ+ADRQ+IDECQ+PT-2) = VAL
                      PT = PT + 1
  460               CONTINUE
                  END IF
  470           CONTINUE
  480         CONTINUE
  490       CONTINUE
            CALL JEDETR('&&RVCALQ.V1X')
            CALL JEDETR('&&RVCALQ.V1Y')
            CALL JEDETR('&&RVCALQ.V2X')
            CALL JEDETR('&&RVCALQ.V2Y')
  500     CONTINUE
          CALL JEDETR('&&RVCALQ.NUM.CP.CD')
        END IF
      END IF
      CALL JEECRA(NVALEQ,'DOCU',I,DOCU)
      IF (DOCU.EQ.'CHNO') THEN
        CALL JEDETR('&&RVCALQ.CHNO.PNSP')
        CALL JEDETR('&&RVCALQ.CHNO.PNCO')
        CALL JEDETR('&&RVCALQ.CHNO.PNBN')
      END IF
      CALL JEDEMA()
      END
