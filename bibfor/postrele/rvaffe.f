      SUBROUTINE RVAFFE ( MCF,IOCC,SDLIEU,SDEVAL,SDMAIL,TYPAFF,QUANT,
     +                    OPTION,REP,NOMTAB,XNOVAR,NCHEFF,I1,ISD )
      IMPLICIT   NONE
      INTEGER             IOCC, ISD
      CHARACTER*1         TYPAFF
      CHARACTER*16        NCHEFF
      CHARACTER*19        SDEVAL, NOMTAB
      CHARACTER*24        SDLIEU, SDMAIL, XNOVAR
      CHARACTER*(*)       MCF, REP, OPTION, QUANT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 02/10/2007   AUTEUR VIVAN L.VIVAN 
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
C     ------------------------------------------------------------------
C     AFFICHAGE EXTRACTION
C     ------------------------------------------------------------------
C IN  SDLIEU : K : SD DU LIEU TRAITEE
C IN  SDEVAL : K : SD DE L' EVALUATION DE LA QUANTITE SUR CE LIEU
C IN  SDMAIL : K : SD DES NOMS  MAILLES ACTIVES PAR NOEUD (CAS 'LSTN')
C IN  TYPAFF : K : 'N' --> PAR NOEUD, 'E' --> PAR ELEM
C IN  QUANT  : K : NOM DE LA QUANTITE TRAITEE
C IN  OPTION : K : NOM DE L' OPTION   TRAITEE
C     ------------------------------------------------------------------
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      AVALE,APNBN,APADR,ANOCP,NBCP,IOC,AABSC,NBPT,NBOC
      INTEGER      APNSP,APNCO,ACOOR,NBCO,NBSP,APNCA,APNSA, I1
      INTEGER      I,DEB,FIN,ADR1,IFM,ANOMND,J,K,ADRI,DECI,NBNI,AVAUX
      INTEGER      L, LCI, LN, LNI, NBSI, NIV
      INTEGER      LLL, INDIC, INDI1, INDI2
      REAL*8       XA, XB, YA, YB, ZA, ZB, AX, S1, S2, R8VIDE
      CHARACTER*1  K1BID
      CHARACTER*4  DOCUL,DOCU
      CHARACTER*16 OPER
      CHARACTER*24 NVALE, NPNBN, NPADR, NABSC, NNOCP
      CHARACTER*24 NPNCA, NPNSA, NVAUX, NPNCO, NPNSP
C======================================================================
      CALL JEMARQ()
      CALL INFNIV ( IFM, NIV )
      OPER = 'EXTRACTION'
      IF (NIV.GT.1) CALL RVINFA(IFM,MCF,IOCC,QUANT,OPTION,OPER,REP(1:1))
      NVALE = SDEVAL//'.VALE'
      NPNBN = SDEVAL//'.PNBN'
      NNOCP = SDEVAL//'.NOCP'
      NPADR = SDEVAL//'.PADR'
      NPNCO = SDEVAL//'.PNCO'
      NPNSP = SDEVAL//'.PNSP'
      NVAUX = '&&RVAFFE.VECT.INTER.R'
      NPNCA = '&&RVAFFE.VECT.INTER.C'
      NPNSA = '&&RVAFFE.VECT.INTER.S'
      NABSC = SDLIEU(1:19)//'.ABSC'
      CALL JELIRA(SDLIEU(1:19)//'.REFE','DOCU',I,DOCUL)
      CALL JEVEUO(SDLIEU(1:19)//'.DESC','L',ANOMND)
      CALL JELIRA(NVALE,'DOCU',I,DOCU)
      CALL JELIRA(NABSC,'NMAXOC',NBOC,K1BID)
      CALL JELIRA(NNOCP,'LONMAX',NBCP,K1BID)
      CALL JEVEUO(NVALE,'L',AVALE)
      CALL JEVEUO(NPADR,'L',APADR)
      CALL JEVEUO(NPNBN,'L',APNBN)
      CALL JEVEUO(NPNSP,'L',APNSP)
      CALL JEVEUO(NPNCO,'L',APNCO)
      CALL JEVEUO(NNOCP,'L',ANOCP)
      FIN = 0
      DO 100, IOC = 1, NBOC, 1
         CALL JELIRA(JEXNUM(NABSC ,IOC),'LONMAX',NBPT,K1BID)
         CALL JEVEUO(JEXNUM(NABSC ,IOC),'L',AABSC)
         CALL JEVEUO(JEXNUM(SDLIEU(1:19)//'.COOR' ,IOC),'L',ACOOR)
         S1 = ZR(AABSC + 1-1)
         S2 = ZR(AABSC + NBPT-1)
         IF (NIV.GT.1) THEN
            IF ( (DOCUL .EQ. 'LSTN') .OR. (DOCUL .EQ. 'CHMM') )THEN
               WRITE(IFM,*)'CHEMIN DE NOEUDS'
            ELSEIF ( DOCUL .EQ. 'SGTD' ) THEN
               XA = ZR(ANOMND + 1-1)
               YA = ZR(ANOMND + 2-1)
               XB = ZR(ANOMND + 3-1)
               YB = ZR(ANOMND + 4-1)
               AX = SQRT((XA-XB)*(XA-XB) + (YA-YB)*(YA-YB))
               WRITE(IFM,*)'SEGMENT DE DROITE'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' ORIGINE               : (',XA,',',YA,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' EXTREMITE             : (',XB,',',YB,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' ABSCISSES CURVILIGNES : (',S1,',',S2,')'
            ELSEIF ( DOCUL .EQ. 'ARCC' ) THEN
               XA = ZR(ANOMND + 1-1)
               YA = ZR(ANOMND + 2-1)
               XB = ZR(ANOMND + 3-1)
               AX = 57.29577951D0/XB
               WRITE(IFM,*)'ARC DE CERCLE'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' CENTRE                : (',XA,',',YA,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' RAYON                 :  ',XB
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                 ' SECTEUR ANGULAIRE     : (',AX*S1,',',AX*S2,')'
            ELSEIF ( DOCUL .EQ. 'SGT3' ) THEN
               XA = ZR(ANOMND + 1-1)
               YA = ZR(ANOMND + 2-1)
               ZA = ZR(ANOMND + 3-1)
               XB = ZR(ANOMND + 4-1)
               YB = ZR(ANOMND + 5-1)
               ZB = ZR(ANOMND + 6-1)
             AX = SQRT((XA-XB)*(XA-XB)+(YA-YB)*(YA-YB)+(ZA-ZB)*(ZA-ZB))
               WRITE(IFM,*)'SEGMENT DE DROITE'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1,1PD14.6,A1)')
     +                ' ORIGINE               : (',XA,',',YA,',',ZA,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1,1PD14.6,A1)')
     +                ' EXTREMITE             : (',XB,',',YB,',',ZB,')'
               WRITE(IFM,'(A26,1PD14.6,A1,1PD14.6,A1)')
     +                   ' ABSCISSES CURVILIGNES : (',S1,',',S2,')'
            ELSE
            ENDIF
         ENDIF
         DEB   = FIN + 1
         FIN   = DEB + NBPT
         IF ( DOCU .EQ. 'CHLM')  THEN
            FIN = FIN - 2
         ELSE IF ( DOCU .EQ. 'CHNO') THEN
            FIN = FIN - 1
         ENDIF
         ADR1 = ZI(APADR + DEB-1)
         NBCO = ZI(APNCO + DEB-1)
         NBSP = ZI(APNSP + DEB-1)
         IF ( DOCU .EQ. 'CHNO' ) THEN
            CALL RVTECN (ZR(AVALE + ADR1-1),ZR(AABSC),ZI(APNCO + DEB-1),
     +                   ZI(APNSP + DEB-1), ZR(ACOOR), ZK8(ANOCP),
     +                   ZK8(ANOMND), NBCP, NBPT, DOCUL, NOMTAB,
     +                   IOCC, XNOVAR, NCHEFF, I1, IOC, ISD )
         ELSE
            IF ( TYPAFF .EQ. 'E' ) THEN
               CALL RVTEC0 ( ZR(AVALE + ADR1-1), ZI(APNCO+DEB-1),
     +                       ZI(APNSP+DEB-1), ZR(AABSC), ZR(ACOOR),
     +                       ZK8(ANOCP), ZK8(ANOMND), SDMAIL, NBPT,
     +                       DOCUL, NBCP, ZI(APADR), NOMTAB,
     +                       IOC, IOCC, XNOVAR, NCHEFF, I1, ISD )
            ELSE
               CALL WKVECT(NVAUX,'V V R',NBCO*NBSP*NBPT*NBCP,AVAUX)
               CALL WKVECT(NPNCA,'V V I',NBPT,APNCA)
               CALL WKVECT(NPNSA,'V V I',NBPT,APNSA)
               DO 105, I = 1, NBPT-1, 1
                  ZI(APNCA + I-1) = ZI(APNCO + I-1)
                  ZI(APNSA + I-1) = ZI(APNSP + I-1)
105            CONTINUE
               IF ( DOCUL .EQ. 'LSTN' ) THEN
                  ZI(APNCA + NBPT-1) = ZI(APNCO + NBPT-1)
                  ZI(APNSA + NBPT-1) = ZI(APNSP + NBPT-1)
               ELSE
                  ZI(APNCA + NBPT-1) = ZI(APNCO + NBPT-2)
                  ZI(APNSA + NBPT-1) = ZI(APNSP + NBPT-2)
               ENDIF
               LN = NBCP*NBSP
               IF ( DOCUL .EQ. 'LSTN' ) THEN
                  DO 110, I = 1, NBPT, 1
                     ADRI = ZI(APADR + I-1)
                     NBNI = ZI(APNBN + I-1)
                     NBSI = ZI(APNSP + I-1)
                     DECI = LN*NBCO*(I-1)
                     LNI  = NBCP*NBSI
                     LCI  = LNI*NBNI
                     DO 120, J = 1, NBCO, 1
                        DO 130, K = 1, NBSI*NBCP, 1
                           AX = 0.0D0
                           LLL = 0
                           DO 140, L = 1, NBNI, 1
                              INDIC = ADRI-1+(J-1)*LCI+(L-1)*LNI+K-1
                              IF(ZR(AVALE+INDIC).EQ.R8VIDE()) GOTO 140
                              LLL = LLL + 1
                              AX = AX + ZR(AVALE+INDIC)
140                       CONTINUE
                          IF ( LLL .EQ. 0 ) THEN
                             ZR(AVAUX+DECI+(J-1)*LN + K-1) = 0.D0
                          ELSE
                             ZR(AVAUX+DECI+(J-1)*LN + K-1) = AX/LLL
                          ENDIF
130                     CONTINUE
120                  CONTINUE
110               CONTINUE
               ELSE
                  DO 150, I = 1, NBPT, 1
                     IF ( I .EQ. 1 ) THEN
                        ADRI = ZI(APADR+DEB-1)
                        DO 160, K = 1, NBCO, 1
                           DO 161, J = 1, LN, 1
                              INDIC = ADRI - 1 + 2*LN*(K-1) + J-1
                              IF(ZR(AVALE+INDIC).EQ.R8VIDE()) THEN
                                ZR(AVAUX+LN*(K-1)+J-1) = 0.D0
                              ELSE
                                ZR(AVAUX+LN*(K-1)+J-1) = ZR(AVALE+INDIC)
                              ENDIF
161                        CONTINUE
160                     CONTINUE
                     ELSE IF ( I .EQ. NBPT ) THEN
                        ADRI = ZI(APADR +DEB-1+ NBPT-2) + LN
                       DO 170, K = 1, NBCO, 1
                           DO 171, J = 1, LN, 1
                              INDIC = ADRI - 1 + 2*LN*(K-1) + J-1
                              IF(ZR(AVALE+INDIC).EQ.R8VIDE()) THEN
                            ZR(AVAUX+((NBPT-1)*NBCO+K-1)*LN+J-1) = 0.D0
                              ELSE
                                ZR(AVAUX+((NBPT-1)*NBCO+K-1)*LN+J-1) =
     +                                                 ZR(AVALE+INDIC)
                              ENDIF
171                        CONTINUE
170                     CONTINUE
                     ELSE
                        ADRI = ZI(APADR +DEB-1+ I-2)
                        DO 180, K = 1, NBCO, 1
                           DO 181, J = 1, LN, 1
                              INDI1 = ADRI-1+LN*(2*K-1)+J-1
                              INDI2 = ADRI-1+LN*2*(K-1+NBCO)+J-1
                              IF(ZR(AVALE+INDI1).EQ.R8VIDE() .AND.
     +                           ZR(AVALE+INDI2).EQ.R8VIDE()) THEN
                                ZR(AVAUX+LN*(NBCO*(I-1)+K-1)+J-1) = 0.D0
                              ELSEIF(ZR(AVALE+INDI1).EQ.R8VIDE()) THEN
                                ZR(AVAUX+LN*(NBCO*(I-1)+K-1)+J-1) = 
     +                                                   ZR(AVALE+INDI2)
                              ELSEIF(ZR(AVALE+INDI2).EQ.R8VIDE()) THEN
                                ZR(AVAUX+LN*(NBCO*(I-1)+K-1)+J-1) = 
     +                                                   ZR(AVALE+INDI1)
                              ELSE
                                ZR(AVAUX+LN*(NBCO*(I-1)+K-1)+J-1) =
     +                           0.5D0*(ZR(AVALE+INDI1)+ZR(AVALE+INDI2))
                              ENDIF
181                        CONTINUE
180                     CONTINUE
                     ENDIF
150               CONTINUE
               ENDIF
               CALL RVTECN (ZR(AVAUX),ZR(AABSC),ZI(APNCA),ZI(APNSA),
     +                     ZR(ACOOR),ZK8(ANOCP),ZK8(ANOMND),NBCP,NBPT,
     +                     DOCUL,NOMTAB,IOCC,XNOVAR,NCHEFF,I1,IOC,ISD)
               CALL JEDETR(NVAUX)
               CALL JEDETR(NPNCA)
               CALL JEDETR(NPNSA)
            ENDIF
         ENDIF
100   CONTINUE
      CALL JEDEMA()
      END
