      SUBROUTINE COPLAS( TEMPA, K1A, K1B, MATREV, LREV, 
     &                   DEKLAG, PRODEF, ORIDEF,
     &                   KAL, KBL, DKMA, DKMB, K1ACP, K1BCP )
C
      IMPLICIT      NONE
      REAL*8        TEMPA, KAL, KBL, K1A, K1B, LREV, DEKLAG
      REAL*8        DKMA, DKMB, K1ACP, K1BCP, PRODEF
      CHARACTER*8   MATREV, ORIDEF
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 02/11/2010   AUTEUR MACOCCO K.MACOCCO 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C
C ======================================================================
C ======================================================================
C --- BUT : AJOUT DE CORRECTION PLASTIQUE AU CALCUL DES FACTEURS -------
C ------- : D'INTENSITE DE CONTRAINTES ---------------------------------
C ======================================================================
C IN  : TEMPA  : TEMPERATURE EN POINTE A -------------------------------
C --- : K1A    : FACTEUR D'INTENSITE DE CONTRAINTES ELASTIQUE EN A -----
C --- : K1B    : FACTEUR D'INTENSITE DE CONTRAINTES ELASTIQUE EN B -----
C --- : MATREV : MATERIAU DE REVETEMENT --------------------------------
C --- : LREV   : LONGUEUR DE REVETEMENT --------------------------------
C --- : DEKLAG : DECALAGE DU DEFAUT COTE REVETEMENT (TOUJOURS NEGATIF) -
C VAR : KAL    : FACTEUR DE MARGE EN POINTE A --------------------------
C --- : KBL    : FACTEUR DE MARGE EN POINTE B --------------------------
C --- : DKMA   : CORRECTION PLASTIQUE EN A -----------------------------
C --- : DKMB   : CORRECTION PLASTIQUE EN B -----------------------------
C OUT : K1ACP  : FACTEUR D'INTENSITE DE CONTRAINTE AVEC CORRECTION -----
C ------------ : PLASTIQUE EN A ----------------------------------------
C --- : K1BCP  : FACTEUR D'INTENSITE DE CONTRAINTE AVEC CORRECTION -----
C ------------ : PLASTIQUE EN B ----------------------------------------
C ======================================================================
C --------- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNUM, JEXNOM
C --------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ======================================================================
      INTEGER      IADR,LONG,I,J,K,LREEL, INEUT1, INEUT2, INEUT3, INEUT4
      INTEGER      LDIM, INEUT5, INEUT6, INEUT8, NPARA, NVALE, VALP
      INTEGER      NBPT1, NBPT2, ITOT1, ITOT2, ITOT10, ITOT11, ITOT12
      INTEGER      ITOT13, ITOT14, ITOT15
      REAL*8       SIGMA, TEMP1, TEMP2, SIGMA1, SIGMA2, REST, PENT
      REAL*8       TEMPDI, LAMB1, LAMB2, TEMPD, COEF1, COEF2, RYA, PI
      REAL*8       BETAA, BETAB, R8PI, DK, CA, CB, VAL1, VAL2
      CHARACTER*1  K1BID
      CHARACTER*8  PROLN, K8B
      CHARACTER*16 PHENOM, PROLG
      CHARACTER*19 VALNOM, ROMNOM, TRANOM, FONCT
      CHARACTER*24 NOMCMP, TYPNOM, TY2NOM, AUTNOM, VAENOM, COCNOM,PARNOM
      CHARACTER*24 NATNOM, PRONOM
C ======================================================================
      CALL JEMARQ()
C ======================================================================
      NOMCMP = MATREV//'.MATERIAU.NOMRC'
      PI     = R8PI()
      CALL JEVEUO(NOMCMP,'L',IADR)
      CALL JELIRA(NOMCMP,'LONUTI',LONG,K1BID)
      DO 3 I = 0, LONG-1
         PHENOM = ZK16(IADR+I)
         IF (PHENOM.EQ.'ECRO_LINE') THEN
            VALNOM = MATREV//'.'//PHENOM
            TYPNOM = VALNOM//'.VALR'
            TY2NOM = VALNOM//'.VALK'
            CALL JELIRA(TYPNOM,'LONUTI',LREEL,K1BID)
            IF (LREEL.GT.0) THEN
               CALL JEVEUO(TYPNOM,'L',INEUT1)
               CALL JEVEUO(TY2NOM,'L',INEUT2)
               DO 5 J =0,LREEL-1
                  IF (ZK8(INEUT2+J).EQ.'SY') THEN
                     SIGMA = ZR(INEUT1+J)
                     GOTO 30
                  ENDIF
 5             CONTINUE
            ELSE
               CALL JEVEUO(TY2NOM,'L',INEUT2)
               DO 110 J=0,LONG-1
                  IF (ZK8(INEUT2+J).EQ.'SY') THEN
                      FONCT = ZK8(INEUT2+J+LONG)
                      AUTNOM = FONCT//'.PROL'
                      VAENOM = FONCT//'.VALE'
                      CALL JEVEUO(AUTNOM,'L',INEUT4)
                      CALL JEVEUO(VAENOM,'L',INEUT3)
                      CALL JELIRA(VAENOM,'LONUTI',LDIM,K1BID)
                      LDIM = LDIM / 2
                      IF (TEMPA.LT.ZR(INEUT3)) THEN
                         PROLG = ZK24(INEUT4+4)
                         IF (PROLG(1:1).EQ.'E') THEN
                            CALL U2MESS('F','PREPOST_8')
                         ELSE IF (PROLG(1:1).EQ.'C') THEN
                            SIGMA = ZR(INEUT3+LDIM)
                         ELSE IF (PROLG(1:1).EQ.'L') THEN
                            TEMP1  = ZR(INEUT3)
                            TEMP2  = ZR(INEUT3+1)
                            SIGMA1 = ZR(INEUT3+LDIM)
                            SIGMA2 = ZR(INEUT3+LDIM+1)
                            PENT = (SIGMA2-SIGMA1)/(TEMP2-TEMP1)
                            REST = SIGMA1 - PENT*TEMP1
                            SIGMA = PENT*TEMPA + REST
                         ENDIF
                      ELSE IF (TEMPA.GT.ZR(INEUT3+LDIM-1)) THEN
                         PROLG = ZK24(INEUT4+4)
                         IF (PROLG(2:2).EQ.'E') THEN
                            CALL U2MESS('F','PREPOST_9')
                         ELSE IF (PROLG(2:2).EQ.'C') THEN
                            SIGMA = ZR(INEUT3+2*LDIM-1)
                         ELSE IF (PROLG(1:1).EQ.'L') THEN
                            TEMP1  = ZR(INEUT3+LDIM-2)
                            TEMP2  = ZR(INEUT3+LDIM-1)
                            SIGMA1 = ZR(INEUT3+2*LDIM-2)
                            SIGMA2 = ZR(INEUT3+2*LDIM-1)
                            PENT = (SIGMA2-SIGMA1)/(TEMP2-TEMP1)
                            REST = SIGMA1 - PENT*TEMP1
                            SIGMA = PENT*TEMPA + REST
                         ENDIF
                      ELSE
                         DO 50 K = 1, LDIM-1
                            IF (TEMPA.LT.ZR(INEUT3+K)) THEN
                               SIGMA1 = ZR(INEUT3+LDIM+K-1)
                               SIGMA2 = ZR(INEUT3+LDIM+K  )
                               TEMPDI = ZR(INEUT3+K) - ZR(INEUT3+K-1)
                       SIGMA  = (1-(TEMPA-ZR(INEUT3+K-1))/TEMPDI) *
     &                           SIGMA1 +
     &                          (1-(ZR(INEUT3+K)-TEMPA)/TEMPDI) *
     &                           SIGMA2
                            ENDIF
 50                      CONTINUE
                      ENDIF
                  ENDIF
 110           CONTINUE
               GOTO 30
            ENDIF
         ELSE IF (PHENOM.EQ.'TRACTION') THEN
            ROMNOM = MATREV//'.'//PHENOM
            COCNOM = ROMNOM//'.VALK'
            CALL JEVEUO(COCNOM,'L',INEUT6)
            TRANOM = ZK8(INEUT6+1)
            PARNOM = TRANOM//'.PARA'
            CALL JELIRA(PARNOM,'LONUTI',NPARA,K1BID)
            CALL JEVEUO(PARNOM,'L',INEUT5)
            PRONOM = TRANOM//'.PROL'
            CALL JEVEUO(PRONOM,'L',INEUT8)
            PROLN = ZK24(INEUT8+4)
            NATNOM = TRANOM//'.VALE'
            CALL JELIRA(NATNOM,'NUTIOC',NVALE,K1BID)
            VALP = 0
            DO 20 J = 1, NPARA
               IF (TEMPA.LT.ZR(INEUT5-1+J)) THEN
                   VALP = J
                   GOTO 21
               ENDIF
 20         CONTINUE
 21         CONTINUE
            IF (VALP.EQ.1) THEN
               IF (PROLN(1:1).EQ.'E') THEN
                  CALL U2MESS('F','PREPOST_8')
               ELSE IF (PROLN(1:1).EQ.'C') THEN
           CALL JELIRA ( JEXNUM(NATNOM,VALP  ), 'LONMAX', NBPT1, K8B )
               CALL JEVEUO ( JEXNUM(NATNOM,VALP  ), 'L', ITOT1 )
              SIGMA = ZR(ITOT1-1+NBPT1/2+1)
               ELSE
                  TEMP1 = ZR(INEUT5-1+VALP  )
                  TEMP2 = ZR(INEUT5-1+VALP+1)
         CALL JELIRA ( JEXNUM(NATNOM,VALP  ), 'LONMAX', NBPT1, K8B )
               CALL JEVEUO ( JEXNUM(NATNOM,VALP  ), 'L', ITOT10 )
         CALL JELIRA ( JEXNUM(NATNOM,VALP+1), 'LONMAX', NBPT2, K8B )
                  CALL JEVEUO ( JEXNUM(NATNOM,VALP+1), 'L', ITOT11 )
                  LAMB1 = ZR(ITOT10-1+NBPT1/2+1)
                  LAMB2 = ZR(ITOT11-1+NBPT2/2+1)
                  PENT = (LAMB2-LAMB1)/(TEMP2-TEMP1)
                  REST = LAMB1 - PENT*TEMP1
                  SIGMA = PENT*TEMPA+REST
               ENDIF
            ELSE IF (VALP.EQ.0) THEN
               IF (PROLN(2:2).EQ.'E') THEN
                  CALL U2MESS('F','PREPOST_9')
               ELSE IF (PROLN(2:2).EQ.'C') THEN
        CALL JELIRA ( JEXNUM(NATNOM,NPARA), 'LONMAX', NBPT1, K8B )
                  CALL JEVEUO ( JEXNUM(NATNOM,NPARA), 'L', ITOT2 )
                  SIGMA = ZR(ITOT2-1+NBPT1/2+1)
               ELSE
                  TEMP1 = ZR(INEUT5-1+NPARA-1)
                  TEMP2 = ZR(INEUT5-1+NPARA  )
        CALL JELIRA ( JEXNUM(NATNOM,NPARA-1), 'LONMAX', NBPT1, K8B )
        CALL JEVEUO ( JEXNUM(NATNOM,NPARA-1), 'L', ITOT12 )
        CALL JELIRA ( JEXNUM(NATNOM,NPARA  ), 'LONMAX', NBPT2, K8B )
                  CALL JEVEUO ( JEXNUM(NATNOM,NPARA  ), 'L', ITOT13 )
                  LAMB1 = ZR(ITOT12-1+NBPT1/2+1)
                  LAMB2 = ZR(ITOT13-1+NBPT2/2+1)
                  PENT = (LAMB2-LAMB1)/(TEMP2-TEMP1)
                  REST = LAMB1 - PENT*TEMP1
                  SIGMA = PENT*TEMPA+REST
               ENDIF
            ELSE
               TEMP1 = ZR(INEUT5-1+VALP-1)
               TEMP2 = ZR(INEUT5-1+VALP  )
               TEMPD = TEMP2 - TEMP1
               COEF1 = 1 - (TEMPA-TEMP1)/TEMPD
               COEF2 = 1 - (TEMP2-TEMPA)/TEMPD
       CALL JELIRA ( JEXNUM(NATNOM,VALP-1), 'LONMAX', NBPT1, K8B )
               CALL JEVEUO ( JEXNUM(NATNOM,VALP-1), 'L', ITOT14 )
       CALL JELIRA ( JEXNUM(NATNOM,VALP  ), 'LONMAX', NBPT2, K8B )
               CALL JEVEUO ( JEXNUM(NATNOM,VALP  ), 'L', ITOT15 )
               LAMB1 = ZR(ITOT14-1+NBPT1/2+1)
               LAMB2 = ZR(ITOT15-1+NBPT2/2+1)
               SIGMA = COEF1*LAMB1 + COEF2*LAMB2
            ENDIF
               GOTO 30
         ENDIF
 3    CONTINUE
      CALL U2MESS('F','PREPOST_10')
 30   CONTINUE
      RYA = (K1A * K1A)/(6 * PI * SIGMA * SIGMA)
      IF (ORIDEF.EQ.'LONGI') THEN
         CA = 0.165D0*LOG(PRODEF*1000)
         CB = 0.465D0*(1+PRODEF/100*1000)
      ELSE
         CA = 0.5D0
         CB = 0.5D0
      ENDIF
      BETAA = 1 + CA * TANH(36*RYA/(LREV+DEKLAG))
      BETAB = 1 + CB * TANH(36*RYA/(LREV+DEKLAG))
      IF (K1A.LT.KAL) THEN
         K1ACP = K1A + DKMA
      ELSE
         VAL1 = BETAA*K1A
         VAL2 = K1A + DKMA
         IF (VAL1.GT.VAL2) THEN
            K1ACP = VAL1
            DKMA = K1ACP - K1A
         ELSE
            K1ACP = K1A + DKMA
         ENDIF
      ENDIF
      KAL = K1A
      IF (K1B.LT.KBL) THEN
         K1BCP = K1B + DKMB
      ELSE
         VAL1 = BETAB*K1B
         VAL2 = K1B + DKMB
         IF (VAL1.GT.VAL2) THEN
            K1BCP = VAL1
            DKMB = K1BCP - K1B
         ELSE
            K1BCP = K1B + DKMB
         ENDIF
      ENDIF
      KBL = K1B
C ======================================================================
      CALL JEDEMA()
C ======================================================================
      END
