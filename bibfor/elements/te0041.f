      SUBROUTINE TE0041(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16      OPTION,NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 23/10/2008   AUTEUR TORKHANI M.TORKHANI 
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
C     CALCUL DES MATRICES DE RAIDEUR, MASSE, AMORTISSEMENT POUR
C     LES ELEMENTS DISCRETS : MECA_DIS_T_N      MECA_DIS_T_L
C                             MECA_DIS_TR_N     MECA_DIS_TR_L
C                             MECA_2D_DIS_T_N   MECA_2D_DIS_T_L
C                             MECA_2D_DIS_TR_N  MECA_2D_DIS_TR_L
C     ------------------------------------------------------------------
C IN  : OPTION : NOM DE L'OPTION A CALCULER
C IN  : NOMTE  : NOM DU TYPE_ELEMENT
C     ------------------------------------------------------------------
C TOLE  CRP_20
C       --- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                             ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) ,ZK80(1)
C       ---  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------

      INTEGER NDDLM,NL1,NL2,INFOD,INFODI
      PARAMETER (NDDLM=12,NL1=NDDLM*(NDDLM+1)/2,NL2=NDDLM*NDDLM)

      REAL*8     ETA,PGL(3,3),MATV1(NL1),MATV2(NL1),MATP(NDDLM,NDDLM)
      REAL*8     MATVK(144)
      COMPLEX*16 HYST
      CHARACTER*1 K1BID
      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
      CHARACTER*16 NOMCMD,TYPRES
      CHARACTER*19 NOMFON
      REAL*8     MATA1(NL1),MATA2(NL1),MATA3(NL2),MATA4(NL2),VALRES(3)
C     ------------------------------------------------------------------
      CALL JEMARQ()

      ZERO = 0.D0
      VALPAR = 0.D0
      INFODI = 1
      CALL GETRES(NOMFON,TYPRES,NOMCMD)
      IF (NOMCMD.EQ.'MECA_STATIQUE'.OR.NOMCMD.EQ.'CALC_MATR_ELEM') THEN
        IF (OPTION.EQ.'RIGI_MECA'.OR.OPTION.EQ.'MASS_MECA'.OR. 
     &    OPTION.EQ.'MASS_MECA_DIAG'.OR.OPTION.EQ.'AMOR_MECA') THEN
          CALL JEVECH('PCINFDI','L',INFOD)
          INFODI = NINT(ZR(INFOD))
        END IF
      END IF
      IF (NOMTE.EQ.'MECA_DIS_TR_L') THEN
         IF (INFODI.EQ.1) THEN
         N    = 78
         NNO  = 2
         NC   = 6
         NDIM = 3
         ELSEIF (INFODI.EQ.2) THEN
         N    = 144
         NNO  = 2
         NC   = 6
         NDIM = 3
         GOTO 999
         ENDIF
      ELSEIF (NOMTE.EQ.'MECA_DIS_T_L') THEN
         IF (INFODI.EQ.1) THEN
         N    = 21
         NNO  = 2
         NC   = 3
         NDIM = 3
         ELSEIF (INFODI.EQ.2) THEN
         N    = 36
         NNO  = 2
         NC   = 3
         NDIM = 3
         GOTO 999
         ENDIF
      ELSEIF (NOMTE.EQ.'MECA_DIS_TR_N') THEN
         IF (INFODI.EQ.1) THEN
         N    = 21
         NNO  = 1
         NC   = 6
         NDIM = 3
         ELSEIF (INFODI.EQ.2) THEN
         N    = 36
         NNO  = 1
         NC   = 6
         NDIM = 3
         GOTO 999
         ENDIF
      ELSEIF (NOMTE.EQ.'MECA_DIS_T_N' ) THEN
         IF (INFODI.EQ.1) THEN
         N   = 6
         NNO = 1
         NC  = 3
         NDIM = 3
         ELSEIF (INFODI.EQ.2) THEN
         N    = 9
         NNO  = 1
         NC   = 3
         NDIM = 3
         GOTO 999
         ENDIF
      ELSEIF (NOMTE.EQ.'MECA_2D_DIS_TR_L') THEN
         IF (INFODI.EQ.1) THEN
         N    = 21
         NNO  = 2
         NC   = 3
         NDIM = 2
         ELSEIF (INFODI.EQ.2) THEN
         N    = 36
         NNO  = 2
         NC   = 3
         NDIM = 2
         GOTO 999
         ENDIF
      ELSEIF (NOMTE.EQ.'MECA_2D_DIS_T_L') THEN
         IF (INFODI.EQ.1) THEN
         N    = 10
         NNO  = 2
         NC   = 2
         NDIM = 2
         ELSEIF (INFODI.EQ.2) THEN
         N    = 16
         NNO  = 2
         NC   = 2
         NDIM = 2
         GOTO 999
         ENDIF
      ELSEIF (NOMTE.EQ.'MECA_2D_DIS_TR_N') THEN
         IF (INFODI.EQ.1) THEN
         N    = 6
         NNO  = 1
         NC   = 3
         NDIM = 2
         ELSEIF (INFODI.EQ.2) THEN
         N    = 9
         NNO  = 1
         NC   = 3
         NDIM = 2
         GOTO 999
         ENDIF
      ELSEIF (NOMTE.EQ.'MECA_2D_DIS_T_N' ) THEN
         IF (INFODI.EQ.1) THEN
         N    = 3
         NNO  = 1
         NC   = 2
         NDIM = 2
         ELSEIF (INFODI.EQ.2) THEN
         N    = 4
         NNO  = 1
         NC   = 2
         NDIM = 2
         GOTO 999
         ENDIF
      ENDIF
      NDDL = NNO * NC

C
      DO 5 I = 1,NL1
         MATA1(I) = ZERO
         MATA2(I) = ZERO
         MATV1(I) = ZERO
         MATV2(I) = ZERO
  5   CONTINUE
      IF (OPTION.EQ.'RIGI_MECA_HYST') THEN
         CALL JEVECH('PCADISK','L',JDC)
         CALL JEVECH('PRIGIEL','L',JDR)
         CALL JEVECH('PMATUUC','E',JDM)
         ETA  = ZR(JDC+N+1)
         HYST = DCMPLX(1.D0,ETA)
         DO 10 I = 1,N
            ZC(JDM+I-1) = ZR(JDR+I-1) * HYST
 10      CONTINUE
         GOTO 9999
      ENDIF
C
      CALL JEVECH('PCAORIE','L',LORIEN)
      CALL MATROT ( ZR(LORIEN) , PGL )
      IF (OPTION.EQ.'RIGI_MECA'.OR.OPTION.EQ.'RIGI_MECA_TANG'
     &     .OR.OPTION.EQ.'RIGI_FLUI_STRU') THEN
         CALL JEVECH('PCADISK','L',JDC)
         CALL JEVECH('PMATUUR','E',JDM)
      ELSEIF (OPTION.EQ.'MASS_MECA'.OR. OPTION.EQ.'MASS_MECA_DIAG'
     &         .OR. OPTION.EQ.'MASS_MECA_EXPLI'
     &         .OR.OPTION.EQ.'MASS_FLUI_STRU') THEN
         CALL JEVECH('PCADISM','L',JDC)
         CALL JEVECH('PMATUUR','E',JDM)
      ELSEIF (OPTION.EQ.'AMOR_MECA') THEN
         CALL JEVECH('PCADISA','L',JDC)
         CALL JEVECH('PMATUUR','E',JDM)
         IF (NDIM.NE.3) GOTO 6
         CALL TECACH('ONN','PRIGIEL',1,JDR,IRET)
         IF (JDR.EQ.0) GOTO 6
         CALL TECACH('NNN','PMATERC',1,JMA,IRET)
         IF ((JMA.EQ.0) .OR.(IRET.NE.0)) GOTO 6
         NOMRES(1) = 'RIGI_NOR'
         NOMRES(2) = 'AMOR_NOR'
         NOMRES(3) = 'AMOR_TAN'
         VALRES(1) = ZERO
         VALRES(2) = ZERO
         VALRES(3) = ZERO
         CALL UTPSGL ( NNO, NC, PGL, ZR(JDR), MATV1 )
         CALL RCVALA ( ZI(JMA),' ','DIS_CONTACT',0,' ',VALPAR,3,
     &                            NOMRES,VALRES,CODRET, ' ' )
         IF (CODRET(1).EQ.'OK'.AND.VALRES(1).NE.ZERO) THEN
           IF (CODRET(2).EQ.'OK') MATA1(1)=MATV1(1)*VALRES(2)/VALRES(1)
           IF (CODRET(3).EQ.'OK') MATA1(3)=MATV1(1)*VALRES(3)/VALRES(1)
           MATA1(6) = MATA1(3)
         ENDIF
         IF (NNO.EQ.2.AND.NC.EQ.3) THEN
           MATA1(7) = -MATA1(1)
           MATA1(10) = MATA1(1)
           MATA1(15) = MATA1(3)
           MATA1(21) = MATA1(3)
           MATA1(12) = -MATA1(3)
           MATA1(18) = -MATA1(3)
         ELSEIF (NNO.EQ.2.AND.NC.EQ.6) THEN
           MATA1(22) = -MATA1(1)
           MATA1(28) = MATA1(1)
           MATA1(36) = MATA1(3)
           MATA1(45) = MATA1(3)
           MATA1(30) = -MATA1(3)
           MATA1(39) = -MATA1(3)
         ENDIF
  6      CONTINUE
      ELSEIF (OPTION.EQ.'M_GAMMA') THEN
         CALL JEVECH('PCADISM','L',JDC)
         CALL JEVECH('PDEPLAR','L',IACCE)
         CALL JEVECH('PVECTUR','E',IVECT)
      ELSEIF (OPTION.EQ.'RIGI_MECA_SENSI') THEN
         CALL JEVECH('PSEDISK','L',JDC)
         CALL JEVECH('PVAPRIN','L',LVAPR)
         CALL JEVECH('PVECTUR','E',LVECT)
      ELSEIF (OPTION.EQ.'MASS_MECA_SENSI') THEN
         CALL JEVECH('PSEDISM','L',JDC)
         CALL JEVECH('PVAPRIN','L',LVAPR)
         CALL JEVECH('PVECTUR','E',LVECT)
      ELSEIF (OPTION.EQ.'RIGI_MECA_SENS_C') THEN
         CALL JEVECH('PSEDISK','L',JDC)
         CALL JEVECH('PVAPRIN','L',LVAPR)
         CALL JEVECH('PVECTUC','E',LVECT)
      ELSEIF (OPTION.EQ.'MASS_MECA_SENS_C') THEN
         CALL JEVECH('PSEDISM','L',JDC)
         CALL JEVECH('PVAPRIN','L',LVAPR)
         CALL JEVECH('PVECTUC','E',LVECT)



      ELSE
CC OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      ENDIF
      IF (NDIM.EQ.3) CALL UTPSLG ( NNO, NC, PGL, MATA1, MATA2 )
      IREP = NINT(ZR(JDC+N))
      IF (IREP.EQ.1) THEN
C
C        --- REPERE GLOBAL ==> PAS DE ROTATION ---
         IF (OPTION.EQ.'M_GAMMA') THEN
            CALL VECMA(ZR(JDC),N,MATP,NDDL)
            CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
         ELSE IF (OPTION(11:14).EQ.'SENS') THEN
              DO 430 I = 1,N
                MATV1(I) = ZR(JDC+I-1)
430           CONTINUE
              CALL VECMA(MATV1,N,MATP,NDDLM)
              DO 400 I = 1,NDDL
                IF (OPTION(15:16).EQ.'_C') THEN
                  ZC(LVECT-1+I) = DCMPLX(0.D0,0.D0)
                ELSE
                  ZR(LVECT-1+I) = 0.D0
                END IF
                DO 410 J = 1,NDDL
                  IF (OPTION(15:16).EQ.'_C') THEN
                    ZC(LVECT-1+I) = ZC(LVECT-1+I)
     &                   - MATP(I,J)*ZC(LVAPR-1+J)
                  ELSE
                    ZR(LVECT-1+I) = ZR(LVECT-1+I)
     &                   - MATP(I,J)*ZR(LVAPR-1+J)
                  END IF
410             CONTINUE
400           CONTINUE


         ELSE
           DO 20 I = 1,N
             ZR(JDM+I-1) = ZR(JDC+I-1)
             IF (OPTION.EQ.'AMOR_MECA')
     &           ZR(JDM+I-1) = ZR(JDM+I-1) + MATA2(I)
 20        CONTINUE
         ENDIF
C
      ELSE IF (IREP.EQ.2)THEN
C
C        --- LOCAL ==> GLOBAL ---
         IF ( ZR(LORIEN).EQ.0.D0 .AND. ZR(LORIEN+1).EQ.0.D0
     &                           .AND. ZR(LORIEN+2).EQ.0.D0 )THEN
C           --- ANGLES NULS  ===>  PAS DE ROTATION ---
            IF (OPTION.EQ.'M_GAMMA') THEN
               CALL VECMA(ZR(JDC),N,MATP,NDDL)
               CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
            ELSE IF (OPTION(11:14).EQ.'SENS') THEN
              DO 130 I = 1,N
                MATV1(I) = ZR(JDC+I-1)
130           CONTINUE
              CALL VECMA(MATV1,N,MATP,NDDLM)
              DO 100 I = 1,NDDL
                IF (OPTION(15:16).EQ.'_C') THEN
                  ZC(LVECT-1+I) = DCMPLX(0.D0,0.D0)
                ELSE
                  ZR(LVECT-1+I) = 0.D0
                END IF
                DO 110 J = 1,NDDL
                  IF (OPTION(15:16).EQ.'_C') THEN
                    ZC(LVECT-1+I) = ZC(LVECT-1+I)
     &                   - MATP(I,J)*ZC(LVAPR-1+J)
                  ELSE
                    ZR(LVECT-1+I) = ZR(LVECT-1+I)
     &                   - MATP(I,J)*ZR(LVAPR-1+J)
                  END IF
110             CONTINUE
100           CONTINUE


            ELSE
              DO 30 I = 1,N
                  ZR(JDM+I-1) = ZR(JDC+I-1)
                  IF (OPTION.EQ.'AMOR_MECA')
     &             ZR(JDM+I-1) = ZR(JDM+I-1) + MATA2(I)
 30           CONTINUE
            ENDIF
         ELSE
C           --- ANGLES NON NULS  ===>  ROTATION ---
C            CALL MATROT ( ZR(LORIEN) , PGL )
            IF (OPTION.EQ.'M_GAMMA') THEN
               IF (NDIM.EQ.3) THEN
                 CALL UTPSLG ( NNO, NC, PGL, ZR(JDC), MATV1 )
               ELSEIF (NDIM.EQ.2) THEN
                 CALL UT2MLG ( NNO, NC, PGL, ZR(JDC), MATV1 )
               ENDIF
               CALL VECMA(MATV1,N,MATP,NDDL)
               CALL PMAVEC('ZERO',NDDL,MATP,ZR(IACCE),ZR(IVECT))
            ELSE
              IF (NDIM.EQ.3) THEN
               IF (OPTION(11:14).EQ.'SENS') THEN
C ON CREE UN VECTEUR DE TRAVAIL ET ON FAIT LE PRODUIT
                CALL UTPSLG ( NNO, NC, PGL, ZR(JDC), MATV1 )
                CALL VECMA(MATV1,N,MATP,NDDLM)
                DO 200 I = 1,NDDL
                  IF (OPTION(15:16).EQ.'_C') THEN
                    ZC(LVECT-1+I) = DCMPLX(0.D0,0.D0)
                  ELSE
                    ZR(LVECT-1+I) = 0.D0
                  END IF
                  DO 210 J = 1,NDDL
                    IF (OPTION(15:16).EQ.'_C') THEN
                      ZC(LVECT-1+I) = ZC(LVECT-1+I)
     &                   - MATP(I,J)*ZC(LVAPR-1+J)
                    ELSE
                      ZR(LVECT-1+I) = ZR(LVECT-1+I)
     &                   - MATP(I,J)*ZR(LVAPR-1+J)
                    END IF
210               CONTINUE
200             CONTINUE


               ELSE
                CALL UTPSLG ( NNO, NC, PGL, ZR(JDC), ZR(JDM) )
                IF (OPTION.EQ.'AMOR_MECA') THEN
                 DO 25 I = 1,N
                   ZR(JDM+I-1) = ZR(JDM+I-1) + MATA2(I)
 25              CONTINUE
                ENDIF
               ENDIF
              ELSEIF (NDIM.EQ.2) THEN
               IF (OPTION(11:14).EQ.'SENS') THEN
C ON CREE UN VECTEUR DE TRAVAIL ET ON FAIT LE PRODUIT
                CALL UT2MLG ( NNO, NC, PGL, ZR(JDC), MATV1 )
                CALL VECMA(MATV1,N,MATP,NDDLM)
                DO 300 I = 1,NDDL
                  IF (OPTION(15:16).EQ.'_C') THEN
                    ZC(LVECT-1+I) = DCMPLX(0.D0,0.D0)
                  ELSE
                    ZR(LVECT-1+I) = 0.D0
                  END IF
                  DO 310 J = 1,NDDL
                    IF (OPTION(15:16).EQ.'_C') THEN
                      ZC(LVECT-1+I) = ZC(LVECT-1+I)
     &                   - MATP(I,J)*ZC(LVAPR-1+J)
                    ELSE
                      ZR(LVECT-1+I) = ZR(LVECT-1+I)
     &                   - MATP(I,J)*ZR(LVAPR-1+J)
                    END IF
310               CONTINUE
300             CONTINUE


               ELSE
                CALL UT2MLG ( NNO, NC, PGL, ZR(JDC), ZR(JDM) )
               ENDIF
              ENDIF
            ENDIF
         ENDIF
      ENDIF
      GOTO 9999
C
 999  CONTINUE
C

      DO 7 I = 1,NL2
         MATA3(I) = ZERO
         MATA4(I) = ZERO
  7   CONTINUE
C
      CALL JEVECH('PCAORIE','L',LORIEN)
      CALL MATROT ( ZR(LORIEN) , PGL )
      IF (OPTION.EQ.'RIGI_MECA') THEN
         CALL JEVECH('PCADNSK','L',JDC)
         CALL JEVECH('PMATUNS','E',JDM)
      ELSEIF (OPTION.EQ.'MASS_MECA'.OR. OPTION.EQ.'MASS_MECA_DIAG') THEN
         CALL JEVECH('PCADNSM','L',JDC)
         CALL JEVECH('PMATUNS','E',JDM)
      ELSEIF (OPTION.EQ.'AMOR_MECA') THEN
         CALL JEVECH('PCADNSA','L',JDC)
         CALL JEVECH('PMATUNS','E',JDM)
         IF (NDIM.NE.3) GOTO 8
         CALL TECACH('ONN','PRIGIEL',1,JDR,IRET)
         IF (JDR.EQ.0) GOTO 8
         CALL TECACH('NNN','PMATERC',1,JMA,IRET)
         IF ((JMA.EQ.0) .OR.(IRET.NE.0)) GOTO 8
         NOMRES(1) = 'RIGI_NOR'
         NOMRES(2) = 'AMOR_NOR'
         NOMRES(3) = 'AMOR_TAN'
         VALRES(1) = ZERO
         VALRES(2) = ZERO
         VALRES(3) = ZERO
         CALL UTPSGL ( NNO, NC, PGL, ZR(JDR), MATV2)
         CALL RCVALA ( ZI(JMA),' ','DIS_CONTACT',0,' ',VALPAR,3,
     &                            NOMRES,VALRES,CODRET, ' ' )
         IF (CODRET(1).EQ.'OK'.AND.VALRES(1).NE.ZERO) THEN
           IF (CODRET(2).EQ.'OK') MATA3(1)=MATV2(1)*VALRES(2)/VALRES(1)
           IF (CODRET(3).EQ.'OK') MATA3(3)=MATV2(1)*VALRES(3)/VALRES(1)
         ENDIF
         IF (NNO.EQ.2.AND.NC.EQ.3) THEN
           MATA3(19) = -MATA3(1)
           MATA3(22) = MATA3(1)
           MATA3(29) = MATA3(3)
           MATA3(36) = MATA3(3)
           MATA3(26) = -MATA3(3)
           MATA3(33) = -MATA3(3)
           MATA3(8)  = MATA3(3)
           MATA3(15) = MATA3(3)
           MATA3(4)  = MATA3(19)
           MATA3(11) = MATA3(26)
           MATA3(18) = MATA3(33)
           MATA3(3)  = 0.D0
         ELSEIF (NNO.EQ.2.AND.NC.EQ.6) THEN
           MATA3(73) = -MATA3(1)
           MATA3(79) = MATA3(1)
           MATA3(92) = MATA3(3)
           MATA3(105)= MATA3(3)
           MATA3(86) = -MATA3(3)
           MATA3(99) = -MATA3(3)
           MATA3(7)  = MATA3(73)
           MATA3(20) = MATA3(86)
           MATA3(33) = MATA3(99)
           MATA3(14) = MATA3(3)
           MATA3(27) = MATA3(3)
           MATA3(3)  = 0.D0
         ENDIF
  8      CONTINUE
      ELSE
CC OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      ENDIF
      IF (NDIM.EQ.3) CALL UTPPLG ( NNO, NC, PGL, MATA3, MATA4 )
      IREP = NINT(ZR(JDC+N))
      IF (IREP.EQ.1) THEN
C
C        --- REPERE GLOBAL ==> PAS DE ROTATION ---
      DO 21 I = 1,N
        ZR(JDM+I-1) = ZR(JDC+I-1)
        IF (OPTION.EQ.'AMOR_MECA')
     &      ZR(JDM+I-1) = ZR(JDM+I-1) + MATA4(I)
 21   CONTINUE
C
      ELSE IF (IREP.EQ.2)THEN
C
C        --- LOCAL ==> GLOBAL ---
         IF ( ZR(LORIEN).EQ.0.D0 .AND. ZR(LORIEN+1).EQ.0.D0
     &                           .AND. ZR(LORIEN+2).EQ.0.D0 )THEN
C           --- ANGLES NULS  ===>  PAS DE ROTATION ---
              DO 31 I = 1,N
                  ZR(JDM+I-1) = ZR(JDC+I-1)
                  IF (OPTION.EQ.'AMOR_MECA')
     &             ZR(JDM+I-1) = ZR(JDM+I-1) + MATA4(I)
 31           CONTINUE
         ELSE
C           --- ANGLES NON NULS  ===>  ROTATION ---
C            CALL MATROT ( ZR(LORIEN) , PGL )
              IF (NDIM.EQ.3) THEN
                CALL UTPPLG ( NNO, NC, PGL, ZR(JDC), ZR(JDM) )
                IF (OPTION.EQ.'AMOR_MECA') THEN
                 DO 26 I = 1,N
                   ZR(JDM+I-1) = ZR(JDM+I-1) + MATA4(I)
 26              CONTINUE
                ENDIF
              ELSEIF (NDIM.EQ.2) THEN
                CALL UT2PLG ( NNO, NC, PGL, ZR(JDC), ZR(JDM) )
              ENDIF
         ENDIF
      ENDIF
 
 9999 CONTINUE

      CALL JEDEMA()

      END
