      SUBROUTINE TE0415(OPTIOZ,NOMTZ)
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_20
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) OPTIOZ,NOMTZ
      CHARACTER*16 OPTION,NOMTE
C     ----------------------------------------------------------------
C     CALCUL DES OPTIONS DES ELEMENTS DE COQUE 3D
C     OPTIONS : SIEF_ELNO , VARI_ELNO
C          -----------------------------------------------------------

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      PARAMETER (NPGE=3)
      PARAMETER (NPGT=10)
      INTEGER JNUMC,ICOU,NORDO,JMAT,JNBSPI
      INTEGER NB1,NB2,NPGSR,NPGSN,JTAB(7)
      REAL*8 VECTA(9,2,3),VECTN(9,3),VECTPT(9,2,3)
      REAL*8 VECTG(2,3),VECTT(3,3)
      REAL*8 EPAIS
      REAL*8 MATEVN(2,2,NPGT),MATEVG(2,2,NPGT)
      REAL*8 SIGM(6,270),SIGMA(6,120),SIGGN(6,9),EFFGC(8,9),EFFGT(8,9)
      REAL*8 PK2(6,270),SIGGNU(6,9),VAR(12)
      LOGICAL       LGREEN

      OPTION = OPTIOZ
      NOMTE = NOMTZ
      ZERO = 0.0D0
      LGREEN = .FALSE.
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESI',' ',LZI)
      NB1 = ZI(LZI-1+1)
      NB2 = ZI(LZI-1+2)
      NPGSR = ZI(LZI-1+3)
      NPGSN = ZI(LZI-1+4)
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ',LZR)
      IF (NOMTE.EQ.'MEC3QU9H') THEN
        NSO = 4
      ELSE IF (NOMTE.EQ.'MEC3TR7H') THEN
        NSO = 3
      END IF
      IF (OPTION(1:9).EQ.'SIEF_ELNO' .OR.
     &    OPTION.EQ.'SICO_ELNO') THEN

        CALL JEVECH('PGEOMER','L',JGEOM)
        CALL JEVECH('PCACOQU','L',JCARA)

C
        CALL JEVECH('PCONTRR','L',ICHG)

        CALL TECACH('ONN','PCOMPOR',1,ICOMPO,IRET)

        IF(ICOMPO.EQ.0) THEN
             NBCOU = 1
        ELSE
          CALL JEVECH('PNBSP_I','L',JNBSPI)
          NBCOU=ZI(JNBSPI-1+1)
        ENDIF

        IF (NBCOU.LE.0) CALL U2MESS('F','ELEMENTS_12')
        IF (NBCOU.GT.10) CALL U2MESS('F','ELEMENTS_13')
        EPAIS = ZR(JCARA)
        ZMIN = -EPAIS/2.D0
        HIC = EPAIS/NBCOU
        CALL VECTAN(NB1,NB2,ZR(JGEOM),ZR(LZR),VECTA,VECTN,VECTPT)
        KPGS = 0
        DO 50 ICOU = 1,NBCOU
          DO 40 INTE = 1,NPGE
            IF (INTE.EQ.1) THEN
              ZIC = ZMIN + (ICOU-1)*HIC
            ELSE IF (INTE.EQ.2) THEN
              ZIC = ZMIN + HIC/2.D0 + (ICOU-1)*HIC
            ELSE
              ZIC = ZMIN + HIC + (ICOU-1)*HIC
            END IF

            DO 30 INTSN = 1,NPGSN
              KPGS = KPGS + 1
              K1=6*((INTSN-1)*NPGE*NBCOU + (ICOU-1)*NPGE +INTE - 1)
              DO 20 I = 1,6
                SIGM(I,KPGS) = ZR(ICHG-1+K1+I)
   20         CONTINUE
   30       CONTINUE
   40     CONTINUE
   50   CONTINUE
        NCMP = 6
        IF (OPTION.EQ.'SICO_ELNO') THEN
          IF (ZK16(ICOMPO+2).EQ.'GROT_GDEP') THEN
            LGREEN = .TRUE.
          ENDIF
        ENDIF
        IF (LGREEN) THEN
C
C ---   AFFECTATION DES CONTRAINTES DE PIOLA-KIRCHHOFF DE
C ---   SECONDE ESPECE :
C       --------------
          DO 60 I = 1, 6
          DO 60 J = 1, KPGS
            PK2(I,J) = SIGM(I,J)
  60      CONTINUE
C
C ---   TRANSFORMATION DES CONTRAINTES DE PIOLA-KIRCHHOFF DE
C ---   SECONDE ESPECE PK2 EN CONTRAINTES DE CAUCHY :
C       -------------------------------------------
          CALL PK2CAU(NOMTE,NCMP,PK2,SIGM)
        ENDIF
C
C --- DETERMINATION DES REPERES  LOCAUX DE L'ELEMENT AUX POINTS
C --- D'INTEGRATION ET STOCKAGE DE CES REPERES DANS LE VECTEUR .DESR
C     --------------------------------------------------------------
        K = 0
        DO 90 INTSR = 1,NPGSR
          CALL VECTGT(0,NB1,ZR(JGEOM),ZERO,INTSR,ZR(LZR),EPAIS,VECTN,
     &                VECTG,VECTT)

          DO 80 J = 1,3
            DO 70 I = 1,3
              K = K + 1
              ZR(LZR+2000+K-1) = VECTT(I,J)
   70       CONTINUE
   80     CONTINUE
   90   CONTINUE

C--- EXTRAPOLATION VERS LES NOEUDS SOMMETS
C
       CALL JEVETE('&INEL.'//NOMTE//'.B',' ',JMAT)

        DO 130 ICOU = 1,NBCOU
          DO 120 IC = 1,NCMP
            DO 110 I = 1,NPGE*NSO
              L = NPGE*NPGSN* (I-1)
              S = 0.D0
              DO 100 J = 1,NPGE*NPGSN
                JJ = (ICOU-1)*NPGE*NPGSN + J
                S = S + ZR(JMAT-1+L+J)*SIGM(IC,JJ)
  100         CONTINUE
              II = (ICOU-1)*NPGE*NSO + I
              SIGMA(IC,II) = S
  110       CONTINUE
  120     CONTINUE
  130   CONTINUE

C --- DETERMINATION DES MATRICE DE PASSAGE DES REPERES INTRINSEQUES
C --- AUX NOEUDS ET AUX POINTS D'INTEGRATION DE L'ELEMENT
C --- AU REPERE UTILISATEUR :
C     ---------------------
        CALL VDREPE(NOMTE,MATEVN,MATEVG)

         IF (OPTION.EQ.'SICO_ELNO') THEN

C --- PASSAGE DU VECTEUR DES CONTRAINTES DEFINI AUX NOEUDS
C --- DE L'ELEMENT DU REPERE INTRINSEQUE AU REPERE UTILISATEUR :
C     --------------------------------------------------------
          CALL JEVECH('PSIGNOD','L',JCONN)
          CALL JEVECH('PNUMCOR','L',JNUMC)
          ICOU = ZI(JNUMC)
          NORDO = ZI(JNUMC+1)
          IF (ICOU.LE.0 .OR. ICOU.GT.NBCOU) CALL U2MESS('F','ELEMENTS3_9
     &5')
          DO 150 I = 1,NCMP
            DO 140 J = 1,NSO
              JJ = NSO* (NORDO+1) + NSO*NPGE* (ICOU-1) + J
              SIGGN(I,J) = SIGMA(I,JJ)
  140       CONTINUE
            IF (NOMTE.EQ.'MEC3QU9H') THEN
              SIGGN(I,5) = (SIGGN(I,1)+SIGGN(I,2))/2.D0
              SIGGN(I,6) = (SIGGN(I,2)+SIGGN(I,3))/2.D0
              SIGGN(I,7) = (SIGGN(I,3)+SIGGN(I,4))/2.D0
              SIGGN(I,8) = (SIGGN(I,4)+SIGGN(I,1))/2.D0
              SIGGN(I,9) = (SIGGN(I,1)+SIGGN(I,2)+SIGGN(I,3)+
     &                     SIGGN(I,4))/4.D0
            ELSE IF (NOMTE.EQ.'MEC3TR7H') THEN
              SIGGN(I,4) = (SIGGN(I,1)+SIGGN(I,2))/2.D0
              SIGGN(I,5) = (SIGGN(I,2)+SIGGN(I,3))/2.D0
              SIGGN(I,6) = (SIGGN(I,3)+SIGGN(I,1))/2.D0
              SIGGN(I,7) = (SIGGN(I,1)+SIGGN(I,2)+SIGGN(I,3))/3.D0
            END IF
  150     CONTINUE
C
          IF (LGREEN) THEN
            CALL VDSIRO(NB2,MATEVN,SIGGN,SIGGNU)
            CALL CAURTG(NOMTE,NCMP,SIGGNU,ZR(JCONN))
          ELSE
            CALL VDSIRO(NB2,MATEVN,SIGGN,ZR(JCONN))
          ENDIF

        ELSE IF (OPTION(1:9).EQ.'SIEF_ELNO') THEN
          DO 170 I = 1,NB2
            DO 160 J = 1,8
              EFFGT(J,I) = 0.D0
  160       CONTINUE
  170     CONTINUE
          DO 200 IC = 1,NBCOU
            J = (IC-1)*NPGE*NSO + 1
            ZIC = ZMIN + (IC-1)*HIC
            CALL VDEFGN(NOMTE,NB2,HIC,ZIC,SIGMA(1,J),EFFGC)
            DO 190 ISOM = 1,NB2
              DO 180 ICOMP = 1,8
                EFFGT(ICOMP,ISOM) = EFFGT(ICOMP,ISOM) +
     &                              EFFGC(ICOMP,ISOM)
  180         CONTINUE
  190       CONTINUE
  200     CONTINUE

C --- PASSAGE DU VECTEUR DES EFFORTS GENERALISES DEFINI AUX NOEUDS
C --- DE L'ELEMENT DU REPERE INTRINSEQUE AU REPERE UTILISATEUR :
C     --------------------------------------------------------
          CALL JEVECH('PSIEFNOR','L',JEFFG)
          CALL VDEFRO(NB2,MATEVN,EFFGT,ZR(JEFFG))
        END IF

       ELSE IF (OPTION.EQ.'VARI_ELNO') THEN


        CALL JEVECH('PVARIGR','L',ICHG)
        CALL JEVECH('PCOMPOR','L',ICOMPO)
        READ (ZK16(ICOMPO-1+2),'(I16)') NBVARI
        CALL TECACH('OON','PVARIGR',7,JTAB,IRET)
        LGPG = MAX(JTAB(6),1)*JTAB(7)
        CALL JEVECH('PNBSP_I','L',JNBSPI)
        NBCOU=ZI(JNBSPI-1+1)
        IF (NBCOU.LE.0) CALL U2MESS('F','ELEMENTS_12')
        IF (NBCOU.GT.10) CALL U2MESS('F','ELEMENTS_13')

C -- RECUPERATION DES VARIABLES INTERNES
C -- NBVARI = NOMBRES DE VARIABLES INTERNES
C -- STOCKAGE DANS PVARIGR : PAR POINT DE GAUSS DU PREMIER AU DERNIER

        CALL JEVETE('&INEL.'//NOMTE//'.B',' ',JMAT)

C-- EXTRAPOLATION AUX NOEUDS SOMMETS (3 OU 4)

        CALL JEVECH('PVARINR','E',JVARI)

        DO 280 ICOU = 1,NBCOU
          DO 270 IC = 1,NBVARI
            DO 260 I = 1,NPGE*NSO
              L = NPGE*NPGSN* (I-1)
              S = 0.D0
              DO 230 J = 1,NPGE*NPGSN
C -- DETERMINATION DU PT DE GAUSS A PARTIR DE LA POSITION JJ
                DO 220 K1 = 1,NPGSN
                  DO 210 K2 = 1,NPGE
                    J1 = (K1-1)*NPGE + K2
                    IF (J1.EQ.J) THEN
                      INP = K1
                      NEP = K2 - 1
                    END IF
  210             CONTINUE
  220           CONTINUE
                NPP = (INP-1)*LGPG
                NPP = NPP + IC + NBVARI* ((ICOU-1)*NPGE+NEP)
C -- ZR(ICHG-1+NPP) = VARI(IC,JJ)
C                JJ = (ICOU-1)*NPGE*NPGSN + J
                S = S + ZR(JMAT-1+L+J)*ZR(ICHG-1+NPP)
  230         CONTINUE
              II = (ICOU-1)*NPGE*NSO + I
C -- DETERMINATION DU NOEUD SOMMET A PARTIR DE LA POSITION II
              DO 250 K1 = 1,NSO
                DO 240 K2 = 1,NPGE
                  I1 = (K1-1)*NPGE + K2
                  IF (I1.EQ.I) THEN
                    INO = K1
                    NEP = K2 - 1
                  END IF
  240           CONTINUE
  250         CONTINUE
              NPO = (INO-1)*LGPG
              NPO = NPO + IC + NBVARI* ((ICOU-1)*NPGE+NEP)
              ZR(JVARI-1+NPO) = S
  260       CONTINUE
  270     CONTINUE
  280   CONTINUE

C -- CREATION DU CHAMP DE VARIABLES INTERNES POUR LES POINTS
C -- MILIEUX ET LE CENTRE
C -- STOCKAGE DANS PVARINR : PAR NOEUD DU PREMIER AU DERNIER

        IF (NOMTE.EQ.'MEC3QU9H') THEN
          DO 310 IC = 5,NB2
            NPO = (IC-1)*LGPG
            IF (IC.EQ.5) THEN
              NP1 = 0
              NP2 = LGPG
            ELSE IF (IC.EQ.6) THEN
              NP1 = LGPG
              NP2 = 2*LGPG
            ELSE IF (IC.EQ.7) THEN
              NP1 = 2*LGPG
              NP2 = 3*LGPG
            ELSE IF (IC.EQ.8) THEN
              NP1 = 3*LGPG
              NP2 = 0
            ELSE IF (IC.EQ.9) THEN
              NP1 = 0
              NP2 = LGPG
              NP3 = 2*LGPG
              NP4 = 3*LGPG
            END IF
            IF (IC.NE.9) THEN
              DO 290 I = 1,LGPG
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NP1+I) +
     &                              ZR(JVARI-1+NP2+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I)/2.D0
  290         CONTINUE
            ELSE
              DO 300 I = 1,LGPG
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NP1+I) +
     &                              ZR(JVARI-1+NP2+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I) +
     &                              ZR(JVARI-1+NP3+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I) +
     &                              ZR(JVARI-1+NP4+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I)/4.D0
  300         CONTINUE
            END IF
  310     CONTINUE
        ELSE IF (NOMTE.EQ.'MEC3TR7H') THEN
          DO 340 IC = 4,NB2
            NPO = (IC-1)*LGPG
            IF (IC.EQ.4) THEN
              NP1 = 0
              NP2 = LGPG
            ELSE IF (IC.EQ.5) THEN
              NP1 = LGPG
              NP2 = 2*LGPG
            ELSE IF (IC.EQ.6) THEN
              NP1 = 2*LGPG
              NP2 = 0
            ELSE IF (IC.EQ.7) THEN
              NP1 = 0
              NP2 = LGPG
              NP3 = 2*LGPG
            END IF
            IF (IC.NE.7) THEN
              DO 320 I = 1,LGPG
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NP1+I) +
     &                              ZR(JVARI-1+NP2+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I)/2.D0
  320         CONTINUE
            ELSE
              DO 330 I = 1,LGPG
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NP1+I) +
     &                              ZR(JVARI-1+NP2+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I) +
     &                              ZR(JVARI-1+NP3+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I)/3.D0
  330         CONTINUE
            END IF
  340     CONTINUE
        END IF

C ------------------------------------------------------------

       ELSE IF (OPTION.EQ.'VACO_ELNO') THEN

        CALL JEVECH('PNUMCOR','L',JNUMC)
        ICOU = ZI(JNUMC)
        NORDO = ZI(JNUMC+1)

        CALL JEVECH('PVARIGR','L',ICHG)
        CALL JEVECH('PCOMPOR','L',ICOMPO)
        READ (ZK16(ICOMPO-1+2),'(I16)') NBVARI
        CALL TECACH('OON','PVARIGR',7,JTAB,IRET)
        LGPG = MAX(JTAB(6),1)*JTAB(7)

        CALL JEVECH('PNBSP_I','L',JNBSPI)
        NBCOU=ZI(JNBSPI-1+1)

C -- RECUPERATION DES VARIABLES INTERNES
C -- NBVARI = NOMBRES DE VARIABLES INTERNES
C -- STOCKAGE DANS PVARIGR : PAR POINT DE GAUSS DU PREMIER
C -- AU DERNIER

        CALL JEVETE('&INEL.'//NOMTE//'.B',' ',JMAT)

C-- EXTRAPOLATION AUX NOEUDS SOMMETS (3 OU 4)

        CALL JEVECH('PVARINR','E',JVARI)

          DO 470 IC = 1,NBVARI

            DO 460 I = 1,NPGE*NSO
              L = NPGE*NPGSN* (I-1)
              S = 0.D0
              DO 430 J = 1,NPGE*NPGSN
C -- DETERMINATION DU PT DE GAUSS A PARTIR DE LA POSITION JJ
                DO 420 K1 = 1,NPGSN
                  DO 410 K2 = 1,NPGE
C
                    J1 = (K1-1)*NPGE + K2
                    IF (J1.EQ.J) THEN
                      INP = K1
                      NEP = K2 - 1
                    END IF
  410             CONTINUE
  420           CONTINUE
                NPP = (INP-1)*LGPG
                NPP = NPP + IC + NBVARI* ((ICOU-1)*NPGE+NEP)

C -- ZR(ICHG-1+NPP) = VARI(IC,JJ)
C                JJ = (ICOU-1)*NPGE*NPGSN + J
                S = S + ZR(JMAT-1+L+J)*ZR(ICHG-1+NPP)
  430         CONTINUE
            VAR(I)=S
  460       CONTINUE

            IPGE=NORDO+2
            DO 465 INO=1,NSO
               ZR(JVARI-1+(INO-1)*NBVARI+IC) = VAR((INO-1)*NPGE+IPGE)
  465       CONTINUE
  470     CONTINUE

C -- CREATION DU CHAMP DE VARIABLES INTERNES POUR LES POINTS
C -- MILIEUX ET LE CENTRE
C -- STOCKAGE DANS PVARINR : PAR NOEUD DU PREMIER AU DERNIER

        IF (NOMTE.EQ.'MEC3QU9H') THEN
          DO 510 IC = 5,NB2
            NPO = (IC-1)*NBVARI
            IF (IC.EQ.5) THEN
              NP1 = 0
              NP2 = NBVARI
            ELSE IF (IC.EQ.6) THEN
              NP1 = NBVARI
              NP2 = 2*NBVARI
            ELSE IF (IC.EQ.7) THEN
              NP1 = 2*NBVARI
              NP2 = 3*NBVARI
            ELSE IF (IC.EQ.8) THEN
              NP1 = 3*NBVARI
              NP2 = 0
            ELSE IF (IC.EQ.9) THEN
              NP1 = 0
              NP2 = NBVARI
              NP3 = 2*NBVARI
              NP4 = 3*NBVARI
            END IF
            IF (IC.NE.9) THEN
              DO 490 I = 1,NBVARI
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NP1+I) +
     &                              ZR(JVARI-1+NP2+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I)/2.D0
  490         CONTINUE
            ELSE
              DO 500 I = 1,NBVARI
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NP1+I) +
     &                              ZR(JVARI-1+NP2+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I) +
     &                              ZR(JVARI-1+NP3+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I) +
     &                              ZR(JVARI-1+NP4+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I)/4.D0
  500         CONTINUE
            END IF
  510     CONTINUE
        ELSE IF (NOMTE.EQ.'MEC3TR7H') THEN
          DO 540 IC = 4,NB2
            NPO = (IC-1)*NBVARI
            IF (IC.EQ.4) THEN
              NP1 = 0
              NP2 = NBVARI
            ELSE IF (IC.EQ.5) THEN
              NP1 = NBVARI
              NP2 = 2*NBVARI
            ELSE IF (IC.EQ.6) THEN
              NP1 = 2*NBVARI
              NP2 = 0
            ELSE IF (IC.EQ.7) THEN
              NP1 = 0
              NP2 = NBVARI
              NP3 = 2*NBVARI
            END IF
            IF (IC.NE.7) THEN
              DO 520 I = 1,NBVARI
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NP1+I) +
     &                              ZR(JVARI-1+NP2+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I)/2.D0
  520         CONTINUE
            ELSE
              DO 530 I = 1,NBVARI
                ZR(JVARI-1+NPO+I) =ZR(JVARI-1+NP1+I)+ZR(JVARI-1+NP2+I)
                ZR(JVARI-1+NPO+I) =ZR(JVARI-1+NPO+I)+ZR(JVARI-1+NP3+I)
                ZR(JVARI-1+NPO+I) = ZR(JVARI-1+NPO+I)/3.D0
  530         CONTINUE
            END IF
  540     CONTINUE
        END IF
      END IF
      END
