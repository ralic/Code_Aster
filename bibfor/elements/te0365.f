      SUBROUTINE TE0365(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C TOLE CRP_20
C.......................................................................
C  CALCUL DES SECONDS MEMBRES DE CONTACT ET DE FROTTEMENT DE COULOMB STD
C        AVEC LA METHODE CONTINUE DE L'ECP
C  OPTION : 'CHAR_MECA_CONT' (CALCUL DU SECOND MEMBRE  DE CONTACT)
C   'CHAR_MECA_FROT' (CALCUL DU SECOND MEMBRE DE FROTTEMENT STANDARD )
C
C  ENTREES  ---> OPTION : OPTION DE CALCUL
C           ---> NOMTE  : NOM DU TYPE ELEMENT
C
C.......................................................................
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

      INTEGER I,J,K,L,AXIS
      REAL*8  COEFFF,COEFFA,COEFCA

      INTEGER NNE,NNM,NDIM,INDCO,INADH
      INTEGER IGEOM,IDEPL,IVECT,JPCF,IDEPM,NDDL
      INTEGER MGEOM,MDEPL,TYPFRO ,INDNOR

      REAL*8 XC,YC,PDS,ERR,ESS,JAC,JACM,JEU,LAMBDA,MERESE,VTMP(81)
      REAL*8 C(3,3),FFE(9),FFM(9),TAU1(3),TAU2(3),NORM(3),INTER(2),TT(3)
      REAL*8 GEOME(3),DEPLE(6),DEPLM(6),GEOMM(3),RESE(3),VECTT(3),NOOR
      REAL*8 MCH(72,72), M(3,3),MM1(3,3),DEPLEE(3),DEPLMM(3),GAUS,ORD
      CHARACTER*8 ESC,MAIT

C.......................................................................

      CALL JEMARQ()
C
      IF (NOMTE.EQ.'CFS2S2') THEN
        ESC = 'SG2'
        NNM = 2
        NNE = 2
        NDDL = 12
        NDIM = 2
        MAIT = 'SG2'
      ELSE IF (NOMTE.EQ.'CFS2S3') THEN
        ESC = 'SG2'
        NNM = 3
        NNE = 2
        NDDL = 14
        NDIM = 2
        MAIT = 'SG3'

      ELSE IF (NOMTE.EQ.'CFS3S2') THEN
        ESC = 'SG3'
        NNM = 2
        NNE = 3
        NDDL = 16
        NDIM = 2
        MAIT = 'SG2'
      ELSE IF (NOMTE.EQ.'CFS3S3') THEN
        ESC = 'SG3'
        NNM = 3
        NNE = 3
        NDDL = 18
        NDIM = 2
        MAIT = 'SG3'
      ELSE IF (NOMTE.EQ.'CFT3T3') THEN
        ESC = 'TR3'
        NNM = 3
        NNE = 3
        NDDL = 27
        NDIM = 3
        MAIT = 'TR3'
      ELSE IF (NOMTE.EQ.'CFT3T6') THEN
        ESC = 'TR3'
        NNM = 6
        NNE = 3
        NDDL = 36
        NDIM = 3
        MAIT = 'TR6'
      ELSE IF (NOMTE.EQ.'CFT6T3') THEN
        ESC = 'TR6'
        NNM = 3
        NNE = 6
        NDDL = 45
        NDIM = 3
        MAIT = 'TR3'
      ELSE IF (NOMTE.EQ.'CFT6T6') THEN
        ESC = 'TR6'
        NNM = 6
        NNE = 6
        NDDL = 54
        NDIM = 3
        MAIT = 'TR6'
      ELSE IF (NOMTE.EQ.'CFQ4Q4') THEN
        ESC = 'QU4'
        NNM = 4
        NNE = 4
        NDDL = 36
        NDIM = 3
        MAIT = 'QU4'
      ELSE IF (NOMTE.EQ.'CFQ4Q8') THEN
        ESC = 'QU4'
        NNM = 8
        NNE = 4
        NDDL = 48
        NDIM = 3
        MAIT = 'QU8'

      ELSE IF (NOMTE.EQ.'CFQ8Q4') THEN
        ESC = 'QU8'
        NNM = 4
        NNE = 8
        NDDL = 60
        NDIM = 3
        MAIT = 'QU4'
      ELSE IF (NOMTE.EQ.'CFQ8Q8') THEN
        ESC = 'QU8'
        NNM = 8
        NNE = 8
        NDDL = 72
        NDIM = 3
        MAIT = 'QU8'
      ELSE IF (NOMTE.EQ.'CFQ8T6') THEN
        ESC = 'QU8'
        NNM = 6
        NNE = 8
        NDDL = 66
        NDIM = 3
        MAIT = 'TR6'
      ELSE IF (NOMTE.EQ.'CFT6Q8') THEN
        ESC = 'TR6'
        NNM = 8
        NNE = 6
        NDDL = 60
        NDIM = 3
        MAIT = 'QU8'
      ELSE IF (NOMTE.EQ.'CFQ4T3') THEN
        ESC = 'QU4'
        NNM = 3
        NNE = 4
        NDDL = 33
        NDIM = 3
        MAIT = 'TR3'
      ELSE IF (NOMTE.EQ.'CFT3Q4') THEN
        ESC = 'TR3'
        NNM = 4
        NNE = 3
        NDDL = 30
        NDIM = 3
        MAIT = 'QU4'
      ELSE IF (NOMTE.EQ.'CFQ9Q9') THEN
        ESC = 'QU9'
        NNM = 9
        NNE = 9
        NDDL = 81
        NDIM = 3
        MAIT = 'QU9'
      ELSE
        CALL UTMESS('F','TE0365', 'NOM DE L ELEMENT INCONNU')
      END IF

C  RECUPERATION DES DONNEES PORTEES PAR LA CARTE

      CALL JEVECH('PCONFR','L',JPCF)
      ORD = ZR(JPCF-1+1)
      ERR = ZR(JPCF-1+2)
      ESS = ZR(JPCF-1+3)
      TAU1(1) = ZR(JPCF-1+4)
      TAU1(2) = ZR(JPCF-1+5)
      TAU1(3) = ZR(JPCF-1+6)
      TAU2(1) = ZR(JPCF-1+7)
      TAU2(2) = ZR(JPCF-1+8)
      TAU2(3) = ZR(JPCF-1+9)
      GAUS    = ZR(JPCF-1+10)
      INDCO   = NINT(ZR(JPCF-1+11))
      LAMBDA  = ZR(JPCF-1+12)
      COEFCA  = ZR(JPCF-1+13)
      COEFFA  = ZR(JPCF-1+14)
      COEFFF  = ZR(JPCF-1+15)
      TYPFRO  = NINT(ZR(JPCF-1+16))
      INDNOR  = NINT(ZR(JPCF-1+17))
      AXIS    = NINT(ZR(JPCF-1+18))
      PDS     = ZR(JPCF-1+19)
      INDNOR=1

      IF (INDNOR.EQ.1) GOTO 4
4     CONTINUE
C  RECUPERARTION DE LA GEOMETRIE ET DE CHAMPS DE DEPLACEMENT

      CALL JEVECH('PGEOMER','E',IGEOM)
      CALL JEVECH('PDEPL_P','L',IDEPL)
      CALL JEVECH('PDEPL_M','L',IDEPM)


C  RECUPERATION DES VECTEURS 'OUT' (A REMPLIR => MODE ECRITURE)

      CALL JEVECH('PVECTUR','E',IVECT)

C     REACTUALISATION DE LA GEOMETRIE AVEC DEPMPOI (ESCALVE)

      DO 20 I = 1,NNE
        DO 10 J = 1,NDIM
          ZR(IGEOM+(I-1)*NDIM+J-1) = ZR(IGEOM+(I-1)*NDIM+J-1) +
     &                               ZR(IDEPM+(I-1)*(2*NDIM)+J-1)
   10   CONTINUE
   20 CONTINUE

      DO 21 I = 1,NNM
        DO 11 J = 1,NDIM
           ZR(IGEOM+NNE*NDIM+(I-1)*NDIM+J-1)=
     &     ZR(IGEOM+NNE*NDIM+(I-1)*NDIM+J-1)+
     &           ZR(IDEPM+NNE*(2*NDIM)+(I-1)*NDIM+J-1)
   11   CONTINUE
   21 CONTINUE

C  INITIALISATION A ZERO DU VECTEUR DE TRAVAIL

      DO 30 I = 1,NDIM
        VECTT(I) = 0.D0
   30 CONTINUE

C  RECUPERATION DES COORDONNEES DES PCS, DES FFS, JACOBIENS ET NORMALES

      
      CALL CALFFJ(ESC,ORD,GAUS,IGEOM,FFE,JAC,AXIS)
C
      MGEOM = IGEOM + NNE*NDIM

      CALL CALFFJ(MAIT,ERR,ESS,MGEOM,FFM,JACM,AXIS)
C      IF (INDNOR.EQ.0) THEN
        IF (NDIM.EQ.2) THEN
          NORM(1) =-TAU1(2)
          NORM(2) =TAU1(1)
          NORM(3) =0.D0
        ELSE IF (NDIM.EQ.3) THEN
         CALL PROVEC(TAU2,TAU1,NORM)
        END IF
C       ELSE IF (INDNOR.EQ.1) THEN
C          IF (NDIM.EQ.2) THEN
C           NORM(1) =-TAU1(2)
C           NORM(2) =TAU1(1)
C           NORM(3) =0.D0
C          ELSE IF (NDIM.EQ.3) THEN
C           CALL PROVEC(TAU2,TAU1,NORM)
C          END IF
C      END IF
      NOOR = 0.D0
      DO 1 I=1,3
        NOOR = NORM(I)*NORM(I)+NOOR
1     CONTINUE
      DO 2 I=1,3
        NORM(I) = NORM(I)/SQRT(NOOR)
2     CONTINUE



       DO 3 J=1,NDDL
           VTMP(J)=0.D0
3      CONTINUE

C  CALCUL DE LA GEOMETRIE DU PC ET SON VISAVIS

C  --ESCLAVE------------------------------------------------
      DO 211 I = 1,NDIM
        GEOME(I) = 0.D0
        DO 112 J = 1,NNE
          GEOME(I) = GEOME(I) +FFE(J)*ZR(IGEOM+(J-1)*NDIM+I-1)
112     CONTINUE
211   CONTINUE

C  --MAITRE-------------------------------------------------

      DO 212 I = 1,NDIM
        GEOMM(I) = 0.D0
        DO 113 J = 1,NNM
          GEOMM(I) = GEOMM(I) +FFM(J)*ZR(IGEOM+NNE*NDIM+
     & (J-1)*NDIM+I-1)
113    CONTINUE
212     CONTINUE

C  CALCUL DU DEPLACEMENT

C  --ESCLAVE-----------

      DO 213 I = 1,2*NDIM
        DEPLE(I)=0.D0
        DO 114 J = 1,NNE
          DEPLE(I) = DEPLE(I) +FFE(J)*ZR(IDEPL+(J-1)*
     &                              (2*NDIM)+I-1)
114     CONTINUE
213   CONTINUE

C  --MAITRE------------

      DO 214 I = 1,NDIM
        DEPLM(I)=0.D0
        DO 117 J = 1,NNM
          DEPLM(I) = DEPLM(I) +FFM(J)*ZR(IDEPL+NNE*(2*NDIM)+(J-1)*
     &                                                NDIM+I-1)
117     CONTINUE
214   CONTINUE

      IF (OPTION.EQ.'CHAR_MECA_CONT') THEN

        IF (INDCO.EQ.0) THEN
C
C   ---- PAS DE CONTACT
C
          DO 40 I = 1,NNE
          VTMP((I-1)*(2*NDIM)+NDIM+1) =-PDS*JAC*DEPLE(NDIM+1)*
     &        FFE(I)/COEFCA
   40     CONTINUE

          GO TO 220

C   ---- CONTACT

        ELSE IF (INDCO.EQ.1) THEN

C  EVALUTION DU JEU

          JEU = 0.D0

          DO 50 K = 1,NDIM
            JEU = JEU + (GEOME(K)+DEPLE(K)-GEOMM(K)-DEPLM(K))*NORM(K)
   50     CONTINUE


C       DDL DEPLACEMENT DE LA  SURFACE CINEMATIQUE

          DO 70 I = 1,NNE
            DO 60 J = 1,NDIM
              VTMP((I-1)*(2*NDIM)+J) = -PDS*JAC*
     &          (DEPLE(NDIM+1)-COEFCA*JEU)*FFE(I)*NORM(J)
   60       CONTINUE
   70     CONTINUE
C
C      DDL DEPLACEMENT DE LA SURFACE GEOMETRIQUE

          DO 90 I = 1,NNM
            DO 80 J = 1,NDIM
              VTMP(NNE*(2*NDIM)+(I-1)*(NDIM)+J) = PDS*JAC*
     &          (DEPLE(NDIM+1)-COEFCA*JEU)*FFM(I)*NORM(J)
   80       CONTINUE
   90     CONTINUE

C
C      DDL MULTIPLICATEUR CONTACT (DE LA SURFACE CINEMATIQUE)

          DO 100 I = 1,NNE
            VTMP((I-1)*(2*NDIM)+NDIM+1) = -PDS*JAC*JEU*FFE(I)
  100     CONTINUE

          GO TO 220
        ELSE
          CALL JXABOR()
        END IF

      ELSE IF (OPTION.EQ.'CHAR_MECA_FROT') THEN


        IF (COEFFF.EQ.0.D0) INDCO = 0
        IF (TYPFRO.NE.3)  INDCO = 0
        IF (LAMBDA.EQ.0.D0)  INDCO = 0

        IF (INDCO.EQ.0) THEN
C       --------------------

          DO 305 I = 1,3
             TT(I) = 0.D00
 305      CONTINUE

        IF (NDIM.EQ.2) THEN
                DO 301 K = 1,NDIM
                  TT(1) = TAU1(K)*TAU1(K) +TT(1)
 301           CONTINUE
                  TT(1) =DEPLE(NDIM+1+1)*TT(1)
                  TT(2) =0.D00
        ELSE IF (NDIM.EQ.3) THEN
                 DO 31 K = 1,NDIM
                  TT(1) = (DEPLE(NDIM+1+1)*TAU1(K)+ DEPLE(NDIM+1+2)
     +     *TAU2(K))*TAU1(K) +TT(1)
 31            CONTINUE
                 DO 32 K = 1,NDIM
                  TT(2) = (DEPLE(NDIM+1+1)*TAU1(K)+ DEPLE(NDIM+1+2)
     +     *TAU2(K))*TAU2(K) +TT(2)
 32            CONTINUE
        ELSE
            CALL UTMESS('F','TE0365','PROBLEME NDIM NI 2 NI 3')
        END IF


        DO 101 I=1,NNE
          DO 102 L=1,NDIM-1
          VTMP((I-1)*(2*NDIM)+NDIM+1+L)=-JAC*PDS*FFE(I)*TT(L)
  102     CONTINUE
  101   CONTINUE

          GO TO 220

        ELSE IF (INDCO.EQ.1) THEN
C       -------------------------

C  CALCUL DE C(I,J)

          DO 120 I = 1,NDIM
            DO 110 J = 1,NDIM
              C(I,J) = -1.D0*NORM(I)*NORM(J)
  110       CONTINUE
  120     CONTINUE

          DO 130 I = 1,NDIM
            C(I,I) = 1 + C(I,I)
  130     CONTINUE

C   ON CALCULE L'ETAT DE CONATCT ADHERENT OU GLISSANT
C   INADH = 1  ADHERENCE
C   INADH = 0  GLISSEMENT


          CALL TTPRSM(NDIM,C,DEPLE,DEPLM,COEFFA,INADH,RESE,TAU1,TAU2)

C       SI GLISSANT ON NORMALISE RESE

          IF (INADH.EQ.0) THEN
             MERESE = 0.D00
               DO 140 K = 1,NDIM
                  MERESE = RESE(K)*RESE(K) + MERESE
  140          CONTINUE
             MERESE = SQRT(MERESE)
               DO 150 K = 1,NDIM
                  RESE(K) = RESE(K)/MERESE
  150          CONTINUE
          END IF


C  EVALUTION RESE.C(*,I)

          DO 170 I = 1,NDIM
            DO 160 K = 1,NDIM
              VECTT(I) = RESE(K)*C(K,I) + VECTT(I)
  160       CONTINUE
  170     CONTINUE

C       DDL DEPLACEMENT DE LA  SURFACE CINEMATIQUE

          DO 190 I = 1,NNE
            DO 180 J = 1,NDIM
                VTMP((I-1)*(2*NDIM)+J) = -JAC*PDS*COEFFF*LAMBDA*
     &          VECTT(J)*FFE(I)
  180       CONTINUE
  190     CONTINUE

C       DDL DEPLACEMENT DE LA  SURFACE GEOMETRIQUE

          DO 210 I = 1,NNM
            DO 200 J = 1,NDIM
              VTMP(NNE*(2*NDIM)+(I-1)*NDIM+J) = JAC*PDS*COEFFF*
     &          LAMBDA*VECTT(J)*FFM(I)
  200       CONTINUE
  210     CONTINUE

C      DDL LAGRANGES FROTTEMENT
          IF (NDIM.EQ.2) THEN
          INTER(1)=0.D0
          DO 228 I = 1,NDIM
               INTER(1)=(DEPLE(NDIM+1+1)*TAU1(I)-RESE(I))*TAU1(I)
     &         +INTER(1)
  228     CONTINUE

          ELSE IF (NDIM.EQ.3)THEN
          INTER(1)=0.D00
          INTER(2)=0.D00
          DO 233 I = 1,NDIM
              INTER(1)=(DEPLE(NDIM+1+1)*TAU1(I)+DEPLE(NDIM+3)*TAU2(I)
     &        -RESE(I))*TAU1(I) +INTER(1)
              INTER(2)=(DEPLE(NDIM+1+1)*TAU1(I)+DEPLE(NDIM+3)*TAU2(I)
     &        -RESE(I))*TAU2(I) +INTER(2)
  233     CONTINUE
          ELSE
            CALL UTMESS('F','TE0365','STOP NDIM')
          END IF
          DO 3211 I = 1,NNE
            DO 3201 J = 1,NDIM-1
              VTMP((I-1)*2*NDIM+NDIM+1+J) = JAC*PDS*COEFFF*
     &              INTER(J)*LAMBDA*FFE(I)/COEFFA
 3201       CONTINUE
 3211     CONTINUE
        ELSE
          CALL JXABOR()
        END IF
      ELSE
        CALL UTMESS('F','TE0365_03','NOM D OPTION INCONNU')
      END IF
  220 CONTINUE

C  -------FIN

        DO 6 I=1,NDDL
            ZR(IVECT-1+I)=VTMP(I)
6       CONTINUE

        CALL JEDEMA()
      END
