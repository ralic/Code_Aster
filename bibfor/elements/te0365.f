      SUBROUTINE TE0365(OPTION,NOMTE)
      IMPLICIT   NONE
      CHARACTER*16 OPTION,NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/06/2006   AUTEUR MABBAS M.ABBAS 
C TOLE CRP_20
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
C
C  CALCUL DES SECONDS MEMBRES DE CONTACT ET DE FROTTEMENT DE COULOMB STD
C        AVEC LA METHODE CONTINUE DE L'ECP
C  OPTION : 'CHAR_MECA_CONT' (CALCUL DU SECOND MEMBRE  DE CONTACT)
C   'CHAR_MECA_FROT' (CALCUL DU SECOND MEMBRE DE FROTTEMENT STANDARD )
C
C  ENTREES  ---> OPTION : OPTION DE CALCUL
C           ---> NOMTE  : NOM DU TYPE ELEMENT
C
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
      REAL*8  COEFFF,COEFFA,COEFCA,DT,COEASP,ASP,CN

      INTEGER NNE,NNM,NDIM,INDCO,INADH,INDASP,INDCOM,INDM
      INTEGER IGEOM,IDEPL,IVECT,JPCF,IDEPM,NDDL,IACCM,IVITM
      INTEGER MGEOM,MDEPL,TYPFRO,IFORM,DIRLG,CMP
      INTEGER INI1,INI2,INI3,INDNOR,INDRAC
      INTEGER INDNOB,INDNOQ,IMA,IMABAR,TYPBAR
      REAL*8 XC,YC,PDS,ERR,ESS,JAC,JACM,JEU,LAMBDA,MERESE,VTMP(81)
      REAL*8 C(3,3),FFE(9),FFM(9),TAU1(3),TAU2(3),NORM(3),INTER(2)
      REAL*8 GEOME(3),DEPLE(6),DEPLM(6),GEOMM(3),RESE(3),VECTT(3),NOOR
      REAL*8 MCH(72,72), M(3,3),MM1(3,3),GAUS,ORD,TT(3)
      REAL*8 DEPLME(6),DEPLMM(6),DEPLEE(6),JDEPP,JDEPM,JEACCM 
      REAL*8 BETA,GAMMA,JEUSUP
      REAL*8 ACCME(6), ACCMM(6),VITME(6), VITMM(6), JEVITM, JEVITP   
      CHARACTER*8 ESC,MAIT

      CALL JEMARQ()
C***************************************************
C      BETA   = 0.3025D0
C      GAMMA  = 0.6D0
C***************************************************
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
      ELSE IF (NOMTE.EQ.'CFQ4T6') THEN
        ESC = 'QU4'
        NNM = 6
        NNE = 4
        NDDL = 42
        NDIM = 3
        MAIT = 'TR6'  
      ELSE IF (NOMTE.EQ.'CFT3Q4') THEN
        ESC = 'TR3'
        NNM = 4
        NNE = 3
        NDDL = 30
        NDIM = 3
        MAIT = 'QU4'
      ELSE IF (NOMTE.EQ.'CFT3Q8') THEN
        ESC = 'TR3'
        NNM = 8
        NNE = 3
        NDDL = 42
        NDIM = 3
        MAIT = 'QU8'  
      ELSE IF (NOMTE.EQ.'CFQ9Q9') THEN
        ESC = 'QU9'
        NNM = 9
        NNE = 9
        NDDL = 81
        NDIM = 3
        MAIT = 'QU9'
        
C ---- CONTACT POUTRE-POUTRE

      ELSEIF (NOMTE .EQ. 'CFP2P2') THEN
        ESC = 'SG2'
        NNM = 2
        NNE = 2
        NDDL = 18
        NDIM = 3
        MAIT = 'SG2'
        
      ELSE
        CALL UTMESS('F','TE0364', 'NOM DE L ELEMENT INCONNU')
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
      DT      = ZR(JPCF-1+20)
      IFORM   = NINT(ZR(JPCF-1+21))   
      INDM     = NINT(ZR(JPCF-1+22))
      INI1     = NINT(ZR(JPCF-1+23))
      INI2     = NINT(ZR(JPCF-1+24)) 
      INI3     = NINT(ZR(JPCF-1+25)) 
      INDASP   = NINT(ZR(JPCF-1+26)) 
      INDCOM   = NINT(ZR(JPCF-1+27))   
      ASP      = ZR(JPCF-1+28)
      COEASP   = ZR(JPCF-1+29)
      CN       = ZR(JPCF-1+30) 
      BETA     = ZR(JPCF-1+31) 
      GAMMA    = ZR(JPCF-1+32)
      JEUSUP   = ZR(JPCF-1+33)
      IMA      = NINT(ZR(JPCF-1+34))
      IMABAR   = NINT(ZR(JPCF-1+35))
      INDNOB   = NINT(ZR(JPCF-1+36))
      INDNOQ   = NINT(ZR(JPCF-1+37))
      TYPBAR   = NINT(ZR(JPCF-1+38))
      INDRAC   = NINT(ZR(JPCF-1+39))

      IF (INDM .GE. 1) THEN
        IF (NDIM .EQ. 2) THEN
          DIRLG = 1
        ELSE
          DIRLG = 1
        END IF
      END IF
      
C  RECUPERARTION DE LA GEOMETRIE ET DE CHAMPS DE DEPLACEMENT

      CALL JEVECH('PGEOMER','E',IGEOM)
      CALL JEVECH('PDEPL_P','E',IDEPL)
      CALL JEVECH('PDEPL_M','L',IDEPM)
      IF (INDCOM .EQ. 1) THEN
      CALL JEVECH('PDEPLAR','L',IVITM)
      CALL JEVECH('PCHDYNR','L',IACCM)
      END IF

C  RECUPERATION DES VECTEURS 'OUT' (A REMPLIR => MODE ECRITURE)

      CALL JEVECH('PVECTUR','E',IVECT)
C TRAITEMENT EN FOND DE FISSURE      
      IF (INDNOB .GT. 0) THEN
        INDCO = 1
      END IF
C
C TRAITEMENT DU RACCORD SURFACIQUE     
      IF (INDRAC .GT. 0) THEN
        INDCO = 1
      END IF
C

C  REACTUALISATION DE LA GEOMETRIE AVEC DEPMPOI (ESCALVE)

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
      
      IF (IMA .EQ. IMABAR) THEN
      CALL CALFFL(ESC,ORD,GAUS,IGEOM,FFE,JAC,AXIS,NDIM,TYPBAR)
      ELSE
      CALL CALFFJ(ESC,ORD,GAUS,IGEOM,FFE,JAC,AXIS,NDIM)
      END IF
C
      MGEOM = IGEOM + NNE*NDIM

      CALL CALFFJ(MAIT,ERR,ESS,MGEOM,FFM,JACM,AXIS,NDIM)
        IF (NDIM.EQ.2) THEN
          NORM(1) =-TAU1(2)
          NORM(2) =TAU1(1)
          NORM(3) =0.D0
        ELSE IF (NDIM.EQ.3) THEN
         CALL PROVEC(TAU2,TAU1,NORM)
        END IF
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
        DEPLME(I)=0.D0
        VITME(I) =0.D0
        ACCME(I) =0.D0
        DO 114 J = 1,NNE
          DEPLE(I) = DEPLE(I) +FFE(J)*ZR(IDEPL+(J-1)*
     &                              (2*NDIM)+I-1)
          DEPLME(I) = DEPLME(I) +FFE(J)*ZR(IDEPM+(J-1)*
     &                              (2*NDIM)+I-1)   
          IF (INDCOM .EQ. 1) THEN
          VITME(I) = VITME(I) +FFE(J)*ZR(IVITM+(J-1)*
     &                              (2*NDIM)+I-1)
          ACCME(I) = ACCME(I) +FFE(J)*ZR(IACCM+(J-1)*
     &                              (2*NDIM)+I-1) 
          END IF    
     
114     CONTINUE
213   CONTINUE

C TRAITEMENT FOND DE FISSURE    
      IF (IMA .EQ. IMABAR) THEN
        IF (INDNOB .GT. 0) THEN
          IF (INDNOB .LE. 4) THEN
            DEPLE(NDIM+1) = ZR(IDEPL+(INDNOQ-1)*(2*NDIM)+NDIM+1-1)
            ZR(IDEPL+(INDNOB-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
          ELSEIF (INDNOB .GT. 4) THEN  
            IF (INDNOQ .EQ. 2) THEN
              DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(1-1)*(2*NDIM)+NDIM) + 
     &                               ZR(IDEPL+(2-1)*(2*NDIM)+NDIM))
              ZR(IDEPL+(INDNOB-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
            ELSEIF (INDNOQ .EQ. 4) THEN
              DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(1-1)*(2*NDIM)+NDIM) + 
     &                               ZR(IDEPL+(4-1)*(2*NDIM)+NDIM))  
              ZR(IDEPL+(INDNOB-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
            ELSEIF (INDNOQ .EQ. 6) THEN
              DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(3-1)*(2*NDIM)+NDIM) + 
     &                               ZR(IDEPL+(2-1)*(2*NDIM)+NDIM))
              ZR(IDEPL+(INDNOB-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
            ELSEIF (INDNOQ .EQ. 12) THEN
              DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(3-1)*(2*NDIM)+NDIM) + 
     &                               ZR(IDEPL+(4-1)*(2*NDIM)+NDIM))
              ZR(IDEPL+(INDNOB-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
            END IF   
          END IF
        END IF
      END IF
C
C TRAITEMENT DU RACCORD SURFACIQUE 
      IF (INDRAC .GT. 0) THEN
        IF (INDRAC .EQ. 5) THEN
          DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(1-1)*(2*NDIM)+NDIM) + 
     &                           ZR(IDEPL+(2-1)*(2*NDIM)+NDIM))
          ZR(IDEPL+(INDRAC-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
        ELSEIF (INDRAC .EQ. 6) THEN
          DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(2-1)*(2*NDIM)+NDIM) + 
     &                           ZR(IDEPL+(3-1)*(2*NDIM)+NDIM))  
          ZR(IDEPL+(INDRAC-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
        ELSEIF (INDRAC .EQ. 7) THEN
          DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(3-1)*(2*NDIM)+NDIM) + 
     &                           ZR(IDEPL+(4-1)*(2*NDIM)+NDIM))
          ZR(IDEPL+(INDRAC-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
        ELSEIF (INDRAC .EQ. 8) THEN
          DEPLE(NDIM+1) = 0.5D0*(ZR(IDEPL+(4-1)*(2*NDIM)+NDIM) + 
     &                           ZR(IDEPL+(1-1)*(2*NDIM)+NDIM))
          ZR(IDEPL+(INDRAC-1)*(2*NDIM)+NDIM+1-1) = DEPLE(NDIM+1)
        END IF
      END IF
C
C  --MAITRE------------

      DO 214 I = 1,NDIM
        DEPLM(I)=0.D0
        DEPLMM(I)=0.D0 
        VITMM(I) =0.D0
        ACCMM(I) =0.D0       
        DO 117 J = 1,NNM
          DEPLM(I) = DEPLM(I) +FFM(J)*ZR(IDEPL+NNE*(2*NDIM)+(J-1)*
     &                                                NDIM+I-1)
          DEPLMM(I) = DEPLMM(I) +FFM(J)*ZR(IDEPM+NNE*(2*NDIM)+(J-1)*
     &                                                NDIM+I-1)  
          IF (INDCOM .EQ. 1) THEN
          VITMM(I) = VITMM(I) +FFM(J)*ZR(IVITM+NNE*(2*NDIM)+(J-1)*
     &                                                NDIM+I-1)
          ACCMM(I) = ACCMM(I) +FFM(J)*ZR(IACCM+NNE*(2*NDIM)+(J-1)*
     &                                                NDIM+I-1)
          END IF
117     CONTINUE
214   CONTINUE

      IF (OPTION.EQ.'CHAR_MECA_CONT') THEN
      IF (INDNOR .EQ. 1) INDCO = 0

        IF (INDASP.EQ.0) THEN
C
C   ---- PAS DE CONTACT
C
         IF (IFORM .EQ. 1) THEN
          DO 40 I = 1,NNE
          VTMP((I-1)*(2*NDIM)+NDIM+1) =-PDS*JAC*DEPLE(NDIM+1)*
     &        FFE(I)/COEFCA
   40     CONTINUE
         ELSE
          DO 41 I = 1,NNE
          VTMP((I-1)*(2*NDIM)+NDIM+1) =-PDS*JAC*DEPLE(NDIM+1)*
     &        FFE(I)*DT/COEFCA
   41     CONTINUE
         ENDIF
         
          GO TO 220

C   ---- CONTACT

        ELSE IF (INDASP.EQ.1) THEN
             IF (INDCO.EQ.1) THEN

C  EVALUTION DU JEU

          JEU = JEUSUP
          JDEPP = 0.D0
          JDEPM = 0.D0  
          JEVITM = 0.D0 
          JEACCM = 0.D0
                 
          DO 50 K = 1,NDIM
            JEU = JEU + (GEOME(K)+DEPLE(K)-GEOMM(K)-DEPLM(K))*NORM(K)
            JDEPP = JDEPP + (DEPLE(K)-DEPLM(K))*NORM(K)            
            JDEPM = JDEPM + (DEPLME(K)-DEPLMM(K))*NORM(K)
            IF (INDCOM .EQ. 1) THEN
            JEVITM = JEVITM + (VITME(K)-VITMM(K))*NORM(K)
            JEACCM = JEACCM + (ACCME(K)-ACCMM(K))*NORM(K)
            END IF
   50     CONTINUE
             IF (INDCOM .EQ. 1) THEN
             JEVITP= (JDEPP-JDEPM)*GAMMA/(BETA*DT) +
     &                JEVITM*(BETA-GAMMA)/BETA +
     &                JEACCM*DT*(2*BETA-GAMMA)/(2*BETA)
             END IF
  
C FORMULATION EN DEPLACEMENT

        IF (IFORM .EQ. 1) THEN

C       DDL DEPLACEMENT DE LA  SURFACE CINEMATIQUE

          DO 70 I = 1,NNE
            DO 60 J = 1,NDIM
              VTMP((I-1)*(2*NDIM)+J) = -PDS*JAC*
     &  (DEPLE(NDIM+1)-COEFCA*(JEU)-(COEASP*INDCOM*
     &   (JEU-ASP)**2))*FFE(I)*NORM(J)
   60       CONTINUE
   70     CONTINUE
C
C      DDL DEPLACEMENT DE LA SURFACE GEOMETRIQUE

          DO 90 I = 1,NNM
            DO 80 J = 1,NDIM
              VTMP(NNE*(2*NDIM)+(I-1)*(NDIM)+J) = PDS*JAC*
     &        (DEPLE(NDIM+1)-COEFCA*(JEU)-(COEASP*INDCOM*
     &        (JEU-ASP)**2))*FFM(I)*NORM(J)
   80       CONTINUE
   90     CONTINUE

C
C      DDL MULTIPLICATEUR CONTACT (DE LA SURFACE CINEMATIQUE)

          DO 100 I = 1,NNE
            VTMP((I-1)*(2*NDIM)+NDIM+1) = -PDS*JAC*JEU*FFE(I)
  100     CONTINUE

C TRAITEMENT EN FOND DE FISSURE  
          IF (IMA .EQ. IMABAR) THEN
            IF (INDNOB .GT. 0) THEN
              DO 222 I = INDNOB,INDNOB
                VTMP((I-1)*(2*NDIM)+NDIM+1) = -PDS*JAC*JEU*0.D0
  222         CONTINUE
            END IF
          END IF  
C
C TRAITEMENT DE RACCORD SURFACIQUE 
          IF (INDRAC .GT. 0) THEN
              DO 223 I = INDRAC,INDRAC
                VTMP((I-1)*(2*NDIM)+NDIM+1) = -PDS*JAC*JEU*0.D0
  223         CONTINUE
            END IF
C
C  
C FORMULATION EN VITESSE
          ELSE
          
C       DDL DEPLACEMENT DE LA  SURFACE CINEMATIQUE

          DO 71 I = 1,NNE
            DO 61 J = 1,NDIM
              VTMP((I-1)*(2*NDIM)+J) = -PDS*JAC*
     &          (DEPLE(NDIM+1)-COEFCA*(JDEPP-JDEPM)/DT)*
     &          FFE(I)*NORM(J)
   61       CONTINUE
   71     CONTINUE

C      DDL DEPLACEMENT DE LA SURFACE GEOMETRIQUE

          DO 91 I = 1,NNM
            DO 81 J = 1,NDIM
              VTMP(NNE*(2*NDIM)+(I-1)*(NDIM)+J) = PDS*JAC*
     &          (DEPLE(NDIM+1)-COEFCA*(JDEPP-JDEPM)/DT)*
     &          FFM(I)*NORM(J)
   81       CONTINUE
   91     CONTINUE

C      DDL MULTIPLICATEUR CONTACT (DE LA SURFACE CINEMATIQUE)

          DO 105 I = 1,NNE
            VTMP((I-1)*(2*NDIM)+NDIM+1) = -PDS*JAC*FFE(I)*
     &                                    (JDEPP-JDEPM) 
  105     CONTINUE
  
        ENDIF          
        ELSE IF (INDCO.EQ.0) THEN
          IF (IFORM .EQ. 1) THEN
          DO 42 I = 1,NNE
          VTMP((I-1)*(2*NDIM)+NDIM+1) =-PDS*JAC*DEPLE(NDIM+1)*
     &        FFE(I)/COEFCA
   42     CONTINUE
         ELSE
          DO 43 I = 1,NNE
          VTMP((I-1)*(2*NDIM)+NDIM+1) =-PDS*JAC*DEPLE(NDIM+1)*
     &        FFE(I)*DT/COEFCA
   43     CONTINUE
         ENDIF
         
         IF (INDCOM .EQ. 1) THEN
         
C  EVALUTION DU JEU

          JEU = JEUSUP
          JDEPP = 0.D0
          JDEPM = 0.D0  
          JEVITM = 0.D0 
          JEACCM = 0.D0  
          
          DO 51 K = 1,NDIM
            JEU = JEU+(GEOME(K)+DEPLE(K)-GEOMM(K)-DEPLM(K))*NORM(K)
            JDEPP = JDEPP+(DEPLE(K)-DEPLM(K))*NORM(K)            
            JDEPM = JDEPM+(DEPLME(K)-DEPLMM(K))*NORM(K)
            IF (INDCOM .EQ. 1) THEN
            JEVITM = JEVITM+(VITME(K)-VITMM(K))*NORM(K)
            JEACCM = JEACCM+(ACCME(K)-ACCMM(K))*NORM(K)
            END IF
   51     CONTINUE
            
            IF (INDCOM .EQ. 1) THEN
             JEVITP= (JDEPP-JDEPM)*GAMMA/(BETA*DT) +
     &                JEVITM*(BETA-GAMMA)/BETA +
     &                JEACCM*DT*(2*BETA-GAMMA)/(2*BETA)
            END IF

C  DDL DEPLACEMENT DE LA  SURFACE CINEMATIQUE

          DO 72 I = 1,NNE
            DO 62 J = 1,NDIM
              VTMP((I-1)*(2*NDIM)+J) = -PDS*JAC*INDCOM*
     &          (-COEASP*((JEU-ASP)**2)-CN*JEVITP)*FFE(I)*NORM(J)
   62       CONTINUE
   72     CONTINUE

C  DDL DEPLACEMENT DE LA SURFACE GEOMETRIQUE

          DO 92 I = 1,NNM
            DO 82 J = 1,NDIM
              VTMP(NNE*(2*NDIM)+(I-1)*(NDIM)+J) = PDS*JAC*INDCOM*
     &          (-COEASP*((JEU-ASP)**2)-CN*JEVITP)*FFM(I)*NORM(J)
   82       CONTINUE
   92     CONTINUE

        ENDIF  
        ENDIF  

          GO TO 220
        ELSE
          CALL JXABOR()
        END IF

      ELSE IF (OPTION.EQ.'CHAR_MECA_FROT') THEN


        IF (COEFFF.EQ.0.D0) INDCO = 0
        IF (TYPFRO.NE.3)    INDCO = 0
        IF (LAMBDA.EQ.0.D0) INDCO = 0
        IF (INDNOR .EQ. 1)  INDCO = 0

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

C   ON CALCULE L'ETAT DE CONTACT ADHERENT OU GLISSANT
C   INADH = 1  ADHERENCE
C   INADH = 0  GLISSEMENT

          CALL TTPRSM(NDIM,C,DEPLE,DEPLM,COEFFA,INADH,RESE,TAU1,TAU2,
     &                INDM)

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
              IF ((INDM.EQ.3.D0).AND.(J.EQ.DIRLG)) THEN
                IF ((I.EQ.INI1).OR.(I.EQ.INI2).OR.(I.EQ.INI3)) THEN
                  VTMP((I-1)*2*NDIM+NDIM+1+J) = 0.D0
                ELSE
                  VTMP((I-1)*2*NDIM+NDIM+1+J) =
     &            JAC*PDS*COEFFF*INTER(J)*LAMBDA*FFE(I)/COEFFA
                END IF 
              ELSEIF ((INDM.EQ.2.D0).AND.(J.EQ.DIRLG)) THEN
                IF ((I.EQ.INI1).OR.(I.EQ.INI2)) THEN
                  VTMP((I-1)*2*NDIM+NDIM+1+J) = 0.D0
                ELSE
                  VTMP((I-1)*2*NDIM+NDIM+1+J) =
     &            JAC*PDS*COEFFF*INTER(J)*LAMBDA*FFE(I)/COEFFA
                END IF 
              ELSEIF ((INDM.EQ.1.D0).AND.(J.EQ.DIRLG)) THEN
                IF (I.EQ.INI1) THEN
                  VTMP((I-1)*2*NDIM+NDIM+1+J) = 0.D0
                ELSE
                  VTMP((I-1)*2*NDIM+NDIM+1+J) =
     &            JAC*PDS*COEFFF*INTER(J)*LAMBDA*FFE(I)/COEFFA
                END IF 
              ELSE
                 VTMP((I-1)*2*NDIM+NDIM+1+J) =
     &           JAC*PDS*COEFFF*INTER(J)*LAMBDA*FFE(I)/COEFFA
              END IF
 3201       CONTINUE
 3211     CONTINUE
        ELSE
          CALL JXABOR
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
