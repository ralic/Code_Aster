       SUBROUTINE ZACIER (MATOS,NBHIST, FTRC, TRC, COEF, FMOD,CKM
     &             ,NBTRC,TPG0,TPG1,TPG2,DT10,DT21,TAMP,METAPG)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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

      IMPLICIT NONE


C-----------------------------------------------------------------------


C       EVOLUTION METALLURGIQUE POUR ACIER
C
C   - FONCTION :                                                       C
C       CALCUL DE Z(N+1) CONNAISSANT T(N), TPOINT(N), Z(N) ET T(N+1)
C   - ENTREES :
C       MATOS            : MATERIAU
C
C       NBHIST           : NBRE D HISTOIRES EXPERIMENTALE DE DEFI_TRC  C
C       FTRC(3*NBHIST,3) : VECTEUR DZ/DT EXPERIMENTAUX (VIDE EN ENTREE)C
C       TRC (3*NBHIST,5) : VECTEUR Z,T EXPERIMENTAUX (VIDE EN ENTREE)  C
C       FMOD(*)          : ENSEMBLE DES HISTOIRES EXPERIMENTALES       C
C       CKM(6*NBTRC)     : VECTEUR DES LOIS MS(Z) SEUIL,AKM,BKM,TPLM C
C                          ET TAILLE DE GRAIN AUSTENITIQUE DREF,A     C
C       NBTRC            : NBRE DE LOIS MS(Z)                          C
C       TPG0             : TEMPERATURE AU POINT DE GAUSS INSTANT N-1   C
C       TPG1             : TEMPERATURE AU POINT DE GAUSS INSTANT N     C
C       TPG2             : TEMPERATURE AU POINT DE GAUSS INSTANT N+1   C
C       DT10             : DELTATT ENTRE N-1 ET N                      C
C       DT21             : DELTATT ENTRE N  ET N+                      C
C       TAMP(7)          : PHASMETA(N) ZF,ZP,ZB,ZM,P,T,MS           C
C   - SORTIES :                                                        C
C       METAPG(7)          : PHASMETA(N+1) ZF,ZP,ZB,ZM,P,T,MS          C

C-----------------------------------------------------------------------


       REAL*8     METAPG(7),TAMP(7), TEMPO(7)
       INTEGER    MATOS,NBHIST, NBTRC
       REAL*8     FTRC((3*NBHIST),3), TRC((3*NBHIST),5), FMOD(*)
       REAL*8     CKM(6*NBTRC), COEF(*), DT10,DT21,TPG0,TPG1,TPG2
       REAL*8     LAMBD0,QSRK,D10,WSRK,LAMBDA,UNSURL,DMOINS,DD
       REAL*8     DLIM, DZ
C-----------------------------------------------------------------------


      CHARACTER*24       NOMRES(11)
      CHARACTER*2        CODRET(11)
      REAL*8             TPOINT,ZERO,TI,TPI
      REAL*8             AC1,AC3,TAUX1,TAUX3,ZEQ1,ZEQ2,Z1,Z2,AR3,EPSI
      REAL*8             CTES(11) ,UN
      REAL*8             ZEQ1I,ZEQ2I,TI1,TI2,TAUX,Z1I
      REAL*8             A, B, C, DELTA


      INTEGER            KP,I,J,K,NBPAS
      LOGICAL            LREFR

C-----------------------------------------------------------------------

         ZERO  = 0.D0
         EPSI  = 1.D-10
         UN    = 1.D0
C
C        --- DETERMINATION DU SENS DE L'EVOLUTION METALLURGIQUE ---
C
         NOMRES(1) = 'AR3'
         NOMRES(2) = 'ALPHA'
         NOMRES(3) = 'MS0'
         NOMRES(4) = 'AC1'
         NOMRES(5) = 'AC3'
         NOMRES(6) = 'TAUX_1'
         NOMRES(7) = 'TAUX_3'
         CALL RCVALA(MATOS,' ','META_ACIER', 1, 'INST', 0.D0, 7, NOMRES,
     &                 CTES, CODRET, 'FM' )
         AR3   = CTES(1)
         AC1   = CTES(4)
         AC3   = CTES(5)
         TAUX1 = CTES(6)
         TAUX3 = CTES(7)
C
         NOMRES(8) = 'LAMBDA0'
         NOMRES(9) = 'QSR_K'
         NOMRES(10)= 'D10'
         NOMRES(11)= 'WSR_K'
         CALL RCVALA(MATOS,' ','META_ACIER', 1, 'INST', 0.D0, 4,
     &                 NOMRES(8),CTES(8), CODRET(8), '  ' )

         IF (CODRET(8) .EQ. 'NO') CTES(8) = 0.D0
         IF (CODRET(9) .EQ. 'NO') CTES(9) = 0.D0
         IF (CODRET(10) .EQ. 'NO') CTES(10) = 0.D0
         IF (CODRET (11) .EQ. 'NO') CTES(11)=0.D0
         LAMBD0 = CTES(8)
         QSRK = CTES(9)
         D10 = CTES(10)
         WSRK = CTES(11)
         IF ((CODRET(8) .EQ.'OK') .AND. (CODRET(10) .EQ.'NO'))
     &   CALL U2MESS('F','ALGORITH11_73')

         METAPG(6) = TPG2
         METAPG(7) = TAMP(7)
         TPOINT = (TPG1-TPG0)/DT10




         Z1     = ZERO
         DO 209 J = 1,4
            Z1 = Z1+TAMP(J)
209      CONTINUE
         Z1   = UN - Z1
         ZEQ1 = MIN( (TPG1-AC1)/(AC3-AC1) , UN )
         ZEQ2 = MIN( (TPG2-AC1)/(AC3-AC1) , UN )
         IF ( TPOINT .GT. ZERO ) THEN
            LREFR = .FALSE.
         ELSEIF ( TPG2 .GT. AR3 ) THEN
            LREFR = .FALSE.
         ELSEIF ( TPG2 .LT. AC1 ) THEN
            LREFR = .TRUE.
         ELSEIF ( TPOINT .LT. ZERO ) THEN
            LREFR = .TRUE.
         ELSEIF ( Z1 .LE. ZEQ2 ) THEN
            LREFR = .FALSE.
         ELSE
            LREFR = .TRUE.
         ENDIF
         IF ( LREFR ) THEN


            IF ( ABS(TPG2-TPG1) .GT. 5.001D0 ) THEN
               NBPAS = INT(ABS(TPG2-TPG1)/5.D0-0.001D0)+1
               DT21  = DT21/DBLE(NBPAS)
               DO 33 J=1,7
               TEMPO(J) = TAMP(J)
33             CONTINUE
               DO 50 I=1,NBPAS
                  TI = TPG1+(TPG2-TPG1)*DBLE(I-1)/DBLE(NBPAS)
                  METAPG(6) = TPG1+(DBLE(I)*(TPG2-TPG1))
     &                                          /DBLE(NBPAS)
                  TPI = (METAPG(6)-TI)/DT21
                  CALL SMCARC ( NBHIST, FTRC, TRC,
     &                          COEF, FMOD,
     &                          CTES, CKM ,NBTRC,
     &                          TI, TPI, DT10,TEMPO, METAPG )
                  DO 40 J = 1 , 7
                     TEMPO(J) = METAPG(J)
40                CONTINUE
50             CONTINUE
            ELSE
               CALL SMCARC ( NBHIST, FTRC, TRC,
     &                          COEF, FMOD,
     &                          CTES, CKM ,NBTRC,
     &                          TPG1, TPOINT,DT10, TAMP, METAPG )

            ENDIF
         ELSE
            IF ( ABS(TPG2-TPG1) .GT. 5.001D0 ) THEN
C ----------------SUBDIVISION EN PAS DE CING DEGRE MAX
               NBPAS = INT(ABS(TPG2-TPG1)/5.D0-0.001D0)+1
               DT21  = DT21/DBLE(NBPAS)
               Z1I   = Z1
               DMOINS=TAMP(5)
               DO 51 I = 1 , NBPAS
                  TI1 = TPG1+(TPG2-TPG1)*DBLE(I-1)/DBLE(NBPAS)
                  TI2 = TPG1+(TPG2-TPG1)*DBLE(I)/DBLE(NBPAS)
                  TPOINT = (TI2-TI1)/DT21
                  ZEQ1I  = MIN( (TI1-AC1)/(AC3-AC1) , UN )
                  ZEQ2I  = MIN( (TI2-AC1)/(AC3-AC1) , UN )
                  TAUX   = TAUX1 + (TAUX3-TAUX1)*ZEQ1I
                  IF ( (TI1.LT.(AC1-EPSI)) .OR. (Z1I.GE.UN) ) THEN
                     Z2 = Z1I
                  ELSE
                     IF ( ZEQ2I .GE. (UN-EPSI) ) TPOINT = ZERO
                     IF ( Z1I .GT. ZEQ1I ) THEN
                        Z2 = (TAUX*TPOINT/(AC3-AC1))
                        Z2 = Z2*EXP(-DT21/TAUX)
                        Z2 = ((-TAUX*TPOINT/(AC3-AC1))+ZEQ2I+Z2-ZEQ1I)
     &                                              *(UN-Z1I)/(UN-ZEQ1I)
                        Z2 = Z2+Z1I
                     ELSE
                        Z2 = (TAUX*TPOINT/(AC3-AC1))-ZEQ1I+Z1I
                        Z2 = Z2*EXP(-DT21/TAUX)
                        Z2 = (-TAUX*TPOINT/(AC3-AC1))+ZEQ2I+Z2
                     ENDIF
                  ENDIF

C                 CALCUL TAILLE DE GRAIN

                  IF (CODRET(8) .EQ. 'NO') THEN
                     UNSURL = 0.D0
                    METAPG(5) = CKM(5)
                  ELSE
                     IF (Z2 .LT. 1.D-3) THEN
                        METAPG(5)=0.D0
                     ELSE
                        LAMBDA = LAMBD0*EXP(QSRK/(TI1+273.D0))
                        UNSURL = 1/LAMBDA
                        DLIM = D10*EXP(-WSRK/(TI1+273.D0))
                         DZ = Z2-Z1I
                         A = 1.D0
                         B = DMOINS*(1-DZ/Z2)-(DT10*UNSURL/DLIM)
                         C = DT21*UNSURL
                         DELTA = (B**2)+(4.D0*A*C)
                         METAPG(5) = (B+DELTA**0.5D0)/(2.D0*A)
                     ENDIF
                     Z1I = Z2
                     DMOINS = METAPG(5)
                  ENDIF

 51            CONTINUE
            ELSE

               TAUX = TAUX1 + (TAUX3-TAUX1)*ZEQ1
               IF ( (TPG1.LT.(AC1-EPSI)) .OR. (Z1.GE.UN) ) THEN
                  Z2 = Z1
               ELSE
                  IF ( ZEQ2 .GE. (UN-EPSI) ) TPOINT = ZERO
                  IF ( Z1 .GT. ZEQ1 ) THEN
                     Z2 = (TAUX*TPOINT/(AC3-AC1))
                     Z2 = Z2*EXP(-DT21/TAUX)
                     Z2 = ((-TAUX*TPOINT/(AC3-AC1))+ZEQ2+Z2-ZEQ1)
     &                                            *(UN-Z1)/(UN-ZEQ1)
                     Z2 = Z2+Z1
                  ELSE
                     Z2 = (TAUX*TPOINT/(AC3-AC1))-ZEQ1+Z1
                     Z2 = Z2*EXP(-DT21/TAUX)
                     Z2 = (-TAUX*TPOINT/(AC3-AC1))+ZEQ2+Z2
                  ENDIF
               ENDIF
C ---          CALCUL TAILLE DE GRAIN
               IF (CODRET(8) .EQ. 'NO') THEN
                  UNSURL = 0.D0
                  METAPG(5) = CKM(5)
               ELSE
                  IF (Z2 .LT. 1.D-3) THEN
                     METAPG(5)=0.D0
                  ELSE
                     DMOINS = TAMP(5)
                     LAMBDA = LAMBD0*EXP(QSRK/(TPG1+273.D0))
                     UNSURL = 1/LAMBDA
                     DLIM = D10*EXP(-WSRK/(TPG1+273.D0))
                     DZ = Z2-Z1
                     A = 1.D0
                     B = DMOINS*(1-DZ/Z2)-(DT10*UNSURL/DLIM)
                     C = DT21*UNSURL
                     DELTA = (B**2)+(4.D0*A*C)
                     METAPG(5) = (B+DELTA**0.5D0)/(2.D0*A)

                  ENDIF
                ENDIF
            ENDIF
C           REPARTITION DE DZGAMMA SUR DZALPHA
            IF ( Z2 .GT. (UN-EPSI) ) Z2 = UN
            IF ( Z1 .NE. UN ) THEN
               DO 210 J = 1 , 4
                  METAPG(J) = TAMP(J)*(UN-(Z2-Z1)/(UN-Z1))
210            CONTINUE
            ELSE
               DO 211 J = 1 , 4
                  METAPG(J) = TAMP(J)
211            CONTINUE
            ENDIF

         ENDIF
       END
