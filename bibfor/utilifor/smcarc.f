      SUBROUTINE SMCARC ( NBHIST, FTRC, TRC, COEF, FMOD,
     &                    CTES, CKM, NBTRC,TEMPE, TPOINT, DT,ZIN,ZOUT )
      IMPLICIT   NONE
      INTEGER             NBHIST, NBTRC
      REAL*8              FTRC((3*NBHIST),3), TRC((3*NBHIST),5), FMOD(*)
      REAL*8              CTES(11), CKM(6*NBTRC), COEF(*), TEMPE, TPOINT
      REAL*8              ZIN(7), ZOUT(7)
C......................................................................C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C
C   - FONCTION :                                                       C
C       CALCUL DE Z(N+1) CONNAISSANT T(N), TP(N), Z(N) ET T(N+1)       C
C   - ENTREES :                                                        C
C       NBHIST           : NBRE D HISTOIRES EXPERIMENTALE DE DEFI_TRC  C
C       FTRC(3*NBHIST,3) : VECTEUR DZ/DT EXPERIMENTAUX (VIDE EN ENTREE)C
C       TRC (3*NBHIST,5) : VECTEUR Z,T EXPERIMENTAUX (VIDE EN ENTREE)  C
C       FMOD(*)          : ENSEMBLE DES HISTOIRES EXPERIMENTALES       C
C       CTES(3)          : AR3, ALPHA, MS0                             C
C       CKM(6*NBTRC)     : VECTEUR DES LOIS MS(Z) SEUIL,AKM,BKM,TPLM   C
C                        : VECTEUR DES LOIS GRAIN P , A
C       NBTRC            : NBRE DE LOIS MS(Z)                          C
C       TEMPE            : TEMPERATURE AU POINT DE GAUSS SUR LE PAS    C
C       TPOINT           : DERIVEE A GAUCHE DE TEMPE                   C
C       ZIN(7)           : PHASMETA(N) ZF,ZP,ZB,ZM, P ,T,MS           C
C   - SORTIES :                                                        C
C       ZOUT(7)          : PHASMETA(N+1) ZF,ZP,ZB,ZM,P,T,MS            C
C......................................................................C
C
      INTEGER   J, IND(6)
      REAL*8    SDZ,SDZ0,TMF,ZM,DZ(4),X(5),RZ
      REAL*8    TEMPS,TPLI,A,B,C,D,E,F,FT,F7,FPT,FP7,T
      REAL*8    OOUN,QUINZE,UN,ZERO,TLIM,EPSI,TPOIN2
      REAL*8    LAMBDA,DLIM,DMOINS,DT,UNSURL,ZAUST
      REAL*8    A2, B2, C2, DELTA
C     ------------------------------------------------------------------
C
      ZERO   = 0.D0
      OOUN   = 0.01D0
      UN     = 1.D0
      QUINZE = 15.D0
      EPSI   = 1.D-10
      TLIM   = CKM(4)
      T      = 700.D0
C
      IF ( TEMPE .GT. CTES(1) ) THEN
         DO 5  J = 1 , 4
            ZOUT(J) = ZIN(J)
  5      CONTINUE
         ZOUT(7)=CTES(3)
      ELSE
         SDZ0 = ZIN(1) + ZIN(2) + ZIN(3) + ZIN(4)

         TMF  = ZIN(7) - ( LOG(OOUN))/CTES(2 ) - QUINZE
         IF ( (SDZ0.GE.UN-0.001D0) .OR. (TEMPE.LT.TMF) ) THEN
            DO 10 J = 1 , 4
               ZOUT(J) = ZIN(J)
 10         CONTINUE
            ZOUT(7)=ZIN(7)

         ELSE
            IF ( TEMPE .LT. ZIN(7) ) THEN
               DO 15 J = 1 , 3
                  DZ(J) = ZERO
 15            CONTINUE
            ELSE
C
C --- CONSTRUCTION DES POINTS EXPERIMENTAUX DE MEME TEMPERATURE
C

               CALL SMCOMO ( COEF, FMOD, TEMPE, NBHIST, FTRC, TRC )

C --- TPOIN2 POUR EFFET TAILLE DE GRAIN AUSTENITIQUE
               IF (CKM(6) .EQ. 0.D0) THEN
                   TPOIN2 = TPOINT
               ELSE
                   TPOIN2 = TPOINT * EXP(CKM(6)*(ZIN(5)-CKM(5)))
               ENDIF
C --- COMPARAISON DE L ETAT COURANT / HISTOIRES ENVELOPPES
C
               IF ( TPOIN2 .GT. (TRC(1,4)*(UN+EPSI)) ) THEN
                  DO 20 J = 1 , 3
                     DZ(J) = FTRC(1,J)*(ZOUT(6)-TEMPE)
 20               CONTINUE
               ELSE
                  IF ( TPOIN2 .LT. (TRC(NBHIST,4)*(UN-EPSI)) ) THEN
                     DO 30 J = 1 , 3
                        DZ(J) = FTRC(NBHIST,J)*(ZOUT(6)-TEMPE)
 30                  CONTINUE
                  ELSE
C
C --- RECHERCHE DES PLUS PROCHES VOISINS PARMI TRC
C
                     X(1) = ZIN(1)
                     X(2) = ZIN(2)
                     X(3) = ZIN(3)
                     X(4) = TPOIN2
                     X(5) = TEMPE
                     CALL SMCAVO ( X, IND, NBHIST, TRC )
C
C --- CALCUL DES COORDONNEES BARYCENTRIQUES / AUX PLUS PROCHES VOISINS
C
                     CALL SMCABA ( FTRC, TRC, NBHIST, X, DZ, IND )
                     IF ( (ZOUT(6)-TEMPE) .GT. ZERO ) THEN
                        DO 35 J = 1 , 3
                           DZ(J) = ZERO
 35                     CONTINUE
                     ELSE
                        DO 36 J = 1 , 3
                           DZ(J) = DZ(J)*(ZOUT(6)-TEMPE)



 36                     CONTINUE
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
C --- CALCUL DE MS-

        SDZ = SDZ0 - ZIN(4)
        IF ( (SDZ.GE.CKM(1)) .AND. (ZIN(4).EQ.ZERO) ) THEN
            ZOUT(7) = CTES(3) + CKM(2)*SDZ + CKM(3)
        ELSE
            ZOUT(7) = ZIN(7)
        ENDIF


            ZM = UN - SDZ
            IF ( (ZOUT(6).GT.ZOUT(7)) .OR. (ZM.LT.OOUN) ) THEN
               ZOUT(4) = ZIN(4)
            ELSE
               A = COEF(3)
               B = COEF(4)
               C = COEF(5)
               D = COEF(6)
               E = COEF(7)
               F = COEF(8)
               IF ( (A.NE.ZERO).AND.(B.NE.ZERO).AND.(C.NE.ZERO) .AND.
     &              (D.NE.ZERO).AND.(E.NE.ZERO).AND.(F.NE.ZERO) ) THEN
                  F7 = A+B*T+C*T**2+D*T**3+E*T**4+F*T**5
                FT=A+B*TEMPE+C*TEMPE**2+D*TEMPE**3+E*TEMPE**4+F*TEMPE**5
                  FPT=B+2*C*TEMPE+3*D*TEMPE**2+4*E*TEMPE**3+5*F*TEMPE**4
                  FP7=B+2*C*T+3*D*T**2+4*E*T**3+5*F*T**4
                  TEMPS=FT-F7-LOG(FP7*TLIM)
                  TPLI=UN/(FPT*EXP(TEMPS))
               ELSE
                  TPLI = TLIM
               ENDIF
               IF ( (TPOINT.GT.TPLI) .AND. (ZIN(4).EQ.ZERO) ) THEN
                  ZOUT(4) = ZIN(4)
               ELSE
                  ZOUT(4) = ZM*(UN-EXP(CTES(2)*(ZOUT(7)-ZOUT(6))))
               ENDIF
            ENDIF
            DZ(4) = ZOUT(4)-ZIN(4)
            SDZ = ZERO
            DO 40 J = 1 , 4
               SDZ = SDZ+ZIN(J)+DZ(J)
40          CONTINUE
            IF ( SDZ .GT. UN-0.001D0 ) THEN
               RZ = SDZ - SDZ0
               DO 50 J=1,4
                  DZ(J) = DZ(J) / ( RZ/(UN-SDZ0) )
                  ZOUT(J) = ZIN(J) + DZ(J)
50             CONTINUE
            ELSE
               DO 51 J = 1 , 4
                  ZOUT(J) = ZIN(J) + DZ(J)
51            CONTINUE
            ENDIF
         ENDIF
      ENDIF
      ZAUST = ZOUT(1)+ZOUT(2)+ZOUT(3)+ZOUT(4)
      ZAUST = 1-ZAUST
C --- CALCUL TAILLE DE GRAIN
      IF (CTES(8) .EQ. 0.D0) THEN
          UNSURL = 0.D0
          ZOUT(5) = CKM(5)
      ELSE
          IF (ZAUST .LT. 1.D-3) THEN
             ZOUT(5)=0.D0
          ELSE
             DMOINS = ZIN(5)
             LAMBDA = CTES(8)*EXP(CTES(9)/(TEMPE+273.D0))
             UNSURL = 1.D0/LAMBDA
             DLIM = CTES(10)*EXP(-CTES(11)/(TEMPE+273.D0))
              A2 = 1.D0
              B2 = DMOINS-(DT*UNSURL/DLIM)
              C2 = DT*UNSURL
              DELTA = (B2**2)+(4.D0*A2*C2)
              ZOUT(5) = (B2+DELTA**0.5D0)/(2.D0*A2)
          ENDIF
      ENDIF
      END
