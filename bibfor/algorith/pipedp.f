      SUBROUTINE PIPEDP(NDIM, TYPMOD, IMATE, EPSM, SIGM, VIM,
     &                  EPSP, EPSD, ELGEOM, A0, A1)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
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
      CHARACTER*8        TYPMOD(2)
      INTEGER            NDIM, IMATE
      REAL*8             EPSP(6), EPSD(6)
      REAL*8             EPSM(6), VIM(2), SIGM(6), A0, A1
      REAL*8             ELGEOM(*)
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT BETON_DOUBLE_DP
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : NATURE DU MATERIAU
C IN  EPSM    : DEFORMATION EN T-
C IN  SIGM    : CONTRAINTE EN T-
C IN  VIM     : VARIABLES INTERNES EN T-
C IN  ELGEOM  : TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES AUX
C               LOIS DE COMPORTEMENT (DIMENSION MAXIMALE FIXEE EN DUR)
C OUT A0      : LINEARISATION DU CRITERE : FEL = A0 + A1*ETA
C OUT A1      : IDEM A0
C ----------------------------------------------------------------------
      INTEGER     NDIMSI, K, NRAC1, NRAC2
      LOGICAL     TRAC, COMP, NOTRAC, NOCOMP
      REAL*8      TRSIGP, TRSIGD, SIGELP(6), SIGELD(6)
      REAL*8      EPS1(6), EPS2(6), PP(6), DD(6)
      REAL*8      D1, D2, G1, G2, G3, G4
      REAL*8      KRON(6)
      REAL*8      P0, P1, P2, Q0, Q1, Q2, ETA, ETA1, ETA2
      REAL*8      RAC1(2), RAC2(2)
      REAL*8      E, NU, LAMBDA, DEUXMU, GAMMA
      REAL*8      FC , FT , BETA
      REAL*8      A, B, C, D
      REAL*8      DNRM2
      REAL*8      UN, D23, D13, RACI2, DEUX, TROIS ,NEUF
      PARAMETER   ( UN   =  1.D0   )
      PARAMETER   ( DEUX =  2.D0   )
      PARAMETER   ( TROIS = 3.D0   )
      PARAMETER   ( NEUF = 9.D0   )
      PARAMETER   ( D23  =  .66666666666666D0 )
      PARAMETER   ( D13  =  .33333333333333D0 )

      CHARACTER*2 CODRET(3)
      CHARACTER*8 NOMRES(3)
      REAL*8      VALRES(3)

      REAL*8      DDOT

      INTEGER     NDT, NDI, NR, NVI, NMAT
      PARAMETER   ( NMAT = 90     )
      REAL*8      MATERD(NMAT,2), MATERF(NMAT,2)
      REAL*8      PC, PT, KUC, KUT, KE, TBID, RBID, FCP , FTP
      CHARACTER*8 MOD   
      CHARACTER*3 MATCST
   
      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

C --  OPTION ET MODELISATION

      NDIMSI = 2*NDIM 

C --  RECUPERATION MATERIAU
      RACI2   = SQRT (DEUX)
C
      TBID = 0.D0
      MOD  = TYPMOD(1)
      CALL BETMAT ( MOD, IMATE, NMAT, TBID, TBID, TBID, TBID,
     1              TBID, TBID, MATERD, MATERF, MATCST, NDT,
     2              NDI , NR, NVI)
C
      E      = MATERD(1,1)
      NU     = MATERD(2,1)
      BETA   = MATERD(3,2)
C
      PC  = VIM(1)
      PT  = VIM(2)
      CALL BETFPP ( MATERF, NMAT, ELGEOM, PC, PT, 3 , FC, FT,
     &              RBID, RBID, KUC, KUT, KE)
C
      FCP    = MATERF(1,2)
      FTP    = MATERF(2,2)
      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)
      A = RACI2 * (BETA - UN) / (DEUX * BETA - UN)
      B = RACI2 / TROIS * BETA / (DEUX * BETA - UN)
      C = RACI2 
      D = DEUX * RACI2 / TROIS
C
      NOCOMP = .FALSE.
      IF (PC.GT.KUC) NOCOMP = .TRUE.
      NOTRAC = .FALSE.
      IF (PT.GT.KUT) NOTRAC = .TRUE.
C
C ======================================================================
C                CALCUL DES DEFORMATIONS POUR LINEARISATION
C ======================================================================
C
C     COEFFICIENTS DE LA FORME QUADRATIQUE DU CRITERE
      DO 10 K=1,NDIMSI
      SIGELP(K) = SIGM(K) + (LAMBDA*KRON(K)+DEUXMU)*
     &            (EPSP(K)-EPSM(K))         
      SIGELD(K) = (LAMBDA*KRON(K)+DEUXMU)*EPSD(K)
 10   CONTINUE
C 
      TRSIGP = SIGELP(1) + SIGELP(2) + SIGELP(3)
      TRSIGD = SIGELD(1) + SIGELD(2) + SIGELD(3)
      
      DO 20 K=1,NDIMSI
      PP(K) = SIGELP(K)-D13*TRSIGP*KRON(K)
      DD(K) = SIGELD(K)-D13*TRSIGD*KRON(K)
 20   CONTINUE
 
C     CRITERE DE TRACTION
      P0 = D13*DDOT(NDIMSI,PP,1,PP,1)-(D*FT)**2+D23*C*D*FT*TRSIGP
     &                                -C**2/NEUF*TRSIGP**2
      P1 = D13*DDOT(NDIMSI,PP,1,DD,1)+ D13*C*D*FT*TRSIGD
     &                                -C**2/NEUF*TRSIGP*TRSIGD
      P2 = D13*DDOT(NDIMSI,DD,1,DD,1)-C**2/NEUF*TRSIGD**2
      
      P0=P0/(D*FTP)**2
      P1=P1/(D*FTP)**2
      P2=P2/(D*FTP)**2
      
C    CRITERE DE COMPRESSION      

      Q0 = D13*DDOT(NDIMSI,PP,1,PP,1)-(B*FC)**2+D23*A*B*FC*TRSIGP
     &                                -A**2/NEUF*TRSIGP**2
      Q1 = D13*DDOT(NDIMSI,PP,1,DD,1)+ D13*A*B*FC*TRSIGD
     &                                -A**2/NEUF*TRSIGP*TRSIGD
      Q2 = D13*DDOT(NDIMSI,DD,1,DD,1)-A**2/NEUF*TRSIGD**2
      
      Q0=Q0/(B*FCP)**2
      Q1=Q1/(B*FCP)**2
      Q2=Q2/(B*FCP)**2

C    RECHERCHE DES INTERSECTIONS ELLIPSE / DROITE (TRACTION)
      CALL ZEROP2(2*P1/P2, P0/P2, RAC1, NRAC1)

C     RECHERCHE DES INTERSECTIONS ELLIPSE / DROITE (COMPRESSION)
      CALL ZEROP2(2*Q1/Q2, Q0/Q2, RAC2, NRAC2)
C
C     CALCUL DES COEFFICIENTS LORSQUE LES DEUX CRITERES
C     SONT COMPLETEMENT ECROUIS
C     --------------------------------------------------------- 
      IF (NOTRAC.AND.NOCOMP) THEN
         A0 = 0.D0
         A1 = 0.D0
      ENDIF
C
C     CALCUL DES COEFFICIENTS LORSQUE LE CRITERE DE TRACTION
C     EST COMPLETEMENT ECROUI
C     --------------------------------------------------------- 
      IF (NOTRAC) THEN
         IF (NRAC2.NE.0) THEN
            DO 30 K = 1,NDIMSI
               EPS1(K) = EPSP(K) + RAC2(1) * EPSD(K) - EPSM(K)
               EPS2(K) = EPSP(K) + RAC2(2) * EPSD(K) - EPSM(K)
 30         CONTINUE  
            D1 = DNRM2(NDIMSI, EPS1,1)
            D2 = DNRM2(NDIMSI, EPS2,1)
            IF (D1 .LE. D2) THEN
                ETA = RAC2(1)
            ELSE
                ETA = RAC2(2)
            ENDIF   
         ELSE
            ETA = -Q1/Q2
         ENDIF
         A0 = - Q2 * ETA**2 + Q0
         A1 = 2*(ETA*Q2+Q1)
      ENDIF
C
C     CALCUL DES COEFFICIENTS LORSQUE LE CRITERE DE COMPRESSION
C     EST COMPLETEMENT ECROUI
C     --------------------------------------------------------- 
      IF (NOCOMP) THEN
         IF (NRAC1.NE.0) THEN
            DO 40 K = 1,NDIMSI
               EPS1(K) = EPSP(K) + RAC1(1) * EPSD(K) - EPSM(K)
               EPS2(K) = EPSP(K) + RAC1(2) * EPSD(K) - EPSM(K)
 40         CONTINUE  
            D1 = DNRM2(NDIMSI, EPS1,1)
            D2 = DNRM2(NDIMSI, EPS2,1)
            IF (D1 .LE. D2) THEN
                ETA = RAC1(1)
            ELSE
                ETA = RAC1(2)
            ENDIF   
         ELSE
            ETA = -P1/P2
         ENDIF
         A0 = - P2 * ETA**2 + P0
         A1 = 2*(ETA*P2+P1)
      ENDIF
C
C     CALCUL DES COEFFICIENTS LORSQU'AUCUN DES DEUX CRITERES
C     N'EST COMPLETEMENT ECROUI
C     --------------------------------------------------------- 
C
      IF (.NOT.NOCOMP .AND. .NOT.NOTRAC) THEN
C        LA DROITE COUPE LE CRITERE DE TRACTION 
         IF (NRAC1.NE.0) THEN
            G1 = Q0 + DEUX * RAC1(1)*Q1 + RAC1(1)**2*Q2
            G2 = Q0 + DEUX * RAC1(2)*Q1 + RAC1(2)**2*Q2
            TRAC = .TRUE.
            IF ((G1.LT.0).AND.(G2.LT.0)) THEN
               DO 50 K = 1,NDIMSI
                  EPS1(K) = EPSP(K) + RAC1(1) * EPSD(K) - EPSM(K)
                  EPS2(K) = EPSP(K) + RAC1(2) * EPSD(K) - EPSM(K)
 50            CONTINUE  
               D1 = DNRM2(NDIMSI, EPS1,1)
               D2 = DNRM2(NDIMSI, EPS2,1)
               IF (D1 .LE. D2) THEN
                  ETA1 = RAC1(1)
            ELSE
                  ETA1 = RAC1(2)
            ENDIF   
            ELSEIF ((G1.LT.0).AND.(G2.GT.0)) THEN
               ETA1 = RAC1(1)
            ELSEIF ((G1.GT.0).AND.(G2.LT.0)) THEN
               ETA1 = RAC1(2)
            ELSE
               ETA1 = -P1/P2
               TRAC = .FALSE.
            ENDIF
         ELSE
            ETA1 = -P1/P2
            TRAC = .FALSE.
         ENDIF
C 
C        LA DROITE COUPE LE CRITERE DE COMPRESSION     
         IF (NRAC2.NE.0) THEN
            G3 = P0 + DEUX * RAC2(1)*P1 + RAC2(1)**2*P2
            G4 = P0 + DEUX * RAC2(2)*P1 + RAC2(2)**2*P2
            COMP = .TRUE.
            IF ((G3.LT.0).AND.(G4.LT.0)) THEN
               DO 60 K = 1,NDIMSI
                  EPS1(K) = EPSP(K) + RAC2(1) * EPSD(K) - EPSM(K)
                  EPS2(K) = EPSP(K) + RAC2(2) * EPSD(K) - EPSM(K)
 60            CONTINUE  
               D1 = DNRM2(NDIMSI, EPS1,1)
               D2 = DNRM2(NDIMSI, EPS2,1)
               IF (D1 .LE. D2) THEN
                  ETA2 = RAC2(1)
               ELSE
                  ETA2 = RAC2(2)
               END IF   
            ELSE IF ((G3.LT.0).AND.(G4.GT.0)) THEN
               ETA2 = RAC2(1)
            ELSE IF ((G3.GT.0).AND.(G4.LT.0)) THEN
               ETA2 = RAC2(2)
            ELSE
               ETA2 = -Q1/Q2
               COMP = .FALSE.
            ENDIF     
         ELSE
            ETA2 = -Q1/Q2
            COMP = .FALSE.
         ENDIF
C
         IF (TRAC) THEN
            IF (COMP) THEN
               DO 70 K = 1,NDIMSI
                  EPS1(K) = EPSP(K) + ETA1 * EPSD(K) - EPSM(K)
                  EPS2(K) = EPSP(K) + ETA2 * EPSD(K) - EPSM(K)
 70            CONTINUE  
               D1 = DNRM2(NDIMSI, EPS1,1)
               D2 = DNRM2(NDIMSI, EPS2,1)
               IF (D1 .LE. D2) THEN
                  ETA = ETA1
                  A0 = - P2 * ETA**2 + P0
                  A1 = 2*(ETA*P2+P1)
               ELSE
                  ETA = ETA2
                  A0 = - Q2 * ETA**2 + Q0
                  A1 = 2*(ETA*Q2+Q1)
               ENDIF
            ELSE
               ETA = ETA1
               A0 = - P2 * ETA**2 + P0
               A1 = 2*(ETA*P2+P1)
            ENDIF
         ELSE
            IF (COMP) THEN
               ETA = ETA2
               A0 = - Q2 * ETA**2 + Q0
               A1 = 2*(ETA*Q2+Q1)
            ELSE
               DO 80 K = 1,NDIMSI
                  EPS1(K) = EPSP(K) + ETA1 * EPSD(K) - EPSM(K)
                  EPS2(K) = EPSP(K) + ETA2 * EPSD(K) - EPSM(K)
 80            CONTINUE  
               D1 = DNRM2(NDIMSI, EPS1,1)
               D2 = DNRM2(NDIMSI, EPS2,1)
               IF (D1 .LE. D2) THEN
                  ETA = ETA1
                  A0 = - P2 * ETA**2 + P0
                  A1 = 2*(ETA*P2+P1)
               ELSE
                  ETA = ETA2
                  A0 = - Q2 * ETA**2 + Q0
                  A1 = 2*(ETA*Q2+Q1)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      END
