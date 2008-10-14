      SUBROUTINE TE0564 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/10/2008   AUTEUR LEBOUVIER F.LEBOUVIER 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VALEURS MOYENNES ET AMPLITUDES
C                          DE DEFORMATIONS PLASTIQUES ET CONTRAINTES
C                          POUR LA METHODE ZAC
C          ELEMENTS ISOPARAMETRIQUES 2D
C                      OPTIONS : 'SIGM_ELNO_ZAC  '
C                                'EPSP_ELNO_ZAC  '
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      PARAMETER        ( NBRES=10 , NCMP = 4)
      CHARACTER*16       PHENOM
      CHARACTER*8        NOMRES(NBRES),NOMPAR(2)
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALRES(NBRES),VALPAR(2),ZERO
      REAL*8             DFDX(9),DFDY(9),POIDS,R,EPS(5),INSTAN
      REAL*8             DEFOPG(54),CONTPG(54)
      REAL*8             A11,A22,A33,A12,A13,A23,G12
      REAL*8             A11M,A22M,A33M,A12M,A13M,A23M,G12M,C1
      REAL*8             DEFPPG(54),NORSIG,NORMSI,KRON(6),NORM
      REAL*8             E,NU,NUCH,ECH
      INTEGER            NNO,KP,K,NPG,I,ITEMPS,IALPHA,NDIM
      INTEGER            NNOS,JGANO,IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            ICONTZ,IDEFOZ,IALPHS
CC      EXTERNAL           NORSIG
      REAL*8             SIGMIN(6),SIGMAX(6),DELTA(6)
      LOGICAL            LTEATT
C
C
      DATA  ZERO  /0.D0/
      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      R3S2 = SQRT(3.D0/2.D0)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      CALL JEVECH('PALPHAR','L',IALPHA)
      CALL TECACH('ONN','PTEMPER',1,ITEMPE,IRET)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PCONTMR','L',ICONTM)
      CALL JEVECH('PCONTPR','L',ICONTP)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
C
      INSTAN = ZR(ITEMPS)
C
      NBPAR = 1
      NOMPAR(1)='TEMP'
C
      NOMRES(1) = 'E'
      NOMRES(2) = 'NU'
      NOMRES(3)=  'D_SIGM_EPSI'
      NOMRES(4)=  'SY'
C
      CALL JEVECH('PCONTZR','E',ICONTZ)
      CALL JEVECH('PDEFOZR','E',IDEFOZ)
C
      IF (INSTAN.EQ.3.D0) CALL JEVECH('PALPHAP','E',IALPHS)
C
      DO 90 I = 1,NCMP*NPG
         DEFOPG(I) = 0.D0
         DEFPPG(I) = 0.D0
         CONTPG(I) = 0.D0
 90   CONTINUE
C
      DO 101 KP=1,NPG
C
        K=(KP-1)*NNO
        IDPG=(KP-1)*NCMP
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
        R = ZERO
        TPG = ZERO
C
C   CALCUL DES DEFORMATIONS TOTALES
C
        DO 110 I=1,5
           EPS(I) = 0.0D0
 110     CONTINUE
        DO 120 I=1,NNO
          EPS(1) = EPS(1) + DFDX(I) * ZR(IDEPL+2*(I-1)  )
          EPS(2) = EPS(2) + DFDY(I) * ZR(IDEPL+2*(I-1)+1)
          EPS(3) = EPS(3) + ZR(IVF+K+I-1)*ZR(IDEPL+2*(I-1))
          EPS(4) = EPS(4) + DFDY(I) * ZR(IDEPL+2*(I-1)  )
          EPS(5) = EPS(5) + DFDX(I) * ZR(IDEPL+2*(I-1)+1)
          R    = R    +  ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
          TPG  = TPG  +  ZR(ITEMPE+I-1) *ZR(IVF+K+I-1)
 120     CONTINUE
        IF ( LTEATT(' ','AXIS','OUI')) THEN
           IF ( R .NE. 0.D0 ) THEN
             EPS(3) = EPS(3) / R
           ELSE
             EPS(3) = EPS(1)
           ENDIF
           DEFOPG(IDPG+3) = EPS(3)
        ELSE IF (NOMTE(3:4) .EQ. 'CP' ) THEN
           DEFOPG(IDPG+3) = EPS(3)
        ELSE
           DEFOPG(IDPG+3) = 0.D0
        ENDIF
C
        DEFOPG(IDPG+1) =  EPS(1)
        DEFOPG(IDPG+2) =  EPS(2)
        DEFOPG(IDPG+4) = (EPS(4)+EPS(5))/2.D0
C
        IF (ITEMPE.NE.0) VALPAR(1) = TPG
C
        CALL RCVALA ( ZI(IMATE),' ',PHENOM,NBPAR,NOMPAR,VALPAR,2,
     &                NOMRES, VALRES, CODRET, 'FM' )
        CALL RCVALA ( ZI(IMATE),' ','ECRO_LINE',NBPAR,NOMPAR,VALPAR,2,
     &                NOMRES(3), VALRES(3), CODRET, 'FM' )
C
C  CONSTANTES ELASTIQUES
C
        E =  VALRES(1)
        NU = VALRES(2)
        A11M = 1.D0/E
        A12M = -NU/E
        A13M = A12M
        A22M = A11M
        A23M = A12M
        A33M = A11M
        G12M = (1.D0+NU)/E
        IF ( NOMTE(3:4) .EQ. 'DP' ) THEN
           A11M=A11M-A13M*A13M/A33M
           A12M=A12M-A13M*A23M/A33M
           A22M=A22M-A23M*A23M/A33M
           A13M=0.D0
           A23M=0.D0
        ENDIF
C
C  CONSTANTES ELASTIQUES MODIFIEES
C
        DSDE  = VALRES(3)
        SIGY  = VALRES(4)
        C = 2.D0/3.D0*(E*DSDE)/(E-DSDE)
        ECH = 3.D0*C*E/(2.D0*E+3.D0*C)
        NUCH = (3.D0*C*NU+E)/(2.D0*E+3.D0*C)
        C1  = ECH/(1.D0 + NUCH)
        A11 = C1*(1.D0 - NUCH)/(1.D0 - 2.D0*NUCH)
        A12 = C1*        NUCH /(1.D0 - 2.D0*NUCH)
        A13 = A12
        A22 = A11
        A23 = A12
        A33 = A11
        G12 = C1/2.D0
        IF ( NOMTE(3:4) .EQ. 'CP' ) THEN
           A11=A11-A13*A13/A33
           A12=A12-A13*A23/A33
           A22=A22-A23*A23/A33
           A13=0.D0
           A23=0.D0
        ENDIF
C
C   CONTRAINTES RESIDUELLES :
C   CALCUL DE RHO = MCH*(EPSILON-EPSINI)
C
        CONTPG(IDPG+1) = A11*DEFOPG(IDPG+1)+A12*DEFOPG(IDPG+2)+
     &                  A13*DEFOPG(IDPG+3)-2*G12*ZR(IALPHA+IDPG)
C
        CONTPG(IDPG+2) = A12*DEFOPG(IDPG+1)+A22*DEFOPG(IDPG+2)+
     &                  A23*DEFOPG(IDPG+3)-2*G12*ZR(IALPHA+IDPG+1)
C
        CONTPG(IDPG+3) = A13*DEFOPG(IDPG+1)+A23*DEFOPG(IDPG+2)+
     &                  A33*DEFOPG(IDPG+3)-2*G12*ZR(IALPHA+IDPG+2)
C
        CONTPG(IDPG+4) = 2*G12*DEFOPG(IDPG+4)-2*G12*ZR(IALPHA+IDPG+3)
C
C   DEFORMATIONS PLASTIQUES :
C   CALCUL DE EPSP = EPSILON - M*RHO
C
        DEFPPG(IDPG+1) = DEFOPG(IDPG+1) - A11M*CONTPG(IDPG+1)
     &                 - A12M*CONTPG(IDPG+2) - A13M*CONTPG(IDPG+3)
C
        DEFPPG(IDPG+2) = DEFOPG(IDPG+2) - A12M*CONTPG(IDPG+1)
     &                 - A22M*CONTPG(IDPG+2) - A23M*CONTPG(IDPG+3)
C
        DEFPPG(IDPG+3) = DEFOPG(IDPG+3) - A13M*CONTPG(IDPG+1)
     &                 - A23M*CONTPG(IDPG+2) - A33M*CONTPG(IDPG+3)
C
        DEFPPG(IDPG+4) = DEFOPG(IDPG+4) - G12M*CONTPG(IDPG+4)
C
        IF (INSTAN.EQ.2.OR.INSTAN.EQ.4) THEN
           DO 125 I=1,NCMP
             DEFPPG(IDPG+I) = ABS(DEFPPG(IDPG+I))
 125       CONTINUE
        END IF
C
        NORM = R3S2*NORSIG(DEFPPG(IDPG+1),NCMP)
        TRMIN = 0.D0
        TRMAX = 0.D0
        DO 130 I=1,3
           TRMIN = TRMIN + ZR(ICONTM+IDPG-1+I)
           TRMAX = TRMAX + ZR(ICONTP+IDPG-1+I)
 130    CONTINUE
        TRMIN = TRMIN /3.D0
        TRMAX = TRMAX /3.D0
        DO 140 I=1,NCMP
          SIGMIN(I) = ZR(ICONTM+IDPG-1+I) - TRMIN*KRON(I)
          SIGMAX(I) = ZR(ICONTP+IDPG-1+I) - TRMAX*KRON(I)
 140    CONTINUE
        DO 150 I=1,NCMP
          DELTA(I) = SIGMAX(I) - SIGMIN(I)
 150    CONTINUE
        NORMSI = R3S2*NORSIG(DELTA,NCMP)
C
        IF (INSTAN.EQ.1.D0) THEN
C
C    VALEUR MOYENNE CONTRAINTE
C
           DO 160 I=1,NCMP
              CONTPG(IDPG+I) = CONTPG(IDPG+I) +
     &           (ZR(ICONTM+IDPG+I-1) + ZR(ICONTP+IDPG+I-1) )/2.D0
 160       CONTINUE
C
        ELSE IF (INSTAN.EQ.2.D0) THEN
C
C     BORNE INFERIEURE CONTRAINTE
C
           DO 170 I=1,NCMP
C              IF ((NORMSI - 2.D0*SIGY).GE.0.D0) THEN
                 CONTPG(IDPG+I) =   CONTPG(IDPG+I)
     &             +  ZR(ICONTP+IDPG+I-1) - ZR(ICONTM+IDPG+I-1)
C              END IF
               CONTPG(IDPG+I) = ABS ( CONTPG(IDPG+I) )
 170       CONTINUE
C
        ELSE IF (INSTAN.EQ.3.D0) THEN
C
C      CALCUL EPSILON INITIAL POUR LA BORNE SUPERIEURE
C
           DO 180 I=1,NCMP
              TRAC = 0.D0
              DO 190 J=1,3
                 TRAC = TRAC + CONTPG(IDPG+J)
 190          CONTINUE
              TRAC = TRAC/3.D0
              DEFPPG(IDPG+I) = ZR(IALPHA+IDPG+I-1)
     &               + ( CONTPG(IDPG+I) - TRAC* KRON(I) ) / C
 180       CONTINUE
C
           DO 200 I=1,NCMP
              IF ((NORMSI - 2.D0*SIGY).GE.0.D0) THEN
                 DEFPPG(IDPG+I) = (DELTA(I)
     &                        - 2.D0*SIGY*DEFPPG(IDPG+I)/NORM) / C
              ELSE
                 DEFPPG(IDPG+I) = 0.D0
              END IF
              ZR(IALPHS+IDPG+I-1) = DEFPPG(IDPG+I)
 200       CONTINUE
C
        ELSE IF (INSTAN.EQ.4.D0) THEN
C
C      BORNE SUPERIEURE CONTRAINTE
C
           DO 210 I=1,NCMP
C              IF ((NORMSI - 2.D0*SIGY).GE.0.D0) THEN
                 CONTPG(IDPG+I) =  CONTPG(IDPG+I)
     &             +  ZR(ICONTP+IDPG+I-1) - ZR(ICONTM+IDPG+I-1)
C              END IF
               CONTPG(IDPG+I) = ABS ( CONTPG(IDPG+I) )
 210       CONTINUE
C
         END IF
C
 101  CONTINUE
C
C    CALCUL DES VALEURS AUX NOEUDS
C
      CALL PPGAN2(JGANO,NCMP,CONTPG,ZR(ICONTZ))
      CALL PPGAN2(JGANO,NCMP,DEFPPG,ZR(IDEFOZ))
C
      END
