      SUBROUTINE TE0568 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C          ELEMENTS ISOPARAMETRIQUES 3D
C                      OPTION : 'AMPL_ELNO_ZAC  '
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
      PARAMETER        ( NBRES=10 , NCMP = 6, NPGMAX=27)
      CHARACTER*24       CHVAL,CHCTE
      CHARACTER*16       PHENOM
      CHARACTER*8        NOMRES(NBRES),NOMPAR(2),ELREFE
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALRES(NBRES),VALPAR(2),ZERO
      REAL*8      DFDX(NPGMAX),DFDY(NPGMAX),DFDZ(NPGMAX),POIDS,INSTAN
      REAL*8             DEFOPG(NCMP*NPGMAX),CONTPG(NCMP*NPGMAX)
      REAL*8             A11,A22,A33,A12,A13,A23,G12,U(3,27)
      REAL*8             A11M,A22M,A33M,A12M,A13M,A23M,G12M,C1
      REAL*8            DEFPPG(NCMP*NPGMAX),NORSIG,NORMSI,KRON(6),NORM
      REAL*8             E,NU,NUCH,ECH
      INTEGER            NNO,KP,K,I,ITEMPS,IALPHA
      INTEGER            IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            ICONTZ,IDEFOZ,IALPHS,NBPG(10)
CC      EXTERNAL           NORSIG
      REAL*8             SIGMIN(6),SIGMAX(6),DELTA(6)
C
      DATA  ZERO  /0.D0/
      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C
      CALL ELREF1(ELREFE)
      R3S2 = SQRT(3.D0/2.D0)
C
C
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 110 I = 1,NBFPG
        NBPG(I) = ZI(JIN+3-1+I)
110   CONTINUE
      NNOS = ZI(JIN+3-1+NBFPG+1)
C
      NPG = NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO

CJMP   PARAIT FAUX =>  ON INTERDIT
C      CALL UTMESS('F','POST_ZAC','NE MARCHE PAS ENCORE EN 3D')

C      IF(ELREFE.EQ.'TETRA10'.OR.ELREFE.EQ.'HEXA20' ) THEN
C        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
C     &                  + NBPG(2)*(1+(NDIM+1)*NNO)
C       ELSE IF(ELREFE.EQ.'PENTA15' ) THEN
C        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
C      ENDIF

      IVF = IPOIDS + NPG
      IDFDE  = IVF + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      CALL JEVECH('PALPHAR','L',IALPHA)
      CALL TECACH(.TRUE.,.FALSE.,'PTEMPER',1,ITEMPE)
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
      IF (INSTAN.EQ.3.D0) CALL JEVECH('PALPHAP','E',IALPHS)
C
      DO 90 I = 1,NCMP*NPG
         DEFOPG(I) = 0.D0
         DEFPPG(I) = 0.D0
         CONTPG(I) = 0.D0
 90   CONTINUE
C
      DO 113 I=1,NNO
        U(1,I) = ZR(IDEPL + 3 * I - 3)
        U(2,I) = ZR(IDEPL + 3 * I - 2)
        U(3,I) = ZR(IDEPL + 3 * I - 1)
113   CONTINUE
C
      DO 101 KP=1,NPG
C
        K = (KP-1) * 3 * NNO
        IT = (KP-1) * NNO
        IDPG = (KP-1) * NCMP
C
        CALL DFDM3D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
        TPG = ZERO
C
C   CALCUL DES DEFORMATIONS TOTALES
C
        DO 120 I=1,NNO
         TPG  = TPG  +  ZR(ITEMPE+I-1) *ZR(IVF+IT+I-1)
 120     CONTINUE
C
          DO 106 I=1,NNO
            DEFOPG(IDPG+1) = DEFOPG(IDPG+1) + U(1,I) * DFDX(I)
            DEFOPG(IDPG+2) = DEFOPG(IDPG+2) + U(2,I) * DFDY(I)
            DEFOPG(IDPG+3) = DEFOPG(IDPG+3) + U(3,I) * DFDZ(I)
            DEFOPG(IDPG+4) = DEFOPG(IDPG+4) +(U(2,I) * DFDX(I)
     &                                      + U(1,I) * DFDY(I))*0.5D0
            DEFOPG(IDPG+5) = DEFOPG(IDPG+5) +(U(1,I) * DFDZ(I)
     &                                      + U(3,I) * DFDX(I))*0.5D0
            DEFOPG(IDPG+6) = DEFOPG(IDPG+6) +(U(2,I) * DFDZ(I)
     &                                      + U(3,I) * DFDY(I))*0.5D0
106       CONTINUE
C
        IF (ITEMPE.NE.0) VALPAR(1) = TPG
C
        CALL RCVALA ( ZI(IMATE),PHENOM,NBPAR,NOMPAR,VALPAR,2,
     &                NOMRES, VALRES, CODRET, 'FM' )
        CALL RCVALA ( ZI(IMATE),'ECRO_LINE',NBPAR,NOMPAR,VALPAR,2,
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
        CONTPG(IDPG+5) = 2*G12*DEFOPG(IDPG+5)-2*G12*ZR(IALPHA+IDPG+4)
        CONTPG(IDPG+6) = 2*G12*DEFOPG(IDPG+6)-2*G12*ZR(IALPHA+IDPG+5)
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
        DEFPPG(IDPG+5) = DEFOPG(IDPG+5) - G12M*CONTPG(IDPG+5)
        DEFPPG(IDPG+6) = DEFOPG(IDPG+6) - G12M*CONTPG(IDPG+6)
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
                 CONTPG(IDPG+I) =  CONTPG(IDPG+I)
     &             + ZR(ICONTP+IDPG+I-1) - ZR(ICONTM+IDPG+I-1)
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
     &             + ZR(ICONTP+IDPG+I-1) - ZR(ICONTM+IDPG+I-1)
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
      CALL PPGANO (NNOS,NPG,NCMP,CONTPG,ZR(ICONTZ))
      CALL PPGANO (NNOS,NPG,NCMP,DEFPPG,ZR(IDEFOZ))
C
      END
