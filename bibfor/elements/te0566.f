      SUBROUTINE TE0566 ( OPTION , NOMTE )
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
C TOLE CRP_7
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ----------------------------------------------------------------------
C     BUT: CALCUL DE LA PROJECTION DE ALPHA0 (METHODE ZAC) PG
C          ELEMENTS ISOPARAMETRIQUES 3D
C
C     IN   OPTION : OPTION DE CALCUL
C                   'PROJ_ALPHA_ZAC'
C          NOMTE  : NOM DU TYPE ELEMENT
C ----------------------------------------------------------------------
      PARAMETER          (NBRES = 4 , NCMP = 6)
      CHARACTER*24       CHVAL,CHCTE
      CHARACTER*8        NOMRES(NBRES),ELREFE
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALRES(NBRES)
      REAL*8             R3S2,ALPHA2(6)
      REAL*8             KRON(6),SIGMIN(6),SIGMAX(6),DELTA(6),EPS
      REAL*8             NORM,NORM1,NORM2,NORSIG,ZERO,INSTAN
      REAL*8             DFDX(27),DFDY(27),DFDZ(27),TPG,POIDS,X,Y
      INTEGER            NNO,KP,I,K,ITEMPE,IDPG
      INTEGER            IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM,IMATE
      INTEGER            NPG,NBPG(10),ICONTP,ICONTM
      INTEGER            IALPH0,IALPHL,IALPHS,IALPHI,IADAPT
      EXTERNAL           NORSIG
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      DATA               KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C
      CALL ELREF1(ELREFE)
      ZERO = 0.D0
      R3S2 = SQRT(3.D0/2.D0)
C
C
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 10 I = 1,NBFPG
        NBPG(I) = ZI(JIN+3-1+I)
10    CONTINUE
C
      NPG = NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO

CJMP   PARAIT FAUX =>  ON INTERDIT
C      CALL UTMESS('F','POST_ZAC','NE MARCHE PAS ENCORE EN 3D')

C      IF(ELREFE.EQ.'TETRA10 '.OR.ELREFE.EQ.'HEXA20  ' ) THEN
C        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
C     &                  + NBPG(2)*(1+(NDIM+1)*NNO)
C       ELSE IF(ELREFE.EQ.'PENTA15 ' ) THEN
C        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO)
C      ENDIF

      IVF = IPOIDS + NPG
      IDFDE  = IVF + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCONTMR','L',ICONTM)
      CALL JEVECH('PCONTPR','L',ICONTP)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PALPHA0','L',IALPH0)
      CALL JEVECH('PALPHAL','E',IALPHL)
      CALL JEVECH('PALPHAS','E',IALPHS)
      CALL JEVECH('PALPHAI','E',IALPHI)
      CALL JEVECH('PADAPTI','E',IADAPT)
      CALL JEVECH('PALPHA1','E',IALPH1)
C
      INSTAN = ZR(ITEMPS)
C
C     INSTAN = 1 : TEST EXISTENCE INTERSECTION COMMUNE A TOUTES LES
C                  SPHERES EN NON RADIAL
C
      NOMRES(1) = 'E'
      NOMRES(2) = 'SY'
      NOMRES(3) = 'D_SIGM_EPSI'
C
      IF (INSTAN.EQ.1.D0) ZR(IADAPT) = 0.D0
C
      DO 101 KP = 1 , NPG
C
          K=(KP-1)* 3 * NNO
          IT = (KP-1) * NNO
          IDPG=(KP-1) * NCMP
C
          CALL DFDM3D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                  ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
          TPG = 0.D0
          DO 102 I=1,NNO
            TPG = TPG + ZR(ITEMPE+I-1) *ZR(IVF+IT+I-1)
102       CONTINUE
C
          CALL RCVALA ( ZI(IMATE),'ELAS',1,'TEMP',TPG,1,NOMRES(1),
     &                  VALRES, CODRET, 'FM' )
          CALL RCVALA ( ZI(IMATE),'ECRO_LINE',1,'TEMP',TPG,2,NOMRES(2),
     &                  VALRES(2), CODRET, 'FM' )
C
          CECR = 2.D0/3.D0*(VALRES(1)*VALRES(3))/(VALRES(1)-VALRES(3))
          SIG0 = VALRES(2)
C
          TRMIN = 0.D0
          TRMAX = 0.D0
          DO 103 I=1,3
            TRMIN = TRMIN + ZR(ICONTM+IDPG-1+I)
            TRMAX = TRMAX + ZR(ICONTP+IDPG-1+I)
103       CONTINUE
          TRMIN = TRMIN /3.D0
          TRMAX = TRMAX /3.D0
          DO 104 I=1,NCMP
            SIGMIN(I) = ZR(ICONTM+IDPG-1+I) - TRMIN*KRON(I)
            SIGMAX(I) = ZR(ICONTP+IDPG-1+I) - TRMAX*KRON(I)
104       CONTINUE
          DO 105 I=1,NCMP
            DELTA(I) = SIGMAX(I) - SIGMIN(I)
105       CONTINUE
          NORM = R3S2*NORSIG(DELTA,NCMP)
C
C  TEST SUR L'INTERSECTION DES BOULES MIN ET MAX
C
          IF ((NORM - 2.D0*SIG0).LE.ZERO) THEN
C
C  ADAPTATION SUR LE POINT DE GAUSS
C
            IF (INSTAN.EQ.1.D0) GOTO 101
C
            DO 106 I=1,NCMP
              DELTA(I) = ZR(IALPH0+IDPG+I-1) - SIGMIN(I)
106         CONTINUE
            NORM = R3S2*NORSIG(DELTA,NCMP)
            IF ((NORM - SIG0).LE.ZERO) THEN
C
C  ALPHA0 EST DANS LA BOULE MIN
C
              DO 107 I=1,NCMP
                DELTA(I) = ZR(IALPH0+IDPG+I-1) - SIGMAX(I)
107           CONTINUE
              NORM = R3S2*NORSIG(DELTA,NCMP)
C
              IF ((NORM - SIG0).LE.ZERO) THEN
C
C      ALPHA0 EST DANS L'INTERSECTION
C
                 DO 108 I=1,NCMP
                   ZR(IALPHL+IDPG+I-1) = ZR(IALPH0+IDPG+I-1)/CECR
                   ZR(IALPHS+IDPG+I-1) = 0.D0
                   ZR(IALPHI+IDPG+I-1) = 0.D0
                   ZR(IALPH1+IDPG+I-1) = ZR(IALPH0+IDPG+I-1)
108              CONTINUE
C
              ELSE
C
C      ALPHA0 N'EST PAS DANS L'INTERSECTION
C
                CALL PROJZ1(SIGMIN,SIGMAX,DELTA,SIG0,NORM,NCMP,ALPHA2)
                DO 111 I=1,NCMP
                   ZR(IALPHL+IDPG+I-1) = ALPHA2(I)/CECR
                   ZR(IALPHS+IDPG+I-1) = 0.D0
                   ZR(IALPHI+IDPG+I-1) = 0.D0
                   ZR(IALPH1+IDPG+I-1) = ALPHA2(I)
 111           CONTINUE
C
              END IF
C
C  ALPHA0 N'EST PAS DANS LA BOULE MIN
C
            ELSE
C
               DO 116 I=1,NCMP
                  DELTA(I) = ZR(IALPH0+IDPG+I-1) - SIGMAX(I)
 116           CONTINUE
               NORM = R3S2*NORSIG(DELTA,NCMP)
               IF ((NORM - SIG0).LE.ZERO) THEN
                   DO 117 I=1,NCMP
                     PROV = SIGMAX(I)
                     SIGMAX(I)=SIGMIN(I)
                     SIGMIN(I) = PROV
                     DELTA(I) = ZR(IALPH0+IDPG+I-1) - SIGMAX(I)
 117               CONTINUE
                   NORM = R3S2*NORSIG(DELTA,NCMP)
                   CALL PROJZ1(SIGMIN,SIGMAX,DELTA,
     &                          SIG0,NORM,NCMP,ALPHA2)
               ELSE
                   DO 118 I=1,NCMP
                     ALPHA2(I) = ZR(IALPH0+IDPG+I-1)
 118               CONTINUE
                   CALL PROJZ2(SIGMIN,SIGMAX,SIG0,NCMP,ALPHA2)
               END IF
               DO 119 I=1,NCMP
                   ZR(IALPHL+IDPG+I-1) = ALPHA2(I)/CECR
                   ZR(IALPHS+IDPG+I-1) = 0.D0
                   ZR(IALPHI+IDPG+I-1) = 0.D0
                   ZR(IALPH1+IDPG+I-1) = ALPHA2(I)
 119           CONTINUE
C
            END IF
C
          ELSE
C
C   ACCOMODATION SUR LE POINT DE GAUSS
C
            ZR(IADAPT) = 1.D0
            IF (INSTAN.EQ.1.D0) GOTO 101
C
            DO 120 I=1,NCMP
              DELTA(I) = SIGMAX(I)-SIGMIN(I)
              ZR(IALPHL+IDPG+I-1) = (SIGMIN(I)+SIGMAX(I))/(2.D0*CECR)
              ZR(IALPHI+IDPG+I-1) = ((+DELTA(I)-2.D0*SIG0*(+DELTA(I))
     &                          /(R3S2*NORSIG(DELTA,NCMP))))/CECR
              ZR(IALPHS+IDPG+I-1) = (SIGMAX(I)-SIGMIN(I))/(2.D0*CECR)
              ZR(IALPH1+IDPG+I-1) = ZR(IALPH0+IDPG+I-1)
 120        CONTINUE
C
          END IF
C
101   CONTINUE
C
      END
