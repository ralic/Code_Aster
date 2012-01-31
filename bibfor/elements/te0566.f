      SUBROUTINE TE0566(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 31/01/2012   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE PELLET J.PELLET
      IMPLICIT NONE

      CHARACTER*16 OPTION,NOMTE

C ......................................................................
C    - FONCTION REALISEE: CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
C                POUR DES ELEMENTS QUASI-INCOMPRESSIBLES OSGS
C                EN 2D ET AXI
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................

      INTEGER NNO1,NNO2,NPG1,IMATUU,JCRET,CODRET,IRET
      INTEGER IPOIDS,IVF1,IVF2,IDFDE1,IGEOM,NDIM,NNOS,JGANO
      INTEGER IMATE,ICONTM,IVARIM,NPG2,IDFDE2
      INTEGER IINSTM,IINSTP,IDEPLM,IDEPLP,ICOMPO,LGPG,ICARCR
      INTEGER IVECTU,ICONTP,IVARIP,ITAB(7)
      INTEGER I, N ,KK , M, JMAX, J, IBID
      INTEGER NIV,IFM

      REAL*8 DEPLM(2,9),DDEPL(2,9),PRESM(1,4),DPRES(1,4)
      REAL*8 PIM(2,9),DPI(2,9)
      REAL*8 KUU(2,9,2,9), KUA(2,9,1,4),KAA(1,4,1,4)
      REAL*8 KPP(2,9,2,9), KUP(2,9,2,9),KAP(1,4,2,9)
      REAL*8 FINTU(2,9), FINTA(1,4),FINTP(2,9)
      REAL*8 ANGMAS(3),R8NNEM,HK,STAB

      CHARACTER*8   TYPMOD(2),BLAN8
      CHARACTER*8   ELREFE,ELREF2,ALIAS8
      CHARACTER*4   FAMI

      LOGICAL LTEATT

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

C - FONCTIONS DE FORMES ET POINTS DE GAUSS
      CALL ELREF1(ELREFE)

      CALL TEATTR(' ','S','ALIAS8',ALIAS8,IBID)

      IF ( ALIAS8(6:7).EQ.'TR') THEN
         ELREF2 = 'TR3'
      ELSEIF (ALIAS8(6:7).EQ. 'QU'  ) THEN
         ELREF2 = 'QU4'
      ELSE
        CALL U2MESK('F','DVP_4',1,NOMTE(7:10))
      ENDIF

      FAMI = 'RIGI'
      CALL ELREF4(ELREFE,FAMI,NDIM,NNO1,NNOS,NPG1,IPOIDS,IVF1,
     &                                              IDFDE1,JGANO)

      CALL ELREF4(ELREF2,FAMI,NDIM,NNO2,NNOS,NPG2,IPOIDS,IVF2,
     &                                              IDFDE2,JGANO)

      BLAN8='        '
      TYPMOD(1) = BLAN8
      TYPMOD(2) = BLAN8
      IF (LTEATT(' ','AXIS','OUI')) THEN
        TYPMOD(1) = 'AXIS    '
      ELSE IF (NOMTE(3:4).EQ.'PL') THEN
        TYPMOD(1) = 'D_PLAN  '
      END IF
      CODRET = 0

C - PARAMETRES EN ENTREE
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCONTMR','L',ICONTM)
      CALL JEVECH('PVARIMR','L',IVARIM)
      CALL JEVECH('PINSTMR','L',IINSTM)
      CALL JEVECH('PINSTPR','L',IINSTP)
      CALL JEVECH('PDEPLMR','L',IDEPLM)
      CALL JEVECH('PDEPLPR','L',IDEPLP)
      CALL JEVECH('PCOMPOR','L',ICOMPO)
      CALL TECACH('OON','PVARIMR',7,ITAB,IRET)
      LGPG = MAX(ITAB(6),1)*ITAB(7)
      CALL JEVECH('PCARCRI','L',ICARCR)

C - ORIENTATION DU MASSIF
      CALL R8INIR(3, R8NNEM(), ANGMAS ,1)

C - PARAMETRES EN SORTIE
      IF (OPTION(1:14).EQ.'RIGI_MECA_TANG' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN
        CALL JEVECH('PMATUUR','E',IMATUU)
      END IF
      IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN
        CALL JEVECH('PVECTUR','E',IVECTU)
        CALL JEVECH('PCONTPR','E',ICONTP)
        CALL JEVECH('PVARIPR','E',IVARIP)
        CALL JEVECH('PCODRET','E',JCRET )
      END IF

C - FORMATTAGE DES INCONNUES
      KK = 0
      DO 10 N = 1, NNO1
        DO 15 I = 1,5
          IF (I.LE.2) THEN
            DEPLM(I,N) = ZR(IDEPLM+KK)
            DDEPL(I,N) = ZR(IDEPLP+KK)
            KK = KK + 1
          END IF
          IF (I.EQ.3 .AND. N.LE.NNO2) THEN
            PRESM(I-2,N) = ZR(IDEPLM+KK)
            DPRES(I-2,N) = ZR(IDEPLP+KK)
            KK = KK + 1
          END IF
          IF (I.GE.4) THEN
            PIM(I-3,N) = ZR(IDEPLM+KK)
            DPI(I-3,N) = ZR(IDEPLP+KK)
            KK = KK + 1
          END IF
 15     CONTINUE
 10   CONTINUE

      CALL INFNIV(IFM,NIV)
      CALL UTHK(NOMTE,ZR(IGEOM),HK,2,IBID,IBID,IBID,IBID,NIV,IFM)
      STAB = 1.D-4*HK*HK

      IF (ZK16(ICOMPO+2) (1:5).EQ.'PETIT') THEN
          CALL NIPL2O(FAMI,NNO1,NNO2,NPG1,IPOIDS,IVF1,IVF2,IDFDE1,
     &                ZR(IGEOM),TYPMOD,
     &                OPTION,ZI(IMATE),ZK16(ICOMPO),LGPG,ZR(ICARCR),
     &                ZR(IINSTM),ZR(IINSTP),
     &                DEPLM,DDEPL,
     &                ANGMAS,
     &                PRESM , DPRES ,PIM,DPI,
     &                ZR(ICONTM),ZR(IVARIM),ZR(ICONTP),ZR(IVARIP),STAB,
     &                FINTU, FINTA,FINTP,KUU,KAA,KPP,KUA,KUP,KAP,CODRET)

        ELSE IF (ZK16(ICOMPO+2) (1:10).EQ.'SIMO_MIEHE') THEN
          CALL U2MESS('F','ELEMENTS5_9')
        ELSE
          CALL U2MESK('F','ELEMENTS3_16',1,ZK16(ICOMPO+2))
        END IF

C  REMISE EN FORME DE LA SOLUTION

      IF ( OPTION(1:9).EQ.'FULL_MECA'  .OR.
     &     OPTION(1:9).EQ.'RAPH_MECA'  ) THEN
        ZI(JCRET) = CODRET
        KK = 0
        DO 20 N = 1, NNO1
          DO 25 I = 1,5
            IF (I.LE.2) THEN
              ZR(IVECTU+KK) = FINTU(I,N)
              KK = KK + 1
            END IF
            IF (I.EQ.3 .AND. N.LE.NNO2) THEN
              ZR(IVECTU+KK) = FINTA(I-2,N)
              KK = KK + 1
            END IF
            IF (I.GE.4) THEN
              ZR(IVECTU+KK) = FINTP(I-3,N)
              KK = KK + 1
            END IF
 25       CONTINUE
 20     CONTINUE

      ENDIF

      IF ( OPTION(1:9).EQ.'FULL_MECA'  .OR.
     &     OPTION(1:14).EQ.'RIGI_MECA_TANG'  ) THEN

        KK = 0
        DO 80 N = 1,NNO1
          DO 70 I = 1,5
            DO 60 M = 1,N
              IF (M.EQ.N) THEN
                JMAX = I
              ELSE
                JMAX = 5
              END IF
              DO 50 J = 1,JMAX
                IF (I.LE.2 .AND. J.LE.2) THEN
                  ZR(IMATUU+KK) = KUU(I,N,J,M)
                  KK = KK + 1
                END IF
                IF (I.LE.2 .AND. J.EQ.3) THEN
                  ZR(IMATUU+KK) = KUA(I,N,J-2,M)
                  KK = KK + 1
                END IF
                IF (I.LE.2 .AND. J.GE.4) THEN
                  ZR(IMATUU+KK) = KUP(I,N,J-3,M)
                  KK = KK + 1
                END IF
                IF (I.EQ.3 .AND. J.LE.2) THEN
                  ZR(IMATUU+KK) = KUA(J,M,I-2,N)
                  KK = KK + 1
                END IF
                IF (I.GE.4 .AND. J.LE.2) THEN
                  ZR(IMATUU+KK) = KUP(J,M,I-3,N)
                  KK = KK + 1
                END IF
                IF (I.EQ.3 .AND. J.EQ.3) THEN
                  ZR(IMATUU+KK) = KAA(I-2,N,J-2,M)
                  KK = KK + 1
                END IF
                IF (I.EQ.3 .AND.J.GE.4) THEN
                  ZR(IMATUU+KK) = KAP(I-2,N,J-3,M)
                  KK = KK + 1
                END IF
                IF (I.GE.4 .AND.J.EQ.3) THEN
                  ZR(IMATUU+KK) = KAP(J-2,M,I-3,N)
                  KK = KK + 1
                END IF
                IF (I.GE.4 .AND. J.GE.4) THEN
                  ZR(IMATUU+KK) = KPP(I-3,N,J-3,M)
                 KK = KK + 1
                END IF
   50         CONTINUE
   60       CONTINUE
   70     CONTINUE
   80   CONTINUE
      END IF

      END
