      SUBROUTINE TE0027(OPTION,NOMTE)
C-----------------------------------------------------------------------
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
C TOLE CRP_20
C-----------------------------------------------------------------------
C FONCTION REALISEE:
C
C      CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
C      EN ELASTICITE LINEAIRE ET NON LINEAIRE
C      ELEMENTS ISOPARAMETRIQUES 3D
C
C      OPTION : 'CALC_G'     (CHARGES REELLES)
C               'CALC_G_F'   (CHARGES FONCTIONS)
C
C ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       11/12/00 (OB): DEPLACEMENT DU TEST DE LA NULLITE DU THETAFISS,
C                      TOILETTAGE FORTRAN,
C                      RAJOUT DE PARAMETRE DANS L'APPEL A NMELNL POUR
C                      COHERENCE AVEC TE0096 ET OPTION='CALC_DG' EN 2D.
C-----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*16        OPTION , NOMTE

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

C DECLARATION VARIABLES LOCALES
      INTEGER  IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IPOI1,IVF1,IDFDE1,IDFDK1
      INTEGER  ICOMP,IGEOM,ITEMPS,IDEPL,ITREF,ITEMP,IMATE,IDFDN1
      INTEGER  IEPSR,IEPSF,ISIGI,IDEPI,ISIGM,IEPSP,IVARI
      INTEGER  IFORC,IFORF,ITHET,IGTHET,IROTA,IPESA,IER
      INTEGER  NNO,NNOS,NPG,NPG1,NPG2,NPG3,NCMP,NBFPG,NBPG(10)
      INTEGER  I,J,K,KK,L,M,KP,NDIM,COMPT,JIN,JVAL,NBVARI

      REAL*8   EPSI,RAC2,R8PREM,CRIT(3)
      REAL*8   DFDI(81),F(3,3),SR(3,3)
      REAL*8   EPS(6),EPSIN(6),DEPSIN(6,3),EPSP(6),DEPSP(6,3)
      REAL*8   EPSINO(162),EPSIPG(162),FNO(81),EPSNO(162)
      REAL*8   SIGL(6),SIGIN(6),DSIGIN(6,3)
      REAL*8   THET,TREF,TG,TGDM(3)
      REAL*8   PROD,PROD1,PROD2,DIVT,VALPAR(4)
      REAL*8   TCLA,TTHE,TFOR,TPLAS,TINI,POIDS,RBID
      REAL*8   DUDM(3,4),DFDM(3,4),DTDM(3,4),DER(4)
      REAL*8   P,PPG,DPDM(3),RP,ENERGI(2),RHO,OM,OMO
      REAL*8   RPIPO,T1PIPO(6),T2PIPO(2),T3PIPO(6)

      LOGICAL  GRAND,FONC,INCR,EPSINI,LPIPO

      CHARACTER*2    CODRET
      CHARACTER*8    ELREFE,NOMPAR(4),TYPMOD(2)
      CHARACTER*16   COMPOR(4),OPRUPT,PHENOM
      CHARACTER*24   CHVAL,CHCTE

C DEB ------------------------------------------------------------------


C INITIALISATIONS POUR APPEL A NMELNL
      CALL ELREF1(ELREFE)
      LPIPO = .FALSE.
      RPIPO = 0.D0
      T2PIPO(1) = 0.D0
      T2PIPO(2) = 0.D0
      DO 1 I=1,6
        T1PIPO(I) = 0.D0
        T3PIPO(I) = 0.D0
1     CONTINUE

      CALL JEMARQ()
      EPSI   = R8PREM()
      RAC2   = SQRT(2.D0)
      OPRUPT = 'RUPTURE'
      EPSINI = .FALSE.
      TYPMOD(1) = '3D      '

      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,' ',JIN)
      NDIM  = ZI(JIN+1-1)
      NNO   = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      NNOS  = ZI(JIN+3-1+NBFPG+1)
      DO 5 I = 1,NBFPG
        NBPG(I) = ZI(JIN+3-1+I)
5     CONTINUE
      NPG1 = NBPG(1)
      NPG2 = NBPG(2)
      NPG3 = NBPG(3)
      NCMP  = 2*NDIM

C - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
      CALL JEVECH('PTHETAR','L',ITHET)
      CALL JEVECH('PGTHETA','E',IGTHET)
      TCLA  = 0.D0
      TTHE  = 0.D0
      TFOR  = 0.D0
      TPLAS = 0.D0
      TINI  = 0.D0
      COMPT = 0
      DO 15 I=1,NNO
        THET = 0.D0
        DO 14 J=1,NDIM
          THET = THET + ABS(ZR(ITHET+NDIM*(I-1)+J-1))
14      CONTINUE
        IF(THET.LT.EPSI) COMPT = COMPT+1
15    CONTINUE
      IF(COMPT.EQ.NNO)  GOTO 9999

      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,' ',JVAL)
      IPOIDS = JVAL +(NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG1
      IDFDE  = IVF    + NPG1*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PTEREF' ,'L',ITREF)
      CALL JEVECH('PTEMPER','L',ITEMP)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      IF (OPTION.EQ.'CALC_G_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PFFVOLU','L',IFORF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
        VALPAR(4) = ZR(ITEMPS)
      CALL TECACH(.TRUE.,.FALSE.,'PEPSINF',1,IEPSF)
        IF (IEPSF.NE.0) EPSINI = .TRUE.
      ELSE
        FONC = .FALSE.
        CALL JEVECH('PFRVOLU','L',IFORC)
      CALL TECACH(.TRUE.,.FALSE.,'PEPSINR',1,IEPSR)
        IF (IEPSR.NE.0) EPSINI = .TRUE.
      ENDIF
      DO 10 I = 1,4
        COMPOR(I)= ZK16(ICOMP+I-1)
10    CONTINUE
      GRAND = COMPOR(3)(1:5).EQ.'GREEN'
      INCR  = COMPOR(4)(1:9).EQ.'COMP_INCR'
      READ(ZK16(ICOMP+1),'(I16)') NBVARI
      IF (INCR) THEN
        CALL JEVECH('PCONTRR','L',ISIGM)
        CALL JEVECH('PDEFOPL','L',IEPSP)
        CALL JEVECH('PVARIPR','L',IVARI)
      ENDIF
      CALL TECACH(.TRUE.,.FALSE.,'PPESANR',1,IPESA)
      CALL TECACH(.TRUE.,.FALSE.,'PROTATR',1,IROTA)
      CALL TECACH(.TRUE.,.FALSE.,'PSIGINR',1,ISIGI)
      CALL TECACH(.TRUE.,.FALSE.,'PDEPINR',1,IDEPI)

      IF ((ISIGI.NE.0).AND.EPSINI) THEN
        CALL UTMESS('F','CALC_G','UNE DEFORMATION INITIALE EST '//
     &  'PRESENTE DANS LA CHARGE : INCOMPATIBLE AVEC LA CONTRAINTE'//
     &  ' INITIALE SIGMA_INIT')
      ENDIF

      TREF  = ZR(ITREF)
      DO 20 I=1,NCMP*NNO
        EPSINO(I) = 0.D0
20    CONTINUE

C - RECUPERATION DES CHARGES ET DEFORMATIONS INITIALES ----------------

      IF (FONC) THEN
        DO 50 I=1,NNO
          DO 30 J=1,NDIM
            VALPAR(J) = ZR(IGEOM+NDIM*(I-1)+J-1)
30        CONTINUE
          DO 40 J=1,NDIM
            KK = NDIM*(I-1)+J
            CALL FOINTE('FM',ZK8(IFORF+J-1),4,NOMPAR,VALPAR,FNO(KK),IER)
40        CONTINUE
          IF (EPSINI) THEN
            DO 45 J=1,NCMP
              KK = NCMP*(I-1)+J
         CALL FOINTE('FM',ZK8(IEPSF+J-1),4,NOMPAR,VALPAR,EPSINO(KK),IER)
45          CONTINUE
          ENDIF
50      CONTINUE
      ELSE
        DO 80 I=1,NNO
          DO 60 J=1,NDIM
            FNO(NDIM*(I-1)+J)= ZR(IFORC+NDIM*(I-1)+J-1)
60        CONTINUE
          IF (EPSINI) THEN
            DO 70 J=1,3
              EPSINO(NCMP*(I-1)+J  ) = ZR(IEPSR+NCMP*(I-1)+J-1)
              EPSINO(NCMP*(I-1)+J+3) = ZR(IEPSR+NCMP*(I-1)+J-1+3)*RAC2
70          CONTINUE
          ENDIF
80      CONTINUE
      ENDIF

      IF ((IPESA.NE.0).OR.(IROTA.NE.0)) THEN
        CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
        CALL RCVALA(ZI(IMATE),PHENOM,1,' ',RBID,1,'RHO',RHO,CODRET,'FM')
        IF (IPESA.NE.0) THEN
          DO 95 I=1,NNO
            DO 90 J=1,NDIM
              KK = NDIM*(I-1)+J
              FNO(KK)=FNO(KK)+RHO*ZR(IPESA)*ZR(IPESA+J)
90          CONTINUE
95        CONTINUE
        ENDIF
        IF (IROTA.NE.0) THEN
          OM = ZR(IROTA)
          DO 105 I=1,NNO
            OMO = 0.D0
            DO 100 J=1,NDIM
              OMO = OMO + ZR(IROTA+J)* ZR(IGEOM+NDIM*(I-1)+J-1)
100         CONTINUE
            DO 103 J=1,NDIM
              KK = NDIM*(I-1)+J
              FNO(KK)=FNO(KK)+RHO*OM*OM*(ZR(IGEOM+KK-1)-OMO*ZR(IROTA+J))
103         CONTINUE
105       CONTINUE
        ENDIF
      ENDIF

      IF (IDEPI.NE.0) THEN
        IPOIDS = JVAL + (NDIM+1)*NNO*NNO
        IF(ELREFE.EQ.'TETRA10 '.OR.ELREFE.EQ.'HEXA20  '
     &  .OR.ELREFE.EQ.'HEXA27  ' ) THEN
          NPG   = NPG3
          IPOI1 = IPOIDS + (NPG1+NPG2)*(1+(NDIM+1)*NNO)
        ELSE IF(ELREFE.EQ.'PENTA15 ' ) THEN
          NPG   = NPG2
          IPOI1 = IPOIDS + NPG1*(1+(NDIM+1)*NNO)
        ELSE
          NPG   = NPG1
          IPOI1 = IPOIDS
        ENDIF
        IVF1    = IPOI1  + NPG
        IDFDE1  = IVF1   + NPG*NNO
        IDFDN1  = IDFDE1 + 1
        IDFDK1  = IDFDN1 + 1
        DO 120 KP=1,NPG
          L    = (KP-1)*NNO
          CALL NMGEOM (NDIM,NNO,.FALSE.,GRAND,ZR(IGEOM),KP,
     &                 ZR(IPOI1+KP-1),ZR(IVF1+L),ZR(IDFDE1),ZR(IDFDN1),
     &                 ZR(IDFDK1),ZR(IDEPI),RBID,DFDI,F,EPS,RBID)
          DO 110 I=1,NCMP
            EPSIPG((KP-1)*NCMP+I)= EPS(I)
110       CONTINUE
120     CONTINUE
C
        CALL PPGANO (NNOS,NPG,NCMP,EPSIPG,EPSNO)
        DO 121 I=1,NNO*NCMP
          EPSINO(I) = EPSINO(I)+EPSNO(I)
121     CONTINUE
      ENDIF

C ======================================================================

      DO 800 KP=1,NPG1
        L   = (KP-1)*NNO
        TG  = 0.D0
        PPG = 0.D0
        DO 220 I=1,3
          TGDM(I) = 0.D0
          DPDM(I) = 0.D0
          DO 200 J=1,3
            SR(I,J) = 0.D0
200       CONTINUE
          DO 210 J=1,4
            DUDM(I,J) = 0.D0
            DTDM(I,J) = 0.D0
            DFDM(I,J) = 0.D0
210       CONTINUE
220     CONTINUE
        DO 240 I=1,6
          SIGL (I) = 0.D0
          SIGIN(I) = 0.D0
          EPSIN(I) = 0.D0
          EPSP(I)  = 0.D0
          EPS (I)  = 0.D0
          DO 230 J=1,3
            DSIGIN(I,J)= 0.D0
            DEPSIN(I,J)= 0.D0
            DEPSP(I,J) = 0.D0
230       CONTINUE
240     CONTINUE

C - CALCUL DES ELEMENTS GEOMETRIQUES

        CALL NMGEOM (NDIM,NNO,.FALSE.,GRAND,ZR(IGEOM),KP,
     &               ZR(IPOIDS+KP-1),ZR(IVF+L),ZR(IDFDE),ZR(IDFDN),
     &               ZR(IDFDK),ZR(IDEPL),POIDS,DFDI,F,EPS,RBID)

C - CALCULS DES GRADIENTS DE U (DUDM),THETA (DTDM) ET FORCE(DFDM)
C   DE LA TEMPERATURE AUX POINTS DE GAUSS (TG) ET SON GRADIENT (TGDM)

        DO 320 I=1,NNO
          DER(1) = DFDI(I)
          DER(2) = DFDI(I+NNO)
          DER(3) = DFDI(I+2*NNO)
          DER(4) = ZR(IVF+L+I-1)
          TG    = TG + ZR(ITEMP+I-1)*DER(4)
          DO 310 J=1,NDIM
            TGDM(J)     = TGDM(J)   + ZR(ITEMP+I-1)    *DER(J)
            DO 300 K=1,NDIM
              DUDM(J,K) = DUDM(J,K) + ZR(IDEPL+NDIM*(I-1)+J-1)*DER(K)
              DTDM(J,K) = DTDM(J,K) + ZR(ITHET+NDIM*(I-1)+J-1)*DER(K)
              DFDM(J,K) = DFDM(J,K) + FNO(NDIM*(I-1)+J)*DER(K)
300         CONTINUE
              DUDM(J,4) = DUDM(J,4) + ZR(IDEPL+NDIM*(I-1)+J-1)*DER(4)
              DTDM(J,4) = DTDM(J,4) + ZR(ITHET+NDIM*(I-1)+J-1)*DER(4)
              DFDM(J,4) = DFDM(J,4) + FNO(NDIM*(I-1)+J)*DER(4)
310       CONTINUE
320     CONTINUE

C - CALCULS DES GRADIENTS DE P (DPDM) ET EPSP (DEPSP) EN PLASTICITE

        IF (INCR) THEN
          DO 380 I=1,NNO
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = DFDI(I+2*NNO)
            DER(4) = ZR(IVF+L+I-1)
            P   = ZR(IVARI+(I-1)*NBVARI)
            PPG = PPG + P*DER(4)
            DO 350 J=1,NCMP
              EPSP(J) = EPSP(J)+ ZR(IEPSP+NCMP*(I-1)+J-1)*DER(4)
350         CONTINUE
            IF (P.GE.EPSI) THEN
              DO 360 J=1,NDIM
                DPDM(J)= DPDM(J) + ZR(IVARI+(I-1)*NBVARI)*DER(J)
360           CONTINUE
              DO 370 K=1,NDIM
                DO 365 J=1,NCMP
                  DEPSP(J,K)=DEPSP(J,K)+ZR(IEPSP+NCMP*(I-1)+J-1)*DER(K)
365             CONTINUE
370           CONTINUE
            ENDIF
380       CONTINUE
          DO 382 I=4,NCMP
            EPSP(I)=EPSP(I)*RAC2
            DO  381 J=1,NDIM
              DEPSP(I,J)=DEPSP(I,J)*RAC2
381         CONTINUE
382       CONTINUE
          IF (PPG.LT.EPSI)THEN
            PPG = 0.D0
            DO 390 J=1,NCMP
              EPSP(J) =  0.D0
390         CONTINUE
          ENDIF
        ENDIF

C -  DEFORMATIONS INITIALES

        IF ((IDEPI.NE.0).OR.EPSINI) THEN
          DO 420 I=1,NNO
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = DFDI(I+2*NNO)
            DER(4) = ZR(IVF+L+I-1)
            DO 400 J=1,NCMP
              EPSIN(J) = EPSIN(J)+ EPSINO(NCMP*(I-1)+J)*DER(4)
400         CONTINUE
            DO 415 J=1,NCMP
              DO 410 K=1,NDIM
                DEPSIN(J,K) = DEPSIN(J,K)+EPSINO(NCMP*(I-1)+J)*DER(K)
410           CONTINUE
415         CONTINUE
420       CONTINUE
          DO 430 I=1,NCMP
            EPS(I) = EPS(I)-EPSIN(I)
430       CONTINUE
        ENDIF

C -  CONTRAINTES LAGRANGIENNES (SIGL),ENERGIE LIBRE ET DERIVEE / T

        IF (INCR) THEN
          CALL NMPLRU(NDIM,TYPMOD,ZI(IMATE),COMPOR,TG,TREF,PPG,
     &                EPS,EPSP,RP,ENERGI)
          DO 431 I = 1,3
            SIGL(I)  = ZR(ISIGM+NCMP*(KP-1)+I-1)
            SIGL(I+3)= ZR(ISIGM+NCMP*(KP-1)+I-1+3)*RAC2
431       CONTINUE
        ELSE
          CRIT(1) = 300
          CRIT(2) = 0.D0
          CRIT(3) = 1.D-3
          CALL NMELNL(NDIM,TYPMOD,ZI(IMATE),COMPOR,CRIT,TG,TREF,
     &                OPRUPT,EPS,SIGL,RBID,RBID,ENERGI,
     &                LPIPO,RPIPO,T1PIPO,T2PIPO,T3PIPO)
        ENDIF
        DIVT  = DTDM(1,1)+DTDM(2,2)+DTDM(3,3)

C  - CONTRAINTES INITIALES

        IF (ISIGI.NE.0) THEN
          DO 460 I=1,NNO
            DER(1) = DFDI(I)
            DER(2) = DFDI(I+NNO)
            DER(3) = DFDI(I+2*NNO)
            DER(4) = ZR(IVF+L+I-1)
            DO 440 J=1,NCMP
              SIGIN(J) = SIGIN(J)+ ZR(ISIGI+NCMP*(I-1)+J-1)*DER(4)
440         CONTINUE
            DO 455 J=1,NCMP
              DO 450 K=1,NDIM
                DSIGIN(J,K)=DSIGIN(J,K)+ZR(ISIGI+NCMP*(I-1)+J-1)*DER(K)
450           CONTINUE
455         CONTINUE
460       CONTINUE
          DO 463 I=4,NCMP
            SIGIN(I) = SIGIN(I)*RAC2
            DO  462 J=1,NDIM
              DSIGIN(I,J) = DSIGIN(4,1)*RAC2
462         CONTINUE
463       CONTINUE
          DO 464 I=1,NCMP
            SIGL(I) = SIGL(I)+ SIGIN(I)
464       CONTINUE
          DO 465 I=1,NCMP
            ENERGI(1) = ENERGI(1) + (EPS(I)+0.5D0*EPSIN(I))*SIGIN(I)
465       CONTINUE
        ENDIF

        SR(1,1)= SIGL(1)
        SR(2,2)= SIGL(2)
        SR(3,3)= SIGL(3)
        SR(1,2)= SIGL(4)/RAC2
        SR(1,3)= SIGL(5)/RAC2
        SR(2,3)= SIGL(6)/RAC2
        SR(2,1)= SR(1,2)
        SR(3,1)= SR(1,3)
        SR(3,2)= SR(2,3)

C - CALCUL DE G

C - TERME THERMOELASTIQUE CLASSIQUE F.SIG:(GRAD(U).GRAD(THET))-ENER*DIVT

        PROD  = 0.D0
        DO 490 I=1,3
          DO 480 J=1,3
            DO 475 K=1,3
              DO 470 M=1,3
                  PROD =PROD+F(I,J)*SR(J,K)*DUDM(I,M)*DTDM(M,K)
470           CONTINUE
475         CONTINUE
480       CONTINUE
490     CONTINUE
        TCLA  = TCLA + POIDS*( PROD - ENERGI(1)*DIVT)

C - TERME THERMIQUE :   -(D(ENER)/DT)(GRAD(T).THETA)

        PROD = 0.D0
        DO 500 I=1,NDIM
          PROD = PROD + TGDM(I)*DTDM(I,4)
500     CONTINUE
        TTHE = TTHE - POIDS*PROD*ENERGI(2)

C - TERME FORCE VOLUMIQUE

        DO 520 I=1,NDIM
          PROD=0.D0
          DO 510 J=1,NDIM
            PROD = PROD + DFDM(I,J)*DTDM(J,4)
510       CONTINUE
          TFOR = TFOR + DUDM(I,4)*(PROD+DFDM(I,4)*DIVT)*POIDS
520     CONTINUE

C - TERME PLASTIQUE :   SIG:(GRAD(EPSP).THETA)- R(P).GRAD(P).THETA

        IF (INCR) THEN
          PROD1=0.D0
          PROD2=0.D0
          DO 620 I=1,NCMP
            DO 610 J=1,NDIM
                PROD1 = PROD1 + SIGL(I)*DEPSP(I,J)*DTDM(J,4)
610         CONTINUE
620       CONTINUE
          DO 650 I=1,NDIM
            PROD2 = PROD2 + RP*DPDM(I)*DTDM(I,4)
650       CONTINUE
          TPLAS = TPLAS + (PROD1-PROD2)*POIDS
        ENDIF

C - TERME INITIAL:  SIG:GRAD(EPSIN).THETA-(EPS-EPSIN):GRAD(SIGIN).THETA

        IF ((ISIGI.NE.0).OR.(IDEPI.NE.0).OR.EPSINI) THEN
          PROD1=0.D0
          PROD2=0.D0
          DO 670 I=1,NCMP
            DO 660 J=1,NDIM
              PROD1=PROD1+(SIGL(I)-0.5D0*SIGIN(I))*DEPSIN(I,J)*DTDM(J,4)
              PROD2=PROD2+(EPS(I) +0.5D0*EPSIN(I))*DSIGIN(I,J)*DTDM(J,4)
660         CONTINUE
670       CONTINUE
          TINI = TINI + (PROD1-PROD2)*POIDS
        ENDIF

800   CONTINUE
9999  CONTINUE

       ZR(IGTHET) = TTHE+TCLA+TFOR+TPLAS+TINI

      CALL JEDEMA()
      END
