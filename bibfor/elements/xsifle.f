      SUBROUTINE XSIFLE(NDIM,IFA,JPTINT,JAINT,CFACE,IGEOM,
     &                  NFH,SINGU,NFE,DDLC,DDLM,JLST,IPRES,IPREF,
     &                  ITEMPS,IDEPL,NNOP,VALRES,BASLOC,ITHET,NOMPAR,
     &                  PRESN,OPTION,IGTHET,JBASEC)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/11/2012   AUTEUR MARTIN A.MARTIN 
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
C RESPONSABLE GENIAUT S.GENIAUT
C TOLE CRP_21 CRS_1404

      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*8   NOMPAR(4)
      CHARACTER*16 OPTION
      INTEGER   NDIM,IFA,CFACE(5,3),JAINT,IGEOM,NFH,SINGU,JLST,IPRES
      INTEGER   NFE,DDLC,IPREF,ITEMPS,NNOP,ITHET,JPTINT,IGTHET,IDEPL
      INTEGER   DDLM,JBASEC
      REAL*8    VALRES(3)
      REAL*8    BASLOC(9*NNOP),PRESN(27)


C    - FONCTION REALISEE:  CALCUL DES OPTIONS DE POST-TRAITEMENT
C                          EN MÉCANIQUE DE LA RUPTURE
C                          SUR LES LEVRES DES FISSURES X-FEM
C


C OUT IGTHET  : G (OPTION CALC_G) ET K1, K2, K3 (SI OPTION CALC_K_G)



      INTEGER  I,NLI,IN(3),IADZI,IAZK24,IBID2(12,3),IBID,FAC(6,4),NBF
      INTEGER  AR(12,3),NBAR,CPT,INO,NNOF,NPGF,IPOIDF,IVFF,IDFDEF
      INTEGER  IPGF,IER,ILEV,K,J,ZXAIN,XXMMVD
      INTEGER  INDI,DDLD,DDLS,NNOPS
      REAL*8   MULT,XG(4),JAC,FF(27),ND(3),LST,LSN,RR(2),RB9(3,3),RB
      REAL*8   FORREP(3,2),PRES,CISA,DEPLA(3),ANGL(2),R3BID(3)
      REAL*8   E,NU,MU,KA,COEFF,COEFF3,R8PI,R27BID(27)
      REAL*8   U1L(3),U2L(3),U3L(3),U1(3),U2(3),U3(3)
      REAL*8   E1(3),E2(3),E3(3),NORME,P(3,3),DFDI(NNOP,NDIM),DIVT,JM
      REAL*8   THETA(3),DTDM(3,3),HE(2),DFOR(3),G,K1,K2,K3,DPREDI(3,3)
      CHARACTER*8  ELREF,TYPMA,FPG,ELC,ELREFC
      DATA     HE / -1.D0 , 1.D0/

      CALL JEMARQ()

      ZXAIN = XXMMVD('ZXAIN')
C     PAR CONVENTION :
C     LEVRE INFERIEURE (HE=-1) EST LA LEVRE 1, DE NORMALE SORTANTE  ND
C     LEVRE SUPERIEURE (HE=+1) EST LA LEVRE 2, DE NORMALE SORTANTE -ND
      ANGL(1) = -R8PI()
      ANGL(2) =  R8PI()

C     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
      DDLD=NDIM*(1+NFH+NFE)

C     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
      DDLS=DDLD+DDLC

      CALL ELREF1(ELREF)
      CALL ELREF4(' ','RIGI',IBID,IBID,NNOPS,IBID,IBID,IBID,IBID,IBID)
      CALL TECAEL(IADZI,IAZK24)
      TYPMA=ZK24(IAZK24-1+3+ZI(IADZI-1+2)+3)

      IF (NDIM .EQ. 3) THEN
        CALL CONFAC(TYPMA,IBID2,IBID,FAC,NBF)
        ELC='TR3'
        FPG='XCON'
      ELSEIF (NDIM.EQ.2) THEN
        CALL CONARE(TYPMA,AR,NBAR)
        ELC='SE2'
        FPG='MASS'
      ENDIF

C     PETIT TRUC EN PLUS POUR LES FACES EN DOUBLE
      MULT=1.D0
      DO 101 I=1,NDIM
        NLI=CFACE(IFA,I)
        IN(I)=NINT(ZR(JAINT-1+ZXAIN*(NLI-1)+2))
101   CONTINUE
C     SI LES 2/3 SOMMETS DE LA FACETTE SONT DES NOEUDS DE L'ELEMENT
      IF (NDIM .EQ. 3) THEN
        IF (IN(1).NE.0.AND.IN(2).NE.0.AND.IN(3).NE.0) THEN
          DO 102 I=1,NBF
            CPT=0
            DO 103 INO=1,4
              IF (IN(1).EQ.FAC(I,INO).OR.IN(2).EQ.FAC(I,INO).OR.
     &          IN(3).EQ.FAC(I,INO))    CPT=CPT+1
 103        CONTINUE
            IF (CPT.EQ.3) THEN
               MULT=0.5D0
               GOTO 104
            ENDIF
 102      CONTINUE
        ENDIF
      ELSEIF (NDIM .EQ. 2) THEN
        IF (IN(1).NE.0.AND.IN(2).NE.0) THEN
          DO 1021 I=1,NBAR
            CPT=0
            DO 1031 INO=1,2
              IF (IN(1).EQ.AR(I,INO).OR.IN(2).EQ.AR(I,INO))
     &        CPT=CPT+1
 1031       CONTINUE
            IF (CPT.EQ.2) THEN
              MULT=0.5D0
              GOTO 104
            ENDIF
 1021     CONTINUE
        ENDIF
      ENDIF
 104  CONTINUE

      CALL ELREF4(ELC,FPG,IBID,NNOF,IBID,NPGF,IPOIDF,IVFF,IDFDEF,IBID)

C     MATÉRIAU HOMOGENE
C     ON PEUT PAS LE RECUPERER SUR LES POINTS DE GAUSS DES FACETTES
C     CAR LA FAMILLE CONCATENEE DES PG DES FACETTES N'EXISTE PAS
      E = VALRES(1)
      NU = VALRES(2)
      MU = E / (2.D0*(1.D0+NU))
      KA = 3.D0 - 4.D0*NU
      COEFF = E / (1.D0-NU*NU)
      COEFF3 = 2.D0 * MU

C     ----------------------------------------------------------------
C     BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
      DO 900 IPGF=1,NPGF

C       CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)
C       ET DES FF DE L'ÉLÉMENT PARENT AU POINT DE GAUSS
C       ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT
C       ET DE XG : COORDONNEES REELLES DU POINT DE GAUSS
C       ET DE DFDI : DERIVVES DES FF PARENT
        ELREFC='NON'
        IF (NDIM.EQ.3) THEN
          CALL XJACFF(ELREF,ELREFC,ELC,NDIM,FPG,JPTINT,IFA,CFACE,IPGF,
     &                NNOP,IGEOM,JBASEC,XG,JAC,FF,R27BID,DFDI,ND,
     &                R3BID,R3BID)
        ELSEIF (NDIM.EQ.2) THEN
          CALL XJACF2(ELREF,ELREFC,ELC,NDIM,FPG,JPTINT,IFA,CFACE,NDIM,
     &                IPGF,NNOP,IGEOM,JBASEC,XG,JAC,FF,R27BID,
     &                DFDI,ND,R3BID)
        ENDIF

C       BASE LOCALE ET LEVEL SETS AU POINT DE GAUSS
C       DIMENSIONNEMENT A 3 ET NON NDIM POUR POUVOIR UTILISER NORMEV.F
        CALL VECINI(3,0.D0,E1)
        CALL VECINI(3,0.D0,E2)
        LSN=0.D0
        LST=0.D0
        DO 100 INO=1,NNOP
          LSN = LSN + ZR(JLST-1+INO)*FF(INO)
          LST = LST + ZR(JLST-1+INO)*FF(INO)
          DO 110 I=1,NDIM
            E1(I) = E1(I) + BASLOC(3*NDIM*(INO-1)+I+NDIM)   * FF(INO)
            E2(I) = E2(I) + BASLOC(3*NDIM*(INO-1)+I+2*NDIM) * FF(INO)
 110      CONTINUE
 100    CONTINUE

C       NORMALISATION DE LA BASE
        CALL NORMEV(E1,NORME)
        CALL NORMEV(E2,NORME)
        CALL PROVEC(E1,E2,E3)

C       CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
        CALL VECINI(9,0.D0,P)
        DO 120 I=1,NDIM
          P(I,1)=E1(I)
          P(I,2)=E2(I)
          P(I,3)=E3(I)
 120    CONTINUE

C       CALCUL DE RR = SQRT(DISTANCE AU FOND DE FISSURE)
        CALL ASSERT(LST.LT.0.D0)
        RR(1)=-SQRT(-LST)
        RR(2)= SQRT(-LST)

C       -----------------------------------------------
C       1) CALCUL DES FORCES SUIVANT LES OPTIONS
C       -----------------------------------------------

        CALL VECINI(3*2,0.D0,FORREP)

        IF ((OPTION.EQ.'CALC_K_G') .OR. (OPTION.EQ.'CALC_G')) THEN

C         CALCUL DE LA PRESSION AUX POINTS DE GAUSS
          PRES = 0.D0
          CISA = 0.D0
          DO 240 INO = 1,NNOP
            IF (NDIM.EQ.3) PRES = PRES +  ZR(IPRES-1+INO) * FF(INO)
            IF (NDIM.EQ.2) THEN
              PRES = PRES + ZR(IPRES-1+2*(INO-1)+1) * FF(INO)
              CISA = CISA + ZR(IPRES-1+2*(INO-1)+2) * FF(INO)
            ENDIF
 240      CONTINUE
          DO 250 J=1,NDIM
            FORREP(J,1) = -PRES * ND(J)
            FORREP(J,2) = -PRES * (-ND(J))
 250      CONTINUE
          IF (NDIM.EQ.2) THEN
             FORREP(1,1) = FORREP(1,1)- CISA * ND(2)
             FORREP(2,1) = FORREP(2,1)+ CISA * ND(1)
             FORREP(1,2) = FORREP(1,2)- CISA * (-ND(2))
             FORREP(2,2) = FORREP(2,2)+ CISA * (-ND(1))
          ENDIF

        ELSEIF ((OPTION.EQ.'CALC_K_G_F') .OR.
     &                    (OPTION.EQ.'CALC_G_F'))THEN

C         VALEUR DE LA PRESSION
          XG(NDIM+1) = ZR(ITEMPS)
          CALL FOINTE('FM',ZK8(IPREF),NDIM+1,NOMPAR,XG,PRES,IER)
          IF(NDIM.EQ.2)
     &      CALL FOINTE('FM',ZK8(IPREF+1),NDIM+1,NOMPAR,XG,CISA,IER)
          DO 260 J=1,NDIM
            FORREP(J,1) = -PRES * ND(J)
            FORREP(J,2) = -PRES * (-ND(J))
 260      CONTINUE
          IF (NDIM.EQ.2) THEN
             FORREP(1,1) = FORREP(1,1)- CISA * ND(2)
             FORREP(2,1) = FORREP(2,1)+ CISA * ND(1)
             FORREP(1,2) = FORREP(1,2)- CISA * (-ND(2))
             FORREP(2,2) = FORREP(2,2)+ CISA * (-ND(1))
          ENDIF

        ELSE
          CALL U2MESS('F','XFEM_15')
        ENDIF

C       -----------------------------------
C       2) CALCUL DE THETA ET DE DIV(THETA)
C       -----------------------------------

        DIVT= 0.D0
        CALL VECINI(9,0.D0,DTDM)

        DO 390 I=1,NDIM

          THETA(I)=0.D0
          DO 301 INO=1,NNOP
            THETA(I) = THETA(I) + FF(INO) * ZR(ITHET-1+NDIM*(INO-1)+I)
 301      CONTINUE

          DO 310 J=1,NDIM
             DO 311 INO=1,NNOP
               DTDM(I,J) = DTDM(I,J) + ZR(ITHET-1+NDIM*(INO-1)+I)
     &                               * DFDI(INO,J)
 311        CONTINUE
 310      CONTINUE

          DIVT = DIVT + DTDM(I,I)

 390    CONTINUE

C       BOUCLE SUR LES DEUX LEVRES
        DO 300 ILEV = 1,2

C         ---------------------------------------------
C         3) CALCUL DU DEPLACEMENT
C         ---------------------------------------------

          CALL VECINI(NDIM,0.D0,DEPLA)
          DO 200 INO=1,NNOP
            CALL INDENT(INO,DDLS,DDLM,NNOPS,INDI)
            CPT=0
C           DDLS CLASSIQUES
            DO 201 I=1,NDIM
              CPT=CPT+1
               DEPLA(I) = DEPLA(I) + FF(INO)*ZR(IDEPL-1+INDI+CPT)
 201        CONTINUE
C           DDLS HEAVISIDE
            DO 202 I=1,NFH*NDIM
              CPT=CPT+1
              DEPLA(I) = DEPLA(I) + HE(ILEV) * FF(INO)
     &                               * ZR(IDEPL-1+INDI+CPT)
 202        CONTINUE
C           DDL ENRICHIS EN FOND DE FISSURE
              DO 204 I=1,SINGU*NDIM
                CPT=CPT+1
                DEPLA(I) = DEPLA(I) + RR(ILEV) * FF(INO)
     &                               * ZR(IDEPL-1+INDI+CPT)
 204          CONTINUE
 200      CONTINUE

C         --------------------------------
C         4) CALCUL DES CHAMPS AUXILIAIRES
C         --------------------------------

          IF (OPTION(1:8).EQ.'CALC_K_G')  THEN
C           CHAMPS AUXILIARES DANS LA BASE LOCALE : U1L,U2L,U3L
            CALL VECINI(9,0.D0,RB9)
            CALL CHAUXI(NDIM,MU,KA,-LST,ANGL(ILEV),RB9,.FALSE.,
     &                RB,RB9,RB9,RB9,U1L,U2L,U3L)

C           CHAMPS AUXILIARES DANS LA BASE GLOBALE : U1,U2,U3
            CALL VECINI(NDIM,0.D0,U1)
            CALL VECINI(NDIM,0.D0,U2)
            CALL VECINI(NDIM,0.D0,U3)
            DO 510 I=1,NDIM
              DO 511 J=1,NDIM
                U1(I) = U1(I) + P(I,J) * U1L(J)
                U2(I) = U2(I) + P(I,J) * U2L(J)
                IF (NDIM.EQ.3) U3(I) = U3(I) + P(I,J) * U3L(J)
 511          CONTINUE
 510        CONTINUE
          ENDIF

C         -----------------------------------------
C         5) CALCUL DE 'DFOR' =  D(PRES)/DI . THETA
C         -----------------------------------------

          CALL VECINI(9,0.D0,DPREDI)
          CALL VECINI(3,0.D0,DFOR)

          DO 400 I=1,NDIM
            DO 410 J=1,NDIM
               DO 411 INO=1,NNOP
                 IF ((OPTION.EQ.'CALC_K_G')
     &               .OR. (OPTION.EQ.'CALC_G')) THEN
                         DPREDI(I,J) = DPREDI(I,J) +
     &         HE(ILEV) * DFDI(INO,J) * ZR(IPRES-1+INO) * ND(I)
                 ENDIF
                 IF ((OPTION.EQ.'CALC_K_G_F')
     &               .OR. (OPTION.EQ.'CALC_G_F')) THEN
                         DPREDI(I,J) = DPREDI(I,J) +
     &              HE(ILEV) * DFDI(INO,J) * PRESN(INO) * ND(I)
                 ENDIF
 411          CONTINUE
 410        CONTINUE
 400      CONTINUE
          DO 312 I=1,NDIM
            DO 313 K=1,NDIM
              DFOR(I) = DPREDI(I,K) * THETA(K)
 313        CONTINUE
 312      CONTINUE

C         -----------------------------------
C         6) CALCUL EFFECTIF DE G, K1, K2, K3
C         -----------------------------------
          G  = 0.D0
          K1 = 0.D0
          K2 = 0.D0
          K3 = 0.D0

          DO 520 J = 1,NDIM
            G  =  G + (FORREP(J,ILEV) * DIVT + DFOR(J)) * DEPLA(J)
            IF (OPTION(1:8).EQ.'CALC_K_G')  THEN
              K1 = K1 + (FORREP(J,ILEV) * DIVT + DFOR(J)) * U1(J)
              K2 = K2 + (FORREP(J,ILEV) * DIVT + DFOR(J)) * U2(J)
              IF (NDIM.EQ.3)
     &           K3 = K3 + (FORREP(J,ILEV) * DIVT + DFOR(J)) * U3(J)
            ENDIF
 520      CONTINUE

          JM = JAC*MULT*0.5D0

          IF (NDIM.EQ.3) THEN

            ZR(IGTHET-1+1)=ZR(IGTHET-1+1)+G*JAC*MULT

            IF (OPTION(1:8).EQ.'CALC_K_G')  THEN
              ZR(IGTHET-1+2)=ZR(IGTHET-1+2)+K1*JM*SQRT(COEFF)
              ZR(IGTHET-1+3)=ZR(IGTHET-1+3)+K2*JM*SQRT(COEFF)
              ZR(IGTHET-1+4)=ZR(IGTHET-1+4)+K3*JM*SQRT(COEFF3)
              ZR(IGTHET-1+5)=ZR(IGTHET-1+5)+K1*JM*COEFF
              ZR(IGTHET-1+6)=ZR(IGTHET-1+6)+K2*JM*COEFF
              ZR(IGTHET-1+7)=ZR(IGTHET-1+7)+K3*JM*COEFF3
            ENDIF

          ELSEIF(NDIM.EQ.2)THEN

            ZR(IGTHET-1+1)=ZR(IGTHET-1+1)+G*JAC*MULT

            IF (OPTION(1:8).EQ.'CALC_K_G')  THEN
              ZR(IGTHET-1+2)=ZR(IGTHET-1+2)+K1*JM*SQRT(COEFF)
              ZR(IGTHET-1+3)=ZR(IGTHET-1+3)+K2*JM*SQRT(COEFF)
              ZR(IGTHET-1+4)=ZR(IGTHET-1+4)+K1*JM*COEFF
              ZR(IGTHET-1+5)=ZR(IGTHET-1+5)+K2*JM*COEFF
            ENDIF

          ENDIF

 300    CONTINUE
C       FIN DE BOUCLE SUR LES DEUX LEVRES

 900  CONTINUE
C     FIN DE BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
C     ----------------------------------------------------------------
      CALL JEDEMA()
      END
