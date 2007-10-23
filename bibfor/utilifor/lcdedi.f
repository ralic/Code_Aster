        SUBROUTINE LCDEDI ( FAMI, KPG, KSP, NMAT,  MATERD, MATERF,
     &                      TEMPD,TEMPF,TREF,DEPST,EPSDT,DEPSM,EPSDM )
        IMPLICIT NONE
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 23/10/2007   AUTEUR SALMONA L.SALMONA 
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
C       ----------------------------------------------------------------
C       RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE
C       POUR TENIR COMPTE DES CONTRAINTES THERMIQUES :
C       (DILATATION ISOTROPE POUR LE MOMENT !!)
C
C       ON RETIRE       - A DEPST, L INCREMENT DE DEFORMATION DUE
C                         A LA DILATATION(TEMP)
C                       - ET A EPSDT , LA DEFORMATION DE DILATATION A T
C
C       POUR OBTENIR    - L'INCREMENT DE DEFORMATION MECANIQUE DEPSM
C                       - ET LA DEFORMATION MECANIQUE A T      EPSDM
C
C       ON A SIG = HOOK EPSE  = HOOK ( EPST - EPSP - EPSTH )
C                             = HOOK ( EPST - EPSP ) - HOOK EPSTH
C       DONC            SIG   = SIGM                 + SIGTH
C       AVEC            SIGTH = - HOOK EPSTH
C                             = - HOOK ALPHA ( T - TREF ) I
C       OU   EN PRENANT EPS   = EPST - EPSTH
C                       SIG   = HOOK ( EPS - EPSP )
C
C       ON PEUT DONC - SOIT TRAVAILLER AVEC EPST ET AJOUTER SIGTH APRES
C                    - SOIT TRAVAILLER AVEC EPS = EPST - EPSTH
C                      CE QUI EST FAIT ICI
C       ----------------------------------------------------------------
C       IN      NMAT    DIMENSION  DE MATER
C               MATERD  COEFFICIENTS MATERIAU A T
C               MATERF  COEFFICIENTS MATERIAU A T+DT
C               TD      TEMPERATURE DEBUT INCREMENT
C               TF      TEMPERATURE FIN INCREMENT
C               TR      TEMPERATURE DE REFERENCE
C               DEPST   INCREMENT DE DEFORMATION TOTALE
C               EPSDT   DEFORMATION TOTALE A T
C       OUT     DEPSM   INCREMENT DE DEFORMATION MECANIQUE
C               EPSDM   DEFORMATION MECANIQUE A T
C       ----------------------------------------------------------------
        INTEGER         KPG, KSP, NDT  , NDI , NMAT, K, IRET, IISNAN
        CHARACTER*(*)   FAMI
        CHARACTER*2     CE
        REAL*8          TD,  TF , TR,R8VIDE,TEMPD,TEMPF,TREF
        REAL*8          EPSDT(6), DEPST(6)
        REAL*8          EPSDM(6), DEPSM(6),ALPHFN,ALPHFL,ALPHFT
        REAL*8          ALPHAD, ALPHAF,ALPHDL,ALPHDT,ALPHDN
        REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
        IF (IISNAN(TREF).EQ.0) THEN
          IF (TREF.EQ.R8VIDE()) THEN
            CALL RCVARC(' ','TEMP','-',FAMI,KPG,KSP,TD,IRET)
            CALL RCVARC(' ','TEMP','+',FAMI,KPG,KSP,TF,IRET)
            CALL RCVARC(' ','TEMP','REF',FAMI,KPG,KSP,TR,IRET)
          ELSE
            TD=TEMPD
            TF=TEMPF
            TR=TREF
          ENDIF
        ELSE
            TD=TEMPD
            TF=TEMPF
            TR=TREF
        ENDIF

        IF ((IISNAN(TF).EQ.0).AND.(IISNAN(TD).EQ.0)) THEN
          IF (IISNAN(TR).NE.0) THEN
            CALL U2MESS('F','CALCULEL_31')
          ELSE
            IF (MATERD(NMAT,1).EQ.0) THEN
              ALPHAD = MATERD(3,1)
              ALPHAF = MATERF(3,1)
              DO 110 K = 1,NDI
                DEPSM(K) = DEPST(K) - ( ALPHAF*(TF-TR) - 
     &                                ALPHAD*(TD-TR))
                EPSDM(K) = EPSDT(K) - ( ALPHAD*(TD-TR) )
 110          CONTINUE
C
              DO 111 K  = NDI+1,NDT
                DEPSM(K)  = DEPST(K)
                EPSDM(K)  = EPSDT(K)
 111          CONTINUE

            ELSEIF (MATERD(NMAT,1).EQ.1) THEN

              ALPHDL = MATERD(73,1)
              ALPHDT = MATERD(74,1)
              ALPHDN = MATERD(75,1)
C
              ALPHFL = MATERF(73,1)
              ALPHFT = MATERF(74,1)
              ALPHFN = MATERF(75,1)
C
              DEPSM(1) = DEPST(1) - ( ALPHFL*(TF-TR) - 
     &                              ALPHDL*(TD-TR))
              DEPSM(2) = DEPST(2) - ( ALPHFT*(TF-TR) - 
     &                              ALPHDT*(TD-TR))
              DEPSM(3) = DEPST(3) - ( ALPHFN*(TF-TR) - 
     &                              ALPHDN*(TD-TR))

              EPSDM(1) = EPSDT(1) - ( ALPHDL*(TD-TR) )
              EPSDM(2) = EPSDT(2) - ( ALPHDT*(TD-TR) )
              EPSDM(3) = EPSDT(3) - ( ALPHDN*(TD-TR) )

              DO 112 K  = 4,6
                DEPSM(K)  = DEPST(K)
                EPSDM(K)  = EPSDT(K)
 112          CONTINUE
            ENDIF

          ENDIF
        ELSE
          ALPHAD = MATERD(3,1)
          ALPHAF = MATERF(3,1)
          DO 113 K = 1,NDT
              DEPSM(K) = DEPST(K)
              EPSDM(K) = EPSDT(K)
 113      CONTINUE
        ENDIF
        END
