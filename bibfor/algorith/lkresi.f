      SUBROUTINE LKRESI(TYPMOD,NMAT,MATERF,TIMED,TIMEF,  
     &                   NVI,VIND,VINF,YD,YF,DEPS,NR,R)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/01/2013   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FOUCAULT A.FOUCAULT
        IMPLICIT   NONE
C       ----------------------------------------------------------------
C       CALCUL DES TERMES DU SYSTEME NL A RESOUDRE = -R(DY) POUR LETK
C       IN  TYPMOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TIMED  :  INSTANT  T
C           TIMEF  :  INSTANT  T+DT
C           NVI    :  NOMBRE DE VARIABLES INTERNES
C           DEPS   :  INCREMENT DE DEFORMATION
C           VIND   :  VARIABLES INTERNES A T
C           VINF   :  VARIABLES INTERNES A T+DT
C           YD     :  VARIABLES A T    = ( SIGD 0    XIPD XIVPD (EPSD3))
C           YF     :  VARIABLES A T+DT = ( SIGF DLAM XIPF XIVPF (EPS3F))
C           DEPS   :  INCREMENT DE DEFORMATIONS
C           DY     :  SOLUTION         = ( DSIG DLAM DXIP DXIVP (DEPS3))
C           NR     :  DIMENSION DU VECTEUR INCONNUES
C       OUT R      :  SYSTEME NL A T+DT
C       ----------------------------------------------------------------
        INTEGER         NMAT,NR,NVI,NDI,NDT
        REAL*8          DEPS(6),VIND(*),VINF(*)
        REAL*8          R(NR),YD(NR),YF(NR),MATERF(NMAT,2)
        REAL*8          TIMED, TIMEF
        CHARACTER*8     TYPMOD
C
        INTEGER         I,RETCOM,VAL,VARV
        REAL*8          ZERO,VINT(7),DEVSIG(6),I1,UCRIP,SEUILP
        REAL*8          DT,SEUILV,DEPSV(6),DGAMV
        REAL*8          DXIV,XIVMAX,XIPPIC,SEUIVM,UCRIV
        REAL*8          DSDENL(6,6),KK,MU,DHDS(6),DS2HDS(6)
        REAL*8          PARAEP(3), VARPL(4),DFDSP(6),LKBPRI,BPRIMP
        REAL*8          VECNP(6),GP(6),DEVGII,DEUX,TROIS,DEPSE(6)
        REAL*8          DSIGE(6),SIGDT(6),SIGFT(6),DEPST(6),LAMGD2
        PARAMETER       (ZERO  =  0.D0 )
        PARAMETER       (DEUX  =  2.D0 )
        PARAMETER       (TROIS =  3.D0 )
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       --------------------------------------------------------------

C --------------------------------------------------------------------
C --- PASSAGE EN CONVENTION MECANIQUE DES SOLS
C --------------------------------------------------------------------
        DO 10 I = 1, NDT
          SIGFT(I)  = -YF(I)
          SIGDT(I)  = -YD(I)
          DEPST(I)  = -DEPS(I)
  10    CONTINUE

C ----------------------------------------------------------------------
C --- VARIABLES LOCALES TEMPORAIRES
C ----------------------------------------------------------------------
        VARV   = 0
        VAL    = 0 
        DEVGII = ZERO

C --- VECTEUR VARIABLES INTERNES TEMPORAIRES
        CALL LCEQVN ( NVI  ,  VIND , VINT )

        IF(YF(NDT+2).GE.VIND(1))THEN
          VINT(1) = YF(NDT+2)
        ELSE
          VINT(1) = VIND(1)
        ENDIF
        IF(YF(NDT+3).GE.VIND(3))THEN
          VINT(3) = YF(NDT+3)
        ELSE
          VINT(3) = VIND(3)
        ENDIF

C --- INCREMENT DE TEMPS
        DT = TIMEF - TIMED

C --- CONSTRUCTION TENSEUR DEVIATOIRE DES CONTRAINTES ET 1ER INVARIANT
        CALL LCDEVI(SIGFT,DEVSIG)
        I1 = SIGFT(1)+SIGFT(2)+SIGFT(3)

C --- DONNEES MATERIAU : VALEUR MAX DE XIV; XI_PIC 
        XIVMAX = MATERF(20,2)
        XIPPIC = MATERF(18,2)

C --- CONSTRUCTION TENSEUR ELASTIQUE NON LINEAIRE DSDENL
        CALL LKELAS ( NDI, NDT, TYPMOD , NMAT, MATERF,
     &              DEPST, SIGFT, DSDENL, KK, MU)

C ----------------------------------------------------------------------
C --- I) - BUT : CALCUL DE LA DEFORMATION VISQUEUSE -DEPSV- ET DU 
C ---      PARAMETRE D ECROUISSAGE VISQUEUX -DGAMV-
C ----------------------------------------------------------------------

C --- I-1) INDICATEUR SUR ANGLE DE DILATANCE VISQUEUX PSI -> VAL = 0
        VAL = 0

C --- I-2) VARIABLE D'ECROUISSAGE VISQUEUSE VINTR = YF(NDT+3)
C --- I-3) CALCUL SEUIL VISQUEUX PAR RAPPORT A YF(1:6)=SIGF -> SEUILV
C --- I-3-1)  XIT   = YF(NDT+3)
        CALL LKCRIV(VINT(3),I1,DEVSIG,VINT,NMAT,MATERF,UCRIV,SEUILV)

        IF(SEUILV.GE.ZERO)THEN
          CALL LKDGDE(VAL,VINT(3),DT,SEUILV,UCRIV,I1,DEVSIG,VINT,NMAT,
     &              MATERF,DEPSV,DGAMV,RETCOM)
        ELSE
          DGAMV = ZERO
          DO 20 I = 1, NDT
            DEPSV(I) = ZERO
  20      CONTINUE
        ENDIF
C ----------------------------------------------------------------------
C --- II) - BUT : CALCUL DE LA DEFORMATION PLASTIQUE -DEPSP- ET DU 
C ---       PARAMETRE D ECROUISSAGE PLASTIQUE -DGAMP-
C ----------------------------------------------------------------------
C --- II-2-B-2) INDICATEUR CONTRACTANCE OU DILATANCE -> VARV = 0 OU 1
C --- II-2-B-2)-1) CALCUL POSITION YF PAR RAPPORT SEUIL VISQUEUX MAX
        CALL LKCRIV(XIVMAX,I1,DEVSIG,VINT,NMAT,MATERF,UCRIV,SEUIVM)

C --- II-2-B-2)-2) TEST SUR SEUIL >0 OU <0 POUR DEFINIR VARV
        IF(SEUIVM.LE.ZERO)THEN
          VARV = 0
        ELSE
          VARV = 1
        ENDIF

C --- II-1) CALCUL FONCTION SEUIL PLASTIQUE EN YF
        SEUILP = ZERO
        CALL LKCRIP(I1,DEVSIG,VINT,NMAT,MATERF,UCRIP,SEUILP)
 
C --- II-2)SI SEUILP >= 0 ALORS PLASTICITE A PRENDRE EN COMPTE 
        IF((SEUILP.GE.ZERO).OR.(VINF(7).GT.ZERO))THEN
C --- II-2-B-1) INDICATEUR ANGLE DE DILATANCE PLASTIQUE PSI -> 0 OU 1
          IF(YF(NDT+2).LE.XIPPIC)THEN
            VAL = 0
          ELSE
            VAL = 1
          ENDIF

C --- II-2-B-3) CALCUL DE DF/DSIG 
          CALL LKDHDS(NMAT,MATERF,I1,DEVSIG,DHDS,RETCOM)
          CALL LKDS2H(NMAT,MATERF,I1,DEVSIG,DHDS,DS2HDS,RETCOM)
          CALL LKVARP(VINT, NMAT, MATERF, PARAEP)
          CALL LKVACP(NMAT, MATERF,PARAEP, VARPL)
          CALL LKDFDS(NMAT,MATERF,DEVSIG,PARAEP,VARPL,DS2HDS,
     &              UCRIP,DFDSP)

C --- II-2-B-4) CALCUL DE G 
          BPRIMP = LKBPRI (VAL,VINT,NMAT,MATERF,PARAEP,I1,DEVSIG)
          CALL LKCALN(DEVSIG, BPRIMP, VECNP, RETCOM)
          CALL LKCALG(DFDSP,VECNP,GP,DEVGII)
        ENDIF

C ----------------------------------------------------------------------
C --- III) EQUATION D'EQUILIBRE : (CONVENTION MECANIQUE DES SOLS)
C ---      SIGDT - SIGFT + DSDE:(DEPST-DEPSP-DEPSVP) = 0 
C ----------------------------------------------------------------------
        IF((SEUILP.GE.ZERO).OR.(VINF(7).GT.ZERO))THEN
          DO 30 I = 1, NDT
              DEPSE(I) = DEPST(I)-DEPSV(I)-YF(NDT+1)*GP(I)
  30      CONTINUE

          CALL LCPRMV(DSDENL,DEPSE,DSIGE)

          DO 40 I = 1, NDT
            R(I) = DSIGE(I)+SIGDT(I)-SIGFT(I)
  40      CONTINUE
        ELSE
          DO 50 I = 1, NDT
            DEPSE(I) = DEPST(I)-DEPSV(I)
  50      CONTINUE

          CALL LCPRMV(DSDENL,DEPSE,DSIGE)

          DO 60 I = 1, NDT
            R(I) = DSIGE(I)+SIGDT(I)-SIGFT(I)
  60      CONTINUE
        ENDIF
C === =================================================================
C --- MISE A L'ECHELLE DE DEFORMATIONS -> R(I)/MODULE_CISAILLEMENT
C === =================================================================
        DO 70 I = 1, NDT
          R(I) = R(I)/MU
  70    CONTINUE

C ----------------------------------------------------------------------
C --- IV) CONDITION DE KUHN-TUCKER : -FP = 0 OU -DLAM = 0 
C ----------------------------------------------------------------------
C --- APPLICATION DE LA CONDITION DE KHUN-TUCKER SUR R(NDT+1)
        IF(VINF(7).EQ.ZERO)THEN
          R(NDT+1) = -YF(NDT+1)
        ELSE
          R(NDT+1) = -SEUILP/MU
        ENDIF
C ----------------------------------------------------------------------
C --- V) EVOLUTION DE XIP : 
C ---    XIPD - XIPF + DLAM*G_II*SQRT(2/3)(+ DGAMVP) = 0
C ----------------------------------------------------------------------
        LAMGD2 = MAX(ZERO,YF(NDT+1)*DEVGII*SQRT(DEUX/TROIS))

        IF(VARV.EQ.0)THEN
          R(NDT+2) = YD(NDT+2)-YF(NDT+2)+LAMGD2
        ELSE
          R(NDT+2) = YD(NDT+2)-YF(NDT+2)+LAMGD2+DGAMV
        ENDIF
C ----------------------------------------------------------------------
C --- VI) EVOLUTION DE XIVP : 
C ---     XIVPD - XIVPF + MIN(DGAM_VP,XIV_MAX-XIVPD) = 0
C ----------------------------------------------------------------------
C --- TEST POUR DEFINIR MIN(DGAMV,XIV_MAX-XIV)
        DXIV = MIN(DGAMV,XIVMAX-YD(NDT+3))

        R(NDT+3) = YD(NDT+3)-YF(NDT+3)+DXIV

        END
