      SUBROUTINE STATCH(NBOBST,NBPT,TEMPS,DLOC,FCHO,VGLI,IADH,WK1,WK2,
     &                  WK3,IWK4,TDEBUT,TFIN,NBLOC,OFFSET,TREPOS,NOECHO,
     &                  INTITU,NOMRES)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NBOBST,NBPT,NBLOC,IADH(*),IWK4(*)
      REAL*8 TEMPS(*),DLOC(*),FCHO(*),VGLI(*),TREPOS,WK1(*),WK2(*),
     &       WK3(*),TDEBUT,TFIN,OFFSET
      CHARACTER*(*) NOMRES
      CHARACTER*8 NOECHO(*), INTITU(*)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/03/2003   AUTEUR BOYERE E.BOYERE 
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
C-----------------------------------------------------------------------
C     CALCUL ET IMPRESSION DES STATISTIQUES DE CHOC

C     NBOBST       : NB DE NOEUDS DE CHOC
C     NBPT         : NB DE PAS DE TEMPS TEMPORELS ARCHIVES
C     NBLOC        : NB DE BLOCS POUR LE MOYENNAGE
C     TEMPS        : INSTANTS DE CALCUL
C     DLOC         : VECTEUR DES DEPLACEMENTS DANS LE REPERE LOCAL
C     FCHO         : VECTEUR DES FORCES DE CHOC
C     VGLI         : VECTEUR DES VITESSES DE GLISSEMENT
C     IADH         : VECTEUR INDICATEUR D ADHERENCE

C-----------------------------------------------------------------------
      INTEGER IBID,VALEI(3),NBPARA,NDEPL,NUSUR,NFORN,NSTCH
      PARAMETER (NBPARA=20,NDEPL=8,NUSUR=4,NFORN=8,NSTCH=10)
      REAL*8 PARA(7)
      CHARACTER*8 K8B,NOEUD,TPARA(NBPARA)
      CHARACTER*16 TDEPL(NDEPL),TFORN(NFORN),TSTCH(NSTCH),TUSUR(NUSUR),
     &             TVAR(10),NPARA(NBPARA),VALEK(3)
      COMPLEX*16 C16B

      DATA TVAR/'DEPL_X','DEPL_Y','DEPL_Z','DEPL_RADIAL',
     &     'DEPL_ANGULAIRE','FORCE_NORMALE','FORCE_TANG_1',
     &     'FORCE_TANG_2','STAT_CHOC','PUIS_USURE'/

      DATA NPARA/'INTITULE','NOEUD','CALCUL','MOYEN','ECART_TYPE','RMS',
     &     'MAXI','MINI','MOYEN_T_TOTAL','MOYEN_T_CHOC','RMS_T_TOTAL',
     &     'RMS_T_CHOC','NB_CHOC_S','NB_REBON_CHOC','T_CHOC_MOYEN',
     &     'T_CHOC_MAXI','T_CHOC_MINI','T_REBON_MOYEN','%_T_CHOC',
     &     'PUIS_USURE'/
      DATA TPARA/'K8','K8','K16','R','R','R','R','R','R','R','R','R',
     &     'I','I','R','R','R','R','I','R'/

      DATA TDEPL/'INTITULE','NOEUD','CALCUL','MOYEN','ECART_TYPE','RMS',
     &     'MAXI','MINI'/
      DATA TFORN/'INTITULE','NOEUD','CALCUL','MOYEN_T_TOTAL',
     &     'MOYEN_T_CHOC','RMS_T_TOTAL','RMS_T_CHOC','MAXI'/
      DATA TSTCH/'INTITULE','NOEUD','CALCUL','NB_CHOC_S',
     &     'NB_REBON_CHOC','T_CHOC_MOYEN','T_CHOC_MAXI','T_CHOC_MINI',
     &     'T_REBON_MOYEN','%_T_CHOC'/
      DATA TUSUR/'INTITULE','NOEUD','CALCUL','PUIS_USURE'/
C-----------------------------------------------------------------------

      ZERO = 0.0D0
      RAD = R8RDDG()
      CALL INFNIV(IFIRES,IMPR)

      IF (NBPT.GE.4) THEN
        DT = (TEMPS(NBPT-2)-TEMPS(1))/ (NBPT-3)
      ELSE
        DT = TEMPS(2) - TEMPS(1)
      END IF
      IDEBUT = INT((TDEBUT-TEMPS(1))/DT) + 1
      IFIN = MAX(INT((TFIN-TEMPS(1))/DT)+1,NBPT)
      NBPAS = IFIN - IDEBUT + 1
      IF (NBLOC.EQ.0) NBLOC = 1
      NBVAL = NBPAS/NBLOC

      IF (IMPR.EQ.2) THEN
        WRITE (IFIRES,1000)
        WRITE (IFIRES,1010) NBLOC,NBVAL
        WRITE (IFIRES,1000)
        DO 10 I = 1,NBLOC
          WRITE (IFIRES,1020) I,TEMPS(IDEBUT+NBVAL* (I-1)),
     &      TEMPS(IDEBUT+NBVAL*I-1)
   10   CONTINUE
      END IF

      CALL TBCRSD(NOMRES,'G')
      CALL TBAJPA(NOMRES,NBPARA,NPARA,TPARA)

C     BOUCLE SUR LES NOEUDS DE CHOC

      DO 120 I = 1,NBOBST
        NOEUD = NOECHO(I)
        VALEK(1) = INTITU(I)
        VALEK(2) = NOEUD

        IF (IMPR.EQ.2) THEN
          WRITE (IFIRES,*) '   '
          WRITE (IFIRES,1030)
          WRITE (IFIRES,1040)
          WRITE (IFIRES,1050)
          WRITE (IFIRES,1060) NOEUD
          WRITE (IFIRES,1040)
          WRITE (IFIRES,1030)
        END IF

        DO 30 J = 1,3

          VALEK(3) = TVAR(J)
          DXMOYT = ZERO
          DXETYT = ZERO
          DXRMST = ZERO
          DXMAXT = -1.D30
          DXMINT = -DXMAXT

          IDEC = 3* (I-1) + J
          CALL R8COPY(NBPT,DLOC(IDEC),3*NBOBST,WK1(1),1)

          DO 20 IBL = 1,NBLOC
            DXMOY = ZERO
            DXETYP = ZERO
            DXRMS = ZERO
            DXMAX = -1.D30
            DXMIN = -DXMAX
            CALL DSTAT0(NBVAL,WK1((IBL-1)*NBVAL+IDEBUT),DXMOY,DXETYP,
     &                  DXRMS,DXMAX,DXMIN)
            DXMOYT = DXMOYT + DXMOY
            DXETYT = DXETYT + DXETYP**2
            DXRMST = DXRMST + DXRMS**2
            DXMAXT = MAX(DXMAXT,DXMAX)
            DXMINT = MIN(DXMINT,DXMIN)
            IF (IMPR.EQ.2) CALL IMPDEP(IFIRES,J,IBL,DXMOY,DXETYP,DXRMS,
     &                                 DXMAX,DXMIN)
   20     CONTINUE
          DXMOYT = DXMOYT/NBLOC
          DXRMST = SQRT(DXRMST/NBLOC)
          DXETYT = SQRT(DXETYT/NBLOC)
          IF (IBL.GT.1) THEN
            IBL = 0
            IF (IMPR.EQ.2) CALL IMPDEP(IFIRES,J,IBL,DXMOYT,DXETYT,
     &                                 DXRMST,DXMAXT,DXMINT)
          END IF
          PARA(1) = DXMOYT
          PARA(2) = DXETYT
          PARA(3) = DXRMST
          PARA(4) = DXMAXT
          PARA(5) = DXMINT

          CALL TBAJLI(NOMRES,NDEPL,TDEPL,IBID,PARA,C16B,VALEK,0)

   30   CONTINUE

C       --------------------------------------------------------
C       --- ANALYSE DES DEPLACEMENTS EN COORDONNEES POLAIRES ---
C       --------------------------------------------------------

        CALL R8COPY(NBPT,DLOC(3* (I-1)+2),3*NBOBST,WK1,1)
        CALL R8COPY(NBPT,DLOC(3* (I-1)+3),3*NBOBST,WK2,1)
        DO 40 IN = 1,NBPT
          WK3(IN) = SQRT(WK1(IN)*WK1(IN)+WK2(IN)*WK2(IN))
   40   CONTINUE

        DXMOYT = ZERO
        DXETYT = ZERO
        DXRMST = ZERO
        DXMAXT = -1.D30
        DXMINT = -DXMAXT
        DO 50 IBL = 1,NBLOC
          DXMOY = ZERO
          DXETYP = ZERO
          DXRMS = ZERO
          DXMAX = -1.D30
          DXMIN = -DXMAX
          CALL DSTAT0(NBVAL,WK3((IBL-1)*NBVAL+IDEBUT),DXMOY,DXETYP,
     &                DXRMS,DXMAX,DXMIN)
          DXMOYT = DXMOYT + DXMOY
          DXETYT = DXETYT + DXETYP**2
          DXRMST = DXRMST + DXRMS**2
          DXMAXT = MAX(DXMAXT,DXMAX)
          DXMINT = MIN(DXMINT,DXMIN)
          IF (IMPR.EQ.2) CALL IMPDEP(IFIRES,4,IBL,DXMOY,DXETYP,DXRMS,
     &                               DXMAX,DXMIN)
   50   CONTINUE
        DXMOYT = DXMOYT/NBLOC
        DXRMST = SQRT(DXRMST/NBLOC)
        DXETYT = SQRT(DXETYT/NBLOC)
        IF (IBL.GT.1) THEN
          IBL = 0
          IF (IMPR.EQ.2) CALL IMPDEP(IFIRES,4,IBL,DXMOYT,DXETYT,DXRMST,
     &                               DXMAXT,DXMINT)
        END IF
        VALEK(3) = TVAR(4)
        PARA(1) = DXMOYT
        PARA(2) = DXETYT
        PARA(3) = DXRMST
        PARA(4) = DXMAXT
        PARA(5) = DXMINT
        CALL TBAJLI(NOMRES,NDEPL,TDEPL,IBID,PARA,C16B,VALEK,0)

C       ----------------------------------
C       --- ANALYSE DE L ANGLE POLAIRE ---
C       ----------------------------------

        DO 60 IN = 1,NBPT
          IF ((WK1(IN).NE.ZERO) .OR. (WK2(IN).NE.ZERO)) THEN
            WK3(IN) = RAD*ATAN2(WK2(IN),WK1(IN))
          ELSE
            WK3(IN) = ZERO
          END IF
   60   CONTINUE

        DXMOYT = ZERO
        DXETYT = ZERO
        DXRMST = ZERO
        DXMAXT = -1.D30
        DXMINT = -DXMAXT
        DO 70 IBL = 1,NBLOC
          DXMOY = ZERO
          DXETYP = ZERO
          DXRMS = ZERO
          DXMAX = -1.D30
          DXMIN = -DXMAX
          CALL DSTAT0(NBVAL,WK3((IBL-1)*NBVAL+IDEBUT),DXMOY,DXETYP,
     &                DXRMS,DXMAX,DXMIN)
          DXMOYT = DXMOYT + DXMOY
          DXETYT = DXETYT + DXETYP**2
          DXRMST = DXRMST + DXRMS**2
          DXMAXT = MAX(DXMAXT,DXMAX)
          DXMINT = MIN(DXMINT,DXMIN)
          IF (IMPR.EQ.2) CALL IMPDEP(IFIRES,5,IBL,DXMOY,DXETYP,DXRMS,
     &                               DXMAX,DXMIN)
   70   CONTINUE
        DXMOYT = DXMOYT/NBLOC
        DXRMST = SQRT(DXRMST/NBLOC)
        DXETYT = SQRT(DXETYT/NBLOC)
        IF (IBL.GT.1) THEN
          IBL = 0
          IF (IMPR.EQ.2) CALL IMPDEP(IFIRES,5,IBL,DXMOYT,DXETYT,DXRMST,
     &                               DXMAXT,DXMINT)
        END IF
        VALEK(3) = TVAR(5)
        PARA(1) = DXMOYT
        PARA(2) = DXETYT
        PARA(3) = DXRMST
        PARA(4) = DXMAXT
        PARA(5) = DXMINT
        CALL TBAJLI(NOMRES,NDEPL,TDEPL,IBID,PARA,C16B,VALEK,0)

C       ------------------------------------------------------------
C       CALCUL DE LA MOYENNE,ECART TYPE,RMS, MAX DE LA FORCE NORMALE
C       ------------------------------------------------------------

        FXMOYT = ZERO
        FXMOYC = ZERO
        FXRMST = ZERO
        FXRMSC = ZERO
        FXMAXT = ZERO
        FXMINT = ZERO
        CALL R8COPY(NBPT,FCHO(3* (I-1)+1),3*NBOBST,WK1,1)
        DO 80 IBL = 1,NBLOC
          FNMOYT = ZERO
          FNMOYC = ZERO
          FNRMST = ZERO
          FNRMSC = ZERO
          FNMAX = -1.D30
          FNMIN = -FNMAX
          FNMINM = FNMIN
          FNMAXM = FNMAX
          NBMAFN = 0
          NBMIFN = 0
          CALL FSTAT0(NBVAL,WK1((IBL-1)*NBVAL+IDEBUT),OFFSET,FNMOYT,
     &                FNMOYC,FNRMST,FNRMSC,FNMAX,FNMIN,FNMAXM,FNMINM,
     &                NBMAFN,NBMIFN)
          FXMOYT = FXMOYT + FNMOYT
          FXMOYC = FXMOYC + FNMOYC
          FXRMST = FXRMST + FNRMST**2
          FXRMSC = FXRMSC + FNRMSC**2
          FXMAXT = MAX(FXMAXT,FNMAX)
          FXMINT = MIN(FXMINT,FNMIN)
          IF (IMPR.EQ.2) CALL IMPFN0(IFIRES,0,IBL,FNMOYT,FNMOYC,FNRMST,
     &                               FNRMSC,FNMAX)
   80   CONTINUE
        FXMOYT = FXMOYT/NBLOC
        FXMOYC = FXMOYC/NBLOC
        FXRMST = SQRT(FXRMST/NBLOC)
        FXRMSC = SQRT(FXRMSC/NBLOC)
        IF (IBL.GT.1 .AND. IMPR.EQ.2) CALL IMPFN0(IFIRES,0,0,FXMOYT,
     &      FXMOYC,FXRMST,FXRMSC,FXMAXT)
        VALEK(3) = TVAR(6)
        PARA(1) = FXMOYT
        PARA(2) = FXMOYC
        PARA(3) = FXRMST
        PARA(4) = FXRMSC
        PARA(5) = FXMAXT
        CALL TBAJLI(NOMRES,NFORN,TFORN,IBID,PARA,C16B,VALEK,0)

C       ----------------------------------------------------------------
C       CALCUL DE LA MOYENNE,ECART TYPE,RMS,MAX DE LA FORCE TANGENTIELLE
C       ----------------------------------------------------------------

        DO 100 J = 2,3
          FYMOYT = ZERO
          FYETYT = ZERO
          FYRMST = ZERO
          FYMAXT = ZERO
          FYMINT = ZERO
          CALL R8COPY(NBPT,FCHO(3* (I-1)+J),3*NBOBST,WK1,1)
          DO 90 IBL = 1,NBLOC
            FTMOYE = ZERO
            FTETYP = ZERO
            FTRMS = ZERO
            FTMAX = -1.D30
            FTMIN = -FTMAX
            CALL DSTAT0(NBVAL,WK1((IBL-1)*NBVAL+IDEBUT),FTMOYE,FTETYP,
     &                  FTRMS,FTMAX,FTMIN)
            FYMOYT = FYMOYT + FTMOYE
            FYETYT = FYETYT + FTETYP**2
            FYRMST = FYRMST + FTRMS**2
            FYMAXT = MAX(FYMAXT,FTMAX)
            FYMINT = MIN(FYMINT,FTMIN)
            INDIC = J - 1
            IF (IMPR.EQ.2) CALL IMPFT0(IFIRES,INDIC,IBL,FTMOYE,FTETYP,
     &                                 FTRMS,FTMAX,FTMIN)
   90     CONTINUE
          FYMOYT = FYMOYT/NBLOC
          FYETYT = SQRT(FYETYT/NBLOC)
          FYRMST = SQRT(FYRMST/NBLOC)
          IF (IBL.GT.1 .AND. IMPR.EQ.2) CALL IMPFT0(IFIRES,INDIC,0,
     &        FYMOYT,FYETYT,FYRMST,FYMAXT,FYMINT)
          VALEK(3) = TVAR(5+J)
          PARA(1) = FYMOYT
          PARA(2) = FYETYT
          PARA(3) = FYRMST
          PARA(4) = FYMAXT
          PARA(5) = FYMINT
          CALL TBAJLI(NOMRES,NDEPL,TDEPL,IBID,PARA,C16B,VALEK,0)
  100   CONTINUE

C       -------------------------------------------------------
C       --- CALCUL DU NB DE CHOC, DUREE MOYENNE DE CHOC,... ---
C       -------------------------------------------------------

        NBCHOT = 0
        NBREBT = 0
        TCHOCG = ZERO
        TREBOG = ZERO
        TCHOMI = ZERO
        TCHOMA = ZERO
        TCHOMY = ZERO
        CALL R8COPY(NBPT,FCHO(3* (I-1)+1),3*NBOBST,WK1,1)
        DO 110 IBL = 1,NBLOC
          NBCHOC = 0
          TCHOCM = ZERO
          TCHMAX = ZERO
          TCHMIN = ZERO
          NBREBO = 0
          TREBOM = ZERO
          TCHOCT = ZERO
          CALL COMPT(NBVAL,WK1((IBL-1)*NBVAL+IDEBUT),OFFSET,TEMPS,
     &               TREPOS,NBCHOC,TCHOCM,TCHMAX,TCHMIN,NBREBO,TREBOM,
     &               TCHOCT)
          TCHOMA = MAX(TCHOMA,TCHMAX)
          TCHOMI = MIN(TCHOMI,TCHMIN)
          TCHOCG = TCHOCG + TCHOCT
          TREBOG = TREBOG + NBREBO*TREBOM
          NBCHOT = NBCHOT + NBCHOC
          NBREBT = NBREBT + NBREBO
          IF (IMPR.EQ.2) CALL IMPC0(IFIRES,0,IBL,NBCHOC,TCHOCM,TCHMAX,
     &                              TCHMIN,NBREBO,TREBOM,TCHOCT,TEMPS,
     &                              NBVAL)
  110   CONTINUE
        IF (NBCHOT.NE.0) THEN
          TCHOMY = TCHOCG/NBCHOT
        ELSE
          TCHOMY = ZERO
        END IF
        IF (NBREBT.NE.0) THEN
          TREBMY = TREBOG/NBREBT
        ELSE
          TREBMY = ZERO
        END IF
        IF (IBL.GT.1 .AND. IMPR.EQ.2) THEN
          CALL IMPC0(IFIRES,0,0,NBCHOT,TCHOMY,TCHOMA,TCHOMI,NBREBT,
     &               TREBMY,TCHOCG,TEMPS,NBLOC*NBVAL)
        END IF
        VALEK(3) = TVAR(9)
        TACQUI = DT*NBLOC*NBVAL
        IF (NBCHOT.NE.0) THEN
          NREPC = NBREBT/NBCHOT
        ELSE
          NREPC = 0
        END IF
        VALEI(1) = INT(NBCHOT/TACQUI)
        VALEI(2) = NREPC
        PARA(1) = TCHOMY
        PARA(2) = TCHOMA
        PARA(3) = TCHOMI
        PARA(4) = TREBMY
        VALEI(3) = INT(100.D0*TCHOCG/TACQUI)
        CALL TBAJLI(NOMRES,NSTCH,TSTCH,VALEI,PARA,C16B,VALEK,0)

C       --------------------------------------------------------
C       --- CALCUL DE LA PUISSANCE D'USURE AU SENS D'ARCHARD ---
C       --------------------------------------------------------

        CALL STATPU(NBOBST,NBPT,TEMPS,FCHO,VGLI,IADH,WK1,WK2,WK3,IWK4,
     &              IDEBUT,NBLOC,NBVAL,IFIRES,I,IMPR,PUSURN)
        VALEK(3) = TVAR(10)
        CALL TBAJLI(NOMRES,NUSUR,TUSUR,IBID,PUSURN,C16B,VALEK,0)

  120 CONTINUE

 1000 FORMAT (7X,'---------------------------------------------------')
 1010 FORMAT (9X,'STATISTIQUES SUR ',I3,' BLOC(S) DE ',I7,' VALEURS')
 1020 FORMAT (8X,1P,'BLOC NO: ',I3,' DE T= ',E12.5,' A T= ',E12.5)
 1030 FORMAT (14X,'****************************************')
 1040 FORMAT (14X,'*                                      *')
 1050 FORMAT (14X,'*        TRAITEMENT STATISTIQUE        *')
 1060 FORMAT (14X,'*         NOEUD DE CHOC: ',A8,'      *')

      END
