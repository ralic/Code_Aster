      SUBROUTINE DICOUL (OPTION,COMPOR,NNO,NBT,NEQ,NC,ICODMA,RCOGEO,
     &                   ULM,DUL,SIM,TP,VARIM,PGL,KLV,
     &                   VARIP,FONO,SIP,ITEMP,TEMPM,TEMPP)    
C ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NBT,NEQ,ICODMA,NC,IDECOL,JTAB(7),ITEMP
      REAL*8  ULM(NEQ),DUL(NEQ),SIM(NEQ),SIP(NEQ),VARIM(*),TEMPM,TEMPP
      REAL*8  PGL(3,3),KLV(NBT),VARIP(*),FONO(NEQ),TP,RCOGEO(6)
      CHARACTER*16      OPTION, COMPOR(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 01/10/2002   AUTEUR CIBHHLV L.VIVAN 
C TOLE CRP_21
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
C
C  COMPORTEMENT DIS_COULOMB : APPLICATION : LIAISON GRILLE-CRAYON COMBU
C           RELATION DE COMPORTEMENT : ELASTIQUE PARTOUT
C           SAUF SUIVANT Y LOCAL : FROTTEMENT DE COULOMB
C       ELEMENTS MECA_DIS_TR_L ET MECA_DIS_T_L
C
C IN  : NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
C       NEQ    : NOMBRE DE DDL DE L'ELEMENT
C       NC     : NOMBRE DE DDL PAR NOEUD = 3 OU 6
C       ICODMA : ADRESSE DU MATERIAU CODE
C       DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL
C       SIM    : EFFORTS GENERALISES A L'INSTANT PRECEDENT
C       TP     : INSTANT ACTUEL
C       VARIM  : VARIABLE INTERNE A L'INSTANT PRECEDENT
C       PGL    : MATRICE DE PASSAGE REPERE GLOBAL -> LOCAL
C
C VAR : KLV    : MATRICE ELASTIQUE REPERE LOCAL EN ENTREE
C              : MATRICE TANGENTE EN SORTIE
C OUT : VARIP  : VARIABLE INTERNE REACTUALISEE
C       FONI   : FORCES NODALES
C       SIP    : EFFORTS INTERNES
C

C **************** DEBUT COMMUNS NORMALISES JEVEUX *********************

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

C ***************** FIN COMMUNS NORMALISES JEVEUX **********************
      INTEGER NBRE2,NBRE3,NBPAR,I,NNO,IGLISS,JPROLP,JVALEP,NBVALP
      INTEGER ITRAC,LGPG,IIRRAP

      PARAMETER    ( NBRE2 = 5, NBRE3 = 1)
      REAL*8 VALRE2(NBRE2),MU,FKN,RN0,RNP,RTM,RTP,DRTE,SEUIL
      REAL*8 KLC(144),DFL(12),FL(12),KN,KT,UNM,UNP,KTRIG
      REAL*8 MOPX, MOMX, MOPELX, NU, MOPZ, MOMZ, MOPELZ, CP
      REAL*8 CPRAG, CPRAGM, CPMOD, MOPESZ, MOPESX
      REAL*8 XMX, XPX, XMZ, XPZ
      REAL*8 PMX, PMZ, SIEEQX, SIEEQZ, RPZ, RPX, DPZ, DPX
      REAL*8 CPTANX, CPTANZ, DTETAZ, DTETAX, RPRIMX, RPRIMZ
      REAL*8 TM0,TP0,ET,ALPH,DUT,EPSPM,PM,EPSP,PP,DSDEM,DSDEP,DUN
      REAL*8 RBID, VALRE3(NBRE3)
      CHARACTER*2  CODRE2(NBRE2),CODRE3(NBRE3)
      CHARACTER*8  NOMPAR, NOMRE2(NBRE2), NOMRE3(NBRE3)
C
      DATA NOMRE2 / 'COULOMB', 'RIGI_N_FO', 'EFFO_N_INIT',
     &              'C_PRAGER_MZ', 'KT_ULTM' /
      DATA NOMRE3 / 'RIGI_N_IRRA' /
C
C ----------------------------------------------------------------------
      CALL TECACH(.TRUE.,.TRUE.,'PVARIMR',7,JTAB)
      LGPG = MAX(JTAB(6),1)*JTAB(7)

C ---    SURCHARGE DE LA RIGIDITE EN ROTATION

      IF ( NC .GT. 3) THEN
         CALL RCMOME (ICODMA,JPROLP,JVALEP,NBVALP,CP,ITRAC)
         IF (ITRAC.EQ.1) THEN
            KLV(10)  =  CP
            KLV(49)  = -CP
            KLV(55)  =  CP
            KLV(21)  =  CP
            KLV(72)  = -CP
            KLV(78)  =  CP
         ENDIF
      ENDIF


C --- CALCUL ELASTIQUE

C --- DEMI-MATRICE KLV TRANSFORMEE EN MATRICE PLEINE KLC
C
      CALL VECMA (KLV,NBT,KLC,NEQ)
C
C --- CALCUL DE FL = KLC.DUL (INCREMENT D'EFFORT ELASTIQUE)
C
      CALL PMAVEC ('ZERO',NEQ,KLC,DUL,DFL)

      DO 100 I = 1,NC
         SIP(I)      = -DFL(I)      + SIM(I)
         SIP(I+NC)   =  DFL(I+NC)   + SIM(I+NC)
         FL(I)       =  DFL(I)      - SIM(I)
         FL(I+NC)    =  DFL(I+NC)   + SIM(I+NC)
 100  CONTINUE

C -------------------------------------------------

C --- TRAITEMENT DU COMPORTEMENT LIE AU MATERIAU DIS_CONTACT

C
C --- CARACTERISTIQUES DU MATERIAU
C
      NBPAR = 0
      NOMPAR = '   '
      RBID = 0.D0
      CALL RCVALA ( ICODMA, 'DIS_CONTACT', NBPAR, NOMPAR, RBID, 1,
     &                         NOMRE2(1), VALRE2(1), CODRE2(1), '  ' )
      CALL RCVALA ( ICODMA, 'DIS_CONTACT', NBPAR, NOMPAR, RBID, 2,
     &                         NOMRE2(4), VALRE2(4), CODRE2(4), '  ' )
      NBPAR = 1
      NOMPAR = 'INST'
      RBID = TP
      CALL RCVALA ( ICODMA, 'DIS_CONTACT', NBPAR, NOMPAR, RBID, 1,
     &                         NOMRE2(2), VALRE2(2), CODRE2(2), '  ' )
      IF ( ITEMP .EQ. 0 ) THEN
         NBPAR = 0
         NOMPAR = '   '
         RBID = 0.D0
      ELSE
         NBPAR = 1
         NOMPAR = 'TEMP'
         RBID = 0.5D0 * ( TEMPM + TEMPP )
      ENDIF
      CALL RCVALA ( ICODMA, 'DIS_CONTACT', NBPAR, NOMPAR, RBID, 1,
     &                         NOMRE2(3), VALRE2(3), CODRE2(3), '  ' )
C
      IF ( (CODRE2(1).EQ.'OK') .AND.
     &     (CODRE2(3).EQ.'OK') .AND. (CODRE2(5).EQ.'OK')) THEN


        MU  = VALRE2(1)
        RN0 = VALRE2(3)
        ET  = VALRE2(5)
        IF (CODRE2(2).EQ.'OK') THEN
           FKN = VALRE2(2)
        ELSE
C          PRISE EN COMPTE DE L'IRRADIATION
           CALL TECACH(.TRUE.,.TRUE.,'PIRRAPR',1,IIRRAP)
           IF (IIRRAP.NE.0) THEN
              NBPAR = 1
              NOMPAR = 'INST'
           CALL RCVALA ( ICODMA,'DIS_CONTACT',NBPAR,NOMPAR,ZR(IIRRAP),
     &                   NBRE3,NOMRE3,VALRE3,CODRE3, ' ' )
              FKN = VALRE3(1)
           ELSE
              CALL UTMESS('F','DICOUL','LE CHAMP D''IRRADIATION EST '//
     &                                 'INCOMPLET OU NON PRESENT')
           ENDIF
        ENDIF
C
      DUN = DUL(1+NC)-DUL(1)
      UNM = ULM(1+NC)-ULM(1)
      UNP = UNM + DUN
      DUT = DUL(2+NC)-DUL(2)
      KN  = KLV(1)*FKN
      KT  = KLV(3)
      RNP = (RN0*FKN)+(KN*UNP)
      RTM = (SIM(2+NC)+SIM(2))/2.D0
      SEUIL = - MU * RNP

      PM    = VARIM(1)
      EPSPM = VARIM(2)

      TM0 = 0.D0
      TP0 = 0.D0
      ALPH = 0.D0

      CALL NM1DIS(TM0,TP0,KT,ET,ALPH,SEUIL,RTM,DUT,EPSPM,PM,RTP,EPSP,
     &            PP,DSDEM,DSDEP)

      DFL(2)    = -(RTP-RTM)
      DFL(2+NC) =   RTP-RTM
      VARIP(1)       =   PP
      VARIP(LGPG+1)  =   PP
      VARIP(2)       =   EPSP
      VARIP(LGPG+2)  =   EPSP
C
C --- EFFORTS GENERALISES AUX NOEUDS 1 ET 2 (REPERE LOCAL)
C         ---- ON CHANGE LE SIGNE DES EFFORTS SUR LE PREMIER NOEUD
C         ---- POUR LES MECA_DIS_TR_L ET MECA_DIS_T_L
C
      SIP(1)      =  RNP
      SIP(1+NC)   =  RNP
      FL(1)       = -RNP
      FL(1+NC)    =  RNP
      SIP(2)      = -DFL(2)      + SIM(2)
      SIP(2+NC)   =  DFL(2+NC)   + SIM(2+NC)
      FL(2)       =  DFL(2)      - SIM(2)
      FL(2+NC)    =  DFL(2+NC)   + SIM(2+NC)
C
      IF (OPTION.EQ.'FULL_MECA'.OR.OPTION.EQ.'RIGI_MECA_TANG') THEN
        KTRIG = DSDEP

        IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
          KTRIG = DSDEM

        ENDIF

      IF (NC.EQ.3) THEN
         KLV(1)  =  KN
         KLV(7)  = -KN
         KLV(10) =  KN

         KLV(3)  =  KTRIG
         KLV(12) = -KTRIG
         KLV(15) =  KTRIG

      ELSEIF (NC.EQ.6) THEN
         KLV(1)  =  KN
         KLV(22) = -KN
         KLV(28) =  KN

         KLV(3)  =  KTRIG
         KLV(30) = -KTRIG
         KLV(36) =  KTRIG
      ENDIF
      ENDIF

      ENDIF

C -------------------------------------------------

C --- TRAITEMENT DU COMPORTEMENT LIE AU MATERIAU RELA_MZ

      IF ((NC .GT. 3).AND.(ITRAC.EQ.1)) THEN

      CPRAG = VALRE2(4)
      CPRAGM = 2.D0 * CPRAG / 3.D0

C --- DIRECTION LOCALE Z

      DTETAZ = DUL(6+NC)-DUL(6)
      PMZ = VARIM(3)
      XMZ = VARIM(5)

      MOMZ = (SIM(6+NC)+SIM(6))/2.D0
      MOPELZ = MOMZ + CP * DTETAZ
      MOPESZ = MOPELZ - XMZ

      CALL RCFON2('V',JPROLP,JVALEP,NBVALP,RBID,RBID,RBID,PMZ,
     &            RPZ,RPRIMZ,CPRAGM,RBID,RBID)

      IF ( ABS(MOPESZ) .LE. RPZ ) THEN
        DPZ = 0.D0
        MOPZ = MOPELZ
        XPZ  = XMZ
        CPTANZ = CP
      ELSE
        CPMOD = (2.D0 * CP ) / 3.D0
        NU = 0.D0
        SIEEQZ = ABS(MOPESZ)
        CALL RCFON2('E',JPROLP,JVALEP,NBVALP,RBID,CPMOD,NU,PMZ,
     &              RPZ,RPRIMZ,CPRAGM,SIEEQZ,DPZ)
        XPZ  = XMZ + CPRAG * DPZ * MOPESZ /ABS(MOPESZ)
        MOPZ = XPZ + MOPESZ*(1.D0-(CP+CPRAG)*DPZ/ABS(MOPESZ))
        CPTANZ = CP * ( CPRAG + RPRIMZ ) / (CP+CPRAG+RPRIMZ)
      ENDIF

      VARIP(3)      = PMZ+ DPZ
      VARIP(LGPG+3) = PMZ + DPZ
      VARIP(5)      = XPZ
      VARIP(LGPG+5) = XPZ

C --- DIRECTION LOCALE X

      DTETAX = DUL(4+NC)-DUL(4)
      PMX = VARIM(4)
      XMX = VARIM(6)

      MOMX = (SIM(4+NC)+SIM(4))/2.D0
      MOPELX = MOMX + CP * DTETAX
      MOPESX = MOPELX - XMX

      CALL RCFON2('V',JPROLP,JVALEP,NBVALP,RBID,RBID,RBID,PMX,
     &            RPX,RPRIMX,CPRAGM,RBID,RBID)

      IF ( ABS(MOPESX) .LE. RPX ) THEN
        DPX = 0.D0
        MOPX = MOPELX
        XPX  = XMX
        CPTANX = CP
      ELSE
        CPMOD = (2.D0 * CP ) / 3.D0
        NU = 0.D0
        SIEEQX = ABS(MOPESX)
        CALL RCFON2('E',JPROLP,JVALEP,NBVALP,RBID,CPMOD,NU,PMX,
     &              RPX,RPRIMX,CPRAGM,SIEEQX,DPX)
        XPX  = XMX + CPRAG * DPX * MOPESX /ABS(MOPESX)
        MOPX = XPX + MOPESX * (1.D0 -(CP+CPRAG) * DPX / ABS(MOPESX))
        CPTANX = CP * ( CPRAG + RPRIMX ) / (CP+CPRAG+RPRIMX)
      ENDIF

      VARIP(4)      = PMX + DPX
      VARIP(LGPG+4) = PMX + DPX
      VARIP(6)      = XPX
      VARIP(LGPG+6) = XPX

C
C --- EFFORTS GENERALISES AUX NOEUDS 1 ET 2 (REPERE LOCAL)
C         ---- ON CHANGE LE SIGNE DES EFFORTS SUR LE PREMIER NOEUD
C         ---- POUR LES MECA_DIS_TR_L ET MECA_DIS_T_L
C
      SIP(6)      =   MOPZ
      SIP(6+NC)   =   MOPZ
      FL(6)       =  -MOPZ
      FL(6+NC)    =   MOPZ
      SIP(4)      =   MOPX
      SIP(4+NC)   =   MOPX
      FL(4)       =  -MOPX
      FL(4+NC)    =   MOPX
C
      IF (OPTION.EQ.'FULL_MECA'.OR.OPTION.EQ.'RIGI_MECA_TANG') THEN

         IF (NC.EQ.6) THEN
            KLV(21)  =  CPTANZ
            KLV(72)  = -CPTANZ
            KLV(78)  =  CPTANZ

            KLV(10)  =  CPTANX
            KLV(49)  = -CPTANX
            KLV(55)  =  CPTANX
         ENDIF
      ENDIF

      ENDIF

C
C --- FORCES NODALES AUX NOEUDS 1 ET 2 (REPERE GLOBAL)
C
      NNO = 2
      CALL UTPVLG ( NNO, NC, PGL, FL, FONO )


      END
