      SUBROUTINE PECAPO(RESU,MODELE,CARA,NCHAR,LCHAR,NH)
      IMPLICIT   NONE
      INTEGER           NCHAR, NH
      CHARACTER*(*)     RESU, MODELE, CARA, LCHAR(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C     OPERATEUR   POST_ELEM
C     TRAITEMENT DU MOT CLE-FACTEUR "CARA_POUTRE"
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      NBTORS, NBGAUC, NBCISA, IRET, NT, IBID, NOPT,
     &             NTAB, NCT, ILIGN, NCTY, NCTZ, NGM ,IFM, NIV, NGI,
     &             NGRI, IDGRMI, NRT, J, JP, JV, NBPAR, JN, I, NBRT
      PARAMETER    ( NBTORS = 1 , NBGAUC = 1 , NBCISA = 8 , NBRT = 1 )
      REAL*8       VALPAR(NBCISA), AY, AZ, EY, EZ, PCTX, PCTY, R8B,RT,
     &             CT, S, XG, YG, IY, IZ, ALPHA, IOMEGA,DXG,DYG,YGI,ZGI
      CHARACTER*8  K8B, NOMA, NOMAIL, NOGRMA, TEMPER, TEMPE1, TEMPE2,
     &             PTORS(NBTORS), PGAUC(NBGAUC), PCISA(NBCISA),
     &             PRT(NBRT)
      CHARACTER*16 OPTION
      CHARACTER*19 NOMTAB
      CHARACTER*24 CHGEOM, CHCARA(15), CHHARM
      COMPLEX*16   C16B
      REAL*8 K1,K2,KY,KZ,KYEQ,KZEQ,IYEQ,IZEQ,SEQ,EE,GG,HH,KSI,NU,ALPHAI
      REAL*8 C1,C2,PHI1,PHI2,ALPHAR,COS2,SIN2,R8DGRD,ALPHEQ,XGEQ,YGEQ
      CHARACTER*16 LL
      CHARACTER*8 MATER
      CHARACTER*2 CODRET
      INTEGER ILIGNM,N1
C     ------------------------------------------------------------------
      DATA  PTORS / 'CT'      /
      DATA  PRT   / 'RT'      /
      DATA  PGAUC / 'JG'      /
      DATA  PCISA / 'AY'      ,  'AZ'      ,  'EY'      ,  'EZ'      ,
     &              'PCTX'    ,  'PCTY'    ,  'KY'      ,  'KZ'      /
C     ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
C --- RECUPERATION DE LA TABLE A COMPLETER ISSUE DE L'OPTION
C --- CARA_GEOM DE POST_ELEM :
C     ----------------------
      CALL GETVID('CARA_POUTRE','CARA_GEOM',1,1,0,K8B,NTAB)
      IF ( NTAB .NE. 0 ) THEN
         CALL GETVID('CARA_POUTRE','CARA_GEOM',1,1,1,NOMTAB,NTAB)
         CALL TBCOPI('G',NOMTAB,RESU)
      ELSE
         CALL U2MESS('F','UTILITAI3_59')
      ENDIF
C
      OPTION = 'MASS_INER'
C
      CALL MECHAM ( OPTION, MODELE, NCHAR, LCHAR, CARA, NH,
     &                              CHGEOM, CHCARA, CHHARM, IRET )
C
C --- RECUPERATION DU MAILLAGE INITIAL :
C     --------------------------------
      CALL TBEXP2(RESU,'MAILLAGE')
      CALL TBLIVA ( RESU, 0, K8B, IBID, R8B, C16B, 'TOUT', K8B,
     &              R8B, 'MAILLAGE', K8B, IBID, R8B, C16B, NOMA, IRET )
      NOMAIL=NOMA
C
      NGM = 0
      NT = 0

C     INSERTION DU PARAMETRE 'RT' DANS LA TABLE 'RESU'
      CALL TBAJPA ( RESU,1,PRT,'R')
      CALL GETVTX('CARA_POUTRE','TOUT'    ,1,1,0,K8B,NT)
      IF ( NT .EQ. 0 ) THEN
        CALL GETVID ( 'CARA_POUTRE', 'GROUP_MA', 1,1,0, K8B, NGM )
        IF (NGM.NE.0) THEN
          NGM = 1
          CALL GETVID('CARA_POUTRE','GROUP_MA',1,1,NGM,NOGRMA,NGM)
          NOMA=NOGRMA
          CALL GETVR8 ( 'CARA_POUTRE', 'LONGUEUR', 1,1,1, HH , N1 )
          CALL GETVTX ( 'CARA_POUTRE', 'LIAISON' , 1,1,1, LL , N1 )
          CALL GETVID ( 'CARA_POUTRE', 'MATERIAU', 1,1,1,MATER,N1 )
          IF (N1.EQ.0) THEN
            NU=0.D0
          ELSE
            K8B = ' '
            CALL RCVALE(MATER,'ELAS',0,K8B,R8B,
     &                  1,'NU      ',NU,CODRET,'FM')
          ENDIF
        ENDIF
      ENDIF
C
C ---   RECUPERATION DU NUMERO DE LIGNE DE LA TABLE RESULTAT POUR LA
C ---   VARIABLE "NOMA" :
C       ---------------
      CALL TBEXP2(RESU,'LIEU')
      CALL TBNULI ( RESU, 1, 'LIEU', IBID , R8B, C16B, NOMAIL,
     &              R8B, K8B, ILIGNM )
      CALL TBNULI ( RESU, 1, 'LIEU', IBID , R8B, C16B, NOMA,
     &              R8B, K8B, ILIGN )
      IF ( ILIGN .LT. 0 ) ILIGN = 0
C
C ---   RECUPERATION DE L'OPTION DE CALCUL RELATIVE AUX
C ---   CARACTERISTIQUES DE POUTRE :
C       --------------------------
      CALL GETVTX('CARA_POUTRE','OPTION',1,1,0,K8B,NOPT)
      IF ( NOPT .EQ. 0 ) THEN
         CALL U2MESS('F','UTILITAI3_60')
      ENDIF
C
      CALL GETVTX('CARA_POUTRE','OPTION',1,1,1,OPTION,NOPT)
C
C ---   LES SEULES OPTIONS PERMISES, POUR LE MOMENT, SONT
C ---   'CARA_TORSION' ET 'CARA_CISAILLEMENT':
C       ------------------------------------
      IF ( OPTION .NE. 'CARA_TORSION'     .AND.
     &     OPTION .NE. 'CARA_CISAILLEMEN' .AND.
     &     OPTION .NE. 'CARA_GAUCHI'      ) THEN
         CALL U2MESK('F','UTILITAI3_61',1,OPTION)
      ENDIF
C
C     -----------------------------------------------------------
C --- -CALCUL DE LA CONSTANTE DE TORSION
C --- -AJOUT DU RAYON DE TORSION DANS LA TABLE 'RESU'
C     -----------------------------------------------------------

C --- RECUPERATION DU RAYON DE TORSION :
C     --------------------------------
      IF ( OPTION .EQ. 'CARA_TORSION' ) THEN
        CALL GETVR8('CARA_POUTRE','RT',1,1,0,RT,NRT)
        IF ( NRT .NE. 0 ) THEN
           NRT=-NRT
           CALL GETVR8('CARA_POUTRE','RT',1,1,1,RT,NRT)
        ENDIF

C --- RECUPERATION DU RESULTAT DE TYPE EVOL_THER DONT L'INTEGRALE
C --- SUR LA SECTION DE LA POUTRE VA DONNER LA CONSTANTE DE TORSION :
C     -------------------------------------------------------------
        CALL GETVID('CARA_POUTRE','LAPL_PHI',1,1,0,K8B,NCT)
        IF ( NCT .NE. 0 ) THEN
          NCT=-NCT
          CALL GETVID('CARA_POUTRE','LAPL_PHI',1,1,1,TEMPER,NCT)
        ELSE
           CALL U2MESS('F','UTILITAI3_62')
        ENDIF
C
C --- RECUPERATION DES MAILLES DE BORD CONSTITUANT LES
C --- CONTOURS INTERIEURS :
C     -------------------
        CALL GETVID('CARA_POUTRE','GROUP_MA_INTE',1,1,0,K8B,NGI)
        IF (NGI.NE.0) THEN
          NGI = -NGI
          CALL WKVECT('&&PECAPO.GRMA_INTE','V V K8',NGI,IDGRMI)
          CALL GETVID('CARA_POUTRE','GROUP_MA_INTE',1,1,
     &                NGI,ZK8(IDGRMI),NGRI)
        ELSE
          CALL WKVECT('&&PECAPO.GRMA_INTE','V V K8',1,IDGRMI)
        ENDIF
C
C --- CALCUL DE LA CONSTANTE DE TORSION CT :
C     ------------------------------------
        CALL PECAP1 ( CHGEOM, TEMPER, NGI, ZK8(IDGRMI), CT )
C
C --- AJOUT DE CT ET RT DANS LA TABLE 'RESU' :
C     --------------------------------------
        IF ( NRT .NE. 0 ) THEN
             CALL TBAJLI ( RESU, NBRT, PRT, IBID , RT,
     &                     C16B, K8B, ILIGN)
        ENDIF
        CALL TBAJLI ( RESU, NBTORS, PTORS, IBID , CT,
     &                C16B, K8B, ILIGN)
C         ILIGN = 1
C
C     ------------------------------------------
C --- -CALCUL DES COEFFICIENTS DE CISAILLEMENT -
C --- -ET DES COORDONNEES DU CENTRE DE TORSION -
C     ------------------------------------------
      ELSEIF (OPTION.EQ.'CARA_CISAILLEMEN') THEN
C
C --- RECUPERATION DE 2 RESULTATS DE TYPE EVOL_THER DESTINES A
C --- CALCULER LES COEFFICIENTS DE CISAILLEMENT ET LES COORDONNEES
C --- DU CENTRE DE CISAILLEMENT/TORSION :
C     ---------------------------------
        CALL GETVID('CARA_POUTRE','LAPL_PHI_Y',1,1,0,K8B,NCTY)
        IF ( NCTY .NE. 0 ) THEN
          CALL GETVID('CARA_POUTRE','LAPL_PHI_Y',1,1,1,TEMPE1,
     &                NCTY )
        ELSE
           CALL U2MESS('F','UTILITAI3_63')
        ENDIF
C
        CALL GETVID('CARA_POUTRE','LAPL_PHI_Z',1,1,0,K8B,NCTZ)
        IF ( NCTZ .NE. 0 ) THEN
          CALL GETVID('CARA_POUTRE','LAPL_PHI_Z',1,1,1,TEMPE2,
     &                NCTZ)
        ELSE
           CALL U2MESS('F','UTILITAI3_64')
        ENDIF
C
C --- RECUPERATION DANS LA TABLE DE LA SURFACE DE LA SECTION S,
C --- DES INERTIES PRINCIPALES IY ET IZ, DE L'ANGLE ALPHA FORME
C --- PAR LES AXES PRINCIPAUX D'INERTIE AVEC LES AXES GLOBAUX ET
C --- DES COORDONNEES DU CENTRE DE GRAVITE DANS LE REPERE GLOBAL :
C     ----------------------------------------------------------
        CALL TBEXP2(RESU,'LIEU')
        CALL TBEXP2(RESU,'AIRE')
        CALL TBEXP2(RESU,'IY_PRIN_G')
        CALL TBEXP2(RESU,'IZ_PRIN_G')
        CALL TBEXP2(RESU,'ALPHA')
        CALL TBEXP2(RESU,'CDG_X')
        CALL TBEXP2(RESU,'CDG_Y')
        CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMA, K8B,
     &            R8B, 'AIRE', K8B, IBID, S, C16B, K8B, IRET )
        IF ( IRET .NE. 0 ) CALL U2MESS('F','MODELISA2_89')
        CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMA, K8B,
     &            R8B, 'IY_PRIN_G', K8B, IBID, IY, C16B, K8B, IRET )
        IF ( IRET .NE. 0 ) CALL U2MESS('F','MODELISA2_89')
        CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMA, K8B,
     &            R8B, 'IZ_PRIN_G', K8B, IBID, IZ, C16B, K8B, IRET )
        IF ( IRET .NE. 0 ) CALL U2MESS('F','ALGELINE_7')
        CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMA, K8B,
     &            R8B, 'ALPHA', K8B, IBID, ALPHA, C16B, K8B, IRET )
        CALL ASSERT(IRET .EQ. 0 )
        CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMA, K8B,
     &            R8B, 'CDG_X', K8B, IBID, XG, C16B, K8B, IRET )
        CALL ASSERT(IRET.EQ.0)
        CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMA, K8B,
     &            R8B, 'CDG_Y', K8B, IBID, YG, C16B, K8B, IRET )
        CALL ASSERT(IRET.EQ.0)
C
C --- CALCUL DES COORDONNEES DU CENTRE DE CISAILLEMENT/TORSION EY ET EZ
C --- ET DES COEFFICIENTS DE CISAILLEMENT
C --- (OU PLUTOT DE LEUR INVERSE) AY ET AZ :
C     ------------------------------------
        CALL PECAP2( CHGEOM, IY, IZ, S, ALPHA, XG, YG, TEMPE1,
     &               TEMPE2, AY, AZ, EY, EZ, PCTX, PCTY )
C
C     ON CHANGE DE SIGNE EY EZ CAR ON ATTEND CG ET NON PAS GC
C     CF DOC MACRO_CARA_POUTRE
        VALPAR(1) = AY
        VALPAR(2) = AZ
        VALPAR(3) = -EY
        VALPAR(4) = -EZ
        VALPAR(5) = -PCTX
        VALPAR(6) = -PCTY
        VALPAR(7) = 0.D0
        VALPAR(8) = 0.D0
        CALL TBAJLI ( RESU, NBCISA, PCISA, IBID , VALPAR,
     &                C16B, K8B, ILIGN )
        IF (NOMAIL.NE.NOMA) THEN
          CALL TBEXP2(RESU,'KY')
          CALL TBEXP2(RESU,'KZ')
C       CAS OU IL FAUT FAIRE UN CUMUL DANS LE MAILLAGE COMPLET
          CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMAIL,
     &       K8B, R8B, 'AIRE', K8B, IBID, SEQ, C16B, K8B, IRET )
          CALL ASSERT(IRET.EQ.0)
          CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMAIL,
     &       K8B, R8B, 'IY_PRIN_G', K8B, IBID, IYEQ, C16B, K8B, IRET )
          CALL ASSERT(IRET.EQ.0)
          CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMAIL, K8B,
     &             R8B, 'IZ_PRIN_G', K8B, IBID, IZEQ, C16B, K8B, IRET )
          CALL ASSERT(IRET.EQ.0)

          CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMAIL, K8B,
     &             R8B, 'KY', K8B, IBID, KY, C16B, K8B, IRET )
          IF ( IRET .NE. 0 ) KY=0.D0

          CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMAIL, K8B,
     &           R8B, 'KZ', K8B, IBID, KZ, C16B, K8B, IRET )
          IF ( IRET .NE. 0 ) KZ=0.D0

          CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMAIL, K8B,
     &             R8B, 'ALPHA', K8B, IBID, ALPHEQ, C16B, K8B, IRET )
          IF ( IRET .NE. 0 ) CALL U2MESS('F','UTILITAI3_65')
          CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMAIL, K8B,
     &             R8B, 'CDG_X', K8B, IBID, XGEQ, C16B, K8B, IRET )
          IF ( IRET .NE. 0 ) CALL U2MESS('F','UTILITAI3_66')
          CALL TBLIVA ( RESU, 1, 'LIEU', IBID, R8B, C16B, NOMAIL, K8B,
     &             R8B, 'CDG_Y', K8B, IBID, YGEQ, C16B, K8B, IRET )
          IF ( IRET .NE. 0 ) CALL U2MESS('F','UTILITAI3_67')
C
C         VECTEUR GEQ-GI DANS LE REPERE GLOBAL
C
C         DXG=XG-XGEQ
C         DYG=YG-YGEQ
C
C         VECTEUR GEQ-GI DANS LE REPERE PRINCIPAL DE NOMA
C
C         ALPHAI=ALPHA*R8DGRD()
C         YGI= COS(ALPHAI)*DXG+SIN(ALPHAI)*DYG
C         ZGI=-SIN(ALPHAI)*DXG+COS(ALPHAI)*DYG
C
C         MOMENTS D'INERTIE PAR RAPPORT A GEQ,YI,ZI
C         TRANSPORT SUPPRIME CAR NON JUSTIFIE
C         DONNE DES RESULTATS FAUX SUR ZZZZ105H
C          IY = IY + S*ZGI**2
C          IZ = IZ + S*YGI**2
C
C         SEUL LE RAPPORT E/G EST IMPORTANT
C
          GG=1.D0/2.D0/(1.D0+NU)
          EE=1.D0
          KSI=1.D0
          IF (LL.EQ.'ROTULE') KSI=4.D0
          C1 = 12.D0*EE*IZ
          C2 = 12.D0*EE*IY
          PHI1=C1/( (S/AY)*GG*(HH**2) )
          PHI2=C2/( (S/AZ)*GG*(HH**2) )
          K1=C1/(HH**3*(KSI+PHI1))
          K2=C2/(HH**3*(KSI+PHI2))

          ALPHAR = (ALPHA-ALPHEQ)*R8DGRD()
          COS2 = COS(ALPHAR)**2
          SIN2 = SIN(ALPHAR)**2
          KY=KY+ ( K1 * COS2 + K2 * SIN2)
          KZ=KZ+ ( K1 * SIN2 + K2 * COS2)
C
          KYEQ=(12.D0*EE*IZEQ)/
     &         (GG*SEQ*HH**2)/( 12.D0*EE*IZEQ/KY/HH**3 -1.D0 )
C
          KZEQ=(12.D0*EE*IYEQ)/
     &         (GG*SEQ*HH**2)/( 12.D0*EE*IYEQ/KZ/HH**3 -1.D0 )

C         NOUVEAUX AY ET AZ POUR LE MAILLAGE
          VALPAR(1) = 1.D0/KYEQ
          VALPAR(2) = 1.D0/KZEQ
          CALL TBAJLI ( RESU, 2, PCISA(1), IBID , VALPAR(1),
     &                  C16B, K8B, ILIGNM)
          VALPAR(7) = KY
          VALPAR(8) = KZ
          CALL TBAJLI ( RESU, 2, PCISA(7), IBID , VALPAR(7),
     &                  C16B, K8B, ILIGNM)
        ENDIF
C
C     ------------------------------------------
C --- -CALCUL DE LA CONSTANTE DE GAUCHISSEMENT -
C     ------------------------------------------
      ELSEIF (OPTION.EQ.'CARA_GAUCHI') THEN
C
C --- RECUPERATION DU RESULTAT DE TYPE EVOL_THER DONT L'INTEGRALE
C --- SUR LA SECTION DE LA POUTRE VA DONNER LA CONSTANTE DE
C --- GAUCHISSEMENT :
C     -------------
        CALL GETVID('CARA_POUTRE','LAPL_PHI',1,1,0,K8B,NCT)
        IF ( NCT .NE. 0 ) THEN
          CALL GETVID('CARA_POUTRE','LAPL_PHI',1,1,1,TEMPER,NCT)
        ELSE
          CALL U2MESS('F','UTILITAI3_62')
        ENDIF
C
C --- CALCUL DE LA CONSTANTE DE GAUCHISSEMENT IOMEGA :
C     ----------------------------------------------
        CALL PECAP3 ( CHGEOM, TEMPER, IOMEGA )
C
        CALL TBAJLI ( RESU, NBGAUC, PGAUC, IBID , IOMEGA,
     &                C16B, K8B, ILIGN )
      ENDIF
C
      CALL JEDETC('V','&&PECAPO',1)
C
 9999 CONTINUE
      CALL JEDEMA ( )
      END
