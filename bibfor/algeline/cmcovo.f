      SUBROUTINE CMCOVO ( MAIN, MAOUT, NBMA, LIMA, PREFNO,
     &                    PREFMA, INIMA, EPAIS, PLAN, TRANS )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER      INIMA, NBMA, LIMA(NBMA)
      CHARACTER*8  MAIN, MAOUT, PREFNO, PREFMA, PLAN, TRANS
      REAL*8       EPAIS
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C TOLE CRP_20
C ----------------------------------------------------------------------
C         EXTRUSION DU GROUP_MA SURF EN GROUP_MA VOL
C ----------------------------------------------------------------------
C IN        MAIN   K8  NOM DU MAILLAGE INITIAL
C IN/JXOUT  MAOUT  K8  NOM DU MAILLAGE TRANSFORME
C IN        NBMA    I  NOMBRE DE MAILLES A TRAITER
C IN        LIMA    I  NUMERO ET TYPE DES MAILLES A TRAITER
C IN        PREFNO K8  PREFIXE DU NOM DES NOEUDS CREES (EX: N, NO, ...)
C IN        PREFMA K8  PREFIXE DU NOM DES MAILLES CREES (EX: N, NO, ..)
C IN        INIMA   I  NUMERO INITIAL DES MAILLES ET NOEUDS CREES
C IN        EPAIS   R  EPAISSEUR D'EXTRUSON
C IN        PLAN    K8 PLAN 'SUP' 'MOY' 'INF'
C IN        TRANS   K8 CAS PLAN ='MOY' ON TRANSLATE EN PEAU INF OU SUP
C ----------------------------------------------------------------------
      INTEGER      JDIME,JCOOR,NBNIN,NBMIN,NBNOT,NBGRNO,IFM,NIV
      INTEGER      JNORN,IMA,N1,N2,N3,NNOAJ,IC,I,IJ,IQ4,IT3
      INTEGER      JNOSTO,JLISMA,JNBNUM,INO,JNORM
      INTEGER      JTYPM,NUMA,NBNO,LGNO,LXLGUT,INOV,JNEWM
      INTEGER      IRET,JNONEW,JVALE,KVALE,IBID,LGNU,LGPREF,NBGRMV
      INTEGER      TYPHEX,TYPPEN,IATYMA,NBNOMX,IER,IMAV,LGND,NBGRMN
      INTEGER      JOPT,NBPT,JNPT,NBNUMA,N4,JDIMO,J,JVG,JREFE
      INTEGER      NBMAI,JGG,NBMAT,JNO,IMA2
      CHARACTER*1  K1B
      CHARACTER*8  K8B,NOMG,KNUME,CDIM,TYPM,MA1,MA2
      CHARACTER*10 KANGL
      CHARACTER*24 NORMNO,NONUMA,GRPMAI,GRPMAV
      CHARACTER*24 VALK(4)
      CHARACTER*24 NOMMAV,NOMNOV,TYPMAV,CONNEV,GRPNOV,NODIMV
      CHARACTER*24 COOVAV,COODSV,COOREV,NOMMAI,NOMNOE,TYPMAI
      CHARACTER*24 CONNEX,GRPNOE,NODIME,COOVAL,COODSC,COOREF
      CHARACTER*24 LISMA,NEWMA
      REAL*8       COON1(3),COON2(3),COON3(3),COON4(3),N1N3(3),N1N2(3)
      REAL*8       NX,NY,NZ,NT(3),EPS2,SINVEC,COSVEC
      REAL*8       N4N2(3),N4N3(3),NQ(3),NORME,R8RDDG,ANGL
      LOGICAL LOGIC
      INTEGER      IARG
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)
      LOGIC = .FALSE.
C
      NOMMAV = MAIN//'.NOMMAI         '
      NOMNOV = MAIN//'.NOMNOE         '
      TYPMAV = MAIN//'.TYPMAIL        '
      CONNEV = MAIN//'.CONNEX         '
      GRPNOV = MAIN//'.GROUPENO       '
      GRPMAV = MAIN//'.GROUPEMA       '
      NODIMV = MAIN//'.DIME           '
      COOVAV = MAIN//'.COORDO    .VALE'
      COODSV = MAIN//'.COORDO    .DESC'
      COOREV = MAIN//'.COORDO    .REFE'
C
      NOMMAI = MAOUT//'.NOMMAI         '
      NOMNOE = MAOUT//'.NOMNOE         '
      TYPMAI = MAOUT//'.TYPMAIL        '
      CONNEX = MAOUT//'.CONNEX         '
      GRPNOE = MAOUT//'.GROUPENO       '
      GRPMAI = MAOUT//'.GROUPEMA       '
      NODIME = MAOUT//'.DIME           '
      COOVAL = MAOUT//'.COORDO    .VALE'
      COODSC = MAOUT//'.COORDO    .DESC'
      COOREF = MAOUT//'.COORDO    .REFE'
C
      CALL JEVEUO ( TYPMAV, 'L', JTYPM )
C
      CALL JEVEUO ( NODIMV, 'L', JDIME )
      NBNIN = ZI(JDIME)
      NBMIN = ZI(JDIME+3-1)

C
      EPS2 = EPAIS / 2.0D0
C
C --- RECUPERATION DU TABLEAU DES COORDONNEES :
C     ---------------------------------------
      CALL JEVEUO ( COOVAV, 'L', JCOOR )
C
C --- RECUPERATION DU NOMBRE DE NOEUDS A AJOUTER
C     POUR DIMENSIONNER LES VECTEURS
C     -------------------------------------------
      CALL WKVECT ( '&&CMCOVO.NEW_NOEUDS', 'V V K8', NBNIN, JNONEW )
      CALL WKVECT ( '&&CMCOVO.NOEUDS'    , 'V V I' , NBNIN, JNOSTO )
      DO 10 IMA = 1, NBMA
         NUMA = LIMA(IMA)
         CALL JENUNO ( JEXNUM('&CATA.TM.NOMTM',ZI(JTYPM+NUMA-1)), TYPM)
         IF (TYPM.EQ.'QUAD4') THEN
         ELSE IF (TYPM.EQ.'TRIA3') THEN
         ELSE
            CALL U2MESK('F','ALGELINE_14',1,TYPM)
         ENDIF
         CALL JELIRA ( JEXNUM(CONNEV,NUMA), 'LONMAX', NBNO, K8B )
         CALL JEVEUO ( JEXNUM(CONNEV,NUMA), 'L', JOPT )
         DO 12 INO =1, NBNO
            ZI(JNOSTO+ZI(JOPT+INO-1)-1) = 1
 12      CONTINUE
 10   CONTINUE
C
      NNOAJ = 0
      DO 14 INO =1,NBNIN
         IF (ZI(JNOSTO+INO-1).EQ.1) NNOAJ = NNOAJ+1
 14   CONTINUE
C
C --- STOCKAGE DES ELEMENTS DE TRAVAIL
C     --------------------------------
      NORMNO = '&&CMCOVO.NORM'
      LISMA  = '&&CMCOVO.LISTE_MAILLES'
      NONUMA = '&&CMCOVO.NB_NUME_MAILLE'
      NEWMA  = '&&CMCOVO.NEW_MAILLE'
C
      CALL WKVECT ( NORMNO, 'V V R',  3*NBMIN, JNORN  )
      CALL WKVECT ( LISMA , 'V V I', 27*NBNIN, JLISMA )
      CALL WKVECT ( NONUMA, 'V V I',    NBNIN, JNBNUM )
      CALL WKVECT ( NEWMA , 'V V I',     NBMA, JNEWM  )
C
C --- STOCKAGE DES NOEUDS A TRAITER
C     ------------------------------
C
      DO 20 IMA = 1, NBMA
         NUMA = LIMA(IMA)
         CALL JELIRA ( JEXNUM(CONNEV,NUMA), 'LONMAX', NBNO, K1B )
         CALL JEVEUO ( JEXNUM(CONNEV,NUMA), 'L', JOPT )
C
C --- CALCUL DU PRODUIT VECTORIEL RELATIF A LA MAILLE NUMA
C     ----------------------------------------------------
        N1 = ZI(JOPT+1-1)
        N2 = ZI(JOPT+2-1)
        N3 = ZI(JOPT+3-1)
        IF (NBNO.EQ.4) N4 =  ZI(JOPT+4-1)
        DO 24 IC=1,3
          COON1(IC)=ZR(JCOOR+3*(N1-1)+IC-1)
          COON2(IC)=ZR(JCOOR+3*(N2-1)+IC-1)
          COON3(IC)=ZR(JCOOR+3*(N3-1)+IC-1)
          IF (NBNO.EQ.4) THEN
            COON4(IC)=ZR(JCOOR+3*(N4-1)+IC-1)
          ELSE
            COON4(IC)=0.0D0
          ENDIF

 24     CONTINUE
        CALL VDIFF(3,COON3,COON1,N1N3)
        CALL VDIFF(3,COON2,COON1,N1N2)
        CALL PROVEC(N1N2, N1N3, NT)
C
        CALL NORMEV(NT,NORME)

        IF (NBNO.EQ.4) THEN
C --- ELEMENT QUAD: ON VERIFIE QUE LE NOEUD 4 N'EST PAS GAUCHE
C --- ON CALCULE LE PDV ET ON MOYENNE AVEC CELUI CALCULER PRECEDEMMENT
C
          CALL VDIFF(3,COON2,COON4,N4N2)
          CALL VDIFF(3,COON3,COON4,N4N3)

          CALL PROVEC(N4N2, N4N3, NQ)

          CALL NORMEV(NQ,NORME)
C LA NORMALE EST INDICEE PAR LE NUMERO DE LA MAILLE NUMA.
          ZR(JNORN+3*(NUMA-1)+1-1) = (NQ(1)+NT(1))/2
          ZR(JNORN+3*(NUMA-1)+2-1) = (NQ(2)+NT(2))/2
          ZR(JNORN+3*(NUMA-1)+3-1) = (NQ(3)+NT(3))/2
        ELSE
          ZR(JNORN+3*(NUMA-1)+1-1) = NT(1)
          ZR(JNORN+3*(NUMA-1)+2-1) = NT(2)
          ZR(JNORN+3*(NUMA-1)+3-1) = NT(3)
        ENDIF
 20   CONTINUE
C
C --- RECUPERATION DU NOMBRE DE MAILLES ET DE LA
C     LISTE DES MAILLES COMMUNE POUR UN NOEUD DONNE
C     -------------------------
      DO 32 IMA =1 ,NBMA
         NUMA = LIMA(IMA)
         CALL JELIRA (JEXNUM(CONNEV,NUMA),'LONMAX',NBNO,K1B)
         CALL JEVEUO (JEXNUM(CONNEV,NUMA),'L',JOPT)
         DO 34 IJ = 1,NBNO
            INO = ZI(JOPT+IJ-1)
            ZI(JNBNUM+INO-1) = ZI(JNBNUM+INO-1) + 1
            ZI(JLISMA-1+27*(INO-1)+ZI(JNBNUM+INO-1)) = NUMA
 34      CONTINUE
 32   CONTINUE
C
C --- ON MOYENNE LES NORMALES
C
      CALL WKVECT('&&CMCOVO.NORM_NO','V V R8',3*NBNIN, JNORM)
      DO 70 INO = 1 , NBNIN
         IF ( ZI(JNOSTO+INO-1) .EQ. 0 ) GOTO 70
         NBNUMA = ZI(JNBNUM+INO-1)
         NUMA = ZI(JLISMA-1+27*(INO-1)+1)
         CALL JENUNO(JEXNUM(NOMMAV,NUMA),MA1)
         ZR(JNORM+3*(INO-1)  ) = ZR(JNORN+3*(NUMA-1)  )
         ZR(JNORM+3*(INO-1)+1) = ZR(JNORN+3*(NUMA-1)+1)
         ZR(JNORM+3*(INO-1)+2) = ZR(JNORN+3*(NUMA-1)+2)
C
C ------ ON VERIFIE QUE L'ANGLE FORME PAR LES NORMALES < 90 DEGRES
C
         DO 72 IMA =2, NBNUMA
            NUMA = ZI(JLISMA-1+27*(INO-1)+IMA)
            COSVEC =  ZR(JNORM+3*(INO-1)  )*ZR(JNORN+3*(NUMA-1)  )
     &              + ZR(JNORM+3*(INO-1)+1)*ZR(JNORN+3*(NUMA-1)+1)
     &              + ZR(JNORM+3*(INO-1)+2)*ZR(JNORN+3*(NUMA-1)+2)
            CALL PROVEC(ZR(JNORM+3*(INO-1)),ZR(JNORN+3*(NUMA-1)),NT)
            SINVEC = NT(1)*NT(1) + NT(2)*NT(2) + NT(3)*NT(3)
            SINVEC = SQRT(SINVEC)
            ANGL = R8RDDG()*ATAN2(SINVEC,COSVEC)
            IF (ABS(ANGL).GT.90.0D0) THEN
               CALL JENUNO(JEXNUM(NOMNOV,INO),NOMG)
               CALL JENUNO(JEXNUM(NOMMAV,NUMA),MA2)
               CALL CODREE(ABS(ANGL),'E',KANGL)
                VALK(1) = NOMG
                VALK(2) = MA1
                VALK(3) = MA2
                VALK(4) = KANGL
                CALL U2MESK('A','ALGELINE_15', 4 ,VALK)
            ENDIF
 72      CONTINUE

         DO 74 IMA =2, NBNUMA
            NUMA = ZI(JLISMA-1+27*(INO-1)+IMA)
            ZR(JNORM+3*(INO-1)  ) = ZR(JNORM+3* (INO-1)  ) +
     &                              ZR(JNORN+3*(NUMA-1)  )
            ZR(JNORM+3*(INO-1)+1) = ZR(JNORM+3* (INO-1)+1) +
     &                              ZR(JNORN+3*(NUMA-1)+1)
            ZR(JNORM+3*(INO-1)+2) = ZR(JNORM+3* (INO-1)+2) +
     &                              ZR(JNORN+3*(NUMA-1)+2)
 74      CONTINUE

         ZR(JNORM+3*(INO-1)  ) =ZR(JNORM+3*(INO-1)  ) / NBNUMA
         ZR(JNORM+3*(INO-1)+1) =ZR(JNORM+3*(INO-1)+1) / NBNUMA
         ZR(JNORM+3*(INO-1)+2) =ZR(JNORM+3*(INO-1)+2) / NBNUMA
         CALL NORMEV(ZR(JNORM+3*(INO-1)),NORME)
 70   CONTINUE

C
      NBMAT = NBMIN + NBMA
      NBNOT = NBNIN + NNOAJ

C ----------------------------------------------------------------------
C          ON AGRANDIT LE '.NOMNOE' ET LE '.COORDO    .VALE'
C ----------------------------------------------------------------------
C
      CALL JEDUPO ( NODIMV, 'G', NODIME, LOGIC )
      CALL JEVEUO ( NODIME, 'E', JDIMO )
      ZI(JDIMO  ) = NBNOT
      ZI(JDIMO+2) = NBMAT
      ZI(JDIMO+5) = 3
C
      CALL JECREO ( NOMNOE, 'G N K8' )
      CALL JEECRA ( NOMNOE, 'NOMMAX', NBNOT, ' ' )
C
C --- ON ECRIT LES NOEUDS DE MAIN DANS MAOUT
C
      DO 40 INO = 1, NBNIN
         CALL JENUNO ( JEXNUM(NOMNOV,INO), NOMG)
         CALL JEEXIN ( JEXNOM(NOMNOE,NOMG), IRET )
         IF (IRET.EQ.0) THEN
            CALL JECROC ( JEXNOM(NOMNOE,NOMG) )
         ELSE
            VALK(1) = NOMG
            CALL U2MESG('F', 'ALGELINE4_5',1,VALK,0,0,0,0.D0)
         END IF
 40   CONTINUE
C
C --- TRAITEMENT DES NOEUDS AJOUTES
C
      LGNO = LXLGUT(PREFNO)
      INOV = INIMA - 1
      DO 50 INO = 1 , NBNIN
         IF ( ZI(JNOSTO+INO-1) .EQ. 0 ) GOTO 50
         INOV = INOV + 1
         CALL CODENT(INOV,'G',KNUME)
         LGNU = LXLGUT(KNUME)
C
         IF (LGNU+LGNO.GT.8) CALL U2MESS('F','ALGELINE_16')
         NOMG  = PREFNO(1:LGNO)//KNUME
         CALL JEEXIN ( JEXNOM(NOMNOE,NOMG), IRET )
         IF (IRET.EQ.0) THEN
            CALL JECROC ( JEXNOM(NOMNOE,NOMG) )
            ZK8(JNONEW+INO-1) = NOMG
         ELSE
            VALK(1) = NOMG
            CALL U2MESG('F', 'ALGELINE4_5',1,VALK,0,0,0,0.D0)
         END IF
 50   CONTINUE
C
C --- RECUPERATION DES COORDONNES DU MAIN ET CREATION
C --- DES COORDONNEES POUR MAOUT
C
      CALL JEDUPO ( COODSV, 'G', COODSC, LOGIC )
      CALL JEDUPO ( COOREV, 'G', COOREF, LOGIC )
      CALL JEVEUO ( COOREF, 'E', JREFE )
      ZK24(JREFE) = MAOUT
C
C --- MAOUT EST DE DIMENSION 3
C
      CALL JEVEUO ( COOVAV, 'L', JVALE )
      CALL WKVECT ( COOVAL, 'G V R8', 3*NBNOT, KVALE )
      CALL CODENT ( 3, 'G', CDIM )
      CALL JEECRA ( COOVAL, 'DOCU',IBID, CDIM )
C
C --- ON RECOPIE DANS MAOUT LES COORDONNEES DES NOEUDS DE MAIN
C
      DO 60 I = 1 , 3*NBNIN
         ZR(KVALE+I-1) = ZR(JVALE+I-1)
 60   CONTINUE
C
C --- POUR CHAQUE NOEUD A TRAITER ON TRANSLATE LES COORDONNEES
C     DU NOUVEAU NOEUDS DE N
C
      JNO = NBNIN
      DO 80 INO = 1 , NBNIN
         IF ( ZI(JNOSTO+INO-1) .EQ. 0 ) GOTO 80
         JNO = JNO + 1
C
         NX = ZR(JNORM+3*(INO-1))
         NY = ZR(JNORM+3*(INO-1)+1)
         NZ = ZR(JNORM+3*(INO-1)+2)
C
         IF (PLAN.EQ.'SUP') THEN
C
            ZR(KVALE+3*(JNO-1)  ) = ZR(KVALE+3*(INO-1)  ) - NX*EPAIS
            ZR(KVALE+3*(JNO-1)+1) = ZR(KVALE+3*(INO-1)+1) - NY*EPAIS
            ZR(KVALE+3*(JNO-1)+2) = ZR(KVALE+3*(INO-1)+2) - NZ*EPAIS
C
         ELSE IF (PLAN.EQ.'INF') THEN
C
            ZR(KVALE+3*(JNO-1)  ) = ZR(KVALE+3*(INO-1)  ) + NX*EPAIS
            ZR(KVALE+3*(JNO-1)+1) = ZR(KVALE+3*(INO-1)+1) + NY*EPAIS
            ZR(KVALE+3*(JNO-1)+2) = ZR(KVALE+3*(INO-1)+2) + NZ*EPAIS
C
         ELSE IF (PLAN.EQ.'MOY') THEN
C
            IF (TRANS.EQ.'INF') THEN
               ZR(KVALE+3*(INO-1)  ) = ZR(KVALE+3*(INO-1)  ) - NX*EPS2
               ZR(KVALE+3*(INO-1)+1) = ZR(KVALE+3*(INO-1)+1) - NY*EPS2
               ZR(KVALE+3*(INO-1)+2) = ZR(KVALE+3*(INO-1)+2) - NZ*EPS2
C
               ZR(KVALE+3*(JNO-1)  ) = ZR(KVALE+3*(INO-1)  ) + NX*EPAIS
               ZR(KVALE+3*(JNO-1)+1) = ZR(KVALE+3*(INO-1)+1) + NY*EPAIS
               ZR(KVALE+3*(JNO-1)+2) = ZR(KVALE+3*(INO-1)+2) + NZ*EPAIS
C
            ELSE IF (TRANS.EQ.'SUP') THEN
               ZR(KVALE+3*(INO-1)  ) = ZR(KVALE+3*(INO-1)  ) + NX*EPS2
               ZR(KVALE+3*(INO-1)+1) = ZR(KVALE+3*(INO-1)+1) + NY*EPS2
               ZR(KVALE+3*(INO-1)+2) = ZR(KVALE+3*(INO-1)+2) + NZ*EPS2
C
               ZR(KVALE+3*(JNO-1)  ) = ZR(KVALE+3*(INO-1)  ) - NX*EPAIS
               ZR(KVALE+3*(JNO-1)+1) = ZR(KVALE+3*(INO-1)+1) - NY*EPAIS
               ZR(KVALE+3*(JNO-1)+2) = ZR(KVALE+3*(INO-1)+2) - NZ*EPAIS
C
            ENDIF
         ENDIF
 80   CONTINUE

C ----------------------------------------------------------------------
C     LE '.NOMMAI' ET LE '.CONNEX'
C ----------------------------------------------------------------------
      CALL JENONU ( JEXNOM('&CATA.TM.NOMTM', 'HEXA8'  ), TYPHEX )
      CALL JENONU ( JEXNOM('&CATA.TM.NOMTM', 'PENTA6' ), TYPPEN )

      CALL JECREO ( NOMMAI, 'G N K8' )
      CALL JEECRA ( NOMMAI, 'NOMMAX', NBMAT, ' ' )

      CALL WKVECT ( TYPMAI, 'G V I', NBMAT, IATYMA )

C     NBNOMX = NBRE DE NOEUDS MAX. POUR UNE MAILLE :
      CALL DISMOI('F','NB_NO_MAX','&CATA','CATALOGUE',NBNOMX,K1B,IER)

      CALL JECREC ( CONNEX, 'G V I', 'NU', 'CONTIG', 'VARIABLE',
     &                                                    NBMAT )
      CALL JEECRA ( CONNEX, 'LONT', NBNOMX*NBMAT, ' ' )
C
C --- ON RECUPERE LES MAILLES DE MAIN DANS MAOUT
C
      DO 90 IMA = 1 , NBMIN

         CALL JENUNO ( JEXNUM(NOMMAV,IMA), NOMG )
         CALL JEEXIN ( JEXNOM(NOMMAI,NOMG), IRET )
         IF (IRET.EQ.0) THEN
            CALL JECROC(JEXNOM(NOMMAI,NOMG))
         ELSE
            VALK(1) = NOMG
            CALL U2MESG('F', 'ALGELINE4_7',1,VALK,0,0,0,0.D0)
         END IF
C
         ZI(IATYMA-1+IMA) = ZI(JTYPM+IMA-1)
C
         CALL JEVEUO ( JEXNUM(CONNEV,IMA), 'L', JOPT )
         CALL JELIRA ( JEXNUM(CONNEV,IMA), 'LONMAX', NBPT, K1B )
C
         CALL JEECRA(JEXNUM(CONNEX,IMA),'LONMAX',NBPT,K8B)
         CALL JEVEUO(JEXNUM(CONNEX,IMA),'E',JNPT)
C
         DO 92 INO = 1 , NBPT
            ZI(JNPT-1+INO) = ZI(JOPT+INO-1)
 92     CONTINUE
 90   CONTINUE
C
C --- TRAITEMENT DES MAILLES AJOUTEES
C
      IQ4 = 0
      IT3 = 0
      LGPREF = LXLGUT(PREFMA)
      IMAV   = INIMA - 1
      DO 100 IMA = 1 , NBMA
         NUMA = LIMA(IMA)
         CALL JEVEUO ( JEXNUM(CONNEV,NUMA), 'L', JOPT )
         CALL JENUNO ( JEXNUM('&CATA.TM.NOMTM',ZI(JTYPM+NUMA-1)), TYPM)
         IMAV = IMAV + 1
         CALL CODENT ( IMAV, 'G', KNUME )
         LGND = LXLGUT(KNUME)
         IF (LGND+LGPREF.GT.8) CALL U2MESS('F','ALGELINE_17')
         NOMG = PREFMA(1:LGPREF)//KNUME

         CALL JEEXIN(JEXNOM(NOMMAI,NOMG),IRET)
         IF (IRET.EQ.0) THEN
            CALL JECROC ( JEXNOM(NOMMAI,NOMG) )
         ELSE
            VALK(1) = NOMG
            CALL U2MESG('F', 'ALGELINE4_7',1,VALK,0,0,0,0.D0)
         END IF

         CALL JENONU ( JEXNOM(NOMMAI,NOMG), IMA2 )
         ZI(JNEWM+IMA-1) = IMA2

         IF ( TYPM .EQ. 'QUAD4' ) THEN
C             -----------------
C --------- CREATION DE LA MAILLE HEXA8
            NBPT = 8
            ZI(IATYMA-1+IMA2) = TYPHEX
C
            CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
            CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
            DO 102 INO = 1 , 4
               ZI(JNPT-1+INO) = ZI(JOPT-1+INO)
 102         CONTINUE
            DO 104 INO = 5 , 8
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNONEW+ZI(JOPT-1+INO-4)-1)),
     &                               ZI(JNPT-1+INO))
 104        CONTINUE
            IQ4 = IQ4 + 1
C
C
         ELSE IF ( TYPM .EQ. 'TRIA3' ) THEN
C             -----------------
C --------- CREATION DE LA MAILLE PENTA6
            NBPT = 6
            ZI(IATYMA-1+IMA2) = TYPPEN
C
            CALL JEECRA ( JEXNUM(CONNEX,IMA2), 'LONMAX', NBPT, K8B )
            CALL JEVEUO ( JEXNUM(CONNEX,IMA2), 'E', JNPT )
            DO 106 INO = 1 , 3
               ZI(JNPT-1+INO) = ZI(JOPT-1+INO)
 106         CONTINUE
            DO 108 INO = 4 , 6
            CALL JENONU(JEXNOM(NOMNOE,ZK8(JNONEW+ZI(JOPT-1+INO-3)-1)),
     &                               ZI(JNPT-1+INO))

 108        CONTINUE
            IT3 = IT3 + 1
C
         ENDIF

 100  CONTINUE
C  -------------------------------------------------------------
C                       CREATION DES GROUP_MA
C  -------------------------------------------------------------
      CALL JEEXIN(GRPMAV,IRET)
      IF (IRET.EQ.0) THEN
        NBGRMV = 0
      ELSE
        CALL JELIRA(GRPMAV,'NOMUTI',NBGRMV,K1B)
      END IF
      NBGRMN = NBGRMV + 1
      IF (NBGRMN.NE.0) THEN
        CALL JECREC(GRPMAI,'G V I','NO','DISPERSE','VARIABLE',NBGRMN)
        DO 110 I = 1,NBGRMV
          CALL JENUNO(JEXNUM(GRPMAV,I),NOMG)
          CALL JEEXIN(JEXNOM(GRPMAI,NOMG),IRET)
          IF (IRET.EQ.0) THEN
            CALL JECROC(JEXNOM(GRPMAI,NOMG))
          ELSE
            VALK(1) = NOMG
            CALL U2MESG('F', 'ALGELINE4_9',1,VALK,0,0,0,0.D0)
          END IF
          CALL JEVEUO(JEXNUM(GRPMAV,I),'L',JVG)
          CALL JELIRA(JEXNUM(GRPMAV,I),'LONUTI',NBMAI,K1B)
          CALL JEECRA(JEXNOM(GRPMAI,NOMG),'LONMAX',
     &        MAX(1,NBMAI),' ')
          CALL JEECRA(JEXNOM(GRPMAI,NOMG),'LONUTI',NBMAI,' ')
          CALL JEVEUO(JEXNOM(GRPMAI,NOMG),'E',JGG)
          DO 112 J = 0,NBMAI - 1
            ZI(JGG+J) = ZI(JVG+J)
  112     CONTINUE
  110   CONTINUE
C
        CALL GETVTX('COQU_VOLU','NOM',1,IARG,1,NOMG,N1)
        CALL JEEXIN(JEXNOM(GRPMAI,NOMG),IRET)
        IF (IRET.EQ.0) THEN
          CALL JECROC(JEXNOM(GRPMAI,NOMG))
        ELSE
            VALK(1) = NOMG
          CALL U2MESG('F', 'ALGELINE4_9',1,VALK,0,0,0,0.D0)
        END IF
          CALL JEECRA(JEXNOM(GRPMAI,NOMG),'LONMAX',
     &        MAX(1,NBMA),' ')
          CALL JEECRA(JEXNOM(GRPMAI,NOMG),'LONUTI',NBMA,' ')
          CALL JEVEUO(JEXNOM(GRPMAI,NOMG),'E',JGG)
          DO 120 J = 1,NBMA
            ZI(JGG+J-1) = ZI(JNEWM+J-1)
  120     CONTINUE
      END IF
C  -------------------------------------------------------------
C                       CREATION DES GROUP_NO
C  -------------------------------------------------------------
      CALL JEEXIN(GRPNOV,IRET)
      IF (IRET.NE.0) THEN
        CALL JELIRA(GRPNOV,'NOMUTI',NBGRNO,K1B)
        CALL JECREC(GRPNOE,'G V I','NO','DISPERSE','VARIABLE',NBGRNO)
        DO 240 I = 1,NBGRNO
          CALL JENUNO(JEXNUM(GRPNOV,I),NOMG)
          CALL JEVEUO(JEXNUM(GRPNOV,I),'L',JVG)
          CALL JELIRA(JEXNUM(GRPNOV,I),'LONUTI',NBNO,K1B)
          CALL JEEXIN(JEXNOM(GRPNOE,NOMG),IRET)
          IF (IRET.EQ.0) THEN
            CALL JECROC(JEXNOM(GRPNOE,NOMG))
          ELSE
            VALK(1) = NOMG
            CALL U2MESG('F', 'ALGELINE4_11',1,VALK,0,0,0,0.D0)
          END IF
          CALL JEECRA(JEXNOM(GRPNOE,NOMG),'LONMAX',
     &        MAX(1,NBNO),' ')
          CALL JEECRA(JEXNOM(GRPNOE,NOMG),'LONUTI',NBNO,' ')
          CALL JEVEUO(JEXNOM(GRPNOE,NOMG),'E',JGG)
          DO 230 J = 0,NBNO - 1
            ZI(JGG+J) = ZI(JVG+J)
  230     CONTINUE
  240   CONTINUE
      END IF

C     -- RETASSAGE  DE CONNEX (QUI A ETE ALLOUEE TROP GRANDE) :
      CALL JECCTA(CONNEX)
C
C ----------------------------------------------------------------------
C
      IF ( NIV .GE. 1 ) THEN
         WRITE(IFM,1000) 1
         IF ( IQ4 .NE. 0 ) WRITE(IFM,1002) IQ4
         IF ( IT3 .NE. 0 ) WRITE(IFM,1004) IT3
      ENDIF
C
      CALL JEDETR ( '&&CMCOVO.NEW_NOEUDS' )
      CALL JEDETR ( '&&CMCOVO.NOEUDS' )
      CALL JEDETR ( '&&CMCOVO.TRAV' )
      CALL JEDETR ( '&&CMCOVO.NORM_NO' )
C     --------------------------------
      CALL JEDETR ( NORMNO )
      CALL JEDETR ( LISMA  )
      CALL JEDETR ( NONUMA )
      CALL JEDETR ( NEWMA  )
C
 1000 FORMAT('MOT CLE FACTEUR "COQU_VOLU", OCCURRENCE ',I4)
 1002 FORMAT('  EXTRUSION DE ',I6,' MAILLES "QUAD4" EN "HEXA8"')
 1004 FORMAT('  EXTRUSION DE ',I6,' MAILLES "TRIA3" EN "PENTA6"')
C
      CALL JEDEMA()
C
      END
