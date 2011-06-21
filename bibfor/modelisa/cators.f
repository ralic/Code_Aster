      SUBROUTINE CATORS(CHARGZ)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     CHARGZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 21/06/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     TRAITER LE MOT CLE CARA_TORSION DE AFFE_CHAR_THER
C     ET ENRICHIR LA CHARGE (CHARGE) AVEC LE VECTEUR
C     CHAR//'.CARA_TORSION'
C     LE BUT EST DE CALCULER LA CONSTANTE DE TORSION D'UNE SECTION
C     COMPORTANT PLUSIEURS TROUS.
C     A CETTE FIN ON RESOUD UN PROBLEME OU L'OPERATEUR DIFFERENTIEL
C     EST UN LAPLACIEN ET LE CHARGEMENT SUR CHAQUE CONTOUR INTERIEUR
C     EST UN FLUX REPARTI CONSTANT VALANT 2*S/L
C     OU S EST L'AIRE DU TROU ET L LA LONGUEUR DU CONTOUR INTERIEUR
C     LE DELIMITANT.
C
C IN/JXVAR : CHARGZ : NOM D'UNE SD CHARGE
C ----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C
      REAL*8        ARMIN, PREC, ZERO
      COMPLEX*16    C16B
      CHARACTER*1   K1BID
      CHARACTER*8   CHARGE, NOMA, MODELE
      CHARACTER*8   K8BID, NOMAIL
      CHARACTER*16  MOTFAC
      CHARACTER*19  LIGRMO, CARFLU, NOMT19
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CHARGE = CHARGZ
C
      NUL    = 0
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      DEUX   = 2.0D0
      XMIN   = ZERO
      YMIN   = ZERO
C
      MOTFAC = 'CARA_TORSION'
      CARFLU = CHARGE//'.CHTH.FLURE'
C
C --- RECUPERATION DU MODELE :
C     ----------------------
      CALL GETVID(' ','MODELE',0,1,1,MODELE,NMO)
C
C --- RECUPERATION DE LA DIMENSION (2 OU 3) DU PROBLEME :
C     -------------------------------------------------
      CALL DISMOI('F','DIM_GEOM',MODELE,'MODELE',NDIM,K8BID,IER)
      IF (.NOT.(NDIM.EQ.2.OR.NDIM.EQ.3))
     &       CALL U2MESS('F','MODELISA2_6')
C
C --- LIGREL DU MODELE :
C     ----------------
      LIGRMO = MODELE//'.MODELE'
C
C --- RECUPERATION DU NOM DU MAILLAGE :
C     -------------------------------
      CALL JEVEUO(LIGRMO//'.LGRF','L',IDNOMA)
      NOMA = ZK8(IDNOMA)
C
C --- RECUPERATION DES COORDONNEES DES NOEUDS DU MAILLAGE :
C     ---------------------------------------------------
      CALL JEVEUO(NOMA//'.COORDO    .VALE','L',IDCOOR)
C
C --- RECUPERATION DES GROUP_MA DEFINISSANT LES BORDS DES TROUS :
C     ---------------------------------------------------------
      CALL GETVTX(MOTFAC,'GROUP_MA',1,1,0,K8BID,NG)
      IF (NG.NE.0) THEN
        NG = -NG
        CALL WKVECT(CHARGE//'.CARA_TORSION','V V R',NG,IDCARA)
        CALL WKVECT('&&CATORS.GRMA','V V K8',NG,IDGRMA)
        CALL GETVEM(NOMA,'GROUP_MA',MOTFAC,'GROUP_MA',1,1,NG,
     &                                            ZK8(IDGRMA),NGR)
C
C ---   RECUPERATION DES COORDONNEES X_MIN ET Y_MIN DU MAILLAGE :
C       -------------------------------------------------------
        CALL JEEXIN (NOMA//'           .LTNT', IRET1 )
        IF ( IRET1 .NE. 0 ) THEN
          CALL LTNOTB ( NOMA , 'CARA_GEOM' , NOMT19 )
          NBPAR = 0
          CALL TBLIVA (NOMT19, NBPAR, ' ', IBID, R8B, C16B, K8BID,
     &                 K8BID,R8B , 'AR_MIN', K8BID, IBID, ARMIN, C16B,
     &                 K8BID, IRET2 )
          IF ( IRET2 .EQ. 0 ) THEN
             PREC = ARMIN*1.D-06
          ELSEIF ( IRET2 .EQ. 1 ) THEN
             PREC = 1.D-10
          ELSE
             CALL U2MESS('F','MODELISA2_13')
          ENDIF
          CALL TBLIVA (NOMT19, NBPAR, ' ', IBID, R8B, C16B, K8BID,
     &                 K8BID,R8B , 'X_MIN', K8BID, IBID, XMIN, C16B,
     &                K8BID, IRET2 )
          IF ( IRET2 .NE. 0 ) CALL U2MESS('F','MODELISA2_13')
          CALL TBLIVA (NOMT19, NBPAR, ' ', IBID, R8B, C16B, K8BID,
     &                 K8BID,R8B, 'Y_MIN', K8BID, IBID, YMIN, C16B,
     &                K8BID, IRET2 )
          IF ( IRET2 .NE. 0 ) CALL U2MESS('F','MODELISA2_13')
        ELSE
          CALL U2MESS('F','MODELISA3_18')
        ENDIF
C
C ---   CALCUL POUR CHAQUE GROUP_MA CONSTITUANT UN BORD DE
C ---   SA LONGUEUR ET DE L'AIRE DU TROU QU'IL DETERMINE :
C       ================================================
        NBMA = 0
        DO 10 IGR = 1, NGR
C
          STRAP  =  ZERO
          XL     =  ZERO
C
C ---     RECUPERATION DES MAILLES DU GROUP_MA :
C         ------------------------------------
          CALL JEVEUO(JEXNOM(NOMA//'.GROUPEMA',ZK8(IDGRMA+IGR-1)),
     &                                                        'L',JGRO)
C
C ---     RECUPERATION DU NOMBRE DE MAILLES DU GROUP_MA :
C         ---------------------------------------------
          CALL JELIRA(JEXNOM(NOMA//'.GROUPEMA',ZK8(IDGRMA+IGR-1)),
     &                                        'LONMAX',NBMAIL,K1BID)
C
C ---     REORIENTATION DES MAILLES DU GROUP_MA :
C         -------------------------------------
          NORIEN = 0
          CALL ORILMA (  NOMA, NDIM, ZI(JGRO), NBMAIL, NORIEN, NTRAIT,
     &                  .TRUE., PREC, NUL, IBID )
C
          NBMA = NBMA + NBMAIL
C
C ---     BOUCLE SUR LES MAILLES (SEG2 OU SEG3) CONSTITUANT LE CONTOUR
C ---     INTERIEUR COURANT :
C         -----------------
          DO 20 M = 1, NBMAIL
C
C ---       NUMERO DE LA MAILLE :
C           -------------------
            NUMAIL = ZI(JGRO+M-1)
C
C ---       NOM DE LA MAILLE :
C           ----------------
            CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMAIL),NOMAIL)
            CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),NUMA)
C
C ---       NOMBRE DE CONNECTIVITES DE LA MAILLE :
C           ------------------------------------
            CALL JELIRA(JEXNUM(NOMA//'.CONNEX',NUMA),'LONMAX',NBNO,
     &                  K1BID)
C
C ---       RECUPERATION DES CONNECTIVITES DE LA MAILLE :
C           -------------------------------------------
            CALL JEVEUO(JEXNUM(NOMA//'.CONNEX',NUMA),'L',JDES)
C
C ---       COORDONNEEES DES NOEUDS SOMMETS DE LA MAILLE DANS
C ---       UN REPERE OU LES AXES SONT TOUJOURS X ET Y MAIS DONT
C ---       L'ORIGINE SE SITUE AU POINT DE COORDONNEES (XMIN,YMIN)
C ---       OU XMIN ET YMIN SONT LES COORDONNEES LES PLUS 'FAIBLES'
C ---       DE NOEUDS DU MAILLAGE : C'EST POUR POUVOIR APPLIQUER
C ---       SANS ERREUR LA FORMULE DONNANT LA SURFACE DETERMINEE
C ---       PAR LE SEGMENT ET SA PROJECTION SUR L'AXE Y :
C           -------------------------------------------
            X1 = ZR(IDCOOR+3*(ZI(JDES)-1)+1-1)-XMIN
            Y1 = ZR(IDCOOR+3*(ZI(JDES)-1)+2-1)-YMIN
            X2 = ZR(IDCOOR+3*(ZI(JDES+1)-1)+1-1)-XMIN
            Y2 = ZR(IDCOOR+3*(ZI(JDES+1)-1)+2-1)-YMIN
C
C ---       LONGUEUR DE L'ELEMENT COURANT :
C           -----------------------------
            XL = XL + SQRT((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2))
C
C ---       AIRE DU TRAPEZE DETERMINE PAR L'ELEMENT SEGMENT COURANT
C ---       ET PAR SA PROJECTION SUR L'AXE Y :
C           --------------------------------
            STRAP = STRAP + UNDEMI*(X1+X2)*(Y2-Y1)
  20      CONTINUE
C
C ---     VALEUR DU FLUX REPARTI SORTANT DU CONTOUR INTERIEUR DU TROU,
C ---     CA VAUT 2*AIRE(TROU)/LONGUEUR_CONTOUR_INTERIEUR :
C         -----------------------------------------------
          ZR(IDCARA+IGR-1) = DEUX*ABS(STRAP)/XL
C
  10    CONTINUE


C ---   AFFECTATION DE LA VALEUR DU FLUX A LA CARTE DE CHARGEMENT
C ---   DU FLUX REPARTI :
C       ---------------

C ---     SI LA CARTE N'EXISTE PAS, ON LA CREE :
C         ------------------------------------
        CALL JEEXIN(CARFLU//'.VALV',IRET1)
        IF (IRET1.EQ.0)  CALL ALCART('G', CARFLU, NOMA, 'FLUN_R')
C
        CALL JEVEUO(CARFLU//'.NCMP','E',JNCMP)
        CALL JEVEUO(CARFLU//'.VALV','E',JVALV)
C
        NCMP = 3
C
        ZK8(JNCMP+1-1) = 'FLUN'
        ZK8(JNCMP+2-1) = 'FLUN_INF'
        ZK8(JNCMP+3-1) = 'FLUN_SUP'
C
        ZR(JVALV+1-1) = ZERO
        ZR(JVALV+2-1) = ZERO
        ZR(JVALV+3-1) = ZERO
C
        CALL NOCART(CARFLU,1,' ','NOM',0,' ',0,LIGRMO,NCMP)
C
        ICMP = 0
        DO 30 IGR = 1, NGR
          ICMP = ICMP + 1
          ZK8(JNCMP+ICMP-1) = 'FLUN'
          ZR(JVALV+ICMP-1)  = ZR(IDCARA+IGR-1)
  30    CONTINUE
C
        IF (ICMP.NE.0) THEN
          DO 40 IGR = 1, NGR
            CALL NOCART(CARFLU,2,ZK8(IDGRMA+IGR-1),'NOM',0,' ',0,
     &                  LIGRMO,ICMP)
  40      CONTINUE
        ENDIF
C

C
        CALL JEDETR(CHARGE//'.CARA_TORSION')
        CALL JEDETR('&&CATORS.GRMA')
      ENDIF
C
      CALL JEDEMA()
      END
