      SUBROUTINE MDALLR (RESU1,RESU2,BASEMO,NBMODE,NBSAUV,VECPR8,VECPC8,
     &                   ZCMPLX)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/03/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C
C     ALLOCATION DES VECTEURS DE SORTIE (DONNEEES MODALES REELLES)
C     ------------------------------------------------------------------
C IN  : NOMRES : NOM DU CONCEPT RESULTAT
C IN  : NBMODE : NOMBRE DE MODES
C IN  : NBSAUV : NOMBRE DE PAS CALCULE (INITIAL COMPRIS)
C IN  : DATAx  : DONNEES MODALES AU COMPLET (x=I POUR ENTIER, x=K POUR
C                CHAR, x=R POUR REEL)
C ----------------------------------------------------------------------
      IMPLICIT NONE
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER          NBMODE,NBSAUV,IERD,INUMG,LDLIM,
     &                 IMODE,IER,JREFE,IBID,LVALE,J1REFE,I,J,
     &                 LDCONL,IBLO,JREFA
      LOGICAL          LREFE,ZCMPLX
      CHARACTER*8      RESU1,RESU2,CBID,MATGEN,K8B,BASEMO,TYP
      CHARACTER*14     NUGENE
      CHARACTER*16     NOMCMD
      CHARACTER*19     CHAMGE
      CHARACTER*24     RAIDE
      REAL*8           VECPR8(NBMODE,*)
      COMPLEX*16       VECPC8(NBMODE,*)
C
      INTEGER          NBMAX, IPAR, IPAR1, IPAR2
      PARAMETER        (NBMAX=50)
      CHARACTER*24     KPAR(NBMAX)
      CHARACTER*32     JEXNUM

      CALL JEMARQ()

      LREFE = .TRUE.
      NUGENE = RESU2//'.NUGENE'
      MATGEN = '&&MDALMA'

C CREATION DE LA NUMEROTATION GENERALISE SUPPORT
      CALL  NUMMO1(NUGENE,BASEMO,NBMODE,'PLEIN')
      CALL  CRNSLV(NUGENE,'LDLT','SANS','G')

C CREATION DE LA MATRICE GENERALISE SUPPORT
      CALL WKVECT(MATGEN//'           .REFA','V V K24',11,JREFA)
      ZK24(JREFA-1+11)='MPI_COMPLET'
      ZK24(JREFA-1+1)=BASEMO
      ZK24(JREFA-1+2)=NUGENE
      ZK24(JREFA-1+9) = 'MS'
      ZK24(JREFA-1+10) = 'GENE'
      CALL WKVECT(MATGEN//'           .LIME','V V K8',1,LDLIM)
      ZK8(LDLIM)=NUGENE

C recuperation des parametres a garder dans le modele gene
      CALL GETVTX(' ','NOM_PARA',1,1,NBMAX,KPAR,IPAR)

      DO 100 IMODE = 1, NBSAUV
C        --- VECTEUR PROPRE ---
        CALL RSEXCH (RESU2, 'DEPL', IMODE, CHAMGE, IER )
        IF     ( IER .EQ. 0   ) THEN
        ELSEIF ( IER .EQ. 100 .AND. LREFE ) THEN
          IF (.NOT. ZCMPLX) THEN
            CALL VTCREM (CHAMGE, MATGEN, 'G', 'R' )
          ELSE
            CALL VTCREM (CHAMGE, MATGEN, 'G', 'C' )
          ENDIF
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        CALL JEECRA (CHAMGE//'.DESC', 'DOCU', IBID, 'VGEN' )
        CALL JEVEUO (CHAMGE//'.VALE', 'E', LVALE )
        DO 110 IER = 1, NBMODE
          IF (.NOT. ZCMPLX) THEN
              ZR(LVALE+IER-1) = VECPR8(IER,IMODE)
          ELSE
              ZC(LVALE+IER-1) = VECPC8(IER,IMODE)
          ENDIF
 110    CONTINUE
        CALL RSNOCH (RESU2, 'DEPL', IMODE, ' ' )

        DO 200 I = 1 , IPAR
           CALL RSADPA(RESU1,'L',1,KPAR(I),IMODE,1,IPAR1,TYP)
           CALL RSADPA(RESU2,'E',1,KPAR(I),IMODE,0,IPAR2,K8B)
           IF (TYP(1:1) .EQ. 'I') THEN
              ZI(IPAR2) = ZI(IPAR1)
           ELSEIF (TYP(1:1) .EQ. 'R') THEN
              ZR(IPAR2) = ZR(IPAR1)
           ELSEIF (TYP(1:2) .EQ. 'K8') THEN
              ZK8(IPAR2) = ZK8(IPAR1)
           ELSEIF (TYP(1:3) .EQ. 'K16') THEN
              ZK16(IPAR2) = ZK16(IPAR1)
           ELSEIF (TYP(1:3) .EQ. 'K32') THEN
              ZK32(IPAR2) = ZK32(IPAR1)
           ENDIF
 200    CONTINUE
 100  CONTINUE

      CALL VPCREA(0,RESU2,' ',' ',' ',' ',IER)
      CALL JEDETC (' ',MATGEN,1)

      CALL JEDEMA()

      END
