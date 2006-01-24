      SUBROUTINE MDALLR (NOMRES,BASEMO,NBMODE,NBSAUV,VECPR8,VECPC8,
     &                   FREQ,ZCMPLX)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/01/2006   AUTEUR NICOLAS O.NICOLAS 
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
      INTEGER          NBMODE,NBSAUV,IERD,INUMG,LDREF,LDLIM,
     &                 IMODE,IER,JREFE,IBID,LVALE,J1REFE,I,J,
     &                 LLDESC,LDCONL,IBLO
      LOGICAL          LREFE,ZCMPLX
      CHARACTER*8      NOMRES,NUMGEN,TYPROF,CBID,MATGEN,K8B,BASEMO
      CHARACTER*16     NOMCMD
      CHARACTER*19     CHAMNO,NOMNUM,NOMSTO
      CHARACTER*24     RAIDE
      REAL*8           VECPR8(NBMODE,*),FREQ(*)
      COMPLEX*16       VECPC8(NBMODE,*)
C
      INTEGER NBPARI, NBPARR, NBPARK, NBPARA, MXDDL,LADPA
      PARAMETER   ( NBPARI=8 , NBPARR=16 , NBPARK=2, NBPARA=26 )
      CHARACTER*24 NOPARA(NBPARA)
      CHARACTER*32  JEXNUM

C     ------------------------------------------------------------------
      DATA  NOPARA /
     +  'NUME_MODE'       , 'ITER_QR'         , 'ITER_BATHE'      ,
     +  'ITER_ARNO'       , 'ITER_JACOBI'     , 'ITER_SEPARE'     ,
     +  'ITER_AJUSTE'     , 'ITER_INVERSE'    ,
     +  'NORME'           , 'METHODE'         ,
     +  'FREQ'            ,
     +  'OMEGA2'          , 'AMOR_REDUIT'     , 'ERREUR'          ,
     +  'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,
     +  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,
     +  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,
     +  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
C     ------------------------------------------------------------------
      CALL JEMARQ()

      LREFE = .TRUE.
      NUMGEN = '&&MDALNU'
      MATGEN = '&&MDALMA'
      TYPROF = 'LIGN_CIE'
      
C creation de la numerotation generalise support      
      NOMNUM=NUMGEN//'      .NUME'
      NOMSTO=NUMGEN//'      .SLCS'
      CALL  NUMMO1 ( NOMNUM, NOMSTO, BASEMO, NBMODE, TYPROF )

C creation de la matrice generalise support      
      CALL JEVEUO(NOMSTO//'.DESC','L',LLDESC)
      CALL JELIBE(NOMSTO//'.DESC')
      CALL WKVECT(MATGEN//'           .REFA','V V K24',4,LDREF)
      CALL JEECRA(MATGEN//'           .REFA','DOCU',IBID,'SLCS')
      ZK24(LDREF)=BASEMO
      ZK24(LDREF+1)=NOMNUM
      ZK24(LDREF+2)=NOMSTO
      CALL WKVECT(MATGEN//'           .LIME','V V K8',1,LDLIM)
      ZK8(LDLIM)=NUMGEN
      
      DO 100 IMODE = 1, NBSAUV
C        --- VECTEUR PROPRE ---
        CALL RSEXCH (NOMRES, 'DEPL', IMODE, CHAMNO, IER )
        IF     ( IER .EQ. 0   ) THEN
        ELSEIF ( IER .EQ. 100 .AND. LREFE ) THEN
          IF (.NOT. ZCMPLX) THEN
            CALL VTCREM (CHAMNO, MATGEN, 'G', 'R' )
          ELSE
            CALL VTCREM (CHAMNO, MATGEN, 'G', 'C' )
          ENDIF
        ELSE
          CALL UTDEBM('F','MDALLR','APPEL ERRONE')
          CALL UTFINM()
        ENDIF
        CALL JEECRA (CHAMNO//'.DESC','DOCU',IBID,'VGEN')
        CALL JEVEUO (CHAMNO//'.VALE', 'E', LVALE )
        DO 110 IER = 1, NBMODE
          IF (.NOT. ZCMPLX) THEN
              ZR(LVALE+IER-1) = VECPR8(IER,IMODE)
          ELSE
              ZC(LVALE+IER-1) = VECPC8(IER,IMODE)
          ENDIF    
 110    CONTINUE
        CALL RSNOCH (NOMRES, 'DEPL', IMODE, ' ' )

           DO 200 I = 1 , NBPARI
              CALL RSADPA(NOMRES,'E',1,NOPARA(I),IMODE,0,LADPA,K8B)
              ZI(LADPA) = I
 200       CONTINUE
           DO 210 I = 1 , NBPARK
              CALL RSADPA(NOMRES,'E',1,NOPARA(NBPARI+I),IMODE,
     &        0,LADPA,K8B)
              ZK24(LADPA) = ' '
 210       CONTINUE
           J = NBPARI + NBPARK
           DO 220 I = 1 , NBPARR
              CALL RSADPA(NOMRES,'E',1,NOPARA(J+I),IMODE,0,LADPA,K8B)
              ZR(LADPA) = FREQ(IMODE)
 220       CONTINUE

 100  CONTINUE

      CALL VPCREA(0,NOMRES,' ',' ',' ',' ',IER)
      CALL JEDETC (' ',NUMGEN,1)
      CALL JEDETC (' ',MATGEN,1)

      CALL JEDEMA()
      
      END
