      SUBROUTINE WPNORM ( NORM, PARA, LMATR, NEQ, NBMODE, DDLEXC,
     &                   VECPRO, RESUFR , COEF)
      IMPLICIT   NONE
      CHARACTER*(*)       NORM, PARA
      INTEGER             NBMODE, NEQ, LMATR(*), DDLEXC(*)
      COMPLEX*16          VECPRO(NEQ,*)
      REAL*8              RESUFR(NBMODE,*), COEF(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C     NORMALISATION DE VECTEURS COMPLEXES ET DE GRANDEURS MODALES
C     ------------------------------------------------------------------
C IN  NORM   : TYPE DE NORMALISATION
C          = 'AVEC_CMP'
C          = 'MASS_GENE'
C          = 'RIGI_GENE'
C          = 'EUCL'
C IN  PARA   : ON REPERCUTE LA NORMALISATION SUR LES PARAMETRES MODAUX
C          = 'OUI' DANS CE CAS ILS DOIVENT DEJA AVOIR ETE CALCULES
C          = 'NON' ON NE NORMALISE QUE LES VECTEURS PROPRES
C IN  LMTR   : DESCRIPTEUR D'UNE MATRICE
C IN  NEQ    : NOMBRE D'EQUATIONS
C IN  NBMODE : NOMBRE DE MODES
C IN  DDLEXC : TABLEAU DES DDL EXCLUS
C          = 0 SI EXCLUS
C          = 1 SI NON EXCLUS
C VAR VECPRO : TABLEAU DES VECTEURS PROPRES
C VAR RESUFR : TABLEAU DES GRANDEURS MODALES RANGEES SELON
C        'FREQ'            , 'OMEGA2'          , 'AMOR_REDUIT'     ,
C        'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,
C        'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,
C        'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,
C        'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ'
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
C
      INTEGER      IM, IEQ, LACC1, LDYNAM, IBID
      INTEGER VALI
      CHARACTER*1  TYPCST(2)
      CHARACTER*19 MATMOD
      REAL*8       RNORM, RX1, RX2, CONSTR(4), FR, AM ,ZERO
      COMPLEX*16   XX1, CMPL, XNORM,DCONJG,CZERO
      CHARACTER*24 NMATR(2),NDYNAM
      CHARACTER*24 VALK
C     ------------------------------------------------------------------
      DATA        TYPCST/'C','C'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      ZERO=0.D0
      CZERO=DCMPLX(ZERO,ZERO)

      IF ( NORM.EQ.'AVEC_CMP' .OR. NORM .EQ.'EUCL' ) THEN
C
C        --- NORMALISATION SUR LES DDL NON EXCLUS
         DO 2 IM = 1 , NBMODE
            RNORM = 0.0D0
            IF ( NORM .EQ. 'EUCL' ) THEN
               DO 4 IEQ = 1 , NEQ
                  XX1 = VECPRO(IEQ,IM) * DDLEXC(IEQ)
                  RNORM = RNORM + XX1*DCONJG(XX1)
 4             CONTINUE
               RNORM = SQRT( RNORM )
            ELSE
               DO 6 IEQ = 1 , NEQ
                  RX1 = ABS( VECPRO(IEQ,IM)*DDLEXC(IEQ) )
                  RNORM = MAX( RX1 , RNORM )
 6             CONTINUE
            ENDIF
            RX1 = 1.0D0 / RNORM
            COEF(IM) = RX1
            DO 8 IEQ = 1 , NEQ
               VECPRO(IEQ,IM) = VECPRO(IEQ,IM) * RX1
 8          CONTINUE
            IF ( PARA .EQ. 'OUI' ) THEN
               RX2 = RX1 * RX1
               RESUFR(IM,4)  = RESUFR(IM,4)  * RX2
               RESUFR(IM,5)  = RESUFR(IM,5)  * RX2
               RESUFR(IM,6)  = RESUFR(IM,6)  * RX2
            ENDIF
 2       CONTINUE
C
      ELSE IF ( NORM.EQ.'MASS_GENE' .OR.
     &          NORM.EQ.'RIGI_GENE') THEN
C
C        --- ON NORMALISE LA MASSE OU LA RAIDEUR GENERALISEE A 1 ---
C        --- DU PROBLEME GENERALISE ASSOCIE AU PROBLEME QUADRATIQUE ---
         MATMOD = ZK24(ZI(LMATR(1)+1))
         NMATR(1)=ZK24(ZI(LMATR(1)+1))
         NMATR(2)=ZK24(ZI(LMATR(2)+1))
         CALL WKVECT('&&WPNORM.XXXX_GENE_2','V V C',NEQ,LACC1)
         CALL MTDEFS('&&WPNORM.MATR.DYNAMIC',MATMOD,'V','C')
         CALL MTDSCR('&&WPNORM.MATR.DYNAM')
         NDYNAM='&&WPNORM.MATR.DYNAM'//'.&INT'
         CALL JEVEUO(NDYNAM,'E',LDYNAM)
         IF ( NORM.EQ.'MASS_GENE' ) THEN
            CONSTR(3) = 1.D0
            CONSTR(4) = 0.D0
         ELSEIF( NORM.EQ.'RIGI_GENE') THEN
            CONSTR(1) = -1.D0
            CONSTR(2) =  0.D0
         ENDIF
         DO 30 IM = 1, NBMODE
            FR = SQRT( RESUFR(IM,2) )
            AM = RESUFR(IM,3)
            AM = -ABS( AM*FR ) / SQRT( 1.0D0 - AM*AM )
            IF ( NORM.EQ.'MASS_GENE' ) THEN
               CONSTR(1) = 2.D0*AM
               CONSTR(2) = 2.D0*FR
            ELSEIF( NORM.EQ.'RIGI_GENE') THEN
               CMPL = DCMPLX(AM,FR)
               CMPL = CMPL * CMPL
               CONSTR(3) = DBLE(CMPL)
               CONSTR(4) = DIMAG(CMPL)
            ENDIF
            CALL MTCMBL(2,TYPCST,CONSTR,NMATR,NDYNAM,' ',' ')
            CALL MCMULT('ZERO',LDYNAM,VECPRO(1,IM),'C',ZC(LACC1),1)
            XNORM = CZERO
            DO 31 IEQ = 1, NEQ
               XNORM = XNORM + VECPRO(IEQ,IM) * ZC(LACC1+IEQ-1)
   31      CONTINUE
           XNORM = 1.D0 / SQRT(XNORM)
           COEF(IM) = XNORM
           DO 32 IEQ = 1, NEQ
              VECPRO(IEQ,IM) = VECPRO(IEQ,IM) * XNORM
   32      CONTINUE
            IF ( PARA .EQ. 'OUI' ) THEN
               XNORM = XNORM * XNORM
               RESUFR(IM,4) = RESUFR(IM,4) * XNORM
               RESUFR(IM,5) = RESUFR(IM,5) * XNORM
               RESUFR(IM,6) = RESUFR(IM,6) * XNORM
            ENDIF
   30    CONTINUE
         CALL JEDETC('V','&&WPNORM',1)
C
      ELSE
C
         VALK = NORM
         CALL U2MESG('F', 'ALGELINE4_96',1,VALK,0,0,0,0.D0)
C
      ENDIF
C
      CALL JEDEMA()
      END
