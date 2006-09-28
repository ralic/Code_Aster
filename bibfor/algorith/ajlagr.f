      SUBROUTINE AJLAGR ( RIGID , MASSE , MASINV )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       RIGID , MASSE , MASINV
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     AJOUTE LES "LAGRANGE" DANS LA MATRICE DE MASSE A PARTIR DES
C     DONNEES STOCKEES DANS LA MATRICE DE RAIDEUR.
C
C IN  : RIGID  : NOM DE LA MATRICE DE RAIDEUR
C IN  : MASSE  : NOM DE LA MATRICE DE MASSE
C OUT : MASINV : NOM DE LA MATRICE DE MASSE AVEC LES LAGRANGES
C-----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32      JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER      NEQ, HBLOC, NBBLOC, MXDDL, STOCKA
      REAL*8       ZERO, UN, MMAX, KMAX, COEF, LCOEF(2)
      CHARACTER*1  TYPMAT, TYPMA2, TYPCST(2)
      CHARACTER*8  RAID, MASS, MASI, NOMDDL, MATRER
      CHARACTER*14 NUMDDL, NU2DDL
      CHARACTER*19 RIGI2,MASS2,MATRE2,MASIN2
      COMPLEX*16   CZERO, CUN, CMMAX, CKMAX, CCOEF
      CHARACTER*24 NMAT(4),NMATI
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      ZERO   = 0.D0
      UN     = 1.D0
      CUN    = DCMPLX(UN,ZERO)
C
      RAID = RIGID
      CALL MTDSCR (  RIGID )
      RIGI2=RIGID
      CALL JEVEUO(RIGI2//'.&INT','E', IMATR )
      IF ( ZI(IMATR+3) .EQ. 1 ) THEN
         TYPMAT = 'R'
      ELSEIF ( ZI(IMATR+3) .EQ. 2 ) THEN
         TYPMAT = 'C'
      ELSE
         CALL U2MESS('F','ALGORITH_3')
      ENDIF
      CALL JEVEUO(RAID//'           .REFA','L',JREFA1)
      NUMDDL = ZK24(JREFA1-1+2)(1:14)
C
      MASS = MASSE
      CALL MTDSCR (  MASSE )
      MASS2=MASSE
      NMAT(2)=MASS2//'.&INT'
      CALL JEVEUO(NMAT(2),'E', IMATM )
      IF ( ZI(IMATM+3) .EQ. 1 ) THEN
         TYPMA2 = 'R'
      ELSEIF ( ZI(IMATM+3) .EQ. 2 ) THEN
         TYPMA2 = 'C'
      ELSE
         CALL U2MESS('F','ALGORITH_3')
      ENDIF
      CALL JEVEUO(MASS//'           .REFA','L',JREFA2)
      NU2DDL = ZK24(JREFA2-1+2)(1:14)
C
      IF ( TYPMA2 .NE. TYPMAT ) THEN
        CALL UTDEBM('F','AJLAGR','LES TYPES DES DEUX MATRICES SONT '//
     &                           'DIFFERENTS')
        CALL UTIMPK('L','TYPE DE LA MATRICE DE RAIDEUR : ',1,TYPMAT)
        CALL UTIMPK('L','TYPE DE LA MATRICE DE MASSE   : ',1,TYPMA2)
        CALL UTFINM( )
      ENDIF
      IF ( NU2DDL .NE. NUMDDL ) THEN
        CALL UTDEBM('F','AJLAGR','LES NUMEROTATIONS DES DEUX MATRICES '
     &                         //'SONT DIFFERENTES')
        CALL UTIMPK('L','NUMEROTATION MATRICE DE RAIDEUR : ',1,NUMDDL)
        CALL UTIMPK('L','NUMEROTATION MATRICE DE MASSE   : ',1,NU2DDL)
        CALL UTFINM( )
      ENDIF
C
C
      CALL JEVEUO(NUMDDL//'.SMOS.SMDE','L',JSMDE)
      NEQ    = ZI(JSMDE-1+1)
      HBLOC  = ZI(JSMDE-1+2)
      NBBLOC = ZI(JSMDE-1+3)
      CALL ASSERT(NBBLOC.EQ.1)
C
C     --- DETERMINATION DU COEFFICIENT DE CONDITIONNEMENT ---
      IF ( TYPMAT .EQ. 'R' ) THEN
        MMAX = ZERO
        KMAX = ZERO
        DO 10 I = 1 , NBBLOC
           CALL JEVEUO(JEXNUM(RAID//'           .VALM',I),'L',JRAID)
           CALL JEVEUO(JEXNUM(MASS//'           .VALM',I),'L',JMASS)
           DO 12 J = 0 , HBLOC-1
              MMAX = MAX(ZR(JMASS+J),MMAX)
              KMAX = MAX(ZR(JRAID+J),KMAX)
 12        CONTINUE
           CALL JELIBE(JEXNUM(MASS//'           .VALM',I))
           CALL JELIBE(JEXNUM(RAID//'           .VALM',I))
 10     CONTINUE
        COEF = MMAX / KMAX
      ELSE
        CMMAX = ZERO
        CKMAX = ZERO
        DO 20 I = 1 , NBBLOC
          CALL JEVEUO(JEXNUM(RAID//'           .VALM',I),'L',JRAID)
          CALL JEVEUO(JEXNUM(MASS//'           .VALM',I),'L',JMASS)
          DO 22 J = 0 , HBLOC-1
            CMMAX = MAX(ABS(ZC(JMASS+J)),ABS(CMMAX))
            CKMAX = MAX(ABS(ZC(JRAID+J)),ABS(CKMAX))
 22       CONTINUE
          CALL JELIBE(JEXNUM(MASS//'           .VALM',I))
          CALL JELIBE(JEXNUM(RAID//'           .VALM',I))
 20     CONTINUE
        CCOEF = CMMAX / CKMAX
      ENDIF
C
      MATRER = '&&RIGIL'
      CALL MTDEFS ( MATRER , RIGID , 'V' , TYPMAT )
      CALL MTDSCR (  MATRER )
      MATRE2=MATRER
      NMAT(1)=MATRE2//'.&INT'
      CALL JEVEUO(NMAT(1),'E', IMTRER )
C
      CALL MTCMBI ( TYPMAT , IMATR , COEF , CCOEF , IMTRER )
C
      MASI = MASINV
      CALL MTDEFS ( MASINV , RIGID , 'V' , TYPMAT )
      CALL MTDSCR (  MASINV )
      MASIN2=MASINV
      NMATI=MASIN2//'.&INT'
      CALL JEVEUO(NMATI,'E',IMATI)
C
      NBMAT = 2
      NOMDDL = ' '
      LCOEF(1) = 1.D0
      LCOEF(2) = 1.D0
      TYPCST(1) = TYPMAT
      TYPCST(2) = TYPMAT
      CALL MTCMBL(NBMAT,TYPCST,LCOEF,NMAT,NMATI,NOMDDL,' ')
C
      NOMDDL = 'LAGR    '
      MXDDL  = 1
      CALL WKVECT('&&AJLAGR.LAGR','V V I',NEQ*MXDDL,LDDL)
      CALL PTEDDL('NUME_DDL', NUMDDL , MXDDL , NOMDDL , NEQ , ZI(LDDL))
      CALL JEVEUO(MASI//'           .CONL','E',JCONL)
      IF ( TYPMAT .EQ. 'R' ) THEN
        DO 30 I = 1 , NEQ-1
           IF ( ZI(LDDL+I) .NE. 0 ) THEN
              ZR(JCONL+I) = MMAX
           ELSE
              ZR(JCONL+I) = UN
           ENDIF
 30     CONTINUE
      ELSE
        DO 32 I = 1 , NEQ-1
           IF ( ZI(LDDL+I) .NE. 0 ) THEN
              ZC(JCONL+I) = CMMAX
           ELSE
              ZC(JCONL+I) = CUN
           ENDIF
 32     CONTINUE
      ENDIF
C
      CALL JEDETR('&&AJLAGR.LAGR')
      CALL JEDETC('V','&&RIGIL',1)
C
      CALL JEDEMA()
      END
