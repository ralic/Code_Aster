      SUBROUTINE AJLAGR ( RIGID , MASSE , MASINV )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)       RIGID , MASSE , MASINV
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/09/2012   AUTEUR LADIER A.LADIER 
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
      INTEGER      NEQ, HBLOC, NBBLOC, MXDDL
      REAL*8       ZERO, UN, MMAX, KMAX, COEF, LCOEF(2)
      CHARACTER*1  TYPMAT, TYPMA2, TYPCST(2)
      CHARACTER*8  RAID, MASS, MASI, NOMDDL, MATRER
      CHARACTER*14 NUMDDL, NU2DDL
      CHARACTER*19 RIGI2,MASS2,MATRE2,MASIN2
      COMPLEX*16    CUN, CMMAX, CKMAX, CCOEF
      CHARACTER*24 NMAT(4),NMATI
      CHARACTER*24 VALK(2)
C     ------------------------------------------------------------------
C
C-----------------------------------------------------------------------
      INTEGER I ,IMATI ,IMATM ,IMATR ,IMTRER ,J ,JCONL 
      INTEGER JMASS ,JRAID ,JREFA1 ,JREFA2 ,JSMDE ,LDDL ,NBMAT 

C-----------------------------------------------------------------------
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
        VALK (1) = TYPMAT
        VALK (2) = TYPMA2
        CALL U2MESG('F','ALGORITH14_77',2,VALK,0,0,0,0.D0)
      ENDIF
      IF ( NU2DDL .NE. NUMDDL ) THEN
        VALK (1) = NUMDDL
        VALK (2) = NU2DDL
        CALL U2MESG('F','ALGORITH14_78',2,VALK,0,0,0,0.D0)
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
      CALL MTCMBL(NBMAT,TYPCST,LCOEF,NMAT,NMATI,NOMDDL,' ','ELIM=')
C
      NOMDDL = 'LAGR    '
      MXDDL  = 1
      CALL WKVECT('&&AJLAGR.LAGR','V V I',NEQ*MXDDL,LDDL)
      CALL PTEDDL('NUME_DDL', NUMDDL , MXDDL , NOMDDL , NEQ , ZI(LDDL))
      CALL JEVEUO(MASI//'           .CONL','E',JCONL)
      IF ( TYPMAT .EQ. 'R' ) THEN
        DO 30 I = 0 , NEQ-1
           IF ( ZI(LDDL+I) .NE. 0 ) THEN
              ZR(JCONL+I) = MMAX
           ELSE
              ZR(JCONL+I) = UN
           ENDIF
 30     CONTINUE
      ELSE
        DO 32 I = 0 , NEQ-1
           IF ( ZI(LDDL+I) .NE. 0 ) THEN
              ZC(JCONL+I) = CMMAX
           ELSE
              ZC(JCONL+I) = CUN
           ENDIF
 32     CONTINUE
      ENDIF
C
C
C --- MENAGE
C
      CALL JEDETR('&&AJLAGR.LAGR')
      CALL DETRSD('MATR_ASSE','&&RIGIL') 
C
      CALL JEDEMA()
      END
