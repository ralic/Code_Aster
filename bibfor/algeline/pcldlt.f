      SUBROUTINE PCLDLT(MATF,MAT,NIREMP,BAS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 24/05/2000   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT NONE
      CHARACTER*(*) MATF,MAT,BAS
C-----------------------------------------------------------------------
C  FONCTION  :
C     CREATION D'UNE MATRICE DE PRECONDITIONNEMENT MATF
C     PAR FACTORISATION LDLT PLUS OU MOINS COMPLETE DE LA MATRICE MAT
C     STOCKEE SOUS FORME MORSE.
C     ON PEUT CHOISIR LE DEGRE DE REMPLISSAGE : NIREMP

C-----------------------------------------------------------------------
C OUT K*  MATF   : NOM DE LA MATR_ASSE DE PRECONDITIONNEMENT
C IN  K*  MAT    : NOM DE LA MATR_ASSE A PRECONDITIONNER
C IN  I   NIREMP : NIVEAU DE REMPLISSAGE VOULU POUR MATF
C IN  K*  BAS    : NOM DE LA BASE SUR LAQUELLE ON CREE MATF 'G' OU 'V'
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      LOGICAL COMPLT
      CHARACTER*1 BASE
      INTEGER IRET,LREFE,JADIA,JHCOL,JDESC,NEQU,NCOEF,NBLC,IBID
      INTEGER LREFEF,JVALE,IDV,I,NIVEAU,NZMAX,JICPD,JICPLX,NIREMP
      INTEGER JICPCX,JADIA1,JHCOL1,IER,K,JADIAF,JHCOLF,JVALF,JVECT
      REAL*8 DNORM,EPSI
      CHARACTER*19 MATFAC,MATAS
      CHARACTER*1 TYSCA
      CHARACTER*14 NU,NUF
      CHARACTER*8 MA
C----------------------------------------------------------------------
C     DEBUT DES INSTRUCTIONS
      CALL JEMARQ()
      CALL JXVERI(' ',' ')

      MATAS = MAT
      MATFAC = MATF
      BASE = BAS
      MATAS = MAT



C     1. CALCUL DE : MA,NU,JADIA,JHCOL,JDESC
C         NEQU,NCOEF
C         + QQUES VERIFS
C     ------------------------------------------
      CALL JEVEUO(MATAS//'.REFA','L',LREFE)
      MA = ZK24(LREFE-1+1)
      NU = ZK24(LREFE-1+2)

      CALL JEEXIN(NU//'.SMOS.ADIA',IRET)
      IF (IRET.EQ.0) CALL UTMESS('F','PCLDLT',
     &                           'LA MATR_ASSE '//MATAS//' N"EST '//
     &                           'PAS STOCKEE "MORSE" LE GCPC'//
     &                           ' EST DONC IMPOSSIBLE.')

      CALL JEVEUO(NU//'.SMOS.ADIA','L',JADIA)
      CALL JEVEUO(NU//'.SMOS.HCOL','L',JHCOL)
      CALL JEVEUO(NU//'.SMOS.DESC','L',JDESC)
      NEQU = ZI(JDESC-1+1)
      NCOEF = ZI(JDESC-1+2)

      NBLC = ZI(JDESC-1+3)
      IF (NBLC.NE.1) CALL UTMESS('F','PCLDLT','CONFLIT UNE MATRICE'//
     &                         ' STOCKEE MORSE NE PEUT AVOIR QU"UN BLOC'
     &                           )

      CALL JELIRA(JEXNUM(MATAS//'.VALE',1),'TYPE',IBID,TYSCA)
      IF (TYSCA.EQ.'C') CALL UTMESS('F','PCLDLT',
     &                              ' LE PRECONDITIONNEMENT '//
     &            'LDLT_INC D"UNE MATRICE COMPLEXE N"EST PAS IMPLEMENTE'
     &                              )



C     1. CREATION DU NUME_DDL ASSOCIE A MATFAC :
C     ------------------------------------------
      CALL GCNCON('_',NUF)
      CALL COPISD('NUME_DDL',BASE,NU,NUF)


C     2. CREATION DE MATFAC.REFA
C     ---------------------------
      CALL JEDETR(MATFAC//'.REFA')
      CALL JECREO(MATFAC//'.REFA',BASE//' V K24 ')
      CALL JEECRA(MATFAC//'.REFA','DOCU',IBID,'DECT')
      CALL JEECRA(MATFAC//'.REFA','LONMAX',4,' ')
      CALL JEVEUO(MATFAC//'.REFA','E',LREFEF)
      ZK24(LREFEF-1+1) = MA
      ZK24(LREFEF-1+2) = NUF


C     2. CALCUL DE EPSI POUR PCFACT ET ALLOCATION DE .VTRAVAIL:
C     ---------------------------------------------------------
      CALL JEVEUO(JEXNUM(MATAS//'.VALE',1),'L',JVALE)
      CALL WKVECT('&&PCLDLT.VTRAVAIL','V V R',NEQU,IDV)
      DNORM = 0.D0
      DO 10 I = 1,NEQU
        DNORM = MAX(ABS(ZR(JVALE-1+ZI(JADIA-1+I))),DNORM)
   10 CONTINUE
      EPSI = 1.D-16*DNORM
      CALL JELIBE(JEXNUM(MATAS//'.VALE',1))


C     3. ON BOUCLE SUR PCSTRU JUSQU'A TROUVER LA TAILLE DE LA
C        FUTURE FACTORISEE :
C     ------------------------------------------------
      NZMAX = NCOEF
      CALL WKVECT('&&PCLDLT.ADIAF','V V I',NEQU+1,JADIA1)

      DO 7778,K=1,2*NIREMP+2
        CALL WKVECT('&&PCLDLT.ICPD','V V I',NEQU,JICPD)
        CALL WKVECT('&&PCLDLT.ICPLX','V V I',NEQU+1,JICPLX)
        CALL JEDETR('&&PCLDLT.HCOLF')
        CALL WKVECT('&&PCLDLT.HCOLF','V V I',2*NZMAX,JHCOL1)
        CALL WKVECT('&&PCLDLT.ICPCX','V V I',NZMAX,JICPCX)

        CALL PCSTRU(NEQU,ZI(JADIA),ZI(JHCOL),ZI(JADIA1),ZI(JHCOL1),
     &      ZI(JICPD),ZI(JICPCX),ZI(JICPLX),NIREMP,COMPLT,NZMAX,0,
     &            IER)

        CALL JEDETR('&&PCLDLT.ICPLX')
        CALL JEDETR('&&PCLDLT.ICPCX')
        CALL JEDETR('&&PCLDLT.ICPD')
        IF (IER.EQ.0) GO TO 7779
        NZMAX=IER
7778  CONTINUE
      CALL UTMESS('F','PLDLT','STOP 1:ERREUR PROGRAMATION.')
7779  CONTINUE


C     -- ON MET A JOUR NUF.ADIA ET NUF.HCOL  :
C     ------------------------------------------------
      CALL JEVEUO(NUF//'.SMOS.ADIA','E',JADIAF)
      DO 20,K = 1,NEQU
        ZI(JADIAF-1+K) = ZI(JADIA1-1+K)
   20 CONTINUE
      CALL JEDETR('&&PCLDLT.ADIAF')

      CALL JEDETR(NUF//'.SMOS.HCOL')
      CALL WKVECT(NUF//'.SMOS.HCOL',BASE//' V I',NZMAX,JHCOLF)
      DO 30,K = 1,NZMAX
        ZI(JHCOLF-1+K) = ZI(JHCOL1-1+K)
   30 CONTINUE
      CALL JEDETR('&&PCLDLT.HCOLF')



C     -- ON ALLOUE MATFAC.VALE :
C     ------------------------------------------------
      CALL JEDETR(MATFAC//'.VALE')
      CALL JECREC(MATFAC//'.VALE',BASE//' V '//TYSCA,'NU','DISPERSE',
     &            'CONSTANT',1)
      CALL JEECRA(MATFAC//'.VALE','LONMAX',NZMAX,' ')
      CALL JEECRA(MATFAC//'.VALE','DOCU',IBID,'MS')
      CALL JECROC(JEXNUM(MATFAC//'.VALE',1))


C     -- ON INJECTE MATAS.VALE DANS MATFAC.VALE :
C     ------------------------------------------------
      CALL JEVEUO(JEXNUM(MATAS//'.VALE',1),'L',JVALE)
      CALL JEVEUO(JEXNUM(MATFAC//'.VALE',1),'E',JVALF)
      CALL PCCOEF(NEQU,ZI(JADIA),ZI(JHCOL),ZR(JVALE),ZI(JADIAF),
     &            ZI(JHCOLF),ZR(JVALF),ZR(IDV))
      CALL JELIBE(JEXNUM(MATAS//'.VALE',1))


C     -- ON FACTORISE MATFAC.VALE :
C     ------------------------------------------------
      CALL WKVECT('&&PCLDLT.VECT','V V R',NEQU,JVECT)
      CALL PCFACT(MATAS,NEQU,ZI(JADIAF),ZI(JHCOLF),ZR(JVALF),ZR(JVALF),
     &            ZR(JVECT),EPSI)
      CALL JEDETR('&&PCLDLT.VECT')

      CALL JEDETR('&&PCLDLT.VTRAVAIL')
      CALL JXVERI(' ',' ')
      CALL JEDEMA()
      END
