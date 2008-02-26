      SUBROUTINE NMREFE(MODELE,COMPOR,NUMEDD,MATE  ,CARELE,
     &                  VALMOI,PARCON,CNVFRE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/02/2008   AUTEUR FLEJOU J-L.FLEJOU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MABBAS M.ABBAS
C
      IMPLICIT NONE
      REAL*8       PARCON(8)
      CHARACTER*19 CNVFRE
      CHARACTER*24 MODELE
      CHARACTER*24 COMPOR
      CHARACTER*24 NUMEDD
      CHARACTER*24 MATE
      CHARACTER*24 CARELE
      CHARACTER*24 VALMOI(8)
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL)
C
C CALCUL DE LA CARTE POUR RESI_RELA_REFE
C      
C ----------------------------------------------------------------------
C
C
C IN  MODELE : MODELE MECANIQUE
C IN  COMPOR : CARTE COMPORTEMENT
C IN  NUMEDD : NOM DE LA NUMEROTATION MECANIQUE
C IN  MATE   : NOM DU CHAMP DE MATERIAU
C IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
C IN  VALMOI : ETAT EN T-
C IN  PARCON : PARAMETRES DU CRITERE DE CONVERGENCE REFERENCE
C                     1 : SIGM_REFE
C                     2 : EPSI_REFE
C                     3 : FLUX_THER_REFE
C                     4 : FLUX_HYD1_REFE
C                     5 : FLUX_HYD2_REFE
C                     6 : VARI_REFE
C                     7 : EFFORT (FORC_REFE)
C                     8 : MOMENT (FORC_REFE)
C OUT CNVFRE : FORCE DE REFERENCE POUR CONVERGENCE EN REFERENCE
C
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------------------
C
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX -----------------------------
C
      INTEGER      NBOUT,NBIN
      PARAMETER    (NBOUT=1, NBIN=16)
      CHARACTER*8  LPAOUT(NBOUT),LPAIN(NBIN)
      CHARACTER*19 LCHOUT(NBOUT),LCHIN(NBIN)
C
      INTEGER      NBSIG
      PARAMETER    (NBSIG=8)
      CHARACTER*8  SIGERE(NBSIG)      
C
      LOGICAL      LBID
      INTEGER      IBID,JLVE
      COMPLEX*16   CBID
      CHARACTER*8  K8BID
      CHARACTER*19 LIGRMO,VEREFE,CARTE
      CHARACTER*24 CHGEOM,LVEDIP,K24BID
      CHARACTER*24 CHCARA(15),DEPMOI
      CHARACTER*19 PINTTO,CNSETO,HEAVTO,LONCHA
      LOGICAL      DEBUG
      INTEGER      IFMDBG,NIVDBG       
      CHARACTER*16 OPTION
C      
      DATA  SIGERE / 'SIGM','EPSI','FTHERM','FHYDR1','FHYDR2','VARI',
     &               'EFFORT','MOMENT' /
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('PRE_CALCUL',IFMDBG,NIVDBG)      
C
C --- INITIALISATIONS
C
      CALL DESAGG(VALMOI,DEPMOI,K24BID,K24BID,K24BID,
     &            K24BID,K24BID,K24BID,K24BID)
      CARTE  = '&&NMREFE.SIGERE'
      VEREFE = '&&NMREFE.VEREFE'
      LVEDIP = '&&VEREFE.LISTE_RESU'
      LIGRMO = MODELE(1:8) // '.MODELE'
      IF (NIVDBG.GE.2) THEN
        DEBUG  = .TRUE.
      ELSE
        DEBUG  = .FALSE.
      ENDIF 
      OPTION = 'REFE_FORC_NODA'
C
C --- CREATION CARTE DES VALEURS DE REFRENCES
C
      CALL MECACT('V',CARTE,'MODELE',LIGRMO,'PREC',NBSIG, SIGERE,
     &                IBID, PARCON, CBID, K8BID)
C
C --- CARTE DE LA GEOMETRIE
C
      CALL MEGEOM(MODELE,' ', LBID  ,CHGEOM)
C
C --- CARTE POUR LES CARA. ELEM.
C      
      CALL MECARA(CARELE(1:8),LBID  ,CHCARA)      
C
C --- INITIALISATION DES CHAMPS POUR CALCUL
C
      CALL INICAL(NBIN  ,LPAIN ,LCHIN ,
     &            NBOUT ,LPAOUT,LCHOUT)      
C
C --- RECUPERATION DES DONNEES XFEM (TOPOSE)
C
      PINTTO = MODELE(1:8)//'.TOPOSE.PIN'
      CNSETO = MODELE(1:8)//'.TOPOSE.CNS'
      HEAVTO = MODELE(1:8)//'.TOPOSE.HEA'
      LONCHA = MODELE(1:8)//'.TOPOSE.LON'
C       
C --- CREATION DES LISTES DES CHAMPS IN
C            
      LPAIN(1)  = 'PGEOMER'
      LCHIN(1)  =  CHGEOM
      LPAIN(2)  = 'PREFCO'
      LCHIN(2)  =  CARTE
      LPAIN(3)  = 'PCAORIE'
      LCHIN(3)  = CHCARA(1)
      LPAIN(4)  = 'PCOMPOR'
      LCHIN(4)  = COMPOR
      LPAIN(5)  = 'PMATERC'
      LCHIN(5)  = MATE
      LPAIN(6)  = 'PDEPLMR'
      LCHIN(6)  = DEPMOI
      LPAIN(7)  = 'PCACOQU'
      LCHIN(7)  = CHCARA(7)
      LPAIN(8)  = 'PCAGEPO'
      LCHIN(8)  = CHCARA(5)
      LPAIN(9)  = 'PNBSP_I'
      LCHIN(9)  = CHCARA(1) (1:8)//'.CANBSP'
      LPAIN(10) = 'PPINTTO'
      LCHIN(10) = PINTTO
      LPAIN(11) = 'PHEAVTO'
      LCHIN(11) = HEAVTO
      LPAIN(12) = 'PLONCHA'
      LCHIN(12) = LONCHA
      LPAIN(13) = 'PCNSETO'
      LCHIN(13) = CNSETO
      LPAIN(14) = 'PBASLOR'
      LCHIN(14) = MODELE(1:8)//'.BASLOC'
      LPAIN(15) = 'PLSN'
      LCHIN(15) = MODELE(1:8)//'.LNNO'
      LPAIN(16) = 'PLST'
      LCHIN(16) = MODELE(1:8)//'.LTNO'
C       
C --- CREATION DES LISTES DES CHAMPS OUT
C
      LPAOUT(1) = 'PVECTUR'
      LCHOUT(1) =  VEREFE
C      
C --- PREPARATION DES VECT_ELEM
C
      CALL DETRSD('VECT_ELEM',LVEDIP(1:8))
      CALL MEMARE('V',LVEDIP(1:8),MODELE(1:8),' ',' ','CHAR_MECA')
      CALL WKVECT(LVEDIP,'V V K24',1,JLVE)        
C
C --- APPEL A CALCUL
C      
      CALL CALCUL('S',OPTION,LIGRMO,NBIN ,LCHIN ,LPAIN ,
     &                       NBOUT,LCHOUT,LPAOUT,'V')
C
      IF (DEBUG) THEN
        CALL DBGCAL(OPTION,IFMDBG,
     &              NBIN  ,LPAIN ,LCHIN ,
     &              NBOUT ,LPAOUT,LCHOUT)
      ENDIF 
C      
      ZK24(JLVE) = LCHOUT(1)
C      
C --- ASSEMBLAGE DES VECT_ELEM
C
      CALL ASSMIV('V',CNVFRE,1,LVEDIP,1.D0,NUMEDD,' ','ZERO',1)
C
      CALL JEDEMA()
C
      END
