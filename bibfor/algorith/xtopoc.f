      SUBROUTINE XTOPOC(MODELE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/09/2010   AUTEUR MASSIN P.MASSIN 
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
C RESPONSABLE GENIAUT S.GENIAUT
C
      IMPLICIT NONE 
      CHARACTER*8   MODELE
C     
C ----------------------------------------------------------------------
C
C ROUTINE XFEM (METHODE XFEM - PREPARATION)
C
C AJOUTER � LA SD FISS_XFEM LES DONN�ES TOPOLOGIQUES CONCERNANT 
C LES FACETTES DE CONTACT
C      
C ----------------------------------------------------------------------
C
C
C  IN  MODELE : NOM DE L'OBJET MODELE	 
C
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      NBOUT,NBIN
      PARAMETER    (NBOUT=7, NBIN=5)
      CHARACTER*8  LPAOUT(NBOUT),LPAIN(NBIN)
      CHARACTER*19 LCHOUT(NBOUT),LCHIN(NBIN)
C
      INTEGER      JNOMA
      CHARACTER*19 LIGREL,CHGEOM
      CHARACTER*19 PINTER,AINTER,CFACE,FACLON,BASECO
      CHARACTER*19 LNNO,GRLNNO,LTNO,GRLTNO
      CHARACTER*19 GESCLA,GESCLO,GMAITR
      LOGICAL      DEBUG
      CHARACTER*16 OPTION
      INTEGER      IFMDBG,NIVDBG      
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFDBG('PRE_CALCUL',IFMDBG,NIVDBG)      
C
C --- INITIALISATIONS
C 
      LIGREL = MODELE//'.MODELE'
      CALL JEVEUO(MODELE//'.MODELE    .LGRF','L',JNOMA)
      CHGEOM = ZK8(JNOMA)//'.COORDO'
      IF (NIVDBG.GE.2) THEN
        DEBUG  = .TRUE.
      ELSE
        DEBUG  = .FALSE.
      ENDIF
      OPTION = 'TOPOFA'      
C
C --- INITIALISATION DES CHAMPS POUR CALCUL
C
      CALL INICAL(NBIN  ,LPAIN ,LCHIN ,
     &            NBOUT ,LPAOUT,LCHOUT) 
C
C --- RECUPERATION DES DONNEES XFEM
C
      LNNO   = MODELE(1:8)//'.LNNO'
      LTNO   = MODELE(1:8)//'.LTNO'
      GRLNNO = MODELE(1:8)//'.GRLNNO'
      GRLTNO = MODELE(1:8)//'.GRLTNO'
      PINTER = MODELE(1:8)//'.TOPOFAC.PI'
      AINTER = MODELE(1:8)//'.TOPOFAC.AI'
      CFACE  = MODELE(1:8)//'.TOPOFAC.CF'
      FACLON = MODELE(1:8)//'.TOPOFAC.LO'
      BASECO = MODELE(1:8)//'.TOPOFAC.BA'
      GESCLA = MODELE(1:8)//'.TOPOFAC.GE'
      GMAITR = MODELE(1:8)//'.TOPOFAC.GM'
      GESCLO = MODELE(1:8)//'.TOPOFAC.OE'
C       
C --- CREATION DES LISTES DES CHAMPS IN
C
      LPAIN(1)  = 'PGEOMER'
      LCHIN(1)  = CHGEOM
      LPAIN(2)  = 'PLSN'
      LCHIN(2)  = LNNO
      LPAIN(3)  = 'PLST'
      LCHIN(3)  = LTNO
      LPAIN(4)  = 'PGRADLN'
      LCHIN(4)  = GRLNNO
      LPAIN(5)  = 'PGRADLT'
      LCHIN(5)  = GRLTNO
C       
C --- CREATION DES LISTES DES CHAMPS OUT
C
      LPAOUT(1) = 'PPINTER'
      LCHOUT(1) = PINTER
      LPAOUT(2) = 'PAINTER'
      LCHOUT(2) = AINTER
      LPAOUT(3) = 'PCFACE'
      LCHOUT(3) = CFACE
      LPAOUT(4) = 'PLONCHA'
      LCHOUT(4) = FACLON
      LPAOUT(5) = 'PBASECO'
      LCHOUT(5) = BASECO
      LPAOUT(6) = 'PGESCLA'
      LCHOUT(6) = GESCLA
      LPAOUT(7) = 'PGMAITR'
      LCHOUT(7) = GMAITR
C
      IF (DEBUG) THEN
        CALL DBGCAL(OPTION,IFMDBG,
     &              NBIN  ,LPAIN ,LCHIN ,
     &              NBOUT ,LPAOUT,LCHOUT)
      ENDIF 
C  
      CALL CALCUL('C',OPTION,LIGREL,NBIN  ,LCHIN ,LPAIN,
     &                              NBOUT ,LCHOUT,LPAOUT,'G') 
C
      CALL COPISD('CHAMP_GD','G',LCHOUT(6),GESCLO)
  
      CALL JEDEMA()
      END
