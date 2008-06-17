      SUBROUTINE VEIMPD(MODELE,MATE  ,TEMPS ,VITINI,VITENT,
     &                  VECELZ)
C  
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/03/2008   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C 
      IMPLICIT NONE
      CHARACTER*(*) VECELZ
      CHARACTER*24  MODELE,MATE
      CHARACTER*24  VITINI,VITENT
      REAL*8        TEMPS
C 
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (CALCUL)
C
C CALCUL DES VECTEURS ELEMENTAIRES DES IMPEDANCES DE SOL
C      
C ----------------------------------------------------------------------
C
C      
C IN  MODELE : NOM DU MODELE
C IN  MATE   : CHAMP DE MATERIAU
C IN  TEMPS  : INSTANT DE CALCUL
C IN  VITINI : VITESSE INITIALE
C IN  VITENT : VITESSE APPUI
C OUT VECELE : VECTEURS ELEMENTAIRES
C      
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      NBOUT,NBIN
      PARAMETER    (NBOUT=1, NBIN=4)
      CHARACTER*8  LPAOUT(NBOUT),LPAIN(NBIN)
      CHARACTER*19 LCHOUT(NBOUT),LCHIN(NBIN)
C
      INTEGER      IBID
      CHARACTER*8  K8BID
      CHARACTER*19 VECELE
      CHARACTER*16 OPTION
      CHARACTER*24 CHGEOM,LIGRMO
      LOGICAL      DEBUG
      INTEGER      JLVE,JNOMA
      INTEGER      IFMDBG,NIVDBG
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()  
      CALL INFDBG('PRE_CALCUL',IFMDBG,NIVDBG)        
C
C --- INITIALISATIONS
C
      VECELE = VECELZ
      LIGRMO = MODELE(1:8)//'.MODELE'
      CALL JEVEUO(LIGRMO(1:19)//'.LGRF','L',JNOMA)
      CHGEOM = ZK8(JNOMA)//'.COORDO'
      OPTION = 'IMPE_ABSO'
      IF (NIVDBG.GE.2) THEN
        DEBUG  = .TRUE.
      ELSE
        DEBUG  = .FALSE.
      ENDIF
C
C --- INITIALISATION DES CHAMPS POUR CALCUL
C
      CALL INICAL(NBIN  ,LPAIN ,LCHIN ,
     &            NBOUT ,LPAOUT,LCHOUT)                           
C
C --- CHAMPS D'ENTREE
C    
      LPAIN(1) = 'PGEOMER'
      LCHIN(1) = CHGEOM
      LPAIN(2) = 'PMATERC'
      LCHIN(2) = MATE
      LPAIN(3) = 'PVITPLU'
      LCHIN(3) = VITINI
      LPAIN(4) = 'PVITENT'
      LCHIN(4) = VITENT
C
C --- CHAMPS DE SORTIE
C
      LPAOUT(1) = 'PVECTUR'
      LCHOUT(1) = VECELE
C
C --- ALLOCATION DU VECT_ELEM RESULTAT :
C     
      CALL JEDETR(VECELE//'.RELR')
C
C --- CALCUL
C
      CALL CORICH('E',LCHOUT(1),-1,IBID)
      CALL CALCUL('S',OPTION,LIGRMO,NBIN ,LCHIN ,LPAIN ,
     &                              NBOUT,LCHOUT,LPAOUT,'V')
C
      IF (DEBUG) THEN
        CALL DBGCAL(OPTION,IFMDBG,
     &              NBIN  ,LPAIN ,LCHIN ,
     &              NBOUT ,LPAOUT,LCHOUT)
      ENDIF   
C
      CALL REAJRE(VECELE,LCHOUT(1),'V')
C
      CALL JEDEMA()
      END
