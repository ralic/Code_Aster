      SUBROUTINE XTOPOI(MODELE,FISS)
      IMPLICIT NONE 

      CHARACTER*8   MODELE,FISS

C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/07/2005   AUTEUR GENIAUT S.GENIAUT 
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
C
C          BUT : AJOUTER À LA SD FISS_XFEM LES DONNÉES 
C                TOPOLOGIQUES CONCERNANT LA DÉCOUPE DES ÉLÉMENTS
C                POUR L'INTÉGRATION 
C                    
C
C  IN         MODELE    : NOM DE L'OBJET MODELE	 
C  IN/OUT     FISS      : NOM DE LA SD FISS_XFEM
C
C
C     ------------------------------------------------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       JNOMA
      CHARACTER*8   LPAIN(2),LPAOUT(5)
      CHARACTER*19  LCHIN(2),LCHOUT(5)
      CHARACTER*19  LIGREL,CHGEOM,PINTTO,CNSETO,HEAVTO,LONCHA,CRITER

C ----------------------------------------------------------------------

      CALL JEMARQ()

      LIGREL = MODELE//'.MODELE'

C     RÉCUPÉRATION DE LA GÉOMETRIE
      CALL JEVEUO(MODELE//'.MODELE    .NOMA','L',JNOMA)
      CHGEOM = ZK8(JNOMA)//'.COORDO'

C     1) CRÉATION DU .TOPOSE
C     -----------------------

      PINTTO=FISS//'.TOPOSE.PINTTO'
      CNSETO=FISS//'.TOPOSE.CNSETO'
      HEAVTO=FISS//'.TOPOSE.HEAVTO'
      LONCHA=FISS//'.TOPOSE.LONCHAM'
      CRITER=FISS//'.TOPOSE.CRITER'

      LPAIN(1) = 'PGEOMER'
      LCHIN(1) = CHGEOM
      LPAIN(2) = 'PLEVSET'
      LCHIN(2) = FISS//'.LNNO'

      LPAOUT(1) = 'PPINTTO'
      LCHOUT(1) = PINTTO
      LPAOUT(2) = 'PCNSETO'
      LCHOUT(2) = CNSETO
      LPAOUT(3) = 'PHEAVTO'
      LCHOUT(3) = HEAVTO
      LPAOUT(4) = 'PLONCHA'
      LCHOUT(4) = LONCHA
      LPAOUT(5) = 'PCRITER'
      LCHOUT(5) = CRITER

      CALL CALCUL('C','TOPOSE',LIGREL,2,LCHIN,LPAIN,5,LCHOUT,LPAOUT,'G')

      CALL JEDEMA()
      END
