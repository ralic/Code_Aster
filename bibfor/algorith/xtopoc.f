      SUBROUTINE XTOPOC(MODELE,FISS)
      IMPLICIT NONE 

      CHARACTER*8   MODELE,FISS

C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/10/2005   AUTEUR GENIAUT S.GENIAUT 
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
C          BUT : AJOUTER À LA SD FISS_XFEM LES DONNÉES 
C                TOPOLOGIQUES CONCERNANT LES FACETTES DE CONTACT 
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
      CHARACTER*8   LPAIN(4),LPAOUT(5)
      CHARACTER*19  LCHIN(4),LCHOUT(5)
      CHARACTER*19  LIGREL,CHGEOM,PINTER,AINTER,CFACE,LONCHA,BASECO

C ----------------------------------------------------------------------

      CALL JEMARQ()

      LIGREL = MODELE//'.MODELE'

C     RÉCUPÉRATION DE LA GÉOMETRIE
      CALL JEVEUO(MODELE//'.MODELE    .NOMA','L',JNOMA)
      CHGEOM = ZK8(JNOMA)//'.COORDO'

C     CRÉATION DU .TOPOFAC
C     --------------------

      PINTER=FISS//'.TOPOFAC.PINTER'
      AINTER=FISS//'.TOPOFAC.AINTER'
      CFACE =FISS//'.TOPOFAC.CFACE'
      LONCHA=FISS//'.TOPOFAC.LONCHAM'
      BASECO=FISS//'.TOPOFAC.BASECO'

      LPAIN(1) = 'PGEOMER'
      LCHIN(1) = CHGEOM
      LPAIN(2) = 'PLEVSET'
      LCHIN(2) = FISS//'.LNNO'
      LPAIN(3) = 'PGRADLN'
      LCHIN(3) = FISS//'.GRLNNO'
      LPAIN(4) = 'PGRADLT'
      LCHIN(4) = FISS//'.GRLTNO'

      LPAOUT(1) = 'PPINTER'
      LCHOUT(1) = PINTER
      LPAOUT(2) = 'PAINTER'
      LCHOUT(2) = AINTER
      LPAOUT(3) = 'PCFACE'
      LCHOUT(3) = CFACE
      LPAOUT(4) = 'PLONCHA'
      LCHOUT(4) = LONCHA
      LPAOUT(5) = 'PBASECO'
      LCHOUT(5) = BASECO

      CALL CALCUL('C','TOPOFA',LIGREL,4,LCHIN,LPAIN,5,LCHOUT,LPAOUT,'G')

C      CALL IMPRSD('CHAMP',PINTER,6,'PINTER')
C      CALL IMPRSD('CHAMP',AINTER,6,'AINTER')
C      CALL IMPRSD('CHAMP',CFACE ,6,'CFACE' )
C      CALL IMPRSD('CHAMP',LONCHA,6,'LONCHA')
C      CALL IMPRSD('CHAMP',BASECO,6,'BASECO')


      CALL JEDEMA()
      END
