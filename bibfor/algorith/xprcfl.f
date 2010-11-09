      SUBROUTINE XPRCFL(MODEL,LCMIN)
      IMPLICIT NONE

      REAL*8         LCMIN
      CHARACTER*8    MODEL

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/11/2010   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE MASSIN P.MASSIN
C     ------------------------------------------------------------------
C
C       XPRCFL   : X-FEM PROPAGATION : CALCUL DES CONDITIONS CFL
C       ------     -     --                                  ---
C    CALCUL DE LA CONDITIONS CFL POUR LES PHASES DE REORTHOGONALISATION
C    ET REINITIALISATION DES LEVEL SETS, C'EST A DIRE LA LONGUEUR
C    MINIMALE DES ARETES DU MAILLAGE
C
C    ENTREE
C        MODEL   : NOM DU CONCEPT MODELE
C
C    SORTIE
C        LCMIN   : LONGUEUR CARACTERISTIQUE MINIMALE DU MAILLAGE
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
      CHARACTER*32    JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      INTEGER        IBID,IFM,NIV
      CHARACTER*8    LPAIN(1),LPAOUT(1)
      CHARACTER*19   CELLC
      CHARACTER*24   LIGREL,CHGEOM,LCHIN(1),LCHOUT(1)
      LOGICAL        EXIGEO

C-----------------------------------------------------------------------
C     DEBUT
C-----------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)

      CELLC='&&XPRCFL.CELLC'

      CALL MEGEOM(MODEL,' ',EXIGEO,CHGEOM)
      LIGREL=MODEL//'.MODELE'
      LPAIN(1)='PGEOMER'
      LCHIN(1)=CHGEOM
      LPAOUT(1)='PLONCAR'
      LCHOUT(1)=CELLC

      CALL CALCUL('S','CFL_XFEM',LIGREL,1,LCHIN,LPAIN,1,LCHOUT,LPAOUT,
     &            'V','OUI')

C   ON VA CHERCHER LE MINIMUM DE CELLC SUR LES ELEMENTS -->  LCMIN
      CALL MEMAX('MIN',CELLC,'X1',1,'X1',LCMIN,0,IBID)
      CALL JEDETR(CELLC)

      WRITE(IFM,*)'   LONGUEUR DE LA PLUS PETITE ARETE DU MAILLAGE: ',
     &            LCMIN

C-----------------------------------------------------------------------
C     FIN
C-----------------------------------------------------------------------
      CALL JEDEMA()
      END
