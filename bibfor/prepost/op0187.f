      SUBROUTINE OP0187(IER)
      IMPLICIT NONE
      INTEGER IER
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 19/06/2007   AUTEUR PELLET J.PELLET 
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
C     =================================================================
C                      OPERATEUR POST_MAIL_XFEM
C                      ------------------------
C     BUT : GENERER UN MAILLAGE DESTINE UNIQUEMENT AU POST-TRAITEMENT
C           DES ELEMENTS XFEM, ET METTANT EN EVIDENCE LES SOUS-ELEMENTS
C           DES MAILLES FISSUREES
C     =================================================================
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32 JEXNUM,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C     ------------------------------------------------------------------
      INTEGER      IBID,IRET,NSETOT,NNNTOT,NCOTOT,NBNOC,NBMAC,IFM,NIV
      CHARACTER*8  MAXFEM,MO,MALINI,K8B,NOMRES
      CHARACTER*16 K16B
      CHARACTER*19 K19B
      CHARACTER*24 MAILX,MAILC,MAILN,LISTNO,K24B
C
      CALL JEMARQ()
      CALL INFMAJ()
      CALL INFNIV(IFM,NIV)
C
C     ------------------------------------------------------------------
C     1. RECUPERATION DES CONCEPTS UTILISATEURS
C     ------------------------------------------------------------------
C
      IF (NIV.GT.1) WRITE(IFM,*)'1. XPOINI'
      CALL XPOINI(MAXFEM,MO,MALINI,K24B,IBID,K8B,K8B)
C
C     ------------------------------------------------------------------
C     2. SEPARATION DES MAILLES DE MALINI EN 3 GROUPES
C              - MAILN : MAILLES NON AFFECTEES D'UN MODELE
C              - MAILC : MAILLES NON SOUS-DECOUPEES (CLASSIQUE)
C              - MAILX : MAILLES SOUS-DECOUPEES (X-FEM)
C     ------------------------------------------------------------------
C
      IF (NIV.GT.1) WRITE(IFM,*)'2. XPOSEP'
      MAILN = '&&OP0187.MAILN'
      MAILC = '&&OP0187.MAILC'
      MAILX = '&&OP0187.MAILX'
      CALL XPOSEP(MO,MALINI,MAILN,MAILC,MAILX,NSETOT,NNNTOT,NCOTOT)

      IF (NIV.GT.1) THEN
        WRITE(IFM,*)'NOMBRE DE NOUVELLES MAILLES A CREER',NSETOT
        WRITE(IFM,*)'NOMBRE DE NOUVEAUX NOEUDS A CREER',NNNTOT
      ENDIF

C     ------------------------------------------------------------------
C     3. DIMENSIONNEMENT DES OBJETS DE MAXFEM
C     ------------------------------------------------------------------

      IF (NIV.GT.1) WRITE(IFM,*)'3. XPODIM'
      LISTNO = '&&OP0187.LISTNO'
      CALL XPODIM(MALINI,MAILC,MAILX,NSETOT,NNNTOT,NCOTOT,LISTNO,
     &            K19B,K19B,K19B,K19B,K19B,IBID,K8B,NBNOC,NBMAC,MAXFEM)

C     ------------------------------------------------------------------
C     4. TRAITEMENT DES MAILLES DE MAILC
C            LES MAILLES DE MAILC ET LES NOEUDS ASSOCIÉS SONT COPIES
C            DANS MAXFEM A L'IDENTIQUE
C     ------------------------------------------------------------------

      IF (NIV.GT.1) WRITE(IFM,*)'4. XPOMAC'
      CALL XPOMAC(MALINI,MAILC,LISTNO,NBNOC,NBMAC,MAXFEM,
     &                                    K19B,K19B,K19B,K19B)

C     ------------------------------------------------------------------
C     5. TRAITEMENT DES MAILLES DE MAILX
C     ------------------------------------------------------------------

      IF (NIV.GT.1) WRITE(IFM,*)'5. XPOMAX'
      CALL XPOMAX(MO,MALINI,MAILX,NBNOC,NBMAC,MAXFEM,
     &                                    K19B,K19B,K19B,K19B)

      CALL JEEXIN(MAILN,IRET)
      IF (IRET.NE.0) CALL JEDETR(MAILN)

      IF (NIV.GT.1) WRITE(IFM,*)'FIN DE POST_MAIL_XFEM'

      CALL TITRE()
C
C --- CARACTERISTIQUES GEOMETRIQUES :
C     -----------------------------
      CALL GETRES ( NOMRES, K16B, K16B )
      CALL CARGEO ( NOMRES )

      CALL JEDEMA()

      END
