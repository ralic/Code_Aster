      SUBROUTINE COPMAT(MATR,NUMDDL,MAT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C***********************************************************************
C 15/03/91    G.JACQUART AMV/P61 47 65 49 41
C***********************************************************************
C
C     FONCTION : COPIE MATR_ASSE DANS MATRICE PLEINE
C
C-----------------------------------------------------------------------
C    MATR   /I/ : NOM DE LA MATRICE
C    NUMDDL /I/ : NUMEROTATION SI MATRICE STOCKEE LIGNE DE CIEL
C    MAT   /O/ : VECTEUR CONTENANT LA MATONALE DE MATR
C-----------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
C
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
C
      CHARACTER*32  JEXNUM,JEXNOM
C
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      CHARACTER*8 KBID,MATR
      CHARACTER*14 NUMDDL
      REAL*8      MAT(*),PIJ
C
C-----------------------------------------------------------------------
      DATA KBID /'        '/
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      IF (NUMDDL(1:8).EQ.KBID) THEN

          CALL UTMESS('F','COPMAT',
     +                'NUMEROTATION ABSENTE '
     +                //' PROBLEME DANS LA MATRICE '//MATR )
C
      ELSE
        CALL JEVEUO(NUMDDL(1:8)//'      .SLCS.DESC','L',JDESC)
        NEQ    = ZI(JDESC)
        NBBLOC = ZI(JDESC+2)
        CALL JELIBE(NUMDDL(1:8)//'      .SLCS.DESC')
C
C
        CALL JEVEUO(NUMDDL(1:8)//'      .SLCS.ABLO','L',JABLO)
        CALL JEVEUO(NUMDDL(1:8)//'      .SLCS.ADIA','L',IADIA)
        CALL JEVEUO(NUMDDL(1:8)//'      .SLCS.HCOL','L',IHCOL)
        DO 20 IB=1,NBBLOC
          CALL JEVEUO(JEXNUM(MATR//'           .VALE',IB),'L',JBLOC)
          N1BLOC=ZI(JABLO+IB-1)+1
          N2BLOC=ZI(JABLO+IB)
C
C         BOUCLE SUR LES COLONNES DU BLOC
C
          DO 30 J=N1BLOC,N2BLOC
C
C         BOUCLE SUR LES LIGNES DANS LA COLONNE
C
            DO 30 I= (J-ZI(IHCOL+J-1)+1),J
              PIJ = ZR(JBLOC+ZI(IADIA+J-1)+I-J-1)
              MAT(I+ (J-1)*NEQ) = PIJ
              MAT(J+ (I-1)*NEQ) = PIJ
30        CONTINUE
          CALL JELIBE(JEXNUM(MATR//'           .VALE',IB))
20      CONTINUE
        CALL JELIBE(NUMDDL(1:8)//'      .SLCS.ABLO')
        CALL JELIBE(NUMDDL(1:8)//'      .SLCS.ADIA')
C
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
