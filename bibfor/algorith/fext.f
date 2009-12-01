      SUBROUTINE FEXT ( T, NEQ, NVECT, LIAD, LIFO, F )
      IMPLICIT   NONE
      INTEGER           NEQ, NVECT, LIAD(*)
      REAL*8            T, F(*)
      CHARACTER*24      LIFO(*)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
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
C
C  CALCUL DU VECTEUR FEXT: FEXT = SOMME  GI(T)*FI(X)
C
C  INPUT:
C        T        : INSTANT DE CALCUL
C        NEQ      : NOMBRE D'EQUATIONS (D.D.L. ACTIFS)
C        NVECT    : NOMBRE DE VECTEURS CHARGEMENT
C        LIAD     : LISTE DES ADRESSES DES VECTEURS CHARGEMENT (NVECT)
C        LIFO     : LISTE DES NOMS DES FONCTIONS EVOLUTION (NVECT)
C
C  OUTPUT:
C        F        : VECTEUR FORCE EXTERIEURE (NEQ)
C
C-----------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      I, IER
      REAL*8       ZERO, ALPHA
      CHARACTER*8  NOMPAR
C     ------------------------------------------------------------------
C
      NOMPAR = 'INST'
      ZERO   = 0.D0
      CALL R8INIR ( NEQ , ZERO , F , 1 )
      DO 10 I = 1 , NVECT
         CALL FOINTE ( 'F ', LIFO(I), 1, NOMPAR, T, ALPHA, IER )
         CALL DAXPY ( NEQ , ALPHA , ZR(LIAD(I)) , 1 , F , 1 )
 10   CONTINUE
      END
