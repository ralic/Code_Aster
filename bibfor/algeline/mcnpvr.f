      SUBROUTINE MCNPVR ( CUMUL,NOMMAT,ADIA,HCOL,ABLO,NEQ,NBBLOC,
     +                                                 VECT,XSOL,NBVECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       CUMUL
      INTEGER                   ADIA(*),HCOL(*),ABLO(*),NEQ,NBBLOC,NBVEC
      CHARACTER*(*)            NOMMAT
      COMPLEX*16                   VECT(NEQ,NBVECT), XSOL(NEQ,NBVECT)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/01/97   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C             MULTIPLICATION MATRICE NON- SYMETRIQUE PAR N VECTEURS
C         XSOL(1..NEQ,1..NBVECT) = MATRICE  * VECT(1..NEQ,1..NBVECT)
C     ------------------------------------------------------------------
C     VERSION : LES ENTITES SONT REELLES
C             : LA MATRICE EST SYMETRIQUE STOCKEE PROFIL PAR BLOC
C     ------------------------------------------------------------------
C
C
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*6      PGC, PGCANC
      COMMON  /NOMAJE/ PGC
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       IA, IV, LM , LONG
      REAL*8        R8VAL
      CHARACTER*19  NOM19
      CHARACTER*24  VALE
      CHARACTER*32  JEXNUM
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      PGC = 'MCNPVR'
C
C
      NOM19 = NOMMAT
      VALE  = NOM19//'.VALE'
      IF ( CUMUL .EQ. 'ZERO' ) THEN
         DO 10 I= 1, NBVECT
            DO 20 J = 1, NEQ
               XSOL(J,I) = 0.D0
 20         CONTINUE
 10      CONTINUE
      ENDIF
C
C--    TRAITEMENT DES BLOCS SUP
C
      DO 100 IBLOC = 1, NBBLOC
         CALL JEVEUO(JEXNUM(VALE,IBLOC),'L',LMAT)
C
         DO 110 IEQUA = ABLO(IBLOC)+1, ABLO(IBLOC+1)
            ILONG   = HCOL(IEQUA)
            IADIA   = LMAT + ADIA(IEQUA)  - 1
C-            ADRESSE DANS LE BLOC SUP DU PREMIER TERME NON NUL
C-            SUR LA COLONNE IEQUA
            IDE     = IADIA -ILONG + 1
C-            INDICE DU PREMIER TERME NON NUL SUR LA COLONNE IEQUA
            IXX = IEQUA -ILONG + 1
C
            DO 120 NV = 1,NBVECT
               R8VAL = VECT(IEQUA,NV)
               DO 130 I = 0,ILONG-2
                  XSOL(IXX+I,NV)=XSOL(IXX+I,NV)+R8VAL*ZR(IDE+I)
  130          CONTINUE
C
  120       CONTINUE
  110    CONTINUE
C
         CALL JELIBE(JEXNUM(VALE,IBLOC))
  100 CONTINUE
C
C--    TRAITEMENT DES BLOCS INF
C
      DO 200 IBLOC = 1, NBBLOC
         JBLOC = IBLOC + NBBLOC
         CALL JEVEUO(JEXNUM(VALE,JBLOC),'L',LMAT)
C
         DO 210 IEQUA = ABLO(IBLOC)+1, ABLO(IBLOC+1)
            ILONG   = HCOL(IEQUA)
            IADIA   = LMAT + ADIA(IEQUA)  - 1
C-            ADRESSE DANS LE BLOC INF DU PREMIER TERME NON NUL
C-            SUR LA LIGNE IEQUA
            IDE     = IADIA -ILONG + 1
C-            INDICE DU PREMIER TERME NON NUL SUR LA LIGNE IEQUA
            IXX = IEQUA -ILONG + 1
C
            DO 220 NV = 1,NBVECT
               R8VAL = 0.D0
               DO 230 I = 0,ILONG-1
                  R8VAL = R8VAL + ZR(IDE+I) * VECT(IXX+I,NV)
  230          CONTINUE
C
               XSOL(IEQUA,NV) = XSOL(IEQUA,NV) + R8VAL
  220       CONTINUE
  210    CONTINUE
C
         CALL JELIBE(JEXNUM(VALE,JBLOC))
  200 CONTINUE
C
      CALL JEDEMA()
      END
