      SUBROUTINE MRMPVS ( CUMUL,NOMMAT,ADIA,HCOL,ABLO,NEQ,NBBLOC,
     +                                                 VECT,XSOL,NBVECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       CUMUL
      INTEGER                   ADIA(*),HCOL(*),ABLO(*),NEQ,NBBLOC,NBVEC
      CHARACTER*(*)            NOMMAT
      REAL*8                       VECT(NEQ,NBVECT), XSOL(NEQ,NBVECT)
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
C                   MULTIPLICATION MATRICE PAR N VECTEURS
C         XSOL(1..NEQ,1..NBVECT) = MATRICE  * VECT(1..NEQ,1..NBVECT)
C     ------------------------------------------------------------------
C     VERSION : LA MATRICE EST REELLES  -  LES VECTEURS SONT COMPLEXES
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       IA,  IV, LM, LONG
      COMPLEX*16    C8ZERO, C8UN, C8VAL
      CHARACTER*24  VALE
      CHARACTER*32  JEXNUM
C
C     ------------------------------------------------------------------
      DATA          VALE/'                   .VALE'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      VALE(1:19) = NOMMAT
      C8ZERO = DCMPLX(0.D0,0.D0)
      IF ( CUMUL .EQ. 'ZERO' ) THEN
         DO 10 I= 1, NBVECT
            DO 20 J = 1, NEQ
               XSOL(J,I) = C8ZERO
 20         CONTINUE
 10      CONTINUE
      ENDIF
C
      DO 100 IBLOC = 1, NBBLOC
         CALL JEVEUO(JEXNUM(VALE,IBLOC),'L',LMAT)
C
         DO 110 IEQUA = ABLO(IBLOC)+1, ABLO(IBLOC+1)
            LONG = HCOL(IEQUA)
            IA   = LMAT + ADIA(IEQUA) - LONG - 1
            IV   = IEQUA - LONG
            LM   = LONG - 1
C
            DO 120 NV = 1,NBVECT
               C8VAL = VECT(IEQUA,NV)
               DO 130 IL = 1, LM
                  XSOL(IV+IL,NV)=XSOL(IV+IL,NV)+C8VAL*ZR(IA+IL)
  130          CONTINUE
C
               C8VAL = C8ZERO
               DO 140 IL = 1, LM
                  C8VAL = C8VAL + ZR(IA+IL) * VECT(IV+IL,NV)
  140          CONTINUE
C
               XSOL(IEQUA,NV) = XSOL(IEQUA,NV) + C8VAL +
     +                                    ZR(IA+LONG)*VECT(IEQUA,NV)
  120       CONTINUE
  110    CONTINUE
C
         CALL JELIBE(JEXNUM(VALE,IBLOC))
  100 CONTINUE
      CALL JEDEMA()
      END
