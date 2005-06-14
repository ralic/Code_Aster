      SUBROUTINE MRMMVR ( CUMUL,NOMMAT,ADIA,HCOL,NEQ,VECT,XSOL,NBVECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       CUMUL
      INTEGER                          ADIA(*),HCOL(*),NEQ,NBVECT
      CHARACTER*(*)             NOMMAT
      REAL*8                   VECT(NEQ,NBVECT), XSOL(NEQ,NBVECT)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 22/03/99   AUTEUR VABHHTS J.PELLET 
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
C     VERSION : LES ENTITES SONT REELLES
C             : LA MATRICE EST SYMETRIQUE STOCKEE MORSE
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
      REAL*8        ZERO
      CHARACTER*19  NOM19
      CHARACTER*24  VALE
      CHARACTER*32  JEXNUM
C     ------------------------------------------------------------------
C
C
C
      CALL JEMARQ()
      NOM19 = NOMMAT
      VALE  = NOM19//'.VALE'
      ZERO  = 0.D0
      IF ( CUMUL .EQ. 'ZERO' ) THEN
         DO 10 I= 1, NBVECT
            DO 20 J = 1, NEQ
               XSOL(J,I) = ZERO
 20         CONTINUE
 10      CONTINUE
      ENDIF
C
      CALL JEVEUO(JEXNUM(VALE,1),'L',LMAT)
      DO 30 NV= 1, NBVECT
C
C---- PREMIERE LIGNE
         XSOL(1,NV) = XSOL(1,NV) + ZR(LMAT-1+1)*VECT(1,NV)
C
C---- LIGNES SUIVANTES
         DO 40 I = 2 , NEQ
            KDEB = ADIA(I-1)+1
            KFIN = ADIA(I)-1
CCDIR$ IVDEP
            DO 50 KI = KDEB , KFIN
               XSOL(HCOL(KI),NV) =XSOL(HCOL(KI),NV)
     +                                        + ZR(LMAT-1+KI)*VECT(I,NV)
               XSOL(I,NV) =XSOL(I,NV) + ZR(LMAT-1+KI)*VECT(HCOL(KI),NV)
 50         CONTINUE
            XSOL(I,NV) =XSOL(I,NV) + ZR(LMAT+KFIN)*VECT(I,NV)
 40      CONTINUE
 30   CONTINUE
      CALL JEDEMA()
      END
