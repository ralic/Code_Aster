      SUBROUTINE MRCOND (  LMAT, NEQ2, RVECT, NVECT )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER              LMAT, NEQ2,        NVECT
      REAL*8                     RVECT(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/04/97   AUTEUR VABHHTS J.PELLET 
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
C IN  : LMAT    : ADRESSE DU DESCRIPTEUR DE LA MATRICE
C IN  : NEQ2    : NOMBRE D'EQUATIONS DU VECTEUR RVECT(NEQ2,NVECT)
C                 SI NEQ2.LE.0 ALORS NEQ2 = LMAT(2)
C IN  : NVECT   : NOMBRE DE VECTEURS DU VECTEUR RVECT(NEQ2,NVECT)
C
C     (DE)-CONDITIONNEMENT DU VECTEUR RVECT
C     IE        VECT(I) =  VECT(I) * COND(I)
C     ------------------------------------------------------------------
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     ------------------------------------------------------------------
      INTEGER       LCOND
      CHARACTER*1   TYPE(2)
      CHARACTER*24  COND
      COMPLEX*16    C8VAL
C     ------------------------------------------------------------------
      DATA  TYPE/'R','C'/
      DATA  COND/'                   .COND'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
C     --- NOMBRE D'EQUATION ---
      NEQ = NEQ2
      IF (NEQ2.LE.0) NEQ = ZI(LMAT+2)
C
C     --- RECUPERATION DU CONDITIONNEMENT ---
      COND(1:19) = ZK24(ZI(LMAT+1))
      CALL JEEXIN( COND, IER )
      IF ( IER  .NE.  0 ) THEN
         CALL JEVEUO( COND, 'L', LCOND )
         LCOND = LCOND - 1
         IF ( TYPE(ZI(LMAT+3)) .EQ. 'R' ) THEN
            DO 10 IVE = 1, NVECT
               IND = NEQ*(IVE-1)
               DO 12 IEQ = 1, NEQ
                  RVECT(IND+IEQ) = RVECT(IND+IEQ) * ZR(LCOND+IEQ)
 12            CONTINUE
 10         CONTINUE
         ELSEIF ( TYPE(ZI(LMAT+3)) .EQ. 'C' ) THEN
            DO 20 IVE = 1, NVECT
               IND = NEQ*(IVE-1)
               DO 22 IEQ = 1, NEQ
                  II = IND + 2*IEQ
                  C8VAL = DCMPLX( RVECT(II-1),RVECT(II))
                  C8VAL = C8VAL * ZC(LCOND+IEQ)
                  RVECT(II-1) = DBLE (C8VAL)
                  RVECT(II  ) = DIMAG(C8VAL)
 22            CONTINUE
 20         CONTINUE
         ENDIF
      ENDIF
      CALL JEDEMA()
      END
