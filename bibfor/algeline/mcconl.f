      SUBROUTINE MCCONL(LMAT,NEQ2,TYPEV,CVECT,NVECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           LMAT,NEQ2,            NVECT
      CHARACTER*1            TYPEV
      COMPLEX*16                   CVECT(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 09/07/2002   AUTEUR CAMBIER S.CAMBIER 
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
C     TENIR COMPTE DU CONDITIONNEMENT DES LAGRANGE SUR LE SECOND MEMBRE
C     ------------------------------------------------------------------
C IN  : LMAT    : ADRESSE DU DESCRIPTEUR DE LA MATRICE
C IN  : NEQ2    : NOMBRE D'EQUATIONS DU VECTEUR CVECT(NEQ2,NVECT)
C                 SI NEQ2.LE.0 ALORS NEQ2 = LMAT(2)
C IN  : NVECT   : NOMBRE DE VECTEURS DU VECTEUR CVECT(NEQ2,NVECT)
C IN  : TYPEV   : TYPE DES COEFFICIENTS DU VECTEUR
C               = 'R'  : A COEFFICIENTS REELS
C               = 'C'  : A COEFFICIENTS COMPLEXES
C               = ' '  : LES COEFFICIENTS DU VECTEURS SONT DU MEME TYPE
C                        QUE CEUX DE LA MATRICE.
C VAR :  CVECT  : VECTEUR A MODIFIER
C               REMARQUE : CVECT DOIT ETRE DU MEME TYPE QUE LA MATRICE
C                          SOIT REAL*8
C                          SOIT COMPLEX*16
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*1   FTYPE(2), TYPE, TYPECN
      CHARACTER*24  CONL
      COMPLEX*16    C8CST
C     ------------------------------------------------------------------
      DATA CONL/'                   .CONL'/
      DATA FTYPE/'R','C'/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IF (TYPEV .EQ. ' ' ) THEN
         TYPE = FTYPE(ZI(LMAT+3))
      ELSE
         TYPE = TYPEV
      ENDIF
      NEQ = NEQ2
      IF (NEQ2.LE.0) NEQ = ZI(LMAT+2)
C
      CONL(1:19) = ZK24(ZI(LMAT+1))
      CALL JEEXIN(CONL,IRET)
      IF (IRET .NE. 0 ) THEN
         CALL JELIRA(CONL,'TYPE',IBID,TYPECN)
         CALL JEVEUO(CONL,'L',LCONL)
         LCONL = LCONL - 1
         NEQC = NEQ/2
         IF (TYPE .EQ. 'R' .AND. TYPECN .EQ. 'R'  ) THEN
            DO 100 IVE = 1, NVECT
               IND = NEQC*(IVE-1)
               II = 1
               DO 102 IEQ = 1, NEQC
                  REALVE = DBLE(CVECT(IND+IEQ))*ZR(LCONL+II)
                  RIMAGV = DIMAG(CVECT(IND+IEQ))*ZR(LCONL+II+1)
                  CVECT(IND+IEQ) = DCMPLX(REALVE,RIMAGV)
                  II = II + 2
  102          CONTINUE
  100       CONTINUE
         ELSEIF (TYPE .EQ. 'C' ) THEN
            IF ( TYPECN .EQ. 'R' ) THEN
               DO 110 IVE = 1, NVECT
                  IND = NEQ*(IVE-1)
                  DO 112 IEQ = 1, NEQ
                     CVECT(IND+IEQ) = CVECT(IND+IEQ) * ZR(LCONL+IEQ)
  112             CONTINUE
  110          CONTINUE
            ELSEIF ( TYPECN .EQ. 'C' ) THEN
               DO 120 IVE = 1, NVECT
                  IND = NEQ*(IVE-1)
                  DO 122 IEQ = 1, NEQ
                     CVECT(IND+IEQ) =  CVECT(IND+IEQ) * ZC(LCONL+IEQ)
  122             CONTINUE
  120          CONTINUE
            ENDIF
         ENDIF
       ENDIF
      CALL JEDEMA()
      END
