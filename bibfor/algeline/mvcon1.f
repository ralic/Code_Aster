      SUBROUTINE MVCON1(MAT,NEQ2,TYPEV,RVECT,NVECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER               NEQ2,            NVECT
      CHARACTER*1                TYPEV
      CHARACTER*(*)     MAT
      REAL*8                       RVECT(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 15/09/98   AUTEUR VABHHTS J.PELLET 
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
C IN  : MAT     : NOM DE LA MATRICE
C IN  : NEQ2    : NOMBRE D'EQUATIONS DU VECTEUR RVECT(NEQ2,NVECT)
C                 SI NEQ2.LE.0 ALORS NEQ2 = LMAT(2)
C IN  : NVECT   : NOMBRE DE VECTEURS DU VECTEUR RVECT(NEQ2,NVECT)
C IN  : TYPEV   : TYPE DES COEFFICIENTS DU VECTEUR
C               = 'R'  : A COEFFICIENTS REELS
C               = 'C'  : A COEFFICIENTS COMPLEXES
C               = ' '  : LES COEFFICIENTS DU VECTEURS SONT DU MEME TYPE
C                        QUE CEUX DE LA MATRICE.
C VAR :  RVECT  : VECTEUR A MODIFIER
C               REMARQUE : RVECT DOIT ETRE DU MEME TYPE QUE LA MATRICE
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
      CHARACTER*1   TYPE, TYPECN
      CHARACTER*19  KMAT,KSTOC
      CHARACTER*24  CONL
      COMPLEX*16    C8CST
      CHARACTER*32     JEXNUM
C     ------------------------------------------------------------------
      DATA CONL/'                   .CONL'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
      KMAT = MAT
C
      IF (TYPEV .EQ. ' ' ) THEN
        CALL JELIRA(JEXNUM(KMAT//'.VALE',1),'TYPE',IBID,TYPE)
      ELSE
        TYPE = TYPEV
      ENDIF
      CALL JEVEUO(KMAT//'.REFA','L',LREFE)
      KSTOC = ZK24(LREFE+2)
      CALL JEVEUO(KSTOC//'.DESC','L',LDESC)
      NEQ = ZI(LDESC)
      IF ((NEQ2.GT.0).AND.(NEQ2.LE.NEQ)) NEQ = NEQ2
C
      CONL(1:19) = KMAT
      CALL JEEXIN(CONL,IRET)
      IF (IRET .NE. 0 ) THEN
         CALL JELIRA(CONL,'TYPE',IBID,TYPECN)
         CALL JEVEUO(CONL,'L',LCONL)
         LCONL = LCONL - 1
         IF (TYPE .EQ. 'R' .AND. TYPECN .EQ. 'R'  ) THEN
            DO 100 IVE = 1, NVECT
               IND = NEQ*(IVE-1)
               DO 102 IEQ = 1, NEQ
                  RVECT(IND+IEQ) = RVECT(IND+IEQ) * ZR(LCONL+IEQ)
  102          CONTINUE
  100       CONTINUE
         ELSEIF (TYPE .EQ. 'C' ) THEN
            IF ( TYPECN .EQ. 'R' ) THEN
               DO 110 IVE = 1, NVECT
                  IND = NEQ*(IVE-1)
                  DO 112 IEQ = 1, NEQ
                     II = IND + 2*IEQ
                     RVECT(II-1) = RVECT(II-1) * ZR(LCONL+IEQ)
                     RVECT(II  ) = RVECT(II  ) * ZR(LCONL+IEQ)
  112             CONTINUE
  110          CONTINUE
            ELSEIF ( TYPECN .EQ. 'C' ) THEN
               DO 120 IVE = 1, NVECT
                  IND = NEQ*(IVE-1)
                  DO 122 IEQ = 1, NEQ
                     II = IND + 2*IEQ
                     C8CST = DCMPLX( RVECT(II-1) , RVECT(II) )
                     C8CST = C8CST * ZC(LCONL+IEQ)
                     RVECT(II-1) =  DBLE(C8CST)
                     RVECT(II  ) = DIMAG(C8CST)
  122             CONTINUE
  120          CONTINUE
            ENDIF
         ENDIF
       ENDIF
      CALL JEDEMA()
      END
