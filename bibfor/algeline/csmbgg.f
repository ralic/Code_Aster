      SUBROUTINE CSMBGG(LMAT,VSMB,VCINE,CVSMB,CVCINE,TYPE)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 VSMB(*),VCINE(*)
      COMPLEX*16 CVSMB(*),CVCINE(*)
      INTEGER LMAT
      CHARACTER*(*) TYPE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/01/2011   AUTEUR SELLENET N.SELLENET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C BUT : CALCUL DE LA CONTRIBUTION AU SECOND MEMBRE DES DDLS IMPOSEES
C       LORSQU'ILS SONT TRAITEES PAR ELIMINATION

C        ! K    K   ! L POUR NON IMPOSE I POUR IMPOSE
C K  =   !  LL   LI !
C        !  T       !
C        ! K    K   !
C        !  LI   II !

C  LE TRAITEMENT PAR ELIMINATION CONSISTE A RESOUDRE

C    ! K    0 !   ! X  !   ! 1  -K   !   ! F  !
C    !  LL    !   !  L !   !      IL !   !  I !
C    !        ! * !    ! = !         ! * !    ! <=> K' X = F'
C    ! 0    1 !   ! X  !   ! 0    1  !   ! U  !
C    !        !   !  I !   !         !   !  0 !
C  ON A LMAT  :DESCRIPTEUR DE K' CAR DANS L'ASSEMBLAGE ON ASSEMBLE
C              DIRECTEMENT K'   KIL SE TROUVE DANS .CCVA DE K'
C       VSMB  :EN IN (FI,0)  EN OUT = F'
C       VCINE : (0,U0)
C REMARQUES :
C  SI LMAT (7) = 0  ALORS GOTO 9999
C-----------------------------------------------------------------------
C !!!ATTENTION!!! LA MATRICE LE VECTEUR SECOND MEMBRE ET LE VECTEUR
C   CINEMATIQUE DOIVENT TOUS LES TROIS ETRE DE MEME TYPE (R/C)
C-----------------------------------------------------------------------
C IN  LMAT  I   : DESCRIPTEUR DE LA MATRICE SUR LAQUELLE ON A EFFECTUE
C                 LES ELIMINATIONS
C VAR VSMB  SCA : VECTEUR SECOND MEMBRE
C IN  VCINE SCA : VECTEUR DE CHARGEMENT CINEMATIQUE ( LE U0 DE U = U0
C                 SUR G AVEC VCINE = 0 EN DEHORS DE G )
C-----------------------------------------------------------------------
C     COMMUNS JEVEUX
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C     VARIABLES LOCALES
C-----------------------------------------------------------------------
      INTEGER NEQ,NIMPO,JCCLL,JCCII,ECCLL
C-----------------------------------------------------------------------
C     DEBUT
      CALL JEMARQ()
C-----------------------------------------------------------------------
      NEQ = ZI(LMAT+2)
      
      IF (ZI(LMAT+3).EQ.1) THEN
        CALL CSMBMD(ZK24(ZI(LMAT+1)),NEQ,VSMB)
      ELSE IF (ZI(LMAT+3).EQ.2) THEN
        CALL CSMBMC(ZK24(ZI(LMAT+1)),NEQ,CVSMB)
      END IF
      
      NIMPO = ZI(LMAT+7)
      IF (NIMPO.EQ.0) GO TO 10
      
      CALL JEEXIN(ZK24(ZI(LMAT+1))(1:19)//'.CCLL',ECCLL)
      IF ( ECCLL.EQ.0 ) GOTO 10
      
      CALL JEVEUO(ZK24(ZI(LMAT+1))(1:19)//'.CCLL','L',JCCLL)
      CALL JEVEUO(ZK24(ZI(LMAT+1))(1:19)//'.CCII','L',JCCII)

C     ------------------------------------------------------------------

      IF (ZI(LMAT+3).EQ.1) THEN

C        --- SYSTEME REEL:
        CALL ASSERT(TYPE.EQ.'R')
        CALL CSMBR8(ZK24(ZI(LMAT+1)),ZI(JCCLL),ZI(JCCII),NEQ,
     &              VCINE,VSMB)

      ELSE IF (ZI(LMAT+3).EQ.2) THEN

C        --- SYSTEME COMPLEXE:
        CALL ASSERT(TYPE.EQ.'C')
        CALL CSMBC8(ZK24(ZI(LMAT+1)),ZI(JCCLL),ZI(JCCII),NEQ,
     &              CVCINE,CVSMB)
      END IF

   10 CONTINUE

      CALL JEDEMA()
      END
