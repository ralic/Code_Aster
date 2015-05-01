subroutine lcumvi(cmp, vim, epsfm)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
! ROUTINE DE COMPORTEMENT DE FLUAGE UMLV
! PASSAGE DES VARIABLES INTERNES AU VECTEUR DE DEFORMATIONS DE FLUAGE
!
! IN  CMP : COMPOSANTES A CALCULER :
!             'FT' : FLUAGE PROPRE + DESSICCATION
!             'FP' : FLUAGE PROPRE UNIQUEMENT
!             'FD' : FLUAGE DE DESSICCATION
! IN  VIM   : VARIABLES INTERNES
! OUT EPSFM : VECTEUR DE DEFORMATIONS DE FLUAGE
!
!  STRUCTURE DES VARIABLES INTERNES : VIM,VIP ( X = I ou F )
!
!     VIX(1)     = ERSP  : DEFORMATION DE FLUAGE REV SPHERIQUE
!     VIX(2)     = EISP  : DEFORMATION DE FLUAGE IRR SPHERIQUE
!     VIX(3)     = ERD11 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 11
!     VIX(4)     = EID11 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 11
!     VIX(5)     = ERD22 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 22
!     VIX(6)     = EID22 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 22
!     VIX(7)     = ERD33 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 33
!     VIX(8)     = EID33 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 33
!     VIX(9)     = EFD11 : DEFORMATION DE FLUAGE DE DESSICCATION  11
!     VIX(10)    = EFD22 : DEFORMATION DE FLUAGE DE DESSICCATION  22
!     VIX(11)    = EFD33 : DEFORMATION DE FLUAGE DE DESSICCATION  33
!     VIX(12)    = ERD12 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 12
!     VIX(13)    = EID12 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 12
!     VIX(14)    = ERD23 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 23
!     VIX(15)    = EID23 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 23
!     VIX(16)    = ERD31 : DEFORMATION DE FLUAGE REV DEVIATORIQUE 31
!     VIX(17)    = EID31 : DEFORMATION DE FLUAGE IRE DEVIATORIQUE 31
!     VIX(18)    = EFD12 : DEFORMATION DE FLUAGE DE DESSICCATION  12
!     VIX(19)    = EFD23 : DEFORMATION DE FLUAGE DE DESSICCATION  23
!     VIX(20)    = EFD31 : DEFORMATION DE FLUAGE DE DESSICCATION  31
!_______________________________________________________________________
! ----------------------------------------------------------------------
    implicit none
    character(len=*) :: cmp
    real(kind=8) :: vim(20), epsfm(6)
    integer :: i
!
    if (cmp(1:2) .eq. 'FP') then
        do 20 i = 1, 3
            epsfm(i)=(vim(1)+vim(2))
20      continue
        epsfm(1)=epsfm(1)+vim(3)+vim(4)
        epsfm(2)=epsfm(2)+vim(5)+vim(6)
        epsfm(3)=epsfm(3)+vim(7)+vim(8)
        epsfm(4)=vim(12)+vim(13)
        epsfm(5)=vim(14)+vim(15)
        epsfm(6)=vim(16)+vim(17)
    else if (cmp(1:2).eq.'FD') then
        epsfm(1)=vim(9)
        epsfm(2)=vim(10)
        epsfm(3)=vim(11)
        epsfm(4)=vim(18)
        epsfm(5)=vim(19)
        epsfm(6)=vim(20)
    else if (cmp(1:2).eq.'FT') then
        do 25 i = 1, 3
            epsfm(i)=(vim(1)+vim(2))
25      continue
        epsfm(1)=epsfm(1)+vim(3)+vim(4)+vim(9)
        epsfm(2)=epsfm(2)+vim(5)+vim(6)+vim(10)
        epsfm(3)=epsfm(3)+vim(7)+vim(8)+vim(11)
        epsfm(4)=vim(12)+vim(13)+vim(18)
        epsfm(5)=vim(14)+vim(15)+vim(19)
        epsfm(6)=vim(16)+vim(17)+vim(20)
    endif
!
end subroutine
