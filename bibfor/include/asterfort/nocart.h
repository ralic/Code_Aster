!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
subroutine nocart(carte, code, ncmp, groupma, mode, nma,&
                  limano, limanu, ligrel,&
                  rapide,jdesc,jnoma,jncmp,jnoli,jvale,&
                  jvalv,jnocmp,ncmpmx,nec, ctype,&
                  jlima0,jlimac,lontav)

    character(len=*), intent(in) :: carte
    integer, intent(in) :: code
    integer, intent(in) :: ncmp
    character(len=*), intent(in), optional :: groupma
    character(len=*),intent(in), optional :: mode
    integer, intent(in), optional :: nma
    character(len=*), intent(in), optional :: limano(*)
    integer, intent(in), optional :: limanu(*)
    character(len=*), intent(in), optional ::  ligrel

!   -- arguments optionnels pour gagner du CPU :
    character(len=3), intent(in), optional ::  rapide
    integer, intent(inout), optional ::  jdesc
    integer, intent(inout), optional ::  jnoma
    integer, intent(inout), optional ::  jncmp
    integer, intent(inout), optional ::  jnoli
    integer, intent(inout), optional ::  jvale
    integer, intent(inout), optional ::  jvalv
    integer, intent(in)   , optional ::  jnocmp
    integer, intent(in)   , optional ::  ncmpmx
    integer, intent(in)   , optional ::  nec
    character(len=8), intent(in), optional ::  ctype
    integer, intent(inout), optional ::  jlima0
    integer, intent(inout), optional ::  jlimac
    integer, intent(inout), optional ::  lontav

end subroutine nocart
end interface
