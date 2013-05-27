subroutine juvinn(ojb)
    implicit none
    include 'jeveux.h'
    include 'asterc/iisnan.h'
    include 'asterc/r8nnem.h'
    include 'asterc/r8vide.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=*) :: ojb
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     BUT : REMPLACER DANS L'OBJET JEVEUX DE NOM OJB (DE TYPE 'R')
!           LES VALEURS R8VIDE() PAR R8NNEM()
!     ------------------------------------------------------------------
! IN  OJB  : K24 : NOM DE L'OBJET A MODIFIER
!     ------------------------------------------------------------------
!
!
    character(len=8) :: type, cbid
    integer :: n1, k, jojb, ibid
    real(kind=8) :: rvid, rnan
!     ------------------------------------------------------------------
!
    call jemarq()
    call jelira(ojb, 'TYPE  ', ibid, type)
    call assert(type.eq.'R')
!
    call jeveuo(ojb, 'L', jojb)
    call jelira(ojb, 'LONMAX', n1, cbid)
!
    rvid=r8vide()
    rnan=r8nnem()
    do 1, k=1,n1
    if (iisnan(zr(jojb-1+k)) .eq. 0) then
        if (zr(jojb-1+k) .eq. rvid) zr(jojb-1+k)=rnan
    endif
    1 end do
!
    call jedema()
end subroutine
