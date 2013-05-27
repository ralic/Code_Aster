subroutine alcar1(noma, moclef, nbmocs, mocles, typmcl,&
                  nbet)
    implicit none
    include 'asterc/getfac.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/reliem.h'
    integer :: nbmocs, nbet
    character(len=*) :: noma, moclef, mocles(*), typmcl(*)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     RECHERCHE DE NBNO A PARTIR DE MOTS CLES NOEUD, GROUP_NO, ...
! ----------------------------------------------------------------------
    integer :: i, nbocc, nbno
    character(len=8) :: mailla
    character(len=24) :: mesnoe
!     ------------------------------------------------------------------
    call jemarq()
!
    nbet = 0
    mesnoe = '&&ALCAR1.MES_NOEUDS'
    mailla = noma
!
    call getfac(moclef, nbocc)
!
    do 10 i = 1, nbocc
!
        call reliem(' ', mailla, 'NU_NOEUD', moclef, i,&
                    nbmocs, mocles, typmcl, mesnoe, nbno)
!
        if (nbno .ne. 0) call jedetr(mesnoe)
!
        nbet = nbet + nbno
!
10  end do
!
    call jedema()
end subroutine
