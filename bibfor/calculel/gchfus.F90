subroutine gchfus(fonct1, fonct2, fonct3)
    implicit none
    include 'jeveux.h'
    include 'asterfort/copisd.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=*) :: fonct1, fonct2, fonct3
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
!
!     BUT : DETERMINE UNE FONCTION A PARTIR DE 2 FONCTIONS.
!           LES VALEURS DE LA FONCTION OUT CORRESPONDENT
!           A LA SOMMES DES VALEURS DES FONCTIONS IN
!
!     IN :  FONCT1
!
! ======================================================================
! ----------------------------------------------------------------------
    integer :: nptf1, nptf2, jprol, jvale, iret, jval, nptf, i
    real(kind=8) :: y
    character(len=8) :: k8b
    character(len=19) :: fo1, fo2, fo3, fotmp1, fotmp2
!
    call jemarq()
!
    fo1=fonct1
    fo2=fonct2
    fo3=fonct3
!
    call jelira(fo1//'.VALE', 'LONMAX', nptf1, k8b)
    call jelira(fo2//'.VALE', 'LONMAX', nptf2, k8b)
!
    nptf1=nptf1/2
    nptf2=nptf2/2
!
    if (nptf1 .ge. nptf2) then
        call copisd('FONCTION', 'V', fo1, fo3)
        fotmp1=fo2
        fotmp2=fo1
        nptf=nptf1
    else
        call copisd('FONCTION', 'V', fo2, fo3)
        fotmp1=fo1
        fotmp2=fo2
        nptf=nptf2
    endif
!
    call jeveuo(fotmp1//'.PROL', 'L', jprol)
    call jeveuo(fotmp2//'.VALE', 'L', jval)
    call jeveuo(fo3//'.VALE', 'E', jvale)
!
    do 10 i = 1, nptf
        call fointe('A', fotmp1, 1, zk24(jprol+3-1), zr(jval+i-1),&
                    y, iret)
        zr(jvale+nptf+i-1)=zr(jval+nptf+i-1)+y
10  end do
!
    call jedema()
!
end subroutine
