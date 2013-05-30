subroutine liscli(lischa, ichar, nomcha, nomfct, nbinfo,&
                  lisinz, ival)
!
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=19) :: lischa
    integer :: ichar, ival, nbinfo
    character(len=8) :: nomcha, nomfct
    character(len=*) :: lisinz(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! LIRE UNE CHARGE DE LA SD LISTE_CHARGES
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : NOM DE LA SD LISTE_CHARGES
! IN  ICHAR  : INDICE DU CHARGEMENT DANS LA LISTE
! OUT NOMCHA : NOM DE LA CHARGE
! OUT NOMFCT : NOM DE LA FONCT. MULTIPLICATRICE
! OUT INFOCH : INFO SUR LA CHARGE
! OUT IVAL   : VALEUR DE LA CHARGE POUR CERTAINS TYPES
!
!
!
!
!
    character(len=24) :: charge, infcha, fomult
    integer :: jalich, jinfch, jalifc
    integer :: nchar, nbiout, i
    character(len=24) :: infoch(3)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    charge = lischa(1:19)//'.LCHA'
    infcha = lischa(1:19)//'.INFC'
    fomult = lischa(1:19)//'.FCHA'
!
    call jeveuo(charge, 'L', jalich)
    call jeveuo(infcha, 'L', jinfch)
    call jeveuo(fomult, 'L', jalifc)
    nchar = zi(jinfch)
    call assert(ichar.gt.0)
    call assert(ichar.le.nchar)
!
    nomcha = zk24(jalich+ichar-1)(1:8)
    nomfct = zk24(jalifc+ichar-1)(1:8)
    nbiout = 0
!
    if (zi(jinfch+ichar) .eq. -1) then
        nbiout = 1
        infoch(nbiout) = 'CINE_CSTE'
!
    else if (zi(jinfch+ichar) .eq. -2) then
        nbiout = 1
        infoch(nbiout) = 'CINE_FO'
!
    else if (zi(jinfch+ichar) .eq. -3) then
        nbiout = 1
        infoch(nbiout) = 'CINE_FT'
!
    else if (zi(jinfch+ichar) .eq. 5) then
        nbiout = 1
        infoch(nbiout) = 'DIRI_PILO'
!
    else if (zi(jinfch+ichar) .eq. 6) then
        nbiout = 1
        infoch(nbiout) = 'DIRI_PILO_F'
!
    else if (zi(jinfch+ichar) .eq. 1) then
        nbiout = 1
        infoch(nbiout) = 'DIRI_CSTE'
        if (zi(jinfch+3*nchar+2+ichar) .eq. 1) then
            infoch(nbiout) = infoch(nbiout)(1:9)//'_DIDI'
        endif
!
    else if (zi(jinfch+ichar) .eq. 2) then
        nbiout = 1
        infoch(nbiout) = 'DIRI_FO'
        if (zi(jinfch+3*nchar+2+ichar) .eq. 1) then
            infoch(nbiout) = infoch(nbiout)(1:9)//'_DIDI'
        endif
!
    else if (zi(jinfch+ichar) .eq. 3) then
        nbiout = 1
        infoch(nbiout) = 'DIRI_FT'
        if (zi(jinfch+3*nchar+2+ichar) .eq. 1) then
            infoch(nbiout) = infoch(nbiout)(1:9)//'_DIDI'
        endif
    endif
!
    if (zi(jinfch+nchar+ichar) .eq. 6) then
        nbiout = nbiout + 1
        infoch(nbiout) = 'NEUM_ONDE'
!
        elseif ((zi(jinfch+nchar+ichar) .eq. 55) .and. (zi(jinfch+4*nchar+&
    4) .eq. 99) ) then
        nbiout = nbiout + 1
        infoch(nbiout) = 'NEUM_SIGM_INT'
!
    else if (zi(jinfch+nchar+ichar) .eq. 5) then
        nbiout = nbiout + 1
        infoch(nbiout) = 'NEUM_PILO'
!
    else if (zi(jinfch+nchar+ichar) .eq. 4) then
        nbiout = nbiout + 1
        infoch(nbiout) = 'NEUM_SUIV'
!
    else if (zi(jinfch+nchar+ichar) .eq. 2) then
        nbiout = nbiout + 1
        infoch(nbiout) = 'NEUM_FO'
!
    else if (zi(jinfch+nchar+ichar) .eq. 3) then
        nbiout = nbiout + 1
        infoch(nbiout) = 'NEUM_FT'
!
    else if (zi(jinfch+nchar+ichar) .eq. 1) then
        nbiout = nbiout + 1
        infoch(nbiout) = 'NEUM_CSTE'
    endif
!
    if (zi(jinfch+2*nchar+2) .ne. 0) then
        nbiout = nbiout + 1
        infoch(nbiout) = 'NEUM_LAPL'
        ival = zi(jinfch+2*nchar+2)
    endif
!
!     REMPLIT LES NBINFO VALEURS DEMANDEES DANS LISINZ
    nbinfo = min(nbinfo, nbiout)
    do 10 i = 1, nbinfo
        lisinz(i) = infoch(i)
10  end do
!
    call jedema()
end subroutine
