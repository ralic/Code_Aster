subroutine liscad(lischa, ichar, nomcha, nomfct, nbinfo,&
                  lisinz, ival)
!
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
! ADD UNE CHARGE DE LA SD LISTE_CHARGES
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : NOM DE LA SD LISTE_CHARGES
! IN  ICHAR  : INDICE DU CHARGEMENT DANS LA LISTE
! IN  NOMCHA : NOM DE LA CHARGE
! IN  NOMFCT : NOM DE LA FONCT. MULTIPLICATRICE
! IN  NBINFO : NOMBRE D'INFO DISPO
! IN  LISINF : LISTE DES INFO SUR LA CHARGE
! IN  IVAL   : VALEUR DE LA CHARGE POUR CERTAINS TYPES
!
!
!
!
!
    character(len=24) :: charge, infcha, fomult
    integer :: jalich, jinfch, jalifc
    integer :: nchar, inf
    character(len=24) :: infoch
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
    call jeveuo(charge, 'E', jalich)
    call jeveuo(infcha, 'E', jinfch)
    call jeveuo(fomult, 'E', jalifc)
    nchar = zi(jinfch)
    call assert(ichar.gt.0)
    call assert(ichar.le.nchar)
!
    zk24(jalich+ichar-1) = nomcha
    zk24(jalifc+ichar-1) = nomfct
!
!
    do 10 inf = 1, nbinfo
        infoch = lisinz(inf)
        if (infoch .eq. 'CINE_CSTE') then
            zi(jinfch+ichar) = -1
!
        else if (infoch.eq.'CINE_FO') then
            zi(jinfch+ichar) = -2
!
        else if (infoch.eq.'CINE_FT') then
            zi(jinfch+ichar) = -3
!
        else if (infoch.eq.'DIRI_PILO ') then
            zi(jinfch+ichar) = 5
!
        else if (infoch.eq.'DIRI_PILO_F') then
            zi(jinfch+ichar) = 6
!
        else if (infoch(1:9).eq.'DIRI_CSTE') then
            zi(jinfch+ichar) = 1
            if (infoch(10:15) .eq. '_DIDI') then
                zi(jinfch+3*nchar+2+ichar) = 1
            endif
!
        else if (infoch(1:9).eq.'DIRI_FO') then
            zi(jinfch+ichar) = 2
            if (infoch(10:15) .eq. '_DIDI') then
                zi(jinfch+3*nchar+2+ichar) = 1
            endif
!
        else if (infoch(1:9).eq.'DIRI_FT') then
            zi(jinfch+ichar) = 3
            if (infoch(10:15) .eq. '_DIDI') then
                zi(jinfch+3*nchar+2+ichar) = 1
            endif
!
        else if (infoch.eq.'NEUM_ONDE') then
            zi(jinfch+nchar+ichar) = 6
!
        else if (infoch.eq.'NEUM_SIGM_INT') then
            zi(jinfch+nchar+ichar) = 55
            zi(jinfch+4*nchar+4) = 99
!
        else if (infoch.eq.'NEUM_PILO') then
            zi(jinfch+nchar+ichar) = 5
!
        else if (infoch.eq.'NEUM_SUIV') then
            zi(jinfch+nchar+ichar) = 4
!
        else if (infoch.eq.'NEUM_FO') then
            zi(jinfch+nchar+ichar) = 2
!
        else if (infoch.eq.'NEUM_FT') then
            zi(jinfch+nchar+ichar) = 3
!
        else if (infoch.eq.'NEUM_CSTE') then
            zi(jinfch+nchar+ichar) = 1
!
        else if (infoch.eq.'NEUM_LAPL') then
            zi(jinfch+2*nchar+2) = ival
!
        else if (infoch.eq.'ELEM_TARDIF') then
            zi(jinfch+nchar+ichar) = 10
!
        else if (infoch.eq.'EXCIT_SOL') then
            zi(jinfch+nchar+ichar) = 20
!
        else
            write(6,*) 'LISCAD: ',infoch
            call assert(.false.)
!
        endif
10  end do
!
    call jedema()
end subroutine
