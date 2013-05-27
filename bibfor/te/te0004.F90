subroutine te0004(option, nomte)
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/elref4.h'
    include 'asterfort/ppgan2.h'
    include 'asterfort/tecach.h'
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: josselin.delmas at edf.fr
!
!     BUT:
!       !! ROUTINE GENERIQUE !!
!       ASSURER LE PASSAGE ELGA -> ELNO
!
!.......................................................................
!
    include 'jeveux.h'
!
!
    integer :: ndim, nno, nnos, npg, nbsp
    integer :: ipoids, ivf, idfde, jgano
    integer :: iret, nbcmp, itabin(7), itabou(7)
    integer :: iinpg, ioutno
!
    character(len=4) :: fami
!
! ----------------------------------------------------------------------
!
    if (option .eq. 'DERA_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PDERAPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PDERANO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'DISS_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PDISSPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PDISSNO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'EFGE_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PEFGAR', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PEFFORR', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'ENDO_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PTRIAPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PTRIANO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'ENEL_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PENERPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PENERNO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'EPFD_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PDEFOPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PDEFONO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'EPFP_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PDEFOPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PDEFONO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'EPME_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PDEFOPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PDEFONO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'EPMG_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PDEFOPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PDEFONO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'EPSG_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PDEFOPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PDEFONO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'EPSI_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PDEFOPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PDEFONO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'EPSP_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PDEFOPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PDEFONO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'EPVC_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PDEFOPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PDEFONO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'ETOT_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PENERPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PENERNO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'FLUX_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PFLUXPG', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PFLUXNO', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'SIGM_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PCONTRR', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PSIEFNOR', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'SIEF_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PCONTRR', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PSIEFNOR', 'E', 7, itabou,&
                    iret)
!
    else if (option.eq.'VARI_ELNO') then
        fami = 'RIGI'
        call tecach('OOO', 'PVARIGR', 'L', 7, itabin,&
                    iret)
        call tecach('OOO', 'PVARINR', 'E', 7, itabou,&
                    iret)
!
    else
        call assert(.false.)
    endif
!
!    -------------------------------------------------------------------
!    -- COMMUN A TOUTES LES OPTIONS
!    -------------------------------------------------------------------
!
! --- IL FAUDRAIT PENSER A RECUPERER FAMI DE MANIERE AUTOMATIQUE ET SURE
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    iinpg=itabin(1)
    ioutno=itabou(1)
!
! --- NOMBRE DE COMPOSANTE
    nbcmp=itabin(2)/itabin(3)
    call assert(nbcmp.gt.0)
    call assert(nbcmp.eq.itabou(2)/itabou(3))
!
! --- ON ESSAIE UNE AUTRE FAMILLE DE POINT DE GAUSS
    if (itabin(3) .ne. npg) then
        fami='MASS'
        call elref4(' ', fami, ndim, nno, nnos,&
                    npg, ipoids, ivf, idfde, jgano)
    endif
    call assert(itabin(3).eq.npg)
!
! --- NOMBRE DE SOUS-POINT
    nbsp=itabin(7)
    call assert(nbsp.eq.itabou(7))
!
! --- NOMBRE DE COMPOSANTE DYNAMIQUE
    if (option .eq. 'VARI_ELNO') then
        nbcmp=itabin(6)
        call assert(nbcmp.eq.itabou(6))
    endif
!
    call ppgan2(jgano, nbsp, nbcmp, zr(iinpg), zr(ioutno))
!
end subroutine
