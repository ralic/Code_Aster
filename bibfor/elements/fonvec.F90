subroutine fonvec(resu, noma, cnxinv)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getvr8.h'
    include 'asterc/r8prem.h'
    include 'asterfort/fonnor.h'
    include 'asterfort/getvem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/u2mesr.h'
    include 'asterfort/wkvect.h'
    include 'blas/ddot.h'
    character(len=8) :: resu, noma
    character(len=19) :: cnxinv
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
!-----------------------------------------------------------------------
! FONCTION REALISEE:
!
!     VERIFICATION DE LA COHERENCE DES VECTEURS RENSEIGNES DANS
!     DEFI_FOND_FISS
!
!     ENTREES:
!        RESU       : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
!        NOMA       : NOM DU MAILLAGE
!        CNXINV     : CONNECTIVITE INVERSE
!-----------------------------------------------------------------------
!
    integer :: jnorm, jorig, jextr, jvale
    integer :: iagrn, numer
    integer :: ndtaex, ndtaor, nvenor
    integer :: nveor, nveex
    real(kind=8) :: xpfi, xpfo, ypfi, ypfo, zpfi, zpfo, zrbid
    real(kind=8) :: valr(6), ps1, ps2, zero
    character(len=24) :: nomgrp(2)
    character(len=24) :: grpnoe, cooval
    integer :: iarg
!     -----------------------------------------------------------------
!
    call jemarq()
!
    grpnoe = noma//'.GROUPENO       '
    cooval = noma//'.COORDO    .VALE'
    call jeveuo(cooval, 'L', jvale)
!
!     --------------------------------------------------------------
    call getvr8('FOND_FISS', 'DTAN_ORIG', 1, iarg, 0,&
                zrbid, ndtaor)
    if (ndtaor .ne. 0) then
        ndtaor = -ndtaor
        call wkvect(resu//'.DTAN_ORIGINE', 'G V R8', 3, jorig)
        call getvr8('FOND_FISS', 'DTAN_ORIG', 1, iarg, 3,&
                    zr(jorig), ndtaor)
    endif
!
!     --------------------------------------------------------------
    call getvr8('FOND_FISS', 'DTAN_EXTR', 1, iarg, 0,&
                zrbid, ndtaex)
    if (ndtaex .ne. 0) then
        ndtaex = -ndtaex
        call wkvect(resu//'.DTAN_EXTREMITE', 'G V R8', 3, jextr)
        call getvr8('FOND_FISS', 'DTAN_EXTR', 1, iarg, 3,&
                    zr(jextr), ndtaex)
    endif
!
!     --------------------------------------------------------------
    call getvr8(' ', 'NORMALE', 1, iarg, 0,&
                zrbid, nvenor)
    if (nvenor .ne. 0) then
        nvenor = -nvenor
        call wkvect(resu//'.NORMALE', 'G V R8', 3, jnorm)
        call getvr8(' ', 'NORMALE', 1, iarg, 3,&
                    zr(jnorm), nvenor)
    else
        call fonnor(resu, noma, cnxinv)
    endif
!
!     --------------------------------------------------------------
    call getvem(noma, 'GROUP_NO', 'FOND_FISS', 'VECT_GRNO_ORIG', 1,&
                iarg, 0, nomgrp, nveor)
    if (nveor .ne. 0) then
        nveor = -nveor
        call wkvect(resu//'.DTAN_ORIGINE', 'G V R8', 3, jorig)
        call getvem(noma, 'GROUP_NO', 'FOND_FISS', 'VECT_GRNO_ORIG', 1,&
                    iarg, 2, nomgrp, nveor)
!
        call jeveuo(jexnom(grpnoe, nomgrp(1)), 'L', iagrn)
        numer = zi(iagrn)
        xpfo = zr(jvale-1+3*(numer-1)+1)
        ypfo = zr(jvale-1+3*(numer-1)+2)
        zpfo = zr(jvale-1+3*(numer-1)+3)
!
        call jeveuo(jexnom(grpnoe, nomgrp(2)), 'L', iagrn)
        numer = zi(iagrn)
        xpfi = zr(jvale-1+3*(numer-1)+1)
        ypfi = zr(jvale-1+3*(numer-1)+2)
        zpfi = zr(jvale-1+3*(numer-1)+3)
        zr(jorig+0)=xpfi-xpfo
        zr(jorig+1)=ypfi-ypfo
        zr(jorig+2)=zpfi-zpfo
!
    endif
!
!     --------------------------------------------------------------
    call getvem(noma, 'GROUP_NO', 'FOND_FISS', 'VECT_GRNO_EXTR', 1,&
                iarg, 0, nomgrp, nveex)
    if (nveex .ne. 0) then
        nveex = -nveex
        call wkvect(resu//'.DTAN_EXTREMITE', 'G V R8', 3, jextr)
        call getvem(noma, 'GROUP_NO', 'FOND_FISS', 'VECT_GRNO_EXTR', 1,&
                    iarg, 2, nomgrp, nveex)
!
        call jeveuo(jexnom(grpnoe, nomgrp(1)), 'L', iagrn)
        numer = zi(iagrn)
        xpfo = zr(jvale-1+3*(numer-1)+1)
        ypfo = zr(jvale-1+3*(numer-1)+2)
        zpfo = zr(jvale-1+3*(numer-1)+3)
!
        call jeveuo(jexnom(grpnoe, nomgrp(2)), 'L', iagrn)
        numer = zi(iagrn)
        xpfi = zr(jvale-1+3*(numer-1)+1)
        ypfi = zr(jvale-1+3*(numer-1)+2)
        zpfi = zr(jvale-1+3*(numer-1)+3)
        zr(jextr+0)=xpfi-xpfo
        zr(jextr+1)=ypfi-ypfo
        zr(jextr+2)=zpfi-zpfo
!
    endif
!
!
! VERIFICATION DE L'ORTHOGONALITE DE LA NORMALE AU PLAN DES LEVRES
!  ET DES 2 DIRECTIONS TANGENTES
!
    if (nvenor .ne. 0 .and. ndtaor .ne. 0 .and. ndtaex .ne. 0) then
        call jeveuo(resu//'.NORMALE', 'L', jnorm)
        ps1=ddot(3,zr(jnorm),1,zr(jorig),1)
        ps2=ddot(3,zr(jnorm),1,zr(jextr),1)
        zero = r8prem()
        if (abs(ps1) .gt. zero) then
            valr(1) = zr(jnorm)
            valr(2) = zr(jnorm+1)
            valr(3) = zr(jnorm+2)
            valr(4) = zr(jorig)
            valr(5) = zr(jorig+1)
            valr(6) = zr(jorig+2)
            call u2mesr('F', 'RUPTURE0_78', 6, valr)
        endif
        if (abs(ps2) .gt. zero) then
            valr(1) = zr(jnorm)
            valr(2) = zr(jnorm+1)
            valr(3) = zr(jnorm+2)
            valr(4) = zr(jextr)
            valr(5) = zr(jextr+1)
            valr(6) = zr(jextr+2)
            call u2mesr('F', 'RUPTURE0_79', 6, valr)
        endif
    endif
!
!
    call jedema()
end subroutine
