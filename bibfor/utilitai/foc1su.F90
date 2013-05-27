subroutine foc1su(sortie, nbfon, nomfon, coef, coefz,&
                  type, ccplx, fcplx, lpara, base)
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/focoat.h'
    include 'asterfort/fointc.h'
    include 'asterfort/fointr.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/uttrir.h'
    include 'asterfort/wkvect.h'
    integer :: nbfon
    character(len=*) :: sortie, nomfon(*), lpara
    real(kind=8) :: coef(*)
    character(len=1) :: base
    complex(kind=8) :: coefz(*)
    logical :: ccplx(*), fcplx(*)
    character(len=*) :: type
!     ----------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     EFFECTUE LA COMBINAISON LINEAIRE DE FONCTION DE TYPE "FONCTION"
!     ----------------------------------------------------------------
!
    integer :: ip(2), ltp(2)
    real(kind=8) :: eps
    character(len=24) :: prol, vale, nomtem(2)
    character(len=16) :: nopara, noresu, interp, prolgd
    character(len=1) :: k1bid
!     ----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ier, iocc, iperm, isuite, lfon, lpro
    integer :: lpros, lres, lresf, ltres, lvar, nbinst
    integer :: nbpts, nbval, nn
    real(kind=8) :: aim, are, resuim, resure, vim, vre
!-----------------------------------------------------------------------
    data     nomtem/'&&FOC1SU.TEMPORAIRE1', '&&FOC1SU.TEMPORAIRE2'/
    data     ip    /2,1/
!     ----------------------------------------------------------------
!
!     --- DETERMINATION DE LA LISTE DES INSTANTS PRODUIT ---
    call jemarq()
    eps = 1.d-6
    iperm = 1
    nbinst = 0
    vale(20:24) = '.VALE'
    prol(20:24) = '.PROL'
    if (type .eq. 'FONCTION') then
        nn=2
    else
        nn=3
    endif
    if (lpara(1:1) .eq. ' ') then
        do 200 iocc = 1, nbfon
            vale(1:19) = nomfon(iocc)
            call jelira(vale, 'LONUTI', nbval, k1bid)
            call jeveuo(vale, 'L', lvar)
!
            if (fcplx(iocc)) then
                nbpts = nbval/3
            else
                nbpts = nbval/2
            endif
            isuite = ip(iperm)
            call wkvect(nomtem(isuite), 'V V R', nbpts+nbinst, ltp( isuite))
            do 210 i = 1, nbpts
                zr(ltp(isuite)+i-1) = zr(lvar+i-1)
210          continue
            do 220 i = 1, nbinst
                zr(ltp(isuite)+i-1+nbpts) = zr(ltp(iperm)+i-1)
220          continue
            nbinst = nbpts+nbinst
            call uttrir(nbinst, zr(ltp(isuite)), eps)
            if (iocc .gt. 1) call jedetr(nomtem(iperm))
            iperm = isuite
200      continue
!
!           --- CREATION DE L'OBJET DES INSTANTS ET DES VALEURS ---
        vale(1:19) = sortie
        call wkvect(vale, base//' V R', nn*nbinst, lres)
        do 300 i = 0, nbinst-1
            zr(lres+i) = zr(ltp(iperm)+i)
300      continue
        call jedetr(nomtem(iperm))
!
    else
        vale(1:19) = lpara
        call jelira(vale, 'LONUTI', nbinst, k1bid)
        call jeveuo(vale, 'L', lvar)
!
!           --- CREATION DE L'OBJET DES INSTANTS ET DES VALEURS ---
        vale(1:19) = sortie
        call wkvect(vale, base//' V R', nn*nbinst, lres)
        do 310 i = 0, nbinst-1
            zr(lres+i) = zr(lvar+i)
310      continue
    endif
!
!     --- SUPERPOSITION DES VALEURS ---
!
    call wkvect(nomtem(1), 'V V R', nbinst, ltres)
    lresf = lres + nbinst
    do 400 iocc = 1, nbfon
!
        prol(1:19) = nomfon(iocc)
        call jeveuo(prol, 'L', lpro)
!
        vale(1:19) = nomfon(iocc)
        call jelira(vale, 'LONUTI', nbval, k1bid)
        call jeveuo(vale, 'L', lvar)
!
        if (fcplx(iocc)) then
            nbpts = nbval/3
        else
            nbpts = nbval/2
        endif
        lfon = lvar + nbpts
        if (fcplx(iocc)) then
            do 405 i = 1, nbinst
                call fointc('F', nomfon(iocc), 0, ' ', zr(lres-1+i),&
                            resure, resuim, ier)
                if (ccplx(iocc)) then
                    vre=dble (coefz(iocc))
                    vim=dimag(coefz(iocc))
                    are=vre*resure-vim*resuim
                    aim=vre*resuim+vim*resure
                else
                    are=coef(iocc) * resure
                    aim=coef(iocc) * resuim
                endif
                zr(lresf+2*i-2) = zr(lresf+2*i-2) + are
                zr(lresf+2*i-1) = zr(lresf+2*i-1) + aim
405          continue
        else
            call fointr(nomfon(iocc), zk24(lpro), nbpts, zr(lvar), zr( lfon),&
                        nbinst, zr(lres), zr(ltres), ier)
            if (ccplx(iocc)) then
                do 420 i = 1, nbinst
                    vre=dble (coefz(iocc))
                    vim=dimag(coefz(iocc))
                    zr(lresf+2*i-2) = zr(lresf+2*i-2)+vre*zr(ltres+i- 1)
                    zr(lresf+2*i-1) = zr(lresf+2*i-1)+vim*zr(ltres+i- 1)
420              continue
            else
                do 410 i = 1, nbinst
                    zr(lresf+i-1) = zr(lresf+i-1) + coef(iocc) * zr( ltres+i-1)
410              continue
            endif
        endif
400  end do
    call jedetr(nomtem(1))
!
!     FIN DU REMPLISSAGE
    vale(1:19) = sortie
    call jeecra(vale, 'LONUTI', nn*nbinst, ' ')
!
    call focoat(nomfon, nbfon, nopara, noresu, interp,&
                prolgd)
!
!     --- CREATION DU .PROL ---
    prol(1:19) = sortie
    call assert(lxlgut(prol(1:19)).le.24)
    call wkvect(prol, base//' V K24', 6, lpros)
    zk24(lpros ) = type
    zk24(lpros+1) = interp
    zk24(lpros+2) = nopara
    zk24(lpros+3) = noresu
    zk24(lpros+4) = prolgd
    zk24(lpros+5) = prol
!
    call jedema()
end subroutine
