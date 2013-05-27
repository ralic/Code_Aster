subroutine te0008(option, nomte)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/bsigmc.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/nbsigm.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tecach.h'
    include 'asterfort/terefe.h'
    include 'blas/daxpy.h'
    character(len=16) :: option, nomte
!
! ----------------------------------------------------------------------
!
! ELEMENTS ISOPARAMETRIQUES 2D ET 3D
!
! CALCUL DES OPTIONS FORC_NODA ET REFE_FORC_NODA
!
! ----------------------------------------------------------------------
!
!
! IN  OPTION : OPTION DE CALCUL
! IN  NOMTE  : NOM DU TYPE ELEMENT
!
!
!
!
    real(kind=8) :: zero, sigref
    real(kind=8) :: nharm, bsigm(81), geo(81), sigtmp(162), ftemp(81)
    integer :: nbsig, ndim, nno, nnos, npg1
    integer :: ipoids, ivf, idfde, jgano
    integer :: igeom, ivectu
    integer :: idepl, icomp, icontm
    integer :: i, j, iretd, iretc
!
! ----------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
! --- INITIALISATIONS
!
    zero = 0.d0
    nharm = zero
    call assert(nno*ndim.le.81)
!
! --- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
!
    nbsig = nbsigm()
!
! --- PARAMETRE EN SORTIE: VECTEUR DES FORCES INTERNES (BT*SIGMA)
!
    call jevech('PVECTUR', 'E', ivectu)
!
! --- PARAMETRE EN ENTREE: GEROMETRIE
!
    call jevech('PGEOMER', 'L', igeom)
    do 30 i = 1, ndim*nno
        geo(i) = zr(igeom-1+i)
30  end do
!
    if (option .eq. 'FORC_NODA') then
        call tecach('ONN', 'PDEPLMR', 'L', 1, idepl,&
                    iretd)
        call tecach('ONN', 'PCOMPOR', 'L', 1, icomp,&
                    iretc)
        if ((iretd.eq.0) .and. (iretc.eq.0)) then
            if (zk16(icomp+2)(1:6) .ne. 'PETIT ') then
                do 20 i = 1, ndim*nno
                    geo(i) = geo(i) + zr(idepl-1+i)
20              continue
            endif
        endif
!
! ----- CONTRAINTES AUX POINTS D'INTEGRATION
!
        call jevech('PCONTMR', 'L', icontm)
!
! ----- CALCUL DU VECTEUR DES FORCES INTERNES (BT*SIGMA)
!
        call bsigmc(nno, ndim, nbsig, npg1, ipoids,&
                    ivf, idfde, geo, nharm, zr(icontm),&
                    bsigm)
!
! ----- AFFECTATION DU VECTEUR EN SORTIE
!
        do 10 i = 1, ndim*nno
            zr(ivectu+i-1) = bsigm(i)
10      continue
!
    else if (option.eq.'REFE_FORC_NODA') then
!
        call terefe('SIGM_REFE', 'MECA_ISO', sigref)
!
        call r8inir(nbsig*npg1, 0.d0, sigtmp, 1)
        call r8inir(ndim*nno, 0.d0, ftemp, 1)
!
        do 200 i = 1, nbsig*npg1
!
            sigtmp(i) = sigref
            call bsigmc(nno, ndim, nbsig, npg1, ipoids,&
                        ivf, idfde, geo, nharm, sigtmp,&
                        bsigm)
!
            do 21 j = 1, ndim*nno
                ftemp(j) = ftemp(j)+abs(bsigm(j))
21          continue
!
            sigtmp(i) = 0.d0
!
200      continue
!
        call daxpy(ndim*nno, 1.d0/npg1, ftemp, 1, zr(ivectu),&
                   1)
!
    endif
!
end subroutine
