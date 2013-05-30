subroutine te0395(option, nomte)
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
! ======================================================================
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/nmasf3.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tecach.h'
    include 'asterfort/terefe.h'
    include 'blas/daxpy.h'
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! FONCTION REALISEE:  CALCUL DE L'OPTION FORC_NODA ELEMENT HEXAS8
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
    real(kind=8) :: bsigm(3, 8), geo(24), sigtmp(6), ftemp(24), sigref
    integer :: jgano, nno, k, npg1, i, j, ivectu, ndim, nnos
    integer :: ipoids, ivf, idfde, igeom, icontm, imate, idepl
    integer :: icomp, ii, iretc, iretd
! DEB ------------------------------------------------------------------
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
! ---- PARAMETRES EN ENTREE
! ----     COORDONNEES DES CONNECTIVITES
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    do 10 i = 1, ndim*nno
        geo(i) = zr(igeom-1+i)
10  end do
!
! ---- PARAMETRES EN SORTIE
!      --------------------
! ----     VECTEUR DES FORCES INTERNES (BT*SIGMA)
    call jevech('PVECTUR', 'E', ivectu)
!
! ---- CALCUL DE FORC_NODA
    call tecach('ONN', 'PCOMPOR', 'L', 1, icomp,&
                iretc)
!
    if (option .eq. 'FORC_NODA') then
!      --------------------
!         CHAMPS POUR LA REACTUALISATION DE LA GEOMETRIE
        call tecach('ONN', 'PDEPLMR', 'L', 1, idepl,&
                    iretd)
        if ((iretd.eq.0) .and. (iretc.eq.0)) then
            if (zk16(icomp+2) (1:6) .ne. 'PETIT ') then
                do 20 i = 1, ndim*nno
                    geo(i) = geo(i) + zr(idepl-1+i)
20              continue
            endif
        endif
! ----     CONTRAINTES AUX POINTS D'INTEGRATION
        call jevech('PCONTMR', 'L', icontm)
!
! ---- CALCUL DU VECTEUR DES FORCES INTERNES (BT*SIGMA) :
!      --------------------------------------------------
        call nmasf3(nno, npg1, ipoids, ivf, idfde,&
                    zi(imate), geo, zr( idepl), zr(icontm), zr(ivectu),&
                    zk16(icomp))
!
!
    else if (option.eq.'REFE_FORC_NODA') then
        call terefe('SIGM_REFE', 'MECA_ISO', sigref)
!
        call tecach('ONN', 'PDEPLMR', 'L', 1, idepl,&
                    iretd)
!
        call r8inir(6*npg1, 0.d0, sigtmp, 1)
        call r8inir(3*nno, 0.d0, ftemp, 1)
        do 50 i = 1, 6*npg1
!
            sigtmp(i) = sigref
            call nmasf3(nno, npg1, ipoids, ivf, idfde,&
                        zi(imate), geo, zr(idepl), sigtmp, bsigm,&
                        zk16(icomp))
!
            do 40 j = 1, nno
                ii = 3*(j-1)
                do 41 k = 1, 3
                    ftemp(ii+k) = ftemp(ii+k) + abs(bsigm(k,j))
41              continue
40          continue
!
50      continue
!
        call daxpy(ndim*nno, 1.d0/npg1, ftemp, 1, zr(ivectu),&
                   1)
!
    endif
!
! FIN ------------------------------------------------------------------
end subroutine
