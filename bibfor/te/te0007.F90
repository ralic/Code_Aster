subroutine te0007(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/bsigmc.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nbsigm.h"
#include "asterfort/tecach.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! FONCTION REALISEE:  CALCUL DE L'OPTION FORC_NODA
!                       EN 2D POUR ELEMENTS NON LOCAUX A GRAD. DE DEF.
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
    real(kind=8) :: nharm, bsigm(18), geo(18)
! DEB ------------------------------------------------------------------
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
!-----------------------------------------------------------------------
    integer :: i, icomp, icontm, idepl, idfde, igeom, ipoids
    integer :: iretc, iretd, ivectu, ivf, jgano, kp, ku
    integer :: n, nbsig, ndim, ndimsi, nno, nnos, npg
!
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
    nharm = zero
!
! - SPECIFICATION DE LA DIMENSION
!
    if (lteatt('AXIS','OUI')) then
        ndim = 2
    else if (lteatt('C_PLAN','OUI')) then
        ndim = 2
    else if (lteatt('D_PLAN','OUI')) then
        ndim = 2
    endif
!
    ndimsi = ndim*2
!
! ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
!      -----------------------------------------
    nbsig = nbsigm()
!
! ---- PARAMETRES EN ENTREE
!      --------------------
! ----     COORDONNEES DES CONNECTIVITES
    call jevech('PGEOMER', 'L', igeom)
! ----     CONTRAINTES AUX POINTS D'INTEGRATION
    call jevech('PCONTMR', 'L', icontm)
!
!         CHAMPS POUR LA REACTUALISATION DE LA GEOMETRIE
    do 90 i = 1, ndim*nno
        geo(i) =zr(igeom-1+i)
90  end do
    call tecach('ONO', 'PDEPLMR', 'L', iretd, iad=idepl)
    call tecach('ONO', 'PCOMPOR', 'L', iretc, iad=icomp)
    if ((iretd.eq.0) .and. (iretc.eq.0)) then
        if (zk16(icomp+2)(1:6) .ne. 'PETIT ') then
            do 80 i = 1, ndim*nno
                geo(i) =geo(i) + zr(idepl-1+i)
80          continue
        endif
    endif
! ---- PARAMETRES EN SORTIE
!      --------------------
! ----     VECTEUR DES FORCES INTERNES (BT*SIGMA)
    call jevech('PVECTUR', 'E', ivectu)
!
!
! ---- CALCUL DU VECTEUR DES FORCES INTERNES (BT*SIGMA) :
!      --------------------------------------------------
    call bsigmc(nno, ndim, nbsig, npg, ipoids,&
                ivf, idfde, zr(igeom), nharm, zr(icontm),&
                bsigm)
!
! ---- AFFECTATION DU VECTEUR EN SORTIE :
!      ----------------------------------
    do 10 n = 1, nnos
        do 20 i = 1, ndim
            ku = (ndimsi + ndim)*(n-1) + i
            kp = ndim*(n-1) + i
            zr(ivectu+ku-1) = bsigm(kp)
20      continue
        do 30 i = 1, ndimsi
            ku = (ndimsi + ndim)*(n-1) + i + ndim
            zr(ivectu+ku-1) = 0.d0
30      continue
10  continue
    do 40 n = nnos+1, nno
        do 50 i = 1, ndim
            ku = (ndimsi + ndim)*nnos + ndim*(n-nnos-1) + i
            kp = ndim*(n-1) + i
            zr(ivectu+ku-1) = bsigm(kp)
50      continue
40  continue
!
! FIN ------------------------------------------------------------------
end subroutine
