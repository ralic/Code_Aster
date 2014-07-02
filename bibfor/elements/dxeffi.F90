subroutine dxeffi(option, nomte, pgl, cont, ind,&
                  effint)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/dxdmul.h"
#include "asterfort/dxmate.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
    real(kind=8) :: pgl(3, 3), cont(*), effint(*)
    character(len=16) :: nomte
    character(len=*) :: option
    integer :: ind
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     IN  NOMTE  : NOM DE L'ELEMENT TRAITE
!     IN  XYZL   : COORDONNEES DES NOEUDS
!     IN  UL     : DEPLACEMENT A L'INSTANT T
!     IN  IND    : =6 : 6 CMP D'EFFORT PAR NOEUD
!     IN  IND    : =8 : 8 CMP D'EFFORT PAR NOEUD
!     OUT EFFINT : EFFORTS INTERNES
!     ------------------------------------------------------------------
!
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: nbcon, nbcou, npgh, k, ipg, icou, igauh, icpg, icacoq, jnbspi
    real(kind=8) :: hic, h, zic, zmin, coef, zero, deux, distn, coehsd
    real(kind=8) :: n(3), m(3), t(2)
!
    integer :: multic, iniv
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2), dfc(3, 2)
    real(kind=8) :: t2iu(2, 2), t2ui(2, 2), t1ve(3, 3)
    real(kind=8) :: hm(3, 3)
    real(kind=8) :: d1i(2, 2), d2i(2, 4)
    aster_logical :: coupmf
!     ------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jcoopg=icoopg, jvf=ivf, jdfde=idfdx, jdfd2=idfd2,&
                     jgano=jgano)
!
    zero = 0.0d0
    deux = 2.0d0
!
!     RECUPERATION DES OBJETS &INEL ET DES CHAMPS PARAMETRES :
!     --------------------------------------------------------
    if (nomte .ne. 'MEDKTR3 ' .and. nomte .ne. 'MEDSTR3 ' .and. nomte .ne. 'MEDKQU4 ' .and.&
        nomte .ne. 'MEDSQU4 ' .and. nomte .ne. 'MEQ4QU4 ' .and. nomte .ne. 'MET3TR3 ') then
        call utmess('F', 'ELEMENTS_34', sk=nomte)
    endif
!
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcon = 6
    nbcou = zi(jnbspi-1+1)
    if (nbcou .le. 0) then
        call utmess('F', 'ELEMENTS_46')
    endif
!
!
    multic = 0
    if (option .eq. 'FORC_NODA') then
!     ----- CARACTERISTIQUES DES MATERIAUX --------
        call dxmate('RIGI', df, dm, dmf, dc,&
                    dci, dmc, dfc, nno, pgl,&
                    multic, coupmf, t2iu, t2ui, t1ve)
    endif
!
!     -- GRANDEURS GEOMETRIQUES :
!     ---------------------------
    npgh = 3
    if (multic .eq. 0) then
        call jevech('PCACOQU', 'L', icacoq)
        h = zr(icacoq)
        hic = h/nbcou
        distn = zr(icacoq+4)
        zmin = -h/deux + distn
    endif
!
    call r8inir(32, zero, effint, 1)
!
!===============================================================
!     -- BOUCLE SUR LES POINTS DE GAUSS DE LA SURFACE:
!     -------------------------------------------------
    do 100 ipg = 1, npg
        call r8inir(3, zero, n, 1)
        call r8inir(3, zero, m, 1)
        call r8inir(2, zero, t, 1)
!
        do 110 icou = 1, nbcou
            do 120 igauh = 1, npgh
                icpg = nbcon*npgh*nbcou*(ipg-1) + nbcon*npgh*(icou-1) + nbcon*(igauh-1)
!
                if (igauh .eq. 1) then
                    zic = zmin + (icou-1)*hic
                    coef = 1.d0/3.d0
                else if (igauh.eq.2) then
                    zic = zmin + hic/2.d0 + (icou-1)*hic
                    coef = 4.d0/3.d0
                else
                    zic = zmin + hic + (icou-1)*hic
                    coef = 1.d0/3.d0
                endif
                if (multic .gt. 0) then
                    iniv = igauh - 2
                    call dxdmul(.false._1, icou, iniv, t1ve, t2ui,&
                                hm, d1i, d2i, zic, hic)
                endif
!
!         -- CALCUL DES EFFORTS GENERALISES DANS L'EPAISSEUR (N, M ET T)
!         --------------------------------------------------------------
                coehsd = coef*hic/2.d0
                n(1) = n(1) + coehsd*cont(icpg+1)
                n(2) = n(2) + coehsd*cont(icpg+2)
                n(3) = n(3) + coehsd*cont(icpg+4)
                m(1) = m(1) + coehsd*zic*cont(icpg+1)
                m(2) = m(2) + coehsd*zic*cont(icpg+2)
                m(3) = m(3) + coehsd*zic*cont(icpg+4)
                t(1) = t(1) + coehsd*cont(icpg+5)
                t(2) = t(2) + coehsd*cont(icpg+6)
!
120         continue
110     continue
!
        do 140 k = 1, 3
            effint((ipg-1)*ind+k) = n(k)
            effint((ipg-1)*ind+k+3) = m(k)
140     continue
        if (ind .gt. 6) then
            effint((ipg-1)*ind+7) = t(1)
            effint((ipg-1)*ind+8) = t(2)
        endif
!
100 end do
!
end subroutine
