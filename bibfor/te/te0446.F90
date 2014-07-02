subroutine te0446(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/coqrep.h"
#include "asterfort/dxbsig.h"
#include "asterfort/dxefro.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/terefe.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "blas/dcopy.h"
!     ----------------------------------------------------------------
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
    character(len=16) :: option, nomte
!
!   CALCUL DES OPTIONS DES ELEMENTS DE PLAQUE POUR LA MODELISATION DKTG
!   ET LA MODELISATION Q4GG
!     -----------------------------------------------------------------
!                            TRIANGLE  QUADRANGLE
!        KIRCHOFF  (MINCE)      DKT       DKQ
!
!                  (EPAIS)      Q4G       T3G
!
!        OPTIONS     FORC_NODA
!
! person_in_charge: sebastien.fayolle at edf.fr
!
    integer :: nnos, ipoids, ivf, idfdx, jgano
    integer :: jtab(7), ideplm, ideplp
    integer :: icompo, i, i1, i2, j, k, ivectu, ipg, npg
    integer :: icontm, iretc
    integer :: nno, igeom
    integer :: ndim, iret, ind
    integer :: jcara
    real(kind=8) :: pgl(3, 3), xyzl(3, 4), bsigma(24)
    real(kind=8) :: effgt(32), effort(32)
    real(kind=8) :: effref, momref
    real(kind=8) :: alpha, beta, t2ev(4), t2ve(4), c, s
    real(kind=8) :: foref, moref
    aster_logical :: reactu
!
    if (option .eq. 'FORC_NODA') then
!
! ---   RECUPERATION DES ADRESSES DANS ZR DES POIDS DES PG
!       DES FONCTIONS DE FORME DES VALEURS DES DERIVEES DES FONCTIONS
!       DE FORME ET DE LA MATRICE DE PASSAGE GAUSS -> NOEUDS
        call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                         jpoids=ipoids, jvf=ivf, jdfde=idfdx, jgano=jgano)
!
        call jevech('PGEOMER', 'L', igeom)
!
        if (nno .eq. 3) then
            call dxtpgl(zr(igeom), pgl)
        else if (nno.eq.4) then
            call dxqpgl(zr(igeom), pgl, 'S', iret)
        endif
!
        call utpvgl(nno, 3, pgl, zr(igeom), xyzl)
!
        call tecach('ONN', 'PCOMPOR', 'L', iretc, iad=icompo)
!
! --- CALCUL DES MATRICES DE CHANGEMENT DE REPERES
!
!     T2EV : LA MATRICE DE PASSAGE (2X2) : UTILISATEUR -> INTRINSEQUE
!     T2VE : LA MATRICE DE PASSAGE (2X2) : INTRINSEQUE -> UTILISATEUR
!
        call jevech('PCACOQU', 'L', jcara)
        alpha = zr(jcara+1) * r8dgrd()
        beta = zr(jcara+2) * r8dgrd()
        call coqrep(pgl, alpha, beta, t2ev, t2ve,&
                    c, s)
!
! --- VECTEUR DES EFFORTS GENERALISES AUX POINTS
! --- D'INTEGRATION DU REPERE LOCAL
        call tecach('OON', 'PCONTMR', 'L', iret, nval=7,&
                    itab=jtab)
!
! --- PASSAGE DU VECTEUR DES EFFORTS GENERALISES AUX POINTS
! --- D'INTEGRATION DU REPERE LOCAL AU REPERE INTRINSEQUE
        do ipg = 1, npg
            icontm=jtab(1)+8*(ipg-1)
            call dcopy(8, zr(icontm), 1, effort(8*(ipg-1)+1), 1)
        end do
        call dxefro(npg, t2ve, effort, effgt)
!
        reactu = .false.
        if (iretc .eq. 0) then
            if (zk16(icompo+2)(6:10) .eq. '_REAC') call utmess('A', 'ELEMENTS2_72')
            reactu = ( zk16(icompo+2) .eq. 'PETIT_REAC' .or. zk16(icompo+ 2) .eq. 'GROT_GDEP' )
        endif
!
        if (reactu) then
            call jevech('PDEPLMR', 'L', ideplm)
            call jevech('PDEPLPR', 'L', ideplp)
            do i = 1, nno
                i1 = 3* (i-1)
                i2 = 6* (i-1)
                zr(igeom+i1) = zr(igeom+i1) + zr(ideplm+i2) + zr( ideplp+i2)
                zr(igeom+i1+1) = zr(igeom+i1+1) + zr(ideplm+i2+1) + zr(ideplp+i2+1)
                zr(igeom+i1+2) = zr(igeom+i1+2) + zr(ideplm+i2+2) + zr(ideplp+i2+2)
            end do
            if (nno .eq. 3) then
                call dxtpgl(zr(igeom), pgl)
            else if (nno.eq.4) then
                call dxqpgl(zr(igeom), pgl, 'S', iret)
            endif
!
            call utpvgl(nno, 3, pgl, zr(igeom), xyzl)
        endif
!
! --- CALCUL DES EFFORTS INTERNES (I.E. SOMME_VOL(BT_SIG))
        call dxbsig(nomte, xyzl, pgl, effgt, bsigma,&
                    option)
!
! --- AFFECTATION DES VALEURS DE BSIGMA AU VECTEUR EN SORTIE
        call jevech('PVECTUR', 'E', ivectu)
!
        k = 0
        do i = 1, nno
            do j = 1, 6
                k = k + 1
                zr(ivectu+k-1) = bsigma(k)
            end do
        end do
    else if (option.eq.'REFE_FORC_NODA') then
!     -------------------------------------
!
        call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                         jpoids=ipoids, jvf=ivf, jdfde=idfdx, jgano=jgano)
        call jevech('PGEOMER', 'L', igeom)
!
        if (nno .eq. 3) then
            call dxtpgl(zr(igeom), pgl)
        else if (nno.eq.4) then
            call dxqpgl(zr(igeom), pgl, 'S', iret)
        endif
!
        call utpvgl(nno, 3, pgl, zr(igeom), xyzl)
!
        call terefe('EFFORT_REFE', 'MECA_COQUE', foref)
        call terefe('MOMENT_REFE', 'MECA_COQUE', moref)
!
        ind=8
        do i = 1, nno
            do j = 1, 3
                effgt((i-1)*ind+j) = foref
                effgt((i-1)*ind+3+j) = moref
                effgt((i-1)*ind+7) = foref
                effgt((i-1)*ind+8) = foref
            enddo
        enddo
!
! ------ CALCUL DES EFFORTS INTERNES (I.E. SOMME_VOL(BT_SIG))
        call dxbsig(nomte, xyzl, pgl, effgt, bsigma,&
                    option)
!
! ------ AFFECTATION DES VALEURS DE BSIGMA AU VECTEUR EN SORTIE
        call jevech('PVECTUR', 'E', ivectu)
        k=0
        do i = 1, nno
            effref=(abs(bsigma(k+1))+abs(bsigma(k+2))+abs(bsigma(k+3)))/3.d0
            momref=(abs(bsigma(k+4))+abs(bsigma(k+5))+abs(bsigma(k+6)))/3.d0
            ASSERT(abs(effref).gt.r8prem())
            ASSERT(abs(momref).gt.r8prem())
            do j = 1, 6
                k=k+1
                if (j .lt. 4) then
                    zr(ivectu+k-1) = effref
                else
                    zr(ivectu+k-1) = momref
                endif
            enddo
        enddo
    else
        ASSERT(.false.)
    endif
end subroutine
