subroutine intdis(coint, nnoint, noddli, ddlsst, nbsst)
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    M. CORUS     DATE 05/02/10
!-----------------------------------------------------------------------
!
!  BUT:      < DETERMINATION DES PARTIES D'INTERFACES DISJOINTES >
!
!-----------------------------------------------------------------------
!  IN  : COINT  : DEFINITION DE LA CONNECTIVITE DE L'INTERFACE
!  IN  : NNOINT  : NOMBRE DE NOEUD A L'INTERFACE
!  IN  : NODDLI : DEFINITION DES DDL PORTES PAR LES NOEUDS D'INTERFACE
!  OUT : DDLSST   : DEFINITION DES DDL POUR CHAQUE PARTIE D'INTERFACE
!  OUT : NBSST    : NOMBRE DE PARTIE D'INTERFACE DISJOINTES
!-----------------------------------------------------------------------
!
!
!
!
!
!     ------------------------------------------------------------------
!
!-- VARIABLES EN ENTREES / SORTIE
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: nnoint, nbsst
    character(len=24) :: coint, noddli, ddlsst
!
!-- VARIABLES DE LA ROUTINE
    integer :: i1, j1, k1, n1, l1,   lconnc
    integer ::  nz0, nz1, lindin, decal, nbno, nbvois, no, lnddli
    real(kind=8), pointer :: defi_ss_lib(:) => null()
    integer, pointer :: numero_noeuds(:) => null()
    integer, pointer :: vect_ind_mat(:) => null()
    integer, pointer :: vect_indsst(:) => null()
    real(kind=8), pointer :: vect_temp(:) => null()
    integer, pointer :: ind_noeud(:) => null()
!
!-----------C
!--       --C
!-- DEBUT --C
!--       --C
!-----------C
!
    call jemarq()
!
!-- CONSTRUCTION DE LA CONNECTIVITE REDUITE
!
    call jeveuo('&&MOIN93.IND_NOEUD', 'L', vi=ind_noeud)
    AS_ALLOCATE(vr=defi_ss_lib, size=nnoint**2)
    call jeveuo(coint, 'L', lconnc)
!
    do 10 i1 = 1, nnoint
        nbvois=zi(lconnc+i1-1)
        do 20 k1 = 1, nbvois
            no=zi(lconnc+nnoint*k1+i1-1)
            j1=ind_noeud(no)
            defi_ss_lib(1+(j1-1)*nnoint+i1-1)=1.d0
            defi_ss_lib(1+(j1-1)*nnoint+j1-1)=1.d0
            defi_ss_lib(1+(i1-1)*nnoint+i1-1)=1.d0
            defi_ss_lib(1+(i1-1)*nnoint+j1-1)=1.d0
20      continue
10  end do
!
    AS_ALLOCATE(vi=numero_noeuds, size=nnoint)
!
    do 30 i1 = 1, nnoint
        numero_noeuds(i1)=i1
30  end do
!
    AS_ALLOCATE(vr=vect_temp, size=nnoint)
    AS_ALLOCATE(vi=vect_ind_mat, size=nnoint)
    AS_ALLOCATE(vi=vect_indsst, size=nnoint)
!
!-- INITIALISATION
!
    decal=0
    nbsst=0
    nbno=0
    vect_indsst(1)=1
!
!-- RECHERCHE DES PARTIES DISJOINTES
!
!      DO WHILE (NBNO .LT. NNOINT)
666  continue
    nz0=0
    k1=1
!        DO WHILE (NZ0 .EQ. 0)
667  continue
    if (numero_noeuds(k1) .gt. 0) then
        nz0=1
        vect_ind_mat(decal+1)=k1
    endif
    k1=k1+1
    if (nz0 .eq. 0) then
        goto 667
    endif
!        END DO
!
    nz1=1
!        DO WHILE (NZ1 .GT. NZ0)
668  continue
    nz0=nz1
    do 40 j1 = 1, nz1
        do 50 i1 = 1, nnoint
            vect_temp(i1)=vect_temp(i1)+ defi_ss_lib(1+(vect_ind_mat(1+decal+j1-&
            1)-1)*nnoint+i1-1)
50      continue
40  continue
!
    nz1=0
    do 60 i1 = 1, nnoint
        if (vect_temp(i1) .gt. 0.d0) then
            nz1=nz1+1
            vect_ind_mat(1+decal+nz1-1)=i1
            numero_noeuds(i1)=0
            vect_temp(i1)=0.d0
        endif
60  continue
!
    if (nz1 .gt. nz0) then
        goto 668
    endif
!        END DO
!
    nbsst=nbsst+1
    decal=decal+nz1
    nbno=nbno+nz1
    vect_indsst(1+2*nbsst-1)=decal
    vect_indsst(1+2*nbsst)=nbno+1
!
    if (nbno .lt. nnoint) then
        goto 666
    endif
!      END DO
!
    call jeveuo(noddli, 'L', lnddli)
    call wkvect(ddlsst, 'V V I', nbsst*6*nnoint, lindin)
    do 70 i1 = 1, nbsst
        k1=vect_indsst(1+2*(i1-1))
        l1=vect_indsst(1+2*(i1-1)+1)
!
        do 80 j1 = k1, l1
            do 90 n1 = 1, 6
                zi(lindin+6*nnoint*(i1-1)+ 6*(vect_ind_mat(j1)-1)+n1-1 )&
                = 1
90          continue
80      continue
70  end do
!
!----------------------------------------C
!--                                    --C
!-- DESTRUCTION DES OBJETS TEMPORAIRES --C
!--                                    --C
!----------------------------------------C
!
    AS_DEALLOCATE(vr=defi_ss_lib)
    AS_DEALLOCATE(vi=numero_noeuds)
    AS_DEALLOCATE(vr=vect_temp)
    AS_DEALLOCATE(vi=vect_ind_mat)
    AS_DEALLOCATE(vi=vect_indsst)
!
!---------C
!--     --C
!-- FIN --C
!--     --C
!---------C
!
    call jedema()
end subroutine
