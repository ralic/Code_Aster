subroutine vecind(mat, lvec, nbl, nbc, force,&
                  nindep)
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
!
!--------------------------------------------------------------------C
!  M. CORUS     DATE 09/06/11
!-----------------------------------------------------------------------
!  BUT : CONSTRUCTION D'UNE BASE A PARTIR DE VECTEURS QUELCONQUES :
!         - SELECTION D'UNE FAMILLE LIBRE
!         - ORTHONORMALISATION DE LA FAMILLE LIBRE
!
!
!  MAT     /I/   : NOM K19 DE LA MATRICE POUR CONSTRUIRE LA NORME
!  LVEC    /I-O/ : POINTEUR DE LA FAMILLE DE VECTEURS
!  NBL     /I/   : NOMBRE DE LIGNE DE CHAQUE VECTEUR
!  NBC     /I-O/ : NOMBRE DE VECTEURS
!  FORCE   /I/   : FORCE LA NORMALISATION SI VAUT 1
!  NINDEP  /O/   : NOMBRE DE VECTEUR INDEPENDANT EN SORTIE
!
!  NOTE : LA TAILLE DE LA MATRICE ASSOCIE A LVEC EST INCHANGEE EN SORTIE
!         MAIS LES DERNIERES COLONNES SONT MISES A ZERO
!-----------------------------------------------------------------------
!
!
!
!
!
#include "jeveux.h"
#include "asterc/matfpe.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lceqvn.h"
#include "asterfort/mrmult.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/daxpy.h"
#include "blas/ddot.h"
#include "blas/dgemm.h"
#include "blas/dgesvd.h"
    integer :: lvec, nbl, nbc, nindep, lwork,  lmat, ltrav1
    integer ::   i1, k1, l1, iret, lcopy, force, indnz
    integer(kind=4) :: info
    real(kind=8) :: swork(1), norme, sqrt, rij
    character(len=8) :: ortho
    character(len=19) :: mat, nume
    real(kind=8), pointer :: mat_svd_work(:) => null()
    real(kind=8), pointer :: new_stat(:) => null()
    real(kind=8), pointer :: trav2_u(:) => null()
    real(kind=8), pointer :: trav3_v(:) => null()
    integer, pointer :: vec_ind_nz(:) => null()
    integer, pointer :: deeq(:) => null()
!
    AS_ALLOCATE(vr=new_stat, size=nbc*nbc)
    call wkvect('&&VECIND.TRAV1', 'V V R', nbl, ltrav1)
    AS_ALLOCATE(vi=vec_ind_nz, size=nbc)
    indnz=0
    if (mat .ne. ' ') then
        call jeveuo(mat//'.&INT', 'L', lmat)
        call dismoi('NOM_NUME_DDL', mat(1:8), 'MATR_ASSE', repk=nume)
        call jeveuo(nume(1:8)//'      .NUME.DEEQ', 'L', vi=deeq)
    endif
!
!-- NORMER LES MODES DANS L2 AVANT DE CONSTRUIRE LA MATRICE
!-- POUR PLUS DE ROBUSTESSE
!
    call wkvect('&&VECIND.VECTEURS_COPIES', 'V V R', nbl*nbc, lcopy)
    call wkvect('&&VECIND.VECTEURS_TEMP', 'V V R', nbl, ltrav1)
!
    do i1 = 1, nbc
        if (mat .ne. ' ') then
            call zerlag(nbl, deeq, vectr=zr(lvec+nbl*(i1-1)))
            call mrmult('ZERO', lmat, zr(lvec+nbl*(i1-1)), zr(ltrav1), 1,&
                        .true._1)
            call zerlag(nbl, deeq, vectr=zr(ltrav1))
            norme=ddot(nbl,zr(ltrav1),1,zr(lvec+nbl*(i1-1)),1)
!
        else
            norme=ddot(nbl,zr(lvec+nbl*(i1-1)),1,zr(lvec+nbl*(i1-1)),&
            1)
        endif
        norme=sqrt(norme)
        if (norme .gt. 1.d-16) then
            call daxpy(nbl, 1/norme, zr(lvec+nbl*(i1-1)), 1, zr(lcopy+ nbl*(i1-1)),&
                       1)
!        ELSE
!          CALL DAXPY(NBL,0.D0,ZR(LVEC+NBL*(I1-1)),1,
!     &               ZR(LCOPY+NBL*(I1-1)),1)
        endif
    end do
!
    do l1 = 1, nbc
        if (mat .ne. ' ') then
            call mrmult('ZERO', lmat, zr(lcopy+nbl*(l1-1)), zr(ltrav1), 1,&
                        .true._1)
        else
            call lceqvn(nbl, zr(lcopy+nbl*(l1-1)), zr(ltrav1))
        endif
        norme=ddot(nbl,zr(ltrav1),1,zr(lcopy+nbl*(l1-1)),1)
        new_stat(1+(l1-1)*(nbc+1))=norme
        do k1 = l1+1, nbc
            rij=ddot(nbl,zr(ltrav1),1,zr(lcopy+nbl*(k1-1)),1)
            new_stat(1+(l1-1)*nbc+k1-1)=rij
            new_stat(1+(k1-1)*nbc+l1-1)=rij
        end do
    end do
!
    if (force .ne. 1) then
!-- UTILISE APRES LE GRAM SCHMIDT DANS ORTH99, POUR
!-- ELIMINER LES VECTEURS NON INDEPENDANTS
!
        do l1 = 1, nbc
            norme=new_stat(1+(l1-1)*(nbc+1))
!
            if (norme .gt. 1.d-16) then
                do k1 = l1+1, nbc
                    rij=abs(new_stat(1+(l1-1)*nbc+k1-1))
                    rij=rij/norme
                    if (rij .gt. 1.d-8) then
                        write(6,*)' ... ANNULATION DU VECTEUR ',k1
                        do i1 = 1, nbl
                            zr(lvec+((k1-1)*nbl)+i1-1)=0.d0
                        end do
                        do i1 = 1, nbc
                            new_stat(1+((k1-1)*nbc)+i1-1)=0.d0
                            new_stat(1+((i1-1)*nbc)+k1-1)=0.d0
                        end do
                    endif
                end do
            endif
!
        end do
!
        call getvtx('  ', 'ORTHO', iocc=1, nbval=8, vect=ortho,&
                    nbret=iret)
        if ((iret .eq. 1) .and. (ortho.eq.'OUI')) then
!-- SELECTION DES VECTEURS NON NULS POUR REMPLIR LA BASE
            do i1 = 1, nbc
                if (new_stat(1 + (i1-1)*(nbc+1) ) .gt. 1d-10) then
                    vec_ind_nz(indnz+1)=i1
                    indnz=indnz+1
                endif
            end do
!
            do i1 = 1, indnz
                l1=vec_ind_nz(i1)
                if (i1 .ne. l1) then
                    call lceqvn(nbl, zr(lvec+nbl*(l1-1)), zr(lvec+nbl*( i1-1)))
                endif
            end do
            nbc=indnz
        endif
    else
!
!-- ALLOCATION DES MATRICES DE TRAVAIL TEMPORAIRES
        call wkvect('&&VECIND.TRAV1_S', 'V V R', nbc, ltrav1)
        AS_ALLOCATE(vr=trav2_u, size=nbc*nbc)
        AS_ALLOCATE(vr=trav3_v, size=nbc*nbc)
!
!-- DESACTIVATION DU TEST FPE
        call matfpe(-1)
!
        call dgesvd('A', 'N', nbc, nbc, new_stat,&
                    nbc, zr(ltrav1), trav2_u, nbc, trav3_v,&
                    nbc, swork, -1, info)
        lwork=int(swork(1))
        AS_ALLOCATE(vr=mat_svd_work, size=lwork)
!
        call dgesvd('A', 'N', nbc, nbc, new_stat,&
                    nbc, zr(ltrav1), trav2_u, nbc, trav3_v,&
                    nbc, mat_svd_work, lwork, info)
!
        nindep=0
        norme=(nbc+0.d0)*zr(ltrav1)*1.d-16
        do k1 = 1, nbc
            if (zr(ltrav1+k1-1) .gt. norme) nindep=nindep+1
        end do
!
        call wkvect('&&VECIND.MODE_INTF_DEPL', 'V V R', nbl*nbc, lmat)
!
        call dgemm('N', 'N', nbl, nindep, nbc,&
                   1.d0, zr(lcopy), nbl, trav2_u, nbc,&
                   0.d0, zr(lvec), nbl)
!
!-- INUTILE D'ANNULER DES VECTEURS QUI NE SERVIRONT NUL PART...
!        DO 540 I1=1,NBL*(NBC-NINDEP)
!          ZR(LVEC+(NINDEP*NBL)+I1-1)=0.D0
! 540    CONTINUE
!
        call matfpe(1)
!
        AS_DEALLOCATE(vr=mat_svd_work)
        call jedetr('&&VECIND.TRAV1_S')
        AS_DEALLOCATE(vr=trav2_u)
        AS_DEALLOCATE(vr=trav3_v)
        call jedetr('&&VECIND.MODE_INTF_DEPL')
!
    endif
!
!
!
    AS_DEALLOCATE(vr=new_stat)
    call jedetr('&&VECIND.TRAV1')
    call jedetr('&&VECIND.VECTEURS_COPIES')
    call jedetr('&&VECIND.VECTEURS_TEMP')
    AS_DEALLOCATE(vi=vec_ind_nz)
!
end subroutine
