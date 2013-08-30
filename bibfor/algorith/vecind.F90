subroutine vecind(mat, lvec, nbl, nbc, force,&
                  nindep)
    implicit none
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
#include "asterc/getvtx.h"
#include "asterc/matfpe.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lceqvn.h"
#include "asterfort/mrmult.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/daxpy.h"
#include "blas/ddot.h"
#include "blas/dgemm.h"
#include "blas/dgesvd.h"
    integer :: lvec, nbl, nbc, nindep, lwork, jwork, lmat, ltrav1, ltrav2
    integer :: ltrav3, jnsta, i1, k1, l1, info, lcopy, force, indnz, vecnz
    integer :: ideeq
    real(kind=8) :: swork(1), norme, sqrt, rij
    character(len=8) :: ortho
    character(len=19) :: mat, nume
    integer :: iarg
!
    call wkvect('&&VECIND.NEW_STAT', 'V V R', nbc*nbc, jnsta)
    call wkvect('&&VECIND.TRAV1', 'V V R', nbl, ltrav1)
    call wkvect('&&VECIND.VEC_IND_NZ', 'V V I', nbc, vecnz)
    indnz=0
    if (mat .ne. ' ') then
        call jeveuo(mat//'.&INT', 'L', lmat)
        call dismoi('F', 'NOM_NUME_DDL', mat(1:8), 'MATR_ASSE', info,&
                    nume, info)
        call jeveuo(nume(1:8)//'      .NUME.DEEQ', 'L', ideeq)
    endif
!
!-- NORMER LES MODES DANS L2 AVANT DE CONSTRUIRE LA MATRICE
!-- POUR PLUS DE ROBUSTESSE
!
    call wkvect('&&VECIND.VECTEURS_COPIES', 'V V R', nbl*nbc, lcopy)
    call wkvect('&&VECIND.VECTEURS_TEMP', 'V V R', nbl, ltrav1)
!
    do 500 i1 = 1, nbc
        if (mat .ne. ' ') then
            call zerlag(nbl, zi(ideeq), vectr=zr(lvec+nbl*(i1-1)))
            call mrmult('ZERO', lmat, zr(lvec+nbl*(i1-1)), zr(ltrav1), 1,&
                        .true.)
            call zerlag(nbl, zi(ideeq), vectr=zr(ltrav1))
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
500  end do
!
    do 510 l1 = 1, nbc
        if (mat .ne. ' ') then
            call mrmult('ZERO', lmat, zr(lcopy+nbl*(l1-1)), zr(ltrav1), 1,&
                        .true.)
        else
            call lceqvn(nbl, zr(lcopy+nbl*(l1-1)), zr(ltrav1))
        endif
        norme=ddot(nbl,zr(ltrav1),1,zr(lcopy+nbl*(l1-1)),1)
        zr(jnsta+(l1-1)*(nbc+1))=norme
        do 520 k1 = l1+1, nbc
            rij=ddot(nbl,zr(ltrav1),1,zr(lcopy+nbl*(k1-1)),1)
            zr(jnsta+(l1-1)*nbc+k1-1)=rij
            zr(jnsta+(k1-1)*nbc+l1-1)=rij
520      continue
510  end do
!
    if (force .ne. 1) then
!-- UTILISE APRES LE GRAM SCHMIDT DANS ORTH99, POUR
!-- ELIMINER LES VECTEURS NON INDEPENDANTS
!
        do 550 l1 = 1, nbc
            norme=zr(jnsta+(l1-1)*(nbc+1))
!
            if (norme .gt. 1.d-16) then
                do 560 k1 = l1+1, nbc
                    rij=abs(zr(jnsta+(l1-1)*nbc+k1-1))
                    rij=rij/norme
                    if (rij .gt. 1.d-8) then
                        write(6,*)' ... ANNULATION DU VECTEUR ',k1
                        do 570 i1 = 1, nbl
                            zr(lvec+((k1-1)*nbl)+i1-1)=0.d0
570                      continue
                        do 580 i1 = 1, nbc
                            zr(jnsta+((k1-1)*nbc)+i1-1)=0.d0
                            zr(jnsta+((i1-1)*nbc)+k1-1)=0.d0
580                      continue
                    endif
560              continue
            endif
!
550      continue
!
        call getvtx('  ', 'ORTHO', 1, iarg, 8,&
                    ortho, info)
        if ((info .eq. 1) .and. (ortho.eq.'OUI')) then
!-- SELECTION DES VECTEURS NON NULS POUR REMPLIR LA BASE
            do 590 i1 = 1, nbc
                if (zr(jnsta + (i1-1)*(nbc+1) ) .gt. 1d-10) then
                    zi(vecnz+indnz)=i1
                    indnz=indnz+1
                endif
590          continue
!
            do 600 i1 = 1, indnz
                l1=zi(vecnz+i1-1)
                if (i1 .ne. l1) then
                    call lceqvn(nbl, zr(lvec+nbl*(l1-1)), zr(lvec+nbl*( i1-1)))
                endif
600          continue
            nbc=indnz
        endif
    else
!
!-- ALLOCATION DES MATRICES DE TRAVAIL TEMPORAIRES
        call wkvect('&&VECIND.TRAV1_S', 'V V R', nbc, ltrav1)
        call wkvect('&&VECIND.TRAV2_U', 'V V R', nbc*nbc, ltrav2)
        call wkvect('&&VECIND.TRAV3_V', 'V V R', nbc*nbc, ltrav3)
!
!-- DESACTIVATION DU TEST FPE
        call matfpe(-1)
!
        call dgesvd('A', 'N', nbc, nbc, zr(jnsta),&
                    nbc, zr(ltrav1), zr( ltrav2), nbc, zr(ltrav3),&
                    nbc, swork, -1, info)
        lwork=int(swork(1))
        call wkvect('&&VECIND.MAT_SVD_WORK', 'V V R', lwork, jwork)
!
        call dgesvd('A', 'N', nbc, nbc, zr(jnsta),&
                    nbc, zr(ltrav1), zr( ltrav2), nbc, zr(ltrav3),&
                    nbc, zr(jwork), lwork, info)
!
        nindep=0
        norme=(nbc+0.d0)*zr(ltrav1)*1.d-16
        do 530 k1 = 1, nbc
            if (zr(ltrav1+k1-1) .gt. norme) nindep=nindep+1
530      continue
!
        call wkvect('&&VECIND.MODE_INTF_DEPL', 'V V R', nbl*nbc, lmat)
!
        call dgemm('N', 'N', nbl, nindep, nbc,&
                   1.d0, zr(lcopy), nbl, zr( ltrav2), nbc,&
                   0.d0, zr(lvec), nbl)
!
!-- INUTILE D'ANNULER DES VECTEURS QUI NE SERVIRONT NUL PART...
!        DO 540 I1=1,NBL*(NBC-NINDEP)
!          ZR(LVEC+(NINDEP*NBL)+I1-1)=0.D0
! 540    CONTINUE
!
        call matfpe(1)
!
        call jedetr('&&VECIND.MAT_SVD_WORK')
        call jedetr('&&VECIND.TRAV1_S')
        call jedetr('&&VECIND.TRAV2_U')
        call jedetr('&&VECIND.TRAV3_V')
        call jedetr('&&VECIND.MODE_INTF_DEPL')
!
    endif
!
!
!
    call jedetr('&&VECIND.NEW_STAT')
    call jedetr('&&VECIND.TRAV1')
    call jedetr('&&VECIND.VECTEURS_COPIES')
    call jedetr('&&VECIND.VECTEURS_TEMP')
    call jedetr('&&VECIND.VEC_IND_NZ')
!
end subroutine
