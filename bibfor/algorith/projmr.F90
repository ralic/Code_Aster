subroutine projmr(matras, nomres, basemo, nugene, nu,&
                  neq, nbmo)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/copmod.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/ualfva.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/ddot.h"
!
    integer :: neq, nbmo
    character(len=8) :: matras, nomres, basemo
    character(len=14) :: nu, nugene
    character(len=19) :: nomsto
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!
!     CALCUL PROJECTION MATRICE REELLE SUR BASE DE RITZ
!     REMARQUE : LA MATRICE PEUT ETRE SYMETRIQUE OU NON-SYMETRIQUE
!
!-----------------------------------------------------------------------
!
    integer :: nueq, ntbloc, nbloc, ialime, iaconl, jrefa, iadesc
    integer :: i, j, imatra, iblo, ldblo, n1bloc, n2bloc
    integer :: idbase, nbj, ldblo1, ldblo2, hc
    real(kind=8) :: pij
    complex(kind=8) :: cbid
    character(len=16) :: typbas
    character(len=19) :: matr, resu
    aster_logical :: lsym
    real(kind=8), pointer :: vectass2(:) => null()
    integer, pointer :: deeq(:) => null()
    integer, pointer :: smde(:) => null()
    integer(kind=4), pointer :: smhc(:) => null()
    integer, pointer :: smdi(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!-----------------------------------------------------------------------
!
    call jemarq()
!
    resu=nomres
    matr=matras
    call gettco(basemo, typbas)
    call jeveuo(nu//'.NUME.DEEQ', 'L', vi=deeq)
!
    nomsto=nugene//'.SMOS'
    call jeveuo(nomsto//'.SMDE', 'L', vi=smde)
    nueq=smde(1)
    ntbloc=smde(2)
    nbloc=smde(3)
!
    call jeveuo(matr//'.REFA', 'L', jrefa)
    lsym=zk24(jrefa-1+9) .eq. 'MS'
    if (lsym) then
        call jecrec(resu//'.UALF', 'G V R', 'NU', 'DISPERSE', 'CONSTANT',&
                    nbloc)
    else
        call jecrec(resu//'.UALF', 'G V R', 'NU', 'DISPERSE', 'CONSTANT',&
                    2*nbloc)
    endif
!
    call jeecra(resu//'.UALF', 'LONMAX', ntbloc)
!
    call wkvect(resu//'.LIME', 'G V K24', 1, ialime)
    zk24(ialime)=' '
!
    call wkvect(resu//'.CONL', 'G V R', nueq, iaconl)
    do i = 1, nueq
        zr(iaconl+i-1)=1.0d0
    end do
!
    call wkvect(resu//'.REFA', 'G V K24', 20, jrefa)
    zk24(jrefa-1+11)='MPI_COMPLET'
    zk24(jrefa-1+1)=basemo
    zk24(jrefa-1+2)=nugene
    if (lsym) then
        zk24(jrefa-1+9)='MS'
    else
        zk24(jrefa-1+9)='MR'
    endif
    zk24(jrefa-1+10)='GENE'
!
    call wkvect(resu//'.DESC', 'G V I', 3, iadesc)
    zi(iadesc)=2
    zi(iadesc+1)=nueq
!
!     -- ON TESTE LA HAUTEUR MAXIMALE DES COLONNES DE LA MATRICE
!     SI CETTE HAUTEUR VAUT 1, ON SUPPOSE QUE LE STOCKAGE EST DIAGONAL
    if (smde(2) .eq. smde(1)) then
        zi(iadesc+2)=1
!        ASSERT(.NOT.LSYM)
    else
        zi(iadesc+2)=2
    endif
!
    AS_ALLOCATE(vr=vectass2, size=neq)
    call wkvect('&&PROJMR.BASEMO', 'V V R', nbmo*neq, idbase)
! ----- CONVERSION DE BASEMO A LA NUMEROTATION NU
    call copmod(basemo, bmodr=zr(idbase), numer=nu, nequa=neq, nbmodes=nbmo)
!
    call mtdscr(matras)
    call jeveuo(matr//'.&INT', 'E', imatra)
!
! --- RECUPERATION DE LA STRUCTURE DE LA MATR_ASSE_GENE
!
    call jeveuo(nomsto//'.SMDI', 'L', vi=smdi)
    call jeveuo(nomsto//'.SMHC', 'L', vi4=smhc)
!
!
!     -- CAS DES MATRICES SYMETRIQUES :
!     ----------------------------------
    if (lsym) then
        do iblo = 1, nbloc
!
            call jecroc(jexnum(resu//'.UALF', iblo))
            call jeveuo(jexnum(resu//'.UALF', iblo), 'E', ldblo)
!
! ------ PROJECTION DE LA MATRICE ASSEMBLEE
!
!        BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
!
            n1bloc=1
            n2bloc=smde(1)
!
            do i = n1bloc, n2bloc
                hc = smdi(i)
                if (i .gt. 1) hc = hc - smdi(i-1)
                nbj=i-hc+1
                ASSERT(nbj.eq.1 .or. nbj.eq.i)
!
! --------- CALCUL PRODUIT MATRICE*MODE I
                call mrmult('ZERO', imatra, zr(idbase+(i-1)*neq), vectass2, 1,&
                            .true._1)
                call zerlag(neq, deeq, vectr=vectass2)
!
! --------- BOUCLE SUR LES INDICES VALIDES DE LA COLONNE I
                do j = nbj, i
!
! ------------ PRODUIT SCALAIRE VECTASS * MODE
                    pij=ddot(neq,zr(idbase+(j-1)*neq),1,vectass2,1)
!
! ------------ STOCKAGE DANS LE .UALF A LA BONNE PLACE (1 BLOC)
                    zr(ldblo+smdi(i)+j-i-1)=pij
!
                end do
            end do
            call jelibe(jexnum(resu//'.UALF', iblo))
        end do
!
!
    else
!     -- CAS DES MATRICES NON-SYMETRIQUES :
!     --------------------------------------
        ASSERT(nbloc.eq.1)
        call jecroc(jexnum(resu//'.UALF', 1))
        call jecroc(jexnum(resu//'.UALF', 2))
        call jeveuo(jexnum(resu//'.UALF', 1), 'E', ldblo1)
        call jeveuo(jexnum(resu//'.UALF', 2), 'E', ldblo2)
        n1bloc=1
        n2bloc=smde(1)
        ASSERT(n1bloc.eq.1)
        ASSERT(n2bloc.eq.nueq)
!
        do i = 1, nueq
            hc = smdi(i)
            if (i .gt. 1) hc = hc - smdi(i-1)
            nbj=i-hc+1
            ASSERT(nbj.eq.1)
            call mrmult('ZERO', imatra, zr(idbase+(i-1)*neq), vectass2, 1,&
                        .true._1)
            call zerlag(neq, deeq, vectr=vectass2)
            do j = 1, nueq
                pij=ddot(neq,zr(idbase+(j-1)*neq),1,vectass2,1)
                if (j .ge. i) then
                    zr(ldblo1+smdi(j)+i-j-1)=pij
                endif
                if (j .le. i) then
                    zr(ldblo2+smdi(i)+j-i-1)=pij
                endif
            end do
        end do
    endif
!
!
    AS_DEALLOCATE(vr=vectass2)
    call jedetr('&&PROJMR.BASEMO')
!
    call ualfva(resu, 'G')
!
    call jedema()
end subroutine
