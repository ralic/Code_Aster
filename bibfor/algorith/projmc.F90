subroutine projmc(matras, nomres, basemo, nugene, nu,&
                  neq, nbmo)
    implicit none
#include "jeveux.h"
#include "asterfort/gettco.h"
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
#include "asterfort/mcmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/ualfva.h"
#include "asterfort/wkvect.h"
#include "asterfort/zeclag.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: neq, nbmo
    character(len=8) :: matras, nomres, basemo
    character(len=14) :: nu
    character(len=19) :: nomsto
    character(len=14) :: nugene
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
!     CALCUL PROJECTION MATRICE COMPLEXE SUR BASE DE RITZ
!
!-----------------------------------------------------------------------
!
    integer :: iddeeq,  nueq, ntbloc, nbloc, ialime, iaconl, jrefa, iadesc
    integer :: i, j, k, imatra,    iblo, ldblo, n1bloc, n2bloc, hc

    complex(kind=8) :: pij
    character(len=16) :: typbas
    character(len=19) :: resu
    real(kind=8) :: zero
    complex(kind=8) :: cbid
    real(kind=8), pointer :: vbasemo(:) => null()
    complex(kind=8), pointer :: vectass2(:) => null()
    complex(kind=8), pointer :: vectass3(:) => null()
    integer, pointer :: smde(:) => null()
    integer(kind=4), pointer :: smhc(:) => null()
    integer, pointer :: smdi(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!-----------------------------------------------------------------------
!
    call jemarq()
!
    zero = 0.d0
    resu = ' '
    resu(1:8) = nomres
    call gettco(basemo, typbas)
    call jeveuo(nu//'.NUME.DEEQ', 'L', iddeeq)
!
    nomsto=nugene//'.SMOS'
    call jeveuo(nomsto//'.SMDE', 'L', vi=smde)
    nueq = smde(1)
    ntbloc = smde(2)
    nbloc = smde(3)
!
    call jecrec(resu//'.UALF', 'G V C', 'NU', 'DISPERSE', 'CONSTANT',&
                nbloc)
    call jeecra(resu//'.UALF', 'LONMAX', ntbloc)
!
    call wkvect(resu//'.LIME', 'G V K24', 1, ialime)
    zk24(ialime) = '                        '
!
    call wkvect(resu//'.CONL', 'G V C', nueq, iaconl)
    do i = 1, nueq
        zc(iaconl+i-1) = dcmplx(1.0d0,0.0d0)
    end do
!
    call wkvect(resu//'.REFA', 'G V K24', 20, jrefa)
    zk24(jrefa-1+11)='MPI_COMPLET'
    zk24(jrefa-1+1) = basemo
    zk24(jrefa-1+2) = nugene
    zk24(jrefa-1+9) = 'MS'
    zk24(jrefa-1+10) = 'GENE'
!
    call wkvect(resu//'.DESC', 'G V I', 3, iadesc)
    zi(iadesc) = 2
    zi(iadesc+1) = nueq
!   ON TESTE LA HAUTEUR MAXIMALE DES COLONNES DE LA MATRICE
!   SI CETTE HAUTEUR VAUT 1, ON SUPPOSE QUE LE STOCKAGE EST DIAGONAL
    if (smde(2) .eq. smde(1)) then
        zi(iadesc+2) = 1
    else
        zi(iadesc+2) = 2
    endif
!
    AS_ALLOCATE(vc=vectass2, size=neq)
    AS_ALLOCATE(vc=vectass3, size=neq)
    AS_ALLOCATE(vr=vbasemo, size=nbmo*neq)
! ----- CONVERSION DE BASEMO A LA NUMEROTATION NU
    call copmod(basemo, numer=nu, bmodr=vbasemo)
!
    call mtdscr(matras)
    call jeveuo(matras//'           .&INT', 'E', imatra)
!
! --- RECUPERATION DE LA STRUCTURE DE LA MATR_ASSE_GENE
!
    call jeveuo(nomsto//'.SMDI', 'L', vi=smdi)
    call jeveuo(nomsto//'.SMHC', 'L', vi4=smhc)
!
    do iblo = 1, nbloc
!
        call jecroc(jexnum(resu//'.UALF', iblo))
        call jeveuo(jexnum(resu//'.UALF', iblo), 'E', ldblo)
!
! ------ PROJECTION DE LA MATRICE ASSEMBLEE
!
!        BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
!
        n1bloc = 1
        n2bloc = smde(1)
!
        do i = n1bloc, n2bloc
!
            do k = 1, neq
                vectass2(k)=dcmplx(vbasemo(1+(i-1)*neq+k-1),zero)
            end do
!
! --------- CALCUL PRODUIT MATRICE*MODE I
!
            call mcmult('ZERO', imatra, vectass2, vectass3, 1,&
                        .true._1)
            call zeclag(vectass3, neq, zi(iddeeq))
!
! --------- BOUCLE SUR LES INDICES VALIDES DE LA COLONNE I
!
            hc = smdi(i)
            if (i .gt. 1) hc = hc - smdi(i-1)
            do j = (i-hc+1), i
!
! ----------- PRODUIT SCALAIRE VECTASS * MODE
!
                pij = dcmplx(zero,zero)
                do k = 1, neq
                    pij = pij + vectass3(k)* dcmplx(vbasemo(1+(j-1) *neq+k-1),zero)
                end do
!
! ----------- STOCKAGE DANS LE .UALF A LA BONNE PLACE (1 BLOC)
!
                zc(ldblo+smdi(i)+j-i-1) = pij
!
            end do
        end do
        call jelibe(jexnum(resu//'.UALF', iblo))
    end do
!
    AS_DEALLOCATE(vc=vectass2)
    AS_DEALLOCATE(vc=vectass3)
    AS_DEALLOCATE(vr=vbasemo)
!
!
    call ualfva(resu, 'G')
    call jedema()
end subroutine
