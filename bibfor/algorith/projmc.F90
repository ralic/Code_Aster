subroutine projmc(matras, nomres, basemo, nugene, nu,&
                  neq, nbmo)
    implicit none
#include "jeveux.h"
!
#include "asterc/gettco.h"
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
    integer :: neq, nbmo
    character(len=8) :: matras, nomres, basemo
    character(len=14) :: nu
    character(len=19) :: nomsto
    character(len=14) :: nugene
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
!     CALCUL PROJECTION MATRICE COMPLEXE SUR BASE DE RITZ
!
!-----------------------------------------------------------------------
!
    integer :: iddeeq, jscde, nueq, ntbloc, nbloc, ialime, iaconl, jrefa, iadesc
    integer :: i, j, k, imatra, jscdi, jscbl, jschc, iblo, ldblo, n1bloc, n2bloc
    integer :: idvec2, idvec3, idbase
    complex(kind=8) :: pij
    character(len=16) :: typbas
    character(len=19) :: resu
    real(kind=8) :: zero
    complex(kind=8) :: cbid
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
    nomsto=nugene//'.SLCS'
    call jeveuo(nomsto//'.SCDE', 'L', jscde)
    nueq = zi(jscde-1+1)
    ntbloc = zi(jscde-1+2)
    nbloc = zi(jscde-1+3)
!
    call jecrec(resu//'.UALF', 'G V C', 'NU', 'DISPERSE', 'CONSTANT',&
                nbloc)
    call jeecra(resu//'.UALF', 'LONMAX', ntbloc)
!
    call wkvect(resu//'.LIME', 'G V K24', 1, ialime)
    zk24(ialime) = '                        '
!
    call wkvect(resu//'.CONL', 'G V C', nueq, iaconl)
    do 10 i = 1, nueq
        zc(iaconl+i-1) = dcmplx(1.0d0,0.0d0)
10  end do
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
    if (zi(jscde-1+4) .eq. 1) then
        zi(iadesc+2) = 1
    else
        zi(iadesc+2) = 2
    endif
!
    call wkvect('&&PROJMC.VECTASS2', 'V V C', neq, idvec2)
    call wkvect('&&PROJMC.VECTASS3', 'V V C', neq, idvec3)
    call wkvect('&&PROJMC.BASEMO', 'V V R', nbmo*neq, idbase)
! ----- CONVERSION DE BASEMO A LA NUMEROTATION NU
    call copmod(basemo, 'DEPL', neq, nu, nbmo,&
                'R', zr(idbase), [cbid])
!
    call mtdscr(matras)
    call jeveuo(matras//'           .&INT', 'E', imatra)
!
! --- RECUPERATION DE LA STRUCTURE DE LA MATR_ASSE_GENE
!
    call jeveuo(nomsto//'.SCDI', 'L', jscdi)
    call jeveuo(nomsto//'.SCBL', 'L', jscbl)
    call jeveuo(nomsto//'.SCHC', 'L', jschc)
!
    do 20 iblo = 1, nbloc
!
        call jecroc(jexnum(resu//'.UALF', iblo))
        call jeveuo(jexnum(resu//'.UALF', iblo), 'E', ldblo)
!
! ------ PROJECTION DE LA MATRICE ASSEMBLEE
!
!        BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
!
        n1bloc = zi(jscbl+iblo-1)+1
        n2bloc = zi(jscbl+iblo)
!
        do 30 i = n1bloc, n2bloc
!
            do 32 k = 1, neq
                zc(idvec2+k-1)=dcmplx(zr(idbase+(i-1)*neq+k-1),zero)
32          continue
!
! --------- CALCUL PRODUIT MATRICE*MODE I
!
            call mcmult('ZERO', imatra, zc(idvec2), zc(idvec3), 1,&
                        .true.)
            call zeclag(zc(idvec3), neq, zi(iddeeq))
!
! --------- BOUCLE SUR LES INDICES VALIDES DE LA COLONNE I
!
            do 40 j = (i-zi(jschc+i-1)+1), i
!
! ----------- PRODUIT SCALAIRE VECTASS * MODE
!
                pij = dcmplx(zero,zero)
                do 42 k = 1, neq
                    pij = pij + zc(idvec3+k-1)* dcmplx(zr(idbase+(j-1) *neq+k-1),zero)
42              continue
!
! ----------- STOCKAGE DANS LE .UALF A LA BONNE PLACE (1 BLOC)
!
                zc(ldblo+zi(jscdi+i-1)+j-i-1) = pij
!
40          continue
30      continue
        call jelibe(jexnum(resu//'.UALF', iblo))
20  end do
!
    call jedetr('&&PROJMC.VECTASS2')
    call jedetr('&&PROJMC.VECTASS3')
    call jedetr('&&PROJMC.BASEMO')
!
!
    call ualfva(resu, 'G')
    call jedema()
end subroutine
