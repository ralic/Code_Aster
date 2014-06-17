subroutine cupivo(xjvmax, indic, nbliac, ajliai, spliai,&
                  spavan, deficu, resocu)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
!
#include "asterfort/cudisi.h"
#include "asterfort/cuimp2.h"
#include "asterfort/cutabl.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=24) :: resocu
    character(len=24) :: deficu
    real(kind=8) :: xjvmax
    integer :: nbliac
    integer :: indic
    integer :: ajliai
    integer :: spliai
    integer :: spavan
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATERALE (RESOLUTION)
!
! ELIMINATION DES PIVOTS NULS DANS LA MATRICE DE CONTACT
!
!
! ----------------------------------------------------------------------
!
!
! IN  XJVMAX : VALEUR DU PIVOT MAX
! OUT INDIC  : +1 ON A RAJOUTE UNE LIAISON
!              -1 ON A ENLEVE UNE LIAISON
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! IN  SPAVAN : INDICE DE DEBUT DE TRAITEMENT DES LIAISONS
! IN  DEFICU : SD DE DEFINITION (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCU : SD DE TRAITEMENT
!
!
!
!
!
    character(len=1) :: typesp
    character(len=19) :: liac, liot, matr, stoc, ouvert
    integer :: jliac, jliot, jvale, jva,  jouv
    integer :: nbbloc
    real(kind=8) :: copmax
    integer :: kk1, kk2, kk1f, kk2f
    integer :: nbote, pivot, nbliai, lliac, ii
    integer :: niv, ifm
    integer :: bloc,   dercol
    integer :: nnocu
    integer, pointer :: scbl(:) => null()
    integer, pointer :: scib(:) => null()
    integer, pointer :: scde(:) => null()
!
! ----------------------------------------------------------------------
!
    call infniv(ifm, niv)
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES
!
    liac = resocu(1:14)//'.LIAC'
    liot = resocu(1:14)//'.LIOT'
    matr = resocu(1:14)//'.MATC'
    stoc = resocu(1:14)//'.SLCS'
!
    call jeveuo(liac, 'E', jliac)
    call jeveuo(liot, 'E', jliot)
    call jeveuo(stoc//'.SCIB', 'L', vi=scib)
    call jeveuo(stoc//'.SCBL', 'L', vi=scbl)
    call jeveuo(stoc//'.SCDE', 'L', vi=scde)
! ======================================================================
! --- INITIALISATION DES VARIABLES
! ======================================================================
    nnocu = cudisi(deficu,'NNOCU')
    nbliai = nnocu
    typesp = 'S'
    copmax = xjvmax * 1.0d-08
    pivot = 0
    nbbloc=scde(3)
    ouvert='&&ELPIV2.TRAV'
    call wkvect(ouvert, 'V V L', nbbloc, jouv)
!
    do 10 kk1 = spavan+1, nbliac
        do 20 kk2 = 1, nbliac
            if (kk2 .gt. kk1) then
                kk1f = kk2
                kk2f = kk1
            else
                kk1f = kk1
                kk2f = kk2
            endif
            ii = scib(kk1f)
            dercol = scbl(ii)
            bloc = dercol*(dercol+1)/2
            if (.not.zl(jouv-1+ii)) then
                if ((ii.gt.1) .and. (kk1f.ne.(spavan+1))) then
                    call jelibe(jexnum(matr//'.UALF', (ii-1)))
                    zl(jouv-2+ii)=.false.
                endif
                call jeveuo(jexnum(matr//'.UALF', ii), 'E', jvale)
                zl(jouv-1+ii)=.true.
            endif
!
            jva=jvale-1+(kk1f-1)*(kk1f)/2-bloc+kk2f
!
            if (abs(zr(jva)) .lt. copmax) then
                pivot = 1
            else
                pivot = 0
                goto 10
            endif
20      continue
        if (pivot .eq. 1) then
!
            lliac = zi(jliac-1+kk1)
!
            zi(jliot+4*nbliai) = zi(jliot+4*nbliai) + 1
            nbote = zi(jliot+4*nbliai)
            zi(jliot-1+nbote) = zi(jliac-1+kk1)
!
            call cutabl(indic, nbliac, ajliai, spliai, resocu,&
                        typesp, kk1, lliac)
            call cuimp2(ifm, lliac, typesp, 'PIV', resocu)
            goto 40
        endif
10  end do
!
40  continue
    call jedetr(ouvert)
    call jedema()
!
end subroutine
