subroutine elpiv1(xjvmax, indic, nbliac, ajliai, spliai,&
                  spavan, noma, defico, resoco)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/cfdisd.h"
#include "asterfort/cfimp2.h"
#include "asterfort/cftabl.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma
    character(len=24) :: resoco, defico
    real(kind=8) :: xjvmax
    integer :: nbliac
    integer :: indic
    integer :: ajliai, spliai
    integer :: spavan
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - UTILITAIRE)
!
! ELIMINATION DES PIVOTS NULS DANS LA MATRICE DE CONTACT
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
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!                'E': RESOCO(1:14)//'.LIAC'
!                'E': RESOCO(1:14)//'.LIOT'
!
!
!
!
!
    character(len=1) :: typesp
    character(len=2) :: typec0
    character(len=19) :: liac, liot, macont, stoc, ouvert
    integer :: jliac, jliot, jvale, jva, jouv
    integer :: nbbloc, nbliai
    real(kind=8) :: copmax
    integer :: kk1, kk2, kk1f, kk2f, llf, llf1, llf2
    integer :: nbote, lliac
    integer :: iblc
    integer :: niv, ifm
    integer :: bloc, dercol
    aster_logical :: pivnul
    integer, pointer :: scbl(:) => null()
    integer, pointer :: scde(:) => null()
    integer, pointer :: scib(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- LECTURE DES STRUCTURES DE DONNEES
!
    liac = resoco(1:14)//'.LIAC'
    liot = resoco(1:14)//'.LIOT'
    macont = resoco(1:14)//'.MATC'
    stoc = resoco(1:14)//'.SLCS'
    call jeveuo(liac, 'E', jliac)
    call jeveuo(liot, 'E', jliot)
    call jeveuo(stoc//'.SCIB', 'L', vi=scib)
    call jeveuo(stoc//'.SCBL', 'L', vi=scbl)
    call jeveuo(stoc//'.SCDE', 'L', vi=scde)
!
! --- INITIALISATIONS
!
    nbliai = cfdisd(resoco,'NBLIAI')
    typesp = 'S'
    typec0 = 'C0'
    copmax = xjvmax * 1.0d-08
    pivnul = .false.
    llf = 0
    llf1 = 0
    llf2 = 0
    nbbloc = scde(3)
!
! --- BLOC EN LECTURE
!
    ouvert = '&&ELPIV2.TRAV'
    call wkvect(ouvert, 'V V L', nbbloc, jouv)
!
! --- DETECTION DES PIVOTS NULS
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
            iblc = scib(kk1f)
            dercol = scbl(iblc)
            bloc = dercol*(dercol+1)/2
!
! ------- ON ACCEDE AU BLOC
!
            if (.not.zl(jouv-1+iblc)) then
                if ((iblc.gt.1) .and. (kk1f.ne.(spavan+1))) then
                    call jelibe(jexnum(macont//'.UALF', (iblc-1)))
                    zl(jouv-2+iblc) = .false.
                endif
                call jeveuo(jexnum(macont//'.UALF', iblc), 'E', jvale)
                zl(jouv-1+iblc) = .true.
            endif
!
! ------- ACCES A LA DIAGONALE
!
            jva = jvale-1+(kk1f-1)*(kk1f)/2-bloc+kk2f
!
! ------- PIVOT NUL ?
!
            if (abs(zr(jva)) .lt. copmax) then
                pivnul = .true.
            else
                pivnul = .false.
                goto 10
            endif
 20     continue
!
! ----- ON SUPPRIME LA LIAISON
!
        if (pivnul) then
            lliac = zi(jliac-1+kk1)
            zi(jliot+4*nbliai) = zi(jliot+4*nbliai) + 1
            nbote = zi(jliot+4*nbliai)
            zi(jliot-1+nbote) = zi(jliac-1+kk1)
            call cftabl(indic, nbliac, ajliai, spliai, llf,&
                        llf1, llf2, resoco, typesp, kk1,&
                        lliac, typec0)
            call cfimp2(defico, resoco, noma, lliac, typec0,&
                        'PIV')
            goto 40
        endif
 10 end do
!
 40 continue
    call jedetr(ouvert)
    call jedema()
!
end subroutine
