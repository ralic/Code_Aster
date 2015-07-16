subroutine aptgno(sdappa, mesh, sdcont_defi)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/appari.h"
#include "asterfort/aptgnn.h"
#include "asterfort/apzoni.h"
#include "asterfort/apzonl.h"
#include "asterfort/apzonv.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: sdappa
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Compute tangents at each node (average)
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nbzone, ndimg
    integer :: izone, itype
    integer :: jdecnm, nbnom
    integer :: jdecne, nbnoe
    aster_logical :: apcald
    real(kind=8) :: vector(3)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> ...... TANGENTES SUR LES NOEUDS'
    endif
!
! --- INITIALISATIONS
!
    call appari(sdappa, 'APPARI_NDIMG', ndimg)
    call appari(sdappa, 'APPARI_NBZONE', nbzone)
!
! --- BOUCLE SUR LES ZONES
!
    do izone = 1, nbzone
!
! ----- INFORMATION SUR LA ZONE MAITRE
!
        call apzoni(sdappa, izone, 'NBNOM', nbnom)
        call apzoni(sdappa, izone, 'JDECNM', jdecnm)
        call apzoni(sdappa, izone, 'TYPE_NORM_MAIT', itype)
        call apzonv(sdappa, izone, 'VECT_MAIT', vector)
!
! ----- CALCUL SUR LA ZONE MAITRE
!
        call apzonl(sdappa, izone, 'CALC_NORM_MAIT', apcald)
        if (apcald) then
            call aptgnn(sdappa, mesh, sdcont_defi, ndimg, jdecnm,&
                        nbnom, itype, vector)
        endif
!
! ----- INFORMATION SUR LA ZONE ESCLAVE
!
        call apzoni(sdappa, izone, 'NBNOE', nbnoe)
        call apzoni(sdappa, izone, 'JDECNE', jdecne)
        call apzoni(sdappa, izone, 'TYPE_NORM_ESCL', itype)
        call apzonv(sdappa, izone, 'VECT_ESCL', vector)
!
! ----- CALCUL SUR LA ZONE ESCLAVE
!
        call apzonl(sdappa, izone, 'CALC_NORM_ESCL', apcald)
        if (apcald) then
            call aptgnn(sdappa, mesh, sdcont_defi, ndimg, jdecne,&
                        nbnoe, itype, vector)
        endif
    end do
!
    call jedema()
end subroutine