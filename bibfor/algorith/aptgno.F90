subroutine aptgno(sdappa)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/apnomk.h"
#include "asterfort/appari.h"
#include "asterfort/aptgnn.h"
#include "asterfort/apzoni.h"
#include "asterfort/apzonl.h"
#include "asterfort/apzonv.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=19) :: sdappa
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT - TANGENTES
!
! CALCUL DES VECTEURS TANGENTS EN CHAQUE NOEUD (MOYENNE)
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
!
!
!
!
    integer :: ifm, niv
    character(len=8) :: noma
    character(len=24) :: rnomsd, defico
    integer :: nbzone, ndimg
    integer :: izone, itype
    integer :: jdecnm, nbnom
    integer :: jdecne, nbnoe
    logical(kind=1) :: apcald
    real(kind=8) :: vector(3)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> ...... TANGENTES SUR' //&
     &  ' LES NOEUDS'
    endif
!
! --- INITIALISATIONS
!
    call apnomk(sdappa, 'NOMA', rnomsd)
    noma = rnomsd(1:8)
    call apnomk(sdappa, 'DEFICO', defico)
    call appari(sdappa, 'APPARI_NDIMG', ndimg)
    call appari(sdappa, 'APPARI_NBZONE', nbzone)
!
! --- BOUCLE SUR LES ZONES
!
    do 10 izone = 1, nbzone
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
            call aptgnn(sdappa, noma, defico, ndimg, jdecnm,&
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
            call aptgnn(sdappa, noma, defico, ndimg, jdecne,&
                        nbnoe, itype, vector)
        endif
10  end do
!
    call jedema()
end subroutine
