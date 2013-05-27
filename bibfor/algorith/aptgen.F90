subroutine aptgen(sdappa)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
    include 'asterfort/apnomk.h'
    include 'asterfort/appari.h'
    include 'asterfort/apparr.h'
    include 'asterfort/aptgem.h'
    include 'asterfort/apzoni.h'
    include 'asterfort/apzonl.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=19) :: sdappa
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT - TANGENTES
!
! CALCUL DES VECTEURS TANGENTS EN CHAQUE NOEUD POUR CHAQUE ELEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
!
!
!
!
    character(len=24) :: rnomsd, defico
    character(len=19) :: newgeo
    character(len=8) :: noma
    integer :: nbzone, ndimg
    integer :: ifm, niv
    integer :: izone
    integer :: jdecmm, nbmam
    integer :: jdecme, nbmae
    character(len=4) :: typzon
    logical :: apcald
    real(kind=8) :: epsmax
    integer :: itemax
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
     &  ' LES NOEUDS PAR ELEMENT (ELNO)'
    endif
!
! --- INITIALISATIONS
!
    call apnomk(sdappa, 'NOMA', rnomsd)
    noma = rnomsd(1:8)
    call apnomk(sdappa, 'NEWGEO', rnomsd)
    newgeo = rnomsd(1:19)
    call apnomk(sdappa, 'DEFICO', defico)
    call apparr(sdappa, 'PROJ_NEWT_RESI', epsmax)
    call appari(sdappa, 'PROJ_NEWT_ITER', itemax)
    call appari(sdappa, 'APPARI_NDIMG', ndimg)
    call appari(sdappa, 'APPARI_NBZONE', nbzone)
!
! --- BOUCLE SUR LES ZONES
!
    do 10 izone = 1, nbzone
!
! ----- INFORMATION SUR LA ZONE MAITRE
!
        call apzoni(sdappa, izone, 'NBMAM', nbmam)
        call apzoni(sdappa, izone, 'JDECMM', jdecmm)
        typzon = 'MAIT'
!
! ----- CALCUL SUR LA ZONE MAITRE
!
        call apzonl(sdappa, izone, 'CALC_NORM_MAIT', apcald)
        if (apcald) then
            call aptgem(sdappa, noma, newgeo, defico, ndimg,&
                        izone, typzon, itemax, epsmax, jdecmm,&
                        nbmam)
        endif
!
! ----- INFORMATION SUR LA ZONE ESCLAVE
!
        call apzoni(sdappa, izone, 'NBMAE', nbmae)
        call apzoni(sdappa, izone, 'JDECME', jdecme)
        typzon = 'ESCL'
!
! ----- CALCUL SUR LA ZONE ESCLAVE
!
        call apzonl(sdappa, izone, 'CALC_NORM_ESCL', apcald)
        if (apcald) then
            call aptgem(sdappa, noma, newgeo, defico, ndimg,&
                        izone, typzon, itemax, epsmax, jdecme,&
                        nbmae)
        endif
10  end do
!
    call jedema()
end subroutine
