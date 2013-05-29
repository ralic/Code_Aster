subroutine cfmmap(noma, defico, resoco)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/apcrsd.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfmmar.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=8) :: noma
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - SD APPARIEMENT)
!
! CREATION DE LA SD POUR L'APPARIEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
!
!
!
    integer :: ifm, niv
    integer :: nzoco, ntpt, ndimg, ntmano, ntma, nnoco
    integer :: nbno, ier
    character(len=8) :: k8bid
    character(len=19) :: sdappa, newgeo
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- INITIALISATIONS
!
    nzoco = cfdisi(defico,'NZOCO' )
    nnoco = cfdisi(defico,'NNOCO' )
    ntpt = cfdisi(defico,'NTPT' )
    ndimg = cfdisi(defico,'NDIM' )
    ntma = cfdisi(defico,'NMACO' )
    ntmano = cfdisi(defico,'NTMANO')
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbno,&
                k8bid, ier)
!
! --- NOM DES SD
!
    sdappa = resoco(1:14)//'.APPA'
    newgeo = resoco(1:14)//'.NEWG'
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... CREATION SD APPARIEMENT'
    endif
!
! --- CREATION DE LA SD APPARIEMENT
!
    call apcrsd(sdappa, nzoco, ntpt, ntma, nnoco,&
                ntmano, nbno)
!
! --- REMPLISSAGE DE LA SD APPARIEMENT
!
    call cfmmar(noma, defico, newgeo, sdappa, nzoco,&
                ntpt, ndimg, ntma, nnoco, ntmano)
!
    call jedema()
!
end subroutine
