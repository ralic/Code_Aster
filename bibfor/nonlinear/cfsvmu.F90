subroutine cfsvmu(defico, resoco, lconv)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfdisd.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jerazo.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: defico, resoco
    logical :: lconv
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES)
!
! SAUVEGARDE DU LAGRANGE DE CONTACT POUR PERMETTRE LE TRANSPORT
! D'UN APPARIEMENT A UN AUTRE
!
! ----------------------------------------------------------------------
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  LCONV  : SAUVEGARDE T-ON UN ETAT CONVERGE ?
!
!
!
!
    integer :: ifm, niv
    integer :: nnoco
    integer :: iliai, posnoe
    integer :: nbliai
    character(len=19) :: svmu, mu
    integer :: jsvmu, jmu
    character(len=24) :: numlia
    integer :: jnumli
    logical :: lgcp
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- LE LAGRANGE DE CONTACT N'EST SAUVEGARDE QU'EN GCP
!
    lgcp = cfdisl(defico,'CONT_GCP')
!
    if (.not.lgcp) then
        goto 999
    endif
!
! --- ACCES OBJETS
!
    if (lconv) then
        svmu = resoco(1:14)//'.SVM0'
    else
        svmu = resoco(1:14)//'.SVMU'
    endif
    call jeveuo(svmu, 'E', jsvmu)
    mu = resoco(1:14)//'.MU'
    call jeveuo(mu, 'L', jmu)
    numlia = resoco(1:14)//'.NUMLIA'
    call jeveuo(numlia, 'L', jnumli)
!
! --- INITIALISATIONS
!
    nnoco = cfdisi(defico,'NNOCO')
    call jerazo(svmu, nnoco, 1)
!
! --- INFORMATIONS
!
    nbliai = cfdisd(resoco,'NBLIAI')
!
! --- SAUVEGARDE DU STATUT DE FROTTEMENT
!
    do 10 iliai = 1, nbliai
        posnoe = zi(jnumli-1+4*(iliai-1)+2)
        call assert(posnoe.le.nnoco)
        call assert(zr(jsvmu-1+posnoe).eq.0.d0)
        zr(jsvmu-1+posnoe) = zr(jmu-1+iliai)
10  end do
!
999  continue
!
    call jedema()
end subroutine
