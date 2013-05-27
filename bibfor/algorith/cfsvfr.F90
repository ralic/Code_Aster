subroutine cfsvfr(defico, resoco, lconv)
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
! SAUVEGARDE DU STATUT DE FROTTEMENT POUR PERMETTRE LE TRANSPORT
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
    logical :: llagrf
    integer :: nnoco
    integer :: iliac, iliai, posnoe
    integer :: nbliac, llf, llf1, llf2, btotal
    character(len=19) :: statfr, typl, liac
    integer :: jstfr, jtypl, jliac
    character(len=24) :: numlia
    integer :: jnumli
    character(len=8) :: typlia
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- LE STATUT DE FROTTEMENT N'EST SAUVEGARDE QU'EN LAGRANGIEN
!
    llagrf = cfdisl(defico,'FROT_LAGR' )
!
    if (.not.llagrf) then
        goto 999
    endif
!
! --- ACCES OBJETS
!
    if (lconv) then
        statfr = resoco(1:14)//'.STF0'
    else
        statfr = resoco(1:14)//'.STFR'
    endif
    call jeveuo(statfr, 'E', jstfr)
    typl = resoco(1:14)//'.TYPL'
    call jeveuo(typl, 'L', jtypl)
    liac = resoco(1:14)//'.LIAC'
    call jeveuo(liac, 'L', jliac)
    numlia = resoco(1:14)//'.NUMLIA'
    call jeveuo(numlia, 'L', jnumli)
!
! --- INITIALISATIONS
!
    nnoco = cfdisi(defico,'NNOCO')
    call jerazo(statfr, nnoco, 1)
!
! --- INFORMATIONS
!
    nbliac = cfdisd(resoco,'NBLIAC')
    llf = cfdisd(resoco,'LLF' )
    llf1 = cfdisd(resoco,'LLF1' )
    llf2 = cfdisd(resoco,'LLF2' )
!
    btotal = nbliac + llf + llf1 + llf2
!
! --- SAUVEGARDE DU STATUT DE FROTTEMENT
!
    do 10 iliac = 1, btotal
        typlia = zk8(jtypl -1+iliac)
        if (typlia(1:1) .ne. 'F') goto 10
        iliai = zi(jliac -1+iliac)
        posnoe = zi(jnumli-1+4*(iliai-1)+2)
        call assert(posnoe.le.nnoco)
        call assert(zk8(jstfr-1+posnoe).eq.' ')
        zk8(jstfr-1+posnoe) = typlia
10  end do
!
999  continue
!
    call jedema()
end subroutine
