subroutine cfeven(phase, defico, resoco)
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfdisd.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=3) :: phase
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - EVENT-DRIVEN)
!
! DETECTION D'UNE COLLISION
!
! ----------------------------------------------------------------------
!
!
! IN  PHASE  : PHASE DE DETECTION
!              'INI' - AU DEBUT DU PAS DE TEMPS
!              'FIN' - A LA FIN DU PAS DE TEMPS
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
!
!
!
!
    integer :: ifm, niv
    integer :: nbliai, nbliac, llf, llf1, llf2, btotal, ip
    integer :: iliai, iliac
    character(len=24) :: numlia, ctevco
    integer :: jnumli, jctevc
    character(len=19) :: liac
    integer :: jliac
    integer :: zeven
    logical :: lactif
    real(kind=8) :: etacin, etacfi
    logical :: lexiv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... GESTION DES JEUX POUR'//&
        ' EVENT-DRIVEN'
    endif
!
! --- PARAMETRES
!
    nbliai = cfdisd(resoco,'NBLIAI')
    nbliac = cfdisd(resoco,'NBLIAC')
    llf = cfdisd(resoco,'LLF' )
    llf1 = cfdisd(resoco,'LLF1' )
    llf2 = cfdisd(resoco,'LLF2' )
    btotal = nbliac+llf+llf1+llf2
    zeven = cfmmvd('ZEVEN')
!
! --- UNE ZONE EN MODE SANS CALCUL: ON NE PEUT RIEN FAIRE
!
    lexiv = cfdisl(defico,'EXIS_VERIF')
    if (lexiv) goto 999
!
! --- ACCES OBJETS DU CONTACT
!
    liac = resoco(1:14)//'.LIAC'
    numlia = resoco(1:14)//'.NUMLIA'
    ctevco = resoco(1:14)//'.EVENCO'
    call jeveuo(liac, 'L', jliac)
    call jeveuo(numlia, 'L', jnumli)
    call jeveuo(ctevco, 'E', jctevc)
!
! --- DETECTION
!
    do 20 iliai = 1, nbliai
        ip = zi(jnumli+4*(iliai-1)+1-1)
        etacin = zr(jctevc+zeven*(ip-1)+1-1)
        etacfi = zr(jctevc+zeven*(ip-1)+2-1)
        lactif = .false.
!
! ----- LA LIAISON EST-ELLE ACTIVE ?
!
        do 30 iliac = 1, btotal
            if (zi(jliac-1+iliac) .eq. iliai) lactif = .true.
30      continue
!
! ----- CHANGEMENT STATUT
!
        if (lactif) then
            if (phase .eq. 'INI') then
                etacin = 1.d0
            else if (phase.eq.'FIN') then
                etacfi = 1.d0
            else
                call assert(.false.)
            endif
        else
            if (phase .eq. 'INI') then
                etacin = 0.d0
            else if (phase.eq.'FIN') then
                etacfi = 0.d0
            else
                call assert(.false.)
            endif
        endif
        zr(jctevc+zeven*(ip-1)+1-1) = etacin
        zr(jctevc+zeven*(ip-1)+2-1) = etacfi
20  end do
!
999  continue
!
    call jedema()
end subroutine
