subroutine mminfm(posmae, defico, questz, irep)
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
    include 'asterfort/assert.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: defico
    integer :: posmae
    character(len=*) :: questz
    integer :: irep
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - UTILITAIRE)
!
! REPOND A UNE QUESTION SUR UNE OPTION/CARACTERISTIQUE DU CONTACT
! VARIABLE SUIVANT LA MAILLE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
! IN  POSMAE : POSITION DE LA MAILLE ESCLAVE DANS LES SD CONTACT
! IN  QUESTI : QUESTION POSEE
! OUT IREP   : VALEUR
!
!
!
!
    integer :: zmaes, ztypm
    character(len=24) :: maescl, typema
    integer :: jmaesc, jtypma
    character(len=24) :: questi
    integer :: indmae, posma2
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    irep = 0
    questi = questz
!
! --- ACCES AUX SDS
!
    typema = defico(1:16)//'.TYPEMA'
    maescl = defico(1:16)//'.MAESCL'
!
    zmaes = cfmmvd('ZMAES')
    ztypm = cfmmvd('ZTYPM')
!
    call jeveuo(typema, 'L', jtypma)
    call jeveuo(maescl, 'L', jmaesc)
!
! --- INDICE
!
    indmae = zi(jtypma+ztypm*(posmae-1)+2-1)
    posma2 = zi(jmaesc+zmaes*(indmae-1)+1-1)
    call assert(posma2.eq.posmae)
!
! --- QUESTION
!
    if (questi .eq. 'IZONE') then
        irep = zi(jmaesc+zmaes*(indmae-1)+2-1)
    else if (questi.eq.'NPTM') then
        irep = zi(jmaesc+zmaes*(indmae-1)+3-1)
    else if (questi.eq.'NDEXFR') then
        irep = zi(jmaesc+zmaes*(indmae-1)+4-1)
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
