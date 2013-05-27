subroutine lobs(sdobse, numins, inst, lobsv)
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
    include 'asterfort/impfoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nmcrpo.h'
    integer :: numins
    real(kind=8) :: inst
    character(len=19) :: sdobse
    logical :: lobsv
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! DOIT-ON FAIRE UNE OBSERVATION  ?
!
! ----------------------------------------------------------------------
!
!
! IN  NUMINS : NUMERO DE L'INSTANT COURANT
! IN  SDOBSE : NOM DE LA SD POUR OBSERVATION
! IN  INST   : INSTANT DE CALCUL COURANT
! OUT LOBSV  : .TRUE. SI AU MOINS UNE OBSERVATION
!
!
!
!
    character(len=24) :: obsinf, obsact
    integer :: jobsin, jobsac
    integer :: iobs, nbobsv
    character(len=2) :: chaine
    character(len=19) :: listli
    logical :: lselec
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lobsv = .false.
!
! --- ACCES DE LA SD
!
    obsinf = sdobse(1:14)//'     .INFO'
    obsact = sdobse(1:14)//'     .ACTI'
    call jeveuo(obsinf, 'L', jobsin)
    nbobsv = zi(jobsin-1+1)
!
    if (nbobsv .eq. 0) goto 99
!
! --- ON OBSERVE SYSTEMATIQUEMENT L'ETAT INITIAL
!
    if (numins .eq. 0) then
        lobsv = .true.
        call jeveuo(obsact, 'E', jobsac)
        do 14 iobs = 1, nbobsv
            zl(jobsac+iobs-1) = .true.
14      continue
        goto 99
    endif
!
! --- INFO
!
    call jeveuo(obsact, 'E', jobsac)
!
    do 10 iobs = 1, nbobsv
        call impfoi(0, 2, iobs, chaine)
        listli = sdobse(1:14)//chaine(1:2)//'.LI'
        call nmcrpo(listli, numins, inst, lselec)
        zl(jobsac+iobs-1) = lselec
        lobsv = lselec.or.lobsv
10  end do
99  continue
!
    call jedema()
!
end subroutine
