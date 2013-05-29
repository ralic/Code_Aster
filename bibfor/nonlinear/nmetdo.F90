subroutine nmetdo(sdcriq)
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
    include 'asterc/getfac.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mess.h'
    character(len=24) :: sdcriq
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! POUR CALCUL DE L'INDICATEUR D'ERREUR TEMPORELLE, ON VERIFIE QU'ON EST
! BIEN DANS LE CADRE DES MODELISATIONS HM SATUREES AVEC COMPORTEMENT
! MECANIQUE ELASTIQUE
!
! ----------------------------------------------------------------------
!
! IN  SDCRIQ : SD CRITERE QUALITE
!
! ----------------------------------------------------------------------
!
! DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
    integer :: dimaki
    parameter (dimaki=9)
!
    integer :: nbocc, n1, n2, ii, jj, iocc
    integer :: idebut, iret
    logical :: ellisq
    character(len=16) :: comp1, comel(dimaki), argii, argjj
    integer :: iarg
    character(len=24) :: errthm
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATION
!
    errthm = sdcriq(1:19)//'.ERRT'
    call jeexin(errthm, iret)
    if (iret .eq. 0) goto 999
!
! --- INDICATEUR D'ERREUR EN TEMPS -> ON VERIFIE QUE C'EST POSSIBLE
!
    call getfac('COMP_INCR', nbocc)
!
    if (nbocc .eq. 0) then
        call u2mess('F', 'INDICATEUR_25')
    else
        do 10 iocc = 1, nbocc
            call getvtx('COMP_INCR', 'RELATION', iocc, iarg, 1,&
                        comp1, n1)
            if (comp1(1:6) .eq. 'KIT_HM') then
                call getvtx('COMP_INCR', 'RELATION_KIT', iocc, iarg, dimaki,&
                            comel(1), n2)
                if (n2 .eq. 0) then
                    call assert(.false.)
                else if (n2.gt.dimaki) then
                    call assert(.false.)
                else
                    ellisq = .false.
                    do 101 ii = 1, n2
                        argii = comel(ii)
                        if ((argii(1:4).eq.'ELAS') .or. (argii(1:9) .eq.'LIQU_SATU')) then
                            idebut = ii + 1
                            do 102 jj = idebut, n2
                                argjj = comel(jj)
                                if ((argjj(1:4).eq.'ELAS') .or. (argjj(1:9).eq.'LIQU_SATU')) then
                                    ellisq = .true.
                                endif
102                          continue
                        endif
101                  continue
                endif
            endif
10      continue
    endif
!
    if (.not.ellisq) then
        call u2mess('F', 'INDICATEUR_23')
    endif
!
999  continue
!
    call jedema()
!
end subroutine
