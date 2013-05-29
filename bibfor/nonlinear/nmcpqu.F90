subroutine nmcpqu(compor, nomcmz, nompaz, exist)
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
    include 'asterfort/carces.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/cesred.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=19) :: compor
    character(len=*) :: nomcmz
    character(len=*) :: nompaz
    logical :: exist
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! INTERROGATION DE LA CARTE COMPOR
!
! ----------------------------------------------------------------------
!
! IN  COMPOR : CARTE COMPORTEMENT
! IN  NOMCMP : NOM DE LA CMP DE LA GRANDEUR COMPOR QUE L'ON VEUT TESTER
!               'RELCOM': EST-CE QUE CE COMPORTEMENT EXISTE ?
!               'DEFORM': EST-CE QUE DEFORM = NOMPAR
!               'C_PLAN': EST-CE QUE ALGO_C_PLAN OU ALGO_1D =DEBORST
!                ETC...
! IN  NOMPAZ : NO DU PARAMETRE INTERROGE (PAR EX. NOM DU COMPORTEMENT
!                RECHERCHE'
! OUT EXIST  : REPONSE A LA NOMCMPON
!
!
!
!
!
    character(len=24) :: nomcmp
    character(len=16) :: nompar, comp
    integer :: iret, ima, jdecal
    integer :: jcesd, jcesl, jcesv
    integer :: nbma
    character(len=19) :: coto, copm
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nomcmp = nomcmz
    nompar = nompaz
    exist = .false.
    coto = '&&NMCPQU.COTO'
    copm = '&&NMCPQU.COPM'
!
! --- TRANSFO CARTE EN CHAM_ELEM_S
!
    call carces(compor, 'ELEM', ' ', 'V', coto,&
                'A', iret)
!
! --- REDUCTION SUR COMPOSANTE
!
    call cesred(coto, 0, 0, 1, nomcmp,&
                'V', copm)
    call detrsd('CHAM_ELEM_S', coto)
!
! --- ACCES CHAM_ELEM_S
!
    call jeveuo(copm(1:19)//'.CESD', 'L', jcesd)
    call jeveuo(copm(1:19)//'.CESL', 'L', jcesl)
    call jeveuo(copm(1:19)//'.CESV', 'L', jcesv)
    nbma = zi(jcesd-1+1)
!
    do 60 ima = 1, nbma
        call cesexi('C', jcesd, jcesl, ima, 1,&
                    1, 1, jdecal)
        comp = zk16(jcesv-1+jdecal)
        if (comp .eq. nompar) then
            exist = .true.
            goto 99
        endif
60  end do
!
99  continue
!
    call detrsd('CHAM_ELEM_S', copm)
!
    call jedema()
!
end subroutine
