subroutine nmcvgc(sddisc, sderro, numins, fonact)
!
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
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmeceb.h'
    include 'asterfort/nmerge.h'
    include 'asterfort/nmevcv.h'
    include 'asterfort/nmfinp.h'
    include 'asterfort/nmleeb.h'
    integer :: fonact(*)
    character(len=19) :: sddisc
    integer :: numins
    character(len=24) :: sderro
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ETAT DE LA CONVERGENCE DU CALCUL
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDERRO : SD GESTION DES ERREURS
! IN  NUMINS : NUMERO D'INSTANT
! IN  FONACT : FONCTIONNALITES ACTIVEES
!
!
!
!
    character(len=4) :: etinst, etcalc
    logical :: lstop, mtcpup
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ETAT DE LA BOUCLE DES INSTANTS
!
    call nmleeb(sderro, 'INST', etinst)
    call nmerge(sderro, 'ERRE_TIMP', mtcpup)
!
! --- SI PAS DE CONVERGENCE INSTANT -> TRANSFERT ETAT DE LA BOUCLE
!
    if (etinst .ne. 'CONV') then
        if (etinst .eq. 'STOP') then
            call nmeceb(sderro, 'CALC', 'STOP')
        else if (etinst.eq.'ERRE') then
            call nmeceb(sderro, 'CALC', 'ERRE')
        else
            call assert(.false.)
        endif
    else
        call nmevcv(sderro, fonact, 'CALC')
    endif
!
    call nmleeb(sderro, 'CALC', etcalc)
!
! --- ERREUR -> ON NE PEUT RIEN FAIRE
!
    if (etcalc .eq. 'ERRE') goto 99
!
! --- ERREUR FATALE -> SI TEMPS CPU SUR LE PAS, ON ATTEND DE VOIR SI
! --- ON EST AU DERNIER PAS
!
    if (etcalc .eq. 'STOP') then
        if (.not.mtcpup) goto 99
    endif
!
! --- CONVERGENCE DU CALCUL: DERNIER PAS !
!
    call nmfinp(sddisc, numins, lstop)
    if (lstop) then
        call nmeceb(sderro, 'CALC', 'CONV')
    endif
!
99  continue
!
    call jedema()
end subroutine
