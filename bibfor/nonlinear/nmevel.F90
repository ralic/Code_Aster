subroutine nmevel(sddisc, numins, defico, resoco, vale,&
                  nombcl, lsvimx, ldvres, linsta, lerrcv,&
                  lerror, conver)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/eneven.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmevcx.h"
#include "asterfort/nmevdg.h"
#include "asterfort/nmevin.h"
#include "asterfort/utdidt.h"
    character(len=19) :: sddisc, vale(*)
    character(len=4) :: nombcl
    character(len=24) :: defico, resoco
    integer :: numins
    aster_logical :: lsvimx, ldvres, linsta, lerrcv, lerror, conver
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! DETECTION DU PREMIER EVENEMENT DECLENCHE
!
! ----------------------------------------------------------------------
!
! NB: DES QU'UN EVENT-DRIVEN EST SATISFAIT, ON SORT
! ON NE CHERCHE PAS A VERIFIER LES AUTRES EVENEMENTS
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  VALE   : INCREMENTS DES VARIABLES
!               OP0070: VARIABLE CHAPEAU
!               OP0033: TABLE
! IN  NOMBCL : NOM DE LA BOUCLE
!               'RESI' - RESIDUS D'EQUILIBRE
!               'NEWT' - BOUCLE DE NEWTON
!               'FIXE' - BOUCLE DE POINT FIXE
!               'INST' - BOUCLE SUR LES PAS DE TEMPS
! IN  LSVIMX : .TRUE. SI ITERATIONS MAX ATTEINT DANS SOLVEUR ITERATIF
! IN  LDVRES : .TRUE. SI DIVERGENCE DU RESIDU
! IN  LINSTA : .TRUE. SI INSTABILITE DETECTEE
! IN  LERRCV : .TRUE. SI ERREUR A CONVERGENCE DECLENCHEE
! IN  LERROR : .TRUE. SI ERREUR DECLENCHEE
! IN  CONVER : .TRUE. SI BOUCLE COURANTE A CONVERGE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: r8bid
    integer :: ibid, nechec, iechec, ievdac
    character(len=8) :: k8bid
    character(len=16) :: nomevd
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ievdac = 0
!
! --- NOMBRE D'EVENT-DRIVEN : NECHEC
!
    call utdidt('L', sddisc, 'LIST', ibid, 'NECHEC',&
                r8bid, nechec, k8bid)
!
! --- DETECTION DU _PREMIER_ EVENEMENT DECLENCHE
! --- DES QU'UN EVENT-DRIVEN EST SATISFAIT, ON SORT
! --- ON NE CHERCHE PAS A VERIFIER LES AUTRES EVENT
!
    do 100 iechec = 1, nechec
!
! ----- RECUPERATION DU NOM DE L'EVENT-DRIVEN
!
        call utdidt('L', sddisc, 'ECHE', iechec, 'NOM_EVEN',&
                    r8bid, ibid, nomevd)
!
! ----- PAR DEFAUT: EVENEMENT NON ACTIVE
!
        call eneven(sddisc, iechec, .false._1)
!
        if (nomevd .eq. 'ERRE') then
            if (lsvimx .or. lerrcv .or. lerror) then
                ievdac = iechec
                goto 8888
            endif
        else if (nomevd.eq.'DIVE_RESI') then
            if (ldvres) then
                ievdac = iechec
                if (ievdac .ne. 0) goto 8888
            endif
        else if (nomevd.eq.'DELTA_GRANDEUR') then
            if (conver) then
                call nmevdg(sddisc, vale, iechec, ievdac)
                if (ievdac .ne. 0) goto 8888
            endif
        else if (nomevd.eq.'COLLISION') then
            if (nombcl .eq. 'INST') then
                call nmevcx(sddisc, numins, defico, resoco, iechec,&
                            ievdac)
                if (ievdac .ne. 0) goto 8888
            endif
        else if (nomevd.eq.'INTERPENETRATION') then
            if (nombcl .eq. 'INST') then
                call nmevin(sddisc, resoco, iechec, ievdac)
                if (ievdac .ne. 0) goto 8888
            endif
        else if (nomevd.eq.'INSTABILITE') then
            if (linsta) ievdac = iechec
            if (ievdac .ne. 0) goto 8888
        else
            write(6,*) 'NOMEVD: ',nomevd
            ASSERT(.false.)
        endif
100 end do
!
8888 continue
!
! --- DECLENCHEMENT DE L'EVENEMENT
!
    if (ievdac .ne. 0) then
        call eneven(sddisc, ievdac, .true._1)
    endif
!
    call jedema()
end subroutine
