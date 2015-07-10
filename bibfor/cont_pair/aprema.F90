subroutine aprema(sdappa, mesh, sdcont_defi, newgeo)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/apcopt.h"
#include "asterfort/apinfi.h"
#include "asterfort/aporth.h"
#include "asterfort/appari.h"
#include "asterfort/apparr.h"
#include "asterfort/approj.h"
#include "asterfort/apsauv.h"
#include "asterfort/apzoni.h"
#include "asterfort/apzonl.h"
#include "asterfort/apzonr.h"
#include "asterfort/apzonv.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
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
    character(len=19), intent(in) :: sdappa
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
    character(len=19), intent(in) :: newgeo
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Find nearest element from current contact point
!
! --------------------------------------------------------------------------------------------------
!
! In  sdappa           : name of pairing datastructure
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: izone, ip, i, posnom
    integer :: nbzone, ndimg, ntpt
    integer :: nbpt, posmam, iprojm
    real(kind=8) :: coorpt(3), tau1m(3), tau2m(3), distm, ksi1m, ksi2m
    real(kind=8) :: dir(3), toleou, epsmax, vecpmm(3)
    integer :: itemax
    aster_logical :: dirapp, lmaesc, lsauve
    integer :: typapp, entapp
    integer :: vali(2)
    real(kind=8) :: valr(12)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> RECH. MAILLE PLUS PROCHE'
    endif
!
! --- PARAMETRES
!
    call appari(sdappa, 'APPARI_NBZONE', nbzone)
    call appari(sdappa, 'PROJ_NEWT_ITER', itemax)
    call apparr(sdappa, 'PROJ_NEWT_RESI', epsmax)
    call appari(sdappa, 'APPARI_NDIMG', ndimg)
    call appari(sdappa, 'APPARI_NTPT', ntpt)
!
! --- BOUCLE SUR LES ZONES
!
    ip = 1
    do izone = 1, nbzone
!
! ----- INFORMATION SUR LA ZONE
!
        call apzoni(sdappa, izone, 'NBPT', nbpt)
        call apzonl(sdappa, izone, 'APPA_MAIT_ESCL', lmaesc)
        call apzonl(sdappa, izone, 'DIRE_APPA_FIXE', dirapp)
        if (dirapp) then
            call apzonv(sdappa, izone, 'DIRE_APPA_VECT', dir)
        endif
        call apzonr(sdappa, izone, 'TOLE_PROJ_EXT', toleou)
!
! ----- BOUCLE SUR LES POINTS
!
        do i = 1, nbpt
!
! ------- LE POINT DOIT-IL ETRE APPARIE ?
!
            call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
            ASSERT(typapp.ne.0)
!
! ------- POINT A APPARIER !
!
            if (lmaesc) then
!
! --------- COORDONNEES DU POINT
!
                call apcopt(sdappa, ip, coorpt)
!
! --------- NUMERO DU NOEUD MAITRE LE PLUS PROCHE
!
                call apinfi(sdappa, 'APPARI_ENTITE', ip, entapp)
                posnom = entapp
!
! --------- PROJECTION SUR LA MAILLE MAITRE
!
                call approj(sdappa, mesh, newgeo, sdcont_defi, posnom,&
                            dirapp, dir, itemax, epsmax, toleou,&
                            coorpt, posmam, iprojm, ksi1m, ksi2m,&
                            tau1m, tau2m, distm, vecpmm)
!
! --------- ORTHOGONALISATION VECTEURS TANGENTS
!
                call aporth(sdappa, mesh, sdcont_defi, ndimg, posmam,&
                            coorpt, tau1m, tau2m)
!
                if (typapp .eq. 1) then
                    if (iprojm .eq. 2) then
                        typapp = -3
                    else
                        typapp = 2
                    endif
                endif
                lsauve = .true.
            else
                lsauve = .false.
            endif
!
! ------- STOCKAGE DE L'INFORMATION DANS SDAPPA
!
            if (lsauve) then
                vali(1) = typapp
                vali(2) = posmam
                valr(1) = distm
                valr(2) = ksi1m
                valr(3) = ksi2m
                valr(4) = tau1m(1)
                valr(5) = tau1m(2)
                valr(6) = tau1m(3)
                valr(7) = tau2m(1)
                valr(8) = tau2m(2)
                valr(9) = tau2m(3)
                valr(10) = vecpmm(1)
                valr(11) = vecpmm(2)
                valr(12) = vecpmm(3)
                call apsauv('MA_PROCHE', sdappa, izone, ip, vali,&
                            valr)
            endif
!
! ------- POINT SUIVANT
!
            ip = ip + 1
        end do
    end do
!
    call jedema()
!
end subroutine
