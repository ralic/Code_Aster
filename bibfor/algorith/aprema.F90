subroutine aprema(sdappa)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
#include "asterfort/apcopt.h"
#include "asterfort/apinfi.h"
#include "asterfort/apnomk.h"
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
    character(len=19) :: sdappa
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (ALGO)
!
! RECHERCHE DE LA MAILLE MAITRE LA PLUS PROCHE DU POINT COURANT
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: rnomsd, defico
    character(len=19) :: newgeo
    character(len=8) :: noma
    integer :: izone, ip, i, posnom
    integer :: nbzone, ndimg, ntpt
    integer :: nbpt, posmam, iprojm
    real(kind=8) :: coorpt(3), tau1m(3), tau2m(3), distm, ksi1m, ksi2m
    real(kind=8) :: dir(3), toleou, epsmax, vecpmm(3)
    integer :: itemax
    logical :: dirapp, lmaesc, lsauve
    integer :: typapp, entapp
    integer :: vali(2)
    real(kind=8) :: valr(12)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<APPARIEMENT> RECH. MAILLE PLUS PROCHE'
    endif
!
! --- PARAMETRES
!
    call apnomk(sdappa, 'NOMA', rnomsd)
    noma = rnomsd(1:8)
    call apnomk(sdappa, 'NEWGEO', rnomsd)
    newgeo = rnomsd(1:19)
    call apnomk(sdappa, 'DEFICO', defico)
    call appari(sdappa, 'APPARI_NBZONE', nbzone)
    call appari(sdappa, 'PROJ_NEWT_ITER', itemax)
    call apparr(sdappa, 'PROJ_NEWT_RESI', epsmax)
    call appari(sdappa, 'APPARI_NDIMG', ndimg)
    call appari(sdappa, 'APPARI_NTPT', ntpt)
!
! --- BOUCLE SUR LES ZONES
!
    ip = 1
    do 10 izone = 1, nbzone
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
        do 20 i = 1, nbpt
!
! ------- LE POINT DOIT-IL ETRE APPARIE ?
!
            call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
            call assert(typapp.ne.0)
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
                call approj(sdappa, noma, newgeo, defico, posnom,&
                            dirapp, dir, itemax, epsmax, toleou,&
                            coorpt, posmam, iprojm, ksi1m, ksi2m,&
                            tau1m, tau2m, distm, vecpmm)
!
! --------- ORTHOGONALISATION VECTEURS TANGENTS
!
                call aporth(sdappa, noma, defico, ndimg, posmam,&
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
20      continue
10  end do
!
    call jedema()
!
end subroutine
