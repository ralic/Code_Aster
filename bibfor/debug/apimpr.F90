subroutine apimpr(sdappa, ifm)
!
    implicit     none
!
#include "jeveux.h"
#include "asterfort/apcopt.h"
#include "asterfort/apinfi.h"
#include "asterfort/apinfr.h"
#include "asterfort/apnomk.h"
#include "asterfort/apnomp.h"
#include "asterfort/apnumm.h"
#include "asterfort/apnumn.h"
#include "asterfort/appari.h"
#include "asterfort/apvect.h"
#include "asterfort/apzoni.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
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
    integer, intent(in) :: ifm
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! IMPRESSION DES INFOS DETAILLES DE L'APPARIEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: rnomsd, defico
    integer :: nbzone, ntpt, nbpt
    integer :: typapp, entapp
    real(kind=8) :: coorpt(3)
    real(kind=8) :: dist, ksi1, ksi2, tau1(3), tau2(3)
    character(len=16) :: nompt
    integer :: izone, ip, k, i
    integer :: numnom(1), nummam
    integer :: posnom(1), posmam
    character(len=8) :: noma, nomnom, nommam
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NOM SD MAILLAGE
!
    call apnomk(sdappa, 'NOMA', rnomsd)
    noma = rnomsd(1:8)
    call apnomk(sdappa, 'DEFICO', defico)
!
! --- INITIALISATIONS
!
    ip = 1
    call appari(sdappa, 'APPARI_NBZONE', nbzone)
    call appari(sdappa, 'APPARI_NTPT', ntpt)
!
! ----------------------------------------------------------------------
! --- INFOS SUR LES ZONES
! ----------------------------------------------------------------------
!
    write(ifm,*) '<APPARIEMENT> ------ ZONES ------ '
!
    write(ifm,100) nbzone
    write(ifm,101) ntpt
!
    100 format (' <APPARIEMENT> NOMBRE DE ZONES                   : ',i6)
    101 format (' <APPARIEMENT> NOMBRE MAX. DE POINTS A APPARIER  : ',i6)
!
! --- BOUCLE SUR LES ZONES
!
    do izone = 1, nbzone
!
! ----- INFORMATION SUR LA ZONE
!
        call apzoni(sdappa, izone, 'NBPT', nbpt)
!
! ----- BOUCLE SUR LES POINTS
!
        do i = 1, nbpt
!
! ------- INFOS SUR LE POINT
!
            call apnomp(sdappa, ip, nompt)
            write(ifm,400) ip,nompt
            call apcopt(sdappa, ip, coorpt)
!
!
! ------- INFOS APPARIEMENT
!
            call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
            call apinfi(sdappa, 'APPARI_ENTITE', ip, entapp)
            call apinfr(sdappa, 'APPARI_PROJ_KSI1', ip, ksi1)
            call apinfr(sdappa, 'APPARI_PROJ_KSI2', ip, ksi2)
            call apinfr(sdappa, 'APPARI_DIST', ip, dist)
            call apvect(sdappa, 'APPARI_TAU1', ip, tau1)
            call apvect(sdappa, 'APPARI_TAU2', ip, tau2)
!
            if (typapp .eq. -1) then
                write(ifm,501)
            else if (typapp.eq.-2) then
                write(ifm,502)
            else if (typapp.eq.-3) then
                write(ifm,503)
            else if (typapp.eq.0) then
                write(ifm,504)
            else if (typapp.eq.1) then
                write(ifm,401) coorpt(1),coorpt(2),coorpt(3)
                posnom = entapp
                call apnumn(defico, posnom(1), numnom(1))
                call jenuno(jexnum(noma//'.NOMNOE', numnom(1)), nomnom)
                write(ifm,601) nomnom
                write(ifm,801) dist
            else if (typapp.eq.2) then
                write(ifm,401) coorpt(1),coorpt(2),coorpt(3)
                posmam = entapp
                call apnumm(sdappa, defico, posmam, nummam)
                call jenuno(jexnum(noma//'.NOMMAI', nummam), nommam)
                write(ifm,602) nommam
!
                write(ifm,701) ksi1,ksi2
                write(ifm,801) dist
                write(ifm,901) (tau1(k),k=1,3)
                write(ifm,902) (tau2(k),k=1,3)
            else
                write(ifm,504)
            endif
!
! ------- POINT SUIVANT
!
            ip = ip + 1
        end do
    end do
!
    400 format (' <APPARIEMENT> POINT            ',i6,' (',&
     &        a16,')')
    401 format (' <APPARIEMENT> ** DE COORDONNEES ',1pe15.8,&
     &        1pe15.8,1pe15.8)
!
    501 format (' <APPARIEMENT> -> EXCLU - PAR SANS_NOEUD')
    502 format (' <APPARIEMENT> -> EXCLU - PAR TOLE_APPA')
    503 format (' <APPARIEMENT> -> EXCLU - PAR TOLE_PROJ_EXT')
    504 format (' <APPARIEMENT> -> NON APPARIE (ERREUR)')
!
!
    601 format (' <APPARIEMENT> -> APPARIEMENT AVEC NOEUD  ',a8)
    602 format (' <APPARIEMENT> -> APPARIEMENT AVEC MAILLE ',a8)
!
    701 format (' <APPARIEMENT>      SUR POINT KSI1,KSI2: ',&
     &          1pe15.8,1pe15.8)
    801 format (' <APPARIEMENT>      DISTANCE: ',1pe15.8)
    901 format (' <APPARIEMENT>      TANGENTE BRUTE  DIR. 1   : ',&
     &         3(1pe15.8,2x))
    902 format (' <APPARIEMENT>      TANGENTE BRUTE  DIR. 2   : ',&
     &         3(1pe15.8,2x))
!
    call jedema()
!
end subroutine
