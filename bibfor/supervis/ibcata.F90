subroutine ibcata(ier)
    implicit none
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/ibcatc.h"
#include "asterfort/lxcadr.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/uldefi.h"
#include "asterfort/utremt.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
    integer :: ier
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ALLOCATION ET LECTURE DES DIFFERENTS CATALOGUES
!     ------------------------------------------------------------------
!     ROUTINE(S) UTILISEE(S) :
!         GETRES GETFAC GETVXX
!         IBOPER
!     ------------------------------------------------------------------
    real(kind=8) :: temps(6)
    real(kind=8) :: valr
    character(len=8) :: nomres
    character(len=16) :: concep, nomcmd, motfac
!     ------------------------------------------------------------------
!     --- DEFAUT POUR LES CATALOGUES D'ELEMENTS
!     --- 3  = CATAELEM
!     --- 4  = BASELEM
!-----------------------------------------------------------------------
    integer :: icata, ieltdf, ier1, iocc, iplace, iun, mxcata
    integer :: mxdfca, nbcata, nbnom, nbocc, nbunit
!-----------------------------------------------------------------------
    parameter          ( ieltdf = 4 )
!     ------------------------------------------------------------------
    parameter          ( mxdfca = 4 ,       mxcata = 10 )
    character(len=32) :: dfnom(mxdfca), nom (mxcata)
    character(len=24) :: valk
    integer :: dfunit(mxdfca), unite(mxcata)
    integer ::  i
!     ------------------------------------------------------------------
!     OPTIONS PAR DEFAUT :
!
    data ( dfnom(i), dfunit(i), i=1,mxdfca) /&
     &    'COMMANDE_PRIVEE  ',03,&
     &    'COMMANDE         ',02,&
     &    'CATAELEM         ',04,&
     &    'ELEMBASE         ',04/
!     ------------------------------------------------------------------
!
    call uldefi(6, ' ', 'MESSAGE', 'A', 'N',&
                'N')
!     --- LA ROUTINE NE S'INTERRESSE QU'AU MOT CLE FACTEUR "CATALOGUE" -
!     --- DANS LA COMMANDE DEBUT
    ier = 0
    motfac = 'CATALOGUE'
!
!     --- RECUPERATION DU NOM DE LA COMMANDE UTILISATEUR ---
    call getres(nomres, concep, nomcmd)
!
!     --- NOMBRE DE CATALOGUES SPECIFIES PAR L'UTILISATEUR ---
    call getfac(motfac, nbocc)
!
    if (nbocc .gt. mxcata) then
        ier = ier + 1
        call u2mess('F', 'SUPERVIS_18')
        nbocc = mxcata
    endif
!
    iun = 1
    do 10 iocc = 1, nbocc
        call getvtx(motfac, 'FICHIER', iocc=iocc, nbval=iun, vect=nom(iocc),&
                    nbret=nbnom)
        call lxcadr(nom(iocc))
        call getvis(motfac, 'UNITE', iocc=iocc, nbval=iun, vect=unite(iocc),&
                    nbret=nbunit)
        if (nbunit .eq. 0) then
            call utremt(nom(iocc), dfnom, mxdfca, iplace)
            if (iplace .gt. 0) unite(iocc) = dfunit(iplace)
        endif
10  end do
!
!     --- CATALOGUE DES ELEMENTS ---
    nbcata = 0
    call u2mess('I', 'SUPERVIS_19')
    call uttcpu('CPU.IBCATA', 'INIT', ' ')
    call uttcpu('CPU.IBCATA', 'DEBUT', ' ')
    do 300 icata = 1, nbocc
        if (nom(icata) .eq. dfnom(3) .or. nom(icata) .eq. dfnom(4)) then
            if (unite(icata) .gt. 0) then
                call ibcatc(nom(icata), unite(icata), ier1)
                ier = ier + ier1
            endif
            nom(icata) = '  '
            nbcata = nbcata + 1
        endif
300  end do
    if (nbcata .eq. 0 .and. nomcmd .eq. 'DEBUT') then
        call ibcatc(dfnom(ieltdf), dfunit(ieltdf), ier1)
        ier = ier + ier1
    endif
    call uttcpu('CPU.IBCATA', 'FIN', ' ')
    call uttcpr('CPU.IBCATA', 6, temps)
    valr = temps(5)
    valk = ' '
    call u2mesg('I', 'SUPERVIS_52', 1, valk, 0,&
                0, 1, valr)
!
!     --- VERIFICATION DE LA COMPLETUDE DE L'EXECUTION ---
    do 900 icata = 1, nbocc
        if (nom(icata) .ne. ' ') then
            call u2mesk('F', 'SUPERVIS_20', 1, nom(icata))
            ier = ier + 1
        endif
900  end do
!
    if (ier .gt. 0) call u2mess('F', 'SUPERVIS_21')
!
end subroutine
