subroutine avdowh(nbvec, nbordr, nommat, nomcri, ncycl,&
                  gdeq, grdvie, forvie, post, domel,&
                  nrupt)
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
! person_in_charge: jean.angles at edf.fr
    implicit     none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/limend.h"
#include "asterfort/rccome.h"
#include "asterfort/rcpare.h"
#include "asterfort/rcvale.h"
#include "asterfort/renrfa.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: nbvec, nbordr, ncycl(nbvec)
    real(kind=8) :: gdeq(nbvec*nbordr)
    real(kind=8) :: nrupt(nbvec*nbordr), domel(nbvec*nbordr)
    logical :: post
    character(len=8) :: nommat, grdvie
    character(len=16) :: nomcri, forvie
! ----------------------------------------------------------------------
! BUT: CALCULER LE DOMMAGE ELEMENTAIRE A PARTIR D'UNE COURBE
!      GRANDEUR EQ - VIE POUR TOUS LES CYCLES
!      ELEMETAIRES DE CHAQUE VECTEUR NORMAL.
! ----------------------------------------------------------------------
! ARGUMENTS :
!  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
!  NBORDR   IN   I  : NOMBRE DE NUMEROS D'ORDRE.
!  NOMMAT   IN   K  : NOM DU MATERIAU.
!  NOMCRI   IN   K  : NOM DU CRITERE.
!  NCYCL    IN   I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
!  GDEQ     IN   R  : VECTEUR CONTENANT LES VALEURS DE LA GRANDEUR
!                     EQUIVALENTE (SIGEQ OU EPSEQ), POUR TOUS LES SOUS
!                     CYCLES DE CHAQUE VECTEUR NORMAL.
!  DOMEL    OUT  R  : VECTEUR CONTENANT LES VALEURS DES DOMMAGES
!                     ELEMENTAIRES, POUR TOUS LES SOUS CYCLES
!                     DE CHAQUE VECTEUR NORMAL.
!  NRUPT    OUT  R  : VECTEUR CONTENANT LES NOMBRES DE CYCLES
!                     ELEMENTAIRES, POUR TOUS LES SOUS CYCLES
!                     DE CHAQUE VECTEUR NORMAL.
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: ivect, icycl, adrs, i
    integer :: icodre
    character(len=16) :: phenom, kbid
    character(len=8) :: nomgrd
    logical :: limit
!     ------------------------------------------------------------------
!
!234567                                                              012
!
    call jemarq()
!
! INITITIALISATION
    do 100 i = 1, nbvec*nbordr
        domel(i) = 0
100  end do
!
    if (.not. post) then
        call rccome(nommat, 'FATIGUE', phenom, icodre)
        if (icodre .eq. 1) call u2mess('F', 'FATIGUE1_24')
    endif
!
    if (nomcri(1:16) .eq. 'FATESOCI_MODI_AV') then
        call rcpare(nommat, 'FATIGUE', 'MANSON_C', icodre)
        if (icodre .eq. 1) then
            call u2mesk('F', 'FATIGUE1_89', 1, nomcri(1:16))
        endif
!
        do 10 ivect = 1, nbvec
            do 20 icycl = 1, ncycl(ivect)
                adrs = (ivect-1)*nbordr + icycl
!
                call rcvale(nommat, 'FATIGUE', 1, 'EPSI    ', gdeq(adrs),&
                            1, 'MANSON_C', nrupt(adrs), icodre, 1)
!
                call limend(nommat, gdeq(adrs), 'MANSON_C', kbid, limit)
                if (limit) then
                    nrupt(adrs)=r8maem()
                else
                    call rcvale(nommat, 'FATIGUE', 1, 'EPSI    ', gdeq(adrs),&
                                1, 'MANSON_C', nrupt(adrs), icodre, 1)
                endif
!
                domel(adrs) = 1.0d0/nrupt(adrs)
                nrupt(adrs) = nint(nrupt(adrs))
!
20          continue
10      continue
!
        elseif (( nomcri(1:14) .eq. 'MATAKE_MODI_AV' ) .or. ( nomcri(1:16)&
    .eq. 'DANG_VAN_MODI_AV' )) then
        call rcpare(nommat, 'FATIGUE', 'WOHLER', icodre)
        if (icodre .eq. 1) then
            call u2mesk('F', 'FATIGUE1_90', 1, nomcri(1:16))
        endif
!
        do 30 ivect = 1, nbvec
            do 40 icycl = 1, ncycl(ivect)
                adrs = (ivect-1)*nbordr + icycl
!
                call limend(nommat, gdeq(adrs), 'WOHLER', kbid, limit)
                if (limit) then
                    nrupt(adrs)=r8maem()
                else
                    call rcvale(nommat, 'FATIGUE', 1, 'SIGM    ', gdeq(adrs),&
                                1, 'WOHLER  ', nrupt(adrs), icodre, 1)
                endif
!
                domel(adrs) = 1.0d0/nrupt(adrs)
                nrupt(adrs) = nint(nrupt(adrs))
!
40          continue
30      continue
!
    else if (nomcri(1:7) .eq. 'FORMULE') then
!
        do 50 ivect = 1, nbvec
            do 60 icycl = 1, ncycl(ivect)
                adrs = (ivect-1)*nbordr + icycl
!
                call limend(nommat, gdeq(adrs), grdvie, forvie, limit)
!
                if (limit) then
                    nrupt(adrs)=r8maem()
                else
!
                    if (grdvie(1:6) .eq. 'WOHLER') then
                        nomgrd = 'SIGM    '
                        grdvie(7:8) = '  '
!
                        call rcvale(nommat, 'FATIGUE', 1, nomgrd, gdeq(adrs),&
                                    1, grdvie, nrupt(adrs), icodre, 1)
                    endif
!
                    if (grdvie(1:8) .eq. 'MANSON_C') then
                        nomgrd = 'EPSI    '
                        call rcvale(nommat, 'FATIGUE', 1, nomgrd, gdeq(adrs),&
                                    1, grdvie, nrupt(adrs), icodre, 1)
!
                    endif
!
                    if (grdvie(1:8) .eq. 'FORM_VIE') then
                        call renrfa(forvie, gdeq(adrs), nrupt(adrs), icodre)
                    endif
!
                    domel(adrs) = 1.0d0/nrupt(adrs)
                    nrupt(adrs) = nint(nrupt(adrs))
!
                endif
!
!
60          continue
50      continue
!
!
!
    endif
!
    call jedema()
!
end subroutine
