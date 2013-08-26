subroutine avdowh(nbvec, nbordr, nommat, nomcri, ncycl,&
                  jgdeq, grdvie, forvie, post, jdomel,&
                  jnrupt)
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
! person_in_charge: van-xuan.tran at edf.fr
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
!    real(kind=8) :: gdeq(nbvec*nbordr)
!    real(kind=8) :: nrupt(nbvec*nbordr), domel(nbvec*nbordr)
    integer :: jgdeq, jnrupt, jdomel
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
!  JGDEQ     IN   I  : ADDRESSE VECTEUR CONTENANT LES VALEURS DE LA GRANDEUR
!                     EQUIVALENTE (SIGEQ OU EPSEQ), POUR TOUS LES SOUS
!                     CYCLES DE CHAQUE VECTEUR NORMAL.
!  JDOMEL    OUT  I  : ADDRESSE VECTEUR CONTENANT LES VALEURS DES DOMMAGES
!                     ELEMENTAIRES, POUR TOUS LES SOUS CYCLES
!                     DE CHAQUE VECTEUR NORMAL.
!  JNRUPT    OUT  I  : ADDRESSE VECTEUR CONTENANT LES NOMBRES DE CYCLES
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
        zr(jdomel+i) = 0
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
                call rcvale(nommat, 'FATIGUE', 1, 'EPSI    ', zr(jgdeq+adrs),&
                            1, 'MANSON_C', zr(jnrupt+adrs), icodre, 1)
!
                call limend(nommat, zr(jgdeq+adrs), 'MANSON_C', kbid, limit)
                if (limit) then
                    zr(jnrupt+adrs)=r8maem()
                else
                    call rcvale(nommat, 'FATIGUE', 1, 'EPSI    ', &
                      zr(jgdeq+adrs), 1, 'MANSON_C', zr(jnrupt+adrs), icodre, 1)
                endif
!
                zr(jdomel+adrs) = 1.0d0/zr(jnrupt+adrs)
                zr(jnrupt+adrs) = nint(zr(jnrupt+adrs))
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
                call limend(nommat, zr(jgdeq+adrs), 'WOHLER', kbid, limit)
                if (limit) then
                    zr(jnrupt+adrs)=r8maem()
                else
                    call rcvale(nommat, 'FATIGUE', 1, 'SIGM    ', &
                     zr(jgdeq+adrs), 1, 'WOHLER  ', zr(jnrupt+adrs), icodre, 1)
                endif
!
                zr(jdomel+adrs) = 1.0d0/zr(jnrupt+adrs)
                zr(jnrupt+adrs) = nint(zr(jnrupt+adrs))
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
                call limend(nommat, zr(jgdeq+adrs), grdvie, forvie, limit)
!
                if (limit) then
                    zr(jnrupt+adrs)=r8maem()
                else
!
                    if (grdvie(1:6) .eq. 'WOHLER') then
                        nomgrd = 'SIGM    '
                        grdvie(7:8) = '  '
!
                        call rcvale(nommat, 'FATIGUE', 1, nomgrd, &
                          zr(jgdeq+adrs),1, grdvie, zr(jnrupt+adrs), icodre, 1)
                    endif
!
                    if (grdvie(1:8) .eq. 'MANSON_C') then
                        nomgrd = 'EPSI    '
                        call rcvale(nommat, 'FATIGUE', 1, nomgrd, &
                        zr(jgdeq+adrs), 1, grdvie, zr(jnrupt+adrs), icodre, 1)
!
                    endif
!
                    if (grdvie(1:8) .eq. 'FORM_VIE') then
                        call renrfa(forvie, zr(jgdeq+adrs), zr(jnrupt+adrs),&
                                   icodre)
                    endif
!
                    zr(jdomel+adrs) = 1.0d0/zr(jnrupt+adrs)
                    zr(jnrupt+adrs) = nint(zr(jnrupt+adrs))
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
