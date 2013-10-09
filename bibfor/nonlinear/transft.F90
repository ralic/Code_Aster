subroutine transft(modein, kvec, neq, nbpt, nomres)
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
!     -----------------------------------------------------------------
! TRANSFORMER LES CONTRIBUTIONS HARMONIQUES EN REPONSE TEMPORELLE
!     -----------------------------------------------------------------
! IN MODEIN : SD MODE_MECA A TRANSCRIRE EN REPONSE TEMPORELLE
! IN KVEC   : VECTEUR OU SE TROUVENT LES COEFF DE FOURIER
! IN NEQ    : NB DDL
! IN NBPT   : DISCRETISATION TEMPORELLE
! OUT NOMRES: SD DYNA_TRANS EN SORTIE
!
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mnlfft.h"
#include "asterfort/refdaj.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
!     -----------------------------------------------------------------
    character(len=4) :: nomsym(1)
!
    character(len=8) :: nomres, modein, k8b
    character(len=16) :: typres
    character(len=19) :: krefe, kvec
    character(len=24) :: chamno
    character(len=24) :: numedd, rigid, masse, amort, matrice(3)
!
    integer :: nbordr, lvect, ier
    integer :: iadd, iarchi, ieq, itps
    integer :: nbmode, neq, nbpt
    integer :: iaux, jaux, nt, nbhar, lrep
    real(kind=8) :: freq, deltat, temps
!     -----------------------------------------------------------------
    call jemarq()
    typres = 'DYNA_TRANS'
!
!-- RECUPERATION DES CONTRIBUTIONS HARMONIQUES
!   (SOUS FORME DE LISTE DE MODES)
!
    call jelira(modein//'           .ORDR', 'LONUTI', nbmode, k8b)
! RECUPERATION NUMEROS D ORDRE
!
    call dismoi('NUME_DDL', modein, 'RESU_DYNA', repk=numedd)
    call dismoi('REF_RIGI_PREM', modein, 'RESU_DYNA', repk=rigid)
    call dismoi('REF_MASS_PREM', modein, 'RESU_DYNA', repk=masse)
    call dismoi('REF_AMOR_PREM', modein, 'RESU_DYNA', repk=amort)
!
! RECUPERATION DE LA FREQUENCE
    call rsadpa(modein, 'L', 1, 'FREQ', 2,&
                0, sjv=iaux, styp=k8b)
    freq = zr(iaux)
!
!     --- RECUPERATION DES VECTEURS PROPRES ET DES GRANDEURS MODALES ---
!
    call jeveuo(kvec, 'L', lvect)
!
!   CREATION STRUCTURE DE DONNEES DE TYPE : dyna_trans
    nbordr = nbmode
    nbhar = int(nbordr/2)
!
    nt = 2**int(1.+ log10(dble(nbpt))/log10(2.))
    call rscrsd('G', nomres, typres, nt)
    krefe(1:19) = nomres
    matrice(1) = rigid
    matrice(2) = masse
    matrice(3) = amort
    call refdaj('F', nomres, nbmode, numedd, 'DYNAMIQUE',&
                matrice, ier)
!
    iarchi = -1
! IARCHI : numero ordre
!
    deltat = 1./(freq*nt)
!
    call wkvect(krefe//'.REP', 'V V R', neq*nt, lrep)
!
    call mnlfft(neq, zr(lvect), zr(lrep), nbhar, nt,&
                0)
!
    nomsym(1) = 'DEPL'
!
    do 200 itps = 1, nt
        temps = (itps-1)*deltat
!
        iarchi = iarchi + 1
        call rsadpa(nomres, 'E', 1, 'INST', iarchi,&
                    0, sjv=iaux, styp=k8b)
        zr(iaux) = temps
!
        call rsexch(' ', nomres, nomsym(1), iarchi, chamno,&
                    iaux)
        call vtcrem(chamno, rigid, 'G', 'R')
!
        chamno(20:24) = '.VALE'
        call jeveuo(chamno, 'E', jaux)
!
        do 211 , ieq = 1, neq
        iadd = (itps-1)*neq+ieq
        zr(jaux-1+ieq) = zr(lrep-1+iadd)
211     continue
!
        call rsnoch(nomres, nomsym(1), iarchi)
200 continue
!
    call jedema()
end subroutine
