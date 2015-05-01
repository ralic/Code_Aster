subroutine colneu(nbnode, typema)
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
! person_in_charge: nicolas.greffet at edf.fr
! aslint: disable=W1304,W1305
    implicit none
!
!      COLNEU --   LECTURE DES NUMEROS DE NOEUDS ET DE LEURS
!                  COORDONNEES PAR RECEPTION MEMOIRE
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NBNODE         OUT   I         NOMBRE DE NOEUDS DU MAILLAGE
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterc/cpldb.h"
#include "asterc/cplen.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbnode
! -----  VARIABLES LOCALES
!
!
    integer :: inode
    integer :: ndmax
    integer :: jcoor, jgroma, jdetr, jinfo
    integer :: ibid
!
!     COUPLAGE =>
!
!     ANCIENS INCLUDE (CALCIUM.H)
!     ===========================
    integer(kind=4) :: lenvar, cpiter, taille, ibid4, nbnod4, un, nbnod4t(1)
    integer :: nbnmax, iadr
    parameter (lenvar = 144)
    parameter (cpiter= 41)
    parameter (nbnmax= 100000)
    integer(kind=4) :: int4(nbnmax), i4
    integer :: icompo, ifm, niv, vali(2)
    real(kind=4) :: tr4
    character(len=8) :: typema, valk(3)
    character(len=24) :: ayacs
    character(len=lenvar) :: nomvar
!     COUPLAGE <=
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
    call jemarq()
    call infniv(ifm, niv)
!
! --- INITIALISATION :
!     --------------
    nbnode = 0
!
!     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
!     ------------------------------------------------------------
    ayacs='&ADR_YACS'
!
!     RECUPERATION DE L'ADRESSE YACS
!     ------------------------------
    call jeveuo(ayacs, 'L', iadr)
    icompo=zi(iadr)
!
! --- LECTURE DU NOMBRE DE NOEUDS :
!     ---------------------------
    if (typema(1:7) .eq. 'SOMMET') nomvar = 'NB_DYN'
    if (typema(1:7) .eq. 'MILIEU') nomvar = 'NB_FOR'
!
    un = 1
    tr4 = 0.d0
    i4 = 0
    call cplen(icompo, cpiter, tr4, tr4, i4,&
               nomvar, un, taille, nbnod4t, ibid4)
    nbnod4 = nbnod4t(1)
    nbnode = nbnod4
    if (nbnode .gt. nbnmax) then
        vali(1) = nbnode
        vali(2) = nbnmax
        call utmess('F', 'COUPLAGEIFS_12', ni=2, vali=vali)
    endif
!
! --- CREATION DE VECTEURS DE TRAVAIL :
!     -------------------------------
    call jedetr('&&PRECOU.INFO.NOEUDS')
    call jedetr('&&PRECOU.DETR.NOEUDS')
    call jedetr('&&PRECOU.COOR.NOEUDS')
    call jedetr('&&PRECOU.GROUPE.MAILLES')
!
    call wkvect('&&PRECOU.INFO.NOEUDS', 'V V I', nbnode, jinfo)
    call wkvect('&&PRECOU.COOR.NOEUDS', 'V V R', 3*nbnode, jcoor)
    call wkvect('&&PRECOU.GROUPE.MAILLES', 'V V I', nbnode, jgroma)
!
! --- LECTURE DES NUMEROS DE NOEUDS ET DE LEURS COORDONNEES :
!     -----------------------------------------------------
    if (typema(1:7) .eq. 'SOMMET') nomvar = 'COONOD'
    if (typema(1:7) .eq. 'MILIEU') nomvar = 'COOFAC'
    if (niv .eq. 2) then
        valk(1) = 'COLNEU'
        valk(2) = 'NOMVAR'
        valk(3) = nomvar(1:8)
        call utmess('I+', 'COUPLAGEIFS_11', nk=3, valk=valk)
    endif
    call cpldb(icompo, cpiter, 0.d0, 0.d0, int(0, 4),&
               nomvar, int(3*nbnod4, 4), taille, zr(jcoor), ibid4)
    if (niv .eq. 2) then
        valk(1) = 'COLNEU'
        valk(2) = 'IBID'
        ibid = ibid4
        call utmess('I+', 'COUPLAGEIFS_10', nk=2, valk=valk, si=ibid)
    endif
!
    if (typema(1:7) .eq. 'SOMMET') nomvar = 'COLNOD'
    if (typema(1:7) .eq. 'MILIEU') nomvar = 'COLFAC'
    if (niv .eq. 2) then
        valk(1) = 'COLNEU'
        valk(2) = 'NOMVAR'
        valk(3) = nomvar(1:8)
        call utmess('I+', 'COUPLAGEIFS_11', nk=3, valk=valk)
    endif
    call cplen(icompo, cpiter, tr4, tr4, i4,&
               nomvar, nbnod4, taille, int4(1), ibid4)
    if (niv .eq. 2) then
        valk(1) = 'COLNEU'
        valk(2) = 'IBID'
        ibid = ibid4
        call utmess('I+', 'COUPLAGEIFS_10', nk=2, valk=valk, si=ibid)
    endif
    do inode = 1, nbnode
        zi(jgroma-1+inode) = int4(inode)
    end do
    if (niv .eq. 2) then
        valk(1) = 'COLNEU'
        valk(2) = 'NBNODE'
        call utmess('I+', 'COUPLAGEIFS_10', nk=2, valk=valk, si=nbnode)
    endif
!
    ndmax = nbnode
    do inode = 1, nbnode
        zi(jinfo+inode-1) = inode
    end do
!     LISTE DES NOEUDS A DETRUIRE (0: A DETRUIRE, 1: A CONSERVER)
    call wkvect('&&PRECOU.DETR.NOEUDS', 'V V I', ndmax+1, jdetr)
    do inode = 0, ndmax
        zi(jdetr+inode) = 0
    end do
!
    call jedema()
end subroutine
