subroutine nxcerr(sddisc)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/ceil.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmlerr.h"
#include "asterfort/nxdocn.h" 
#include "asterfort/utdidt.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    character(len=19), intent(in) :: sddisc
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE THER_NON_LINE (STRUCTURES DE DONNES - SD DISCRETISATION)
!
! CREATION SD STOCKAGE DES INFOS EN COURS DE CALCUL
!
! --------------------------------------------------------------------------------------------------
!
! Inout  sddisc        : datastructure for time discretization
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: r8bid, pcplus
    real(kind=8) :: resi_glob_rela, resi_glob_maxi, inikry
    real(kind=8), dimension(2) :: parcrr
    integer, dimension(3) :: parcri
    integer :: ibid, nbiter, iter_glob_maxi
    integer :: nmax, nplus
    integer :: i_echec, nb_echec
    character(len=24) :: infocv, infore
    integer :: jifcv, jifre
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES PARAMETRES DU FICHIER DE COMMANDE
!
    call nxdocn(parcri, parcrr)
    iter_glob_maxi = parcri(3)
    resi_glob_rela = parcrr(2)
    resi_glob_maxi = parcrr(1)
!
    nbiter = iter_glob_maxi
!
! --- MEME VALEUR D'INITIALISATION QUE DANS NMCRSU
!
    inikry = 0.9d0
!
! --- CREATION DU VECTEUR D'INFORMATIONS SUR LA CONVERGENCE
!
    infocv = sddisc(1:19)//'.IFCV'
    call wkvect(infocv, 'V V R', 10, jifcv)
!
! --- NB MAX D'ITERATIONS : ITER_GLOB_MAXI
! --- RESIDUS     : RELA ET MAXI
!
    call nmlerr(sddisc, 'E', 'MXITER', r8bid, iter_glob_maxi)
    call nmlerr(sddisc, 'E', 'NBITER', r8bid, nbiter)
    call nmlerr(sddisc, 'E', 'RESI_GLOB_RELA', resi_glob_rela, ibid)
    call nmlerr(sddisc, 'E', 'RESI_GLOB_MAXI', resi_glob_maxi, ibid)
!
! --- RESIDU INITIAL POUR NEWTON-KRYLOV
!
    call nmlerr(sddisc, 'E', 'INIT_NEWTON_KRYLOV', inikry, ibid)
!
! --- RESIDU COURANT POUR NEWTON-KRYLOV
!
    call nmlerr(sddisc, 'E', 'ITER_NEWTON_KRYLOV', inikry, ibid)
!
! --- CREATION DU VECTEUR DE STOCKAGE DES RESIDUS
!
    nbiter = nbiter+1
    infore = sddisc(1:19)//'.IFRE'
    call wkvect(infore, 'V V R', 3*nbiter, jifre)
!
    call jedema()
end subroutine
