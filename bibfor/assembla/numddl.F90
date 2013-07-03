subroutine numddl(nu, base, nbmat, tlimat, method)
! aslint: disable=W1306
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
#include "asterfort/infniv.h"
#include "asterfort/jedetr.h"
#include "asterfort/nueffe.h"
#include "asterfort/numoch.h"
    character(len=2) :: base
    character(len=*) :: nu, tlimat(*), method
    integer :: nbmat
! ----------------------------------------------------------------------
! --- DESCRIPTION DES PARAMETRES
! OUT K*14 NU     : LE CHAMP .NUME DE S.D. NUME_DDL_EQUA DE L'OBJET NU
!                   DE S.D. NUME_DDL EST CREE ET REMPLI.
! IN  K*14 NU     : NOM D'UN NUMERO_DDL(SI UN OBJET DE NOM NU ET
!                   DE S.D. NUME_DDL EXISTE ON L'ECRASE)
! IN  K2   BASE    : BASE(1:1) : BASE POUR CREER LE NUME_DDL
!                    (SAUF LE PROF_CHNO)
!                  : BASE(2:2) : BASE POUR CREER LE PROF_CHNO
! IN  I    NBMAT  : NOMBRE DE MATELE PASSES DANS TLIMAT
! IN  K19  TLIMAT : LISTE DES MATELE DEFINISSANT LA NUMEROTATION
! IN  K*4  METHOD : METHODE DE RENUMEROTATION DES NOEUDS:
!                   'SANS' --> ON NE RENUMEROTE PAS
!                   'RCMK' --> 'REVERSE CUTHIL MAC-KEE'
!                   SINON LA NUMEROTATION EST BASEE SUR CETTE GRANDEUR
!----------------------------------------------------------------------
!
    character(len=14) :: nupgm
    integer :: ibid
    character(len=19) :: k19bid
    character(len=24) :: tlima2(nbmat)
!
!------RECUPERATION DU NIVEAU D'IMPRESSION
!-----------------------------------------------------------------------
    integer :: i, ifm, niv
!-----------------------------------------------------------------------
    call infniv(ifm, niv)
!
    nupgm = nu
    k19bid = ' '
!
    do 10 i = 1, nbmat
        tlima2(i)=tlimat(i)
10  end do
!---- CREATION D'UNE LISTE DE LIGRELS A PARTIR DE LA LISTE DE MATR_ELEM
!
    call numoch(tlima2, nbmat, 'V', nupgm//'.&LMODCHAR')
    call nueffe(nupgm//'.&LMODCHAR', base, nupgm, method, ' ',&
                k19bid, ibid)
    call jedetr(nupgm//'.&LMODCHAR')
!
end subroutine
