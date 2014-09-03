subroutine mdidisvisc(nomres, nbchoc, logcho, noecho, nbsauv, &
                      temps)
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
    implicit none
    character(len=8) :: nomres
    integer :: nbchoc, nbsauv
    integer :: logcho(nbchoc,*)
    character(len=8) :: noecho(nbchoc,*)
    real(kind=8) :: temps(*)
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jemarq.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
!
! --------------------------------------------------------------------------------------------------
!
!                       IMPRESSION DES RESULTATS SUR LES DIS_VISC
!
! --------------------------------------------------------------------------------------------------
!
! IN
!   nomres  : nom du concept résultat
!   nbchoc  : nombre de liaison non-linéaire
!   logcho  : type de liaison : DIS_VISC => logcho(i,6)=1
!   noecho  : noms des noeuds de choc
!   nbsauv  : nombre de pas sauvegardé
!   temps   : instant de sauvegarde
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ific, nbocc, iocc, iret, ii, nbvint, nbvdisc, it, indx, jvint, vv
    character(len=8) :: noeud1,noeud2
!
    call jemarq()
    call getfac('IMPRESSION',  nbocc)
    if ( nbocc.eq.0 ) then
        goto 999
    endif
!   Récupération des variables internes
!   Longueur maximum d'un bloc de variables internes
    nbvint  = nbchoc*mdtr74grd('MAXVINT')
!   Nombre de variables internes sur les DIS_VISC
    nbvdisc = mdtr74grd('DIS_VISC')
    ASSERT( nbvdisc .eq. 4 )
    call jeveuo(nomres//'           .VINT', 'E', jvint)
!
    do iocc = 1, nbocc
        call getvis('IMPRESSION', 'UNITE_DIS_VISC', iocc=iocc, scal=ific, nbret=iret)
        if ( iret.ne.1 ) cycle
!       Impression des informations sur les DIS_VISC
        if (.not. ulexis( ific )) then
            call ulopen(ific, ' ', ' ', 'NEW', 'O')
        endif
!
        do ii = 1 , nbchoc
            if ( logcho(ii,6).ne.1 ) cycle
!           Noeuds du discret
            noeud1 = noecho(ii,1)
            noeud2 = noecho(ii,5)
!           Impressions des variables internes dans l'ordre de stockage
!                                    sigma, epsivisq, epsi, puiss
            write(ific,100) '#'
            write(ific,100) '#--------------------------------------------------'
            write(ific,100) '#RESULTAT '//nomres
            write(ific,101) '#DIS_VISC ',ii,' '//noeud1//' '//noeud2
            write(ific,102) 'INST','FORCE','DEPLVISC','DEPL','PUISS'
            do it = 1 , nbsauv
                indx = jvint-1 + ii + (it-1)*nbvint
                write(ific,103) temps(it), (zr(indx+(vv-1)*nbchoc),vv=1,nbvdisc)
            enddo
        enddo
!       On ferme le fichier pour être sûr que le flush soit fait
        call ulopen(-ific, ' ', ' ', ' ', ' ')
    enddo
100 format(A)
101 format(A,I5,A)
102 format(5(1X,A18))
103 format(5(1X,1pE18.10E3))
!
999 continue
    call jedema()
end subroutine
