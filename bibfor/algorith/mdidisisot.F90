subroutine mdidisisot(sd_nl_, nbnoli, nomres, nbsauv, temps)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*) :: sd_nl_
    integer          :: nbnoli
    character(len=8) :: nomres
    integer          :: nbsauv
    real(kind=8)     :: temps(*)
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jemarq.h"
#include "asterfort/nlget.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
!
! --------------------------------------------------------------------------------------------------
!
!                       IMPRESSION DES RESULTATS SUR LES DIS_ECRO_TRAC
!
! --------------------------------------------------------------------------------------------------
!
! IN
!   sd_nl_  : nom de structure de données pour les calculs non linéaires
!   nomres  : nom du concept résultat
!   nbnoli  : nombre de liaison non-linéaire
!   nbsauv  : nombre de pas sauvegardé
!   temps   : instant de sauvegarde
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ific, nbocc, iocc, iret, ii
    integer :: nbvint, nbvdisc, it, indx, jvint
    integer :: vv, nltype_i, start, finish, jvindx, jdesc
    integer :: imprredir(5)
    character(len=8) :: noeud1, noeud2, sd_nl
!
    call jemarq()
    sd_nl = sd_nl_

    call getfac('IMPRESSION',  nbocc)
    if ( nbocc.eq.0 ) then
        goto 999
    endif

!   Récupération des variables internes
!   Longueur maximum d'un bloc de variables internes
    call jeveuo(nomres//'           .DESC', 'L', jdesc)
    nbvint  = zi(jdesc-1+4)
    if (nbvint.eq.0) goto 999

    call jeveuo(nomres//'        .NL.VINT', 'L', jvint)
    call jeveuo(nomres//'        .NL.VIND', 'L', jvindx)
    imprredir(1:5) = [1,3,4,2,5]
!
    do iocc = 1, nbocc
        call getvis('IMPRESSION', 'UNITE_DIS_ECRO_TRAC', iocc=iocc, scal=ific, nbret=iret)
        if ( iret.ne.1 ) cycle
!       Impression des informations sur les DIS_ECRO_TRAC
        if (.not. ulexis( ific )) then
            call ulopen(ific, ' ', ' ', 'NEW', 'O')
        endif
!
        do ii = 1 , nbnoli
            call nlget(sd_nl, _NL_TYPE, iocc=ii, iscal=nltype_i)
            if ( nltype_i.ne.NL_DIS_ECRO_TRAC ) cycle

            start  = zi(jvindx-1+ii) + 7
            finish = zi(jvindx-1+ii+1)

            nbvdisc = finish - start
            ASSERT(nbvdisc.eq.5)

!           Noeuds du discret
            call nlget(sd_nl, _NO1_NAME, iocc=ii, kscal=noeud1)
            call nlget(sd_nl, _NO2_NAME, iocc=ii, kscal=noeud2)

!           Impressions des variables internes
!               variables internes   : 1        2       3       4       5
!                               vari : force    Up      U       puiss   ip
!               Ordre d'impression   : 1 3 4 2 5
            write(ific,100) '#'
            write(ific,100) '#--------------------------------------------------'
            write(ific,100) '#RESULTAT '//nomres
            write(ific,101) '#DIS_ECRO_TRAC ',ii,' '//noeud1//' '//noeud2
            write(ific,102) 'INST','FORCE','U','PUISS','UP','PCUM'
            do it = 1 , nbsauv
                indx = jvint-1 +(it-1)*nbvint + start
                write(ific,103) temps(it), (zr(indx+imprredir(vv)-1),vv=1,nbvdisc)
            enddo
        enddo
!       On ferme le fichier pour être sûr que le flush soit fait
        call ulopen(-ific, ' ', ' ', ' ', ' ')
    enddo
100 format(A)
101 format(A,I5,A)
102 format(6(1X,A18))
103 format(6(1X,1pE18.10E3))
!
999 continue
    call jedema()
end subroutine
