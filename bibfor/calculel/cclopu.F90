subroutine cclopu(resuin, resuou, lisord, nbordr, lisopt,&
                  nbropt)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
#include "asterfort/rsexch.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: nbordr, nbropt
    character(len=8) :: resuin, resuou
    character(len=19) :: lisord, lisopt
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
! ----------------------------------------------------------------------
!  CALC_CHAMP - DETERMINATION DE LA LISTE D'OPTIONS DE L'UTILISATEUR
!  -    -                           -       --           -
! ----------------------------------------------------------------------
!
!  ROUTINE SERVANT A RECUPERER LA LISTE D'OPTIONS DE CALC_CHAMP
!    SOUHAITEES PAR L'UTILISATEUR
!
!  LES OPTIONS SONT FOURNIES DANS DES MOTS-CLES SIMPLES :
!    - CONTRAINTE
!    - DEFORMATION
!    - ENERGIE
!    - CRITERES
!    - VARI_INTERNE
!    - HYDRAULIQUE
!    - THERMIQUE
!    - ACOUSTIQUE
!    - FORCE
!  ET LE MOT-CLE FACTEUR CHAM_UTIL.
!
! IN  :
!   RESUIN K8   NOM DE LA SD IN
!
! IN/OUT :
!   LISOPT K19  NOM DE LA LISTE D'OPTIONS
!
! OUT :
!   NBOPT  I    NOMBRE D'OPTIONS
! ----------------------------------------------------------------------
    integer :: ntymax
    parameter (ntymax = 9)
!
    integer :: i, ityp, n1, jopt,  postmp, nbopfa, ioc, ibid
    integer :: nuti, nsup,  jord, iordr, iret
!
    character(len=9) :: mcfact
    character(len=12) :: typopt, tygrop(ntymax)
    character(len=16) :: option
    character(len=24) :: chn
    parameter   (mcfact='CHAM_UTIL')
!
    logical :: newcal, vu
    integer, pointer :: nb_op_ty(:) => null()
    character(len=16), pointer :: oputil(:) => null()
!
    data tygrop  /'CONTRAINTE  ','DEFORMATION ','ENERGIE     ',&
     &              'CRITERES    ','VARI_INTERNE','HYDRAULIQUE ',&
     &              'THERMIQUE   ','ACOUSTIQUE  ','FORCE       '/
!
    call jemarq()
!
! --- PREMIERE BOUCLE POUR DETERMINER LE NOMBRE TOTAL D'OPTIONS
    AS_ALLOCATE(vi=nb_op_ty, size=ntymax)
    nbropt = 0
    do 10 ityp = 1, ntymax
        typopt = tygrop(ityp)
        call getvtx(' ', typopt, nbval=0, nbret=n1)
        nb_op_ty(ityp) = -n1
        nbropt = nbropt-n1
10  end do
!
    call wkvect(lisopt, 'V V K16', max(1, nbropt), jopt)
!
!     DEUXIEME BOUCLE POUR REMPLIR LE TABLEAU DES OPTIONS
    postmp = 0
    do 20 ityp = 1, ntymax
        typopt = tygrop(ityp)
        nbopfa = nb_op_ty(ityp)
        if (nbopfa .eq. 0) goto 20
        call getvtx(' ', typopt, nbval=nbopfa, vect=zk16(jopt+postmp), nbret=n1)
        postmp = postmp+nbopfa
20  end do
!
! --- MOT-CLE FACTEUR CHAM_UTIL
!     POUR EVITER L'ALARME LIE AU RECALCUL D'UNE OPTION DEJA PRESENTE
!     ON REGARDE SI ELLE A DEJA ETE CALCULEE ET SI ELLE N'EST PAS DEJA
!     DANS LA LISTE DES OPTIONS DEMANDEES PAR L'UTILISATEUR
    call getfac(mcfact, nuti)
    if (nuti .eq. 0) then
        goto 9999
    endif
!
    newcal = .false.
    call jeexin(resuou//'           .DESC', iret)
    if (iret .eq. 0) newcal = .true.
!
    AS_ALLOCATE(vk16=oputil, size=nuti)
    call jeveuo(lisord, 'L', jord)
    nsup = 0
    do 30 ioc = 1, nuti
        call getvtx(mcfact, 'NOM_CHAM', iocc=ioc, scal=option, nbret=ibid)
        vu = .true.
!       OPTION PRESENTE DANS RESUIN A TOUS LES NUME_ORDRE A CALCULER ?
        do 31 i = 1, nbordr
            iordr = zi(jord-1+i)
            call rsexch(' ', resuin, option, iordr, chn,&
                        iret)
            if (iret .ne. 0) then
                if (.not.newcal) call rsexch(' ', resuou, option, iordr, chn,&
                                             iret)
                if (iret .ne. 0) then
                    vu = .false.
                    goto 32
                endif
            endif
31      continue
        goto 38
!
32      continue
!       OPTION DEJA DANS LA LISTE ?
        vu = .false.
        do 33 i = 1, nbropt
            if (zk16(jopt-1+i) .eq. option) then
                vu = .true.
                goto 30
            endif
33      continue
        do 34 i = 1, nsup
            if (oputil(i) .eq. option) then
                vu = .true.
                goto 30
            endif
34      continue
!
38      continue
!       ON AJOUTE L'OPTION A LA LISTE
        if (.not.vu) then
            nsup = nsup + 1
            oputil(nsup) = option
        endif
30  end do
!
! --- REFAIRE OU AGRANDIR LISOPT
    if (nsup .gt. 0) then
        if (nbropt .eq. 0) then
            call jedetr(lisopt)
            call wkvect(lisopt, 'V V K16', nsup, jopt)
        else
            call juveca(lisopt, nbropt+nsup)
        endif
        do 41 i = 1, nsup
            zk16(jopt-1+nbropt+i) = oputil(i)
41      continue
        nbropt = nbropt + nsup
    endif
!
9999  continue
    AS_DEALLOCATE(vi=nb_op_ty)
    AS_DEALLOCATE(vk16=oputil)
    call jedema()
!
end subroutine
