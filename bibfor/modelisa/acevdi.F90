subroutine acevdi(nbocc, nomaz, nomoz, mcf, nlm,&
                  nlg, nln, nlj, ier)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    integer :: nbocc, nlm, nlg, nln, nlj, ier
    character(len=*) :: nomaz, nomoz, mcf
!
! --------------------------------------------------------------------------------------------------
!
!        AFFE_CARA_ELEM
!           VERIFICATION DES MOTS CLES POUR L'ELEMENT DISCRET
!
! --------------------------------------------------------------------------------------------------
!
! IN
!     NBOCC    :  NOMBRE D'OCCURENCE
!     NOMAZ    :  NOM DU MAILLAGE
!     NOMOZ    :  NOM DU MODELE
!     MCF      :  MOT CLEF
! OUT
!     NLM      :  NOMBRE TOTAL DE MAILLE
!     NLG      :  NOMBRE TOTAL DE GROUPE DE MAILLE
!     NLN      :  NOMBRE TOTAL DE NOEUD
!     NLJ      :  NOMBRE TOTAL DE GROUP_NO
!     IER      :  ERREUR
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/acevd2.h"
#include "asterfort/assert.h"
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/verdis.h"
#include "asterfort/verima.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
! --------------------------------------------------------------------------------------------------
    integer ::  i3d, i2d, ndim1, ioc, nc, ng, nm, nsom, nbmail
    integer :: n1, ima, nbgrm,  ig, jmail, numa, nutyma, lmax2
    integer :: iarg
    character(len=4) :: type
    character(len=8) :: k8b, nomu, noma, nomo, nomail, typel
    character(len=16) :: concep, cmd
    character(len=24) :: grmama, mailma, cara, nogrm
    character(len=24) :: valk(4)
! --------------------------------------------------------------------------------------------------
    integer, pointer :: typmail(:) => null()
    character(len=24), pointer :: group_ma(:) => null()
! --------------------------------------------------------------------------------------------------
    call getres(nomu, concep, cmd)
!
    noma = nomaz
    nomo = nomoz
    nlm = 0
    nlg = 0
    nln = 0
    nlj = 0
    grmama = noma//'.GROUPEMA'
    mailma = noma//'.NOMMAI'
!
!   Vecteur du type des mailles du maillage :
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
!
!   Vérification des dimensions / modélisations
    call verdis(nomo, noma, 'E', i3d, i2d, ndim1, ier)
    ASSERT((mcf.eq.'DISCRET_2D').or.(mcf.eq.'DISCRET'))
!
!   boucle sur les occurences
    do ioc = 1, nbocc
        call getvtx(mcf, 'CARA', iocc=ioc, scal=cara, nbret=nc)
!
        call getvem(noma, 'GROUP_MA', mcf, 'GROUP_MA', ioc, iarg, 0, k8b, ng)
        call getvem(noma, 'MAILLE', mcf, 'MAILLE', ioc, iarg, 0, k8b, nm)
!
        nsom = ng + nm
        if ((nsom.eq.ng) .or. (nsom.eq.nm) ) then
            nlm = max(nlm,-nm)
            nlg = max(nlg,-ng)
        endif
!
!       Vérification du bon type de maille en fonction de cara
        if ((cara(2:7) .eq. '_T_D_N') .or. (cara(2:8) .eq. '_TR_D_N') .or.&
            (cara(2:5) .eq. '_T_N')   .or. (cara(2:6) .eq. '_TR_N')) then
            type = 'POI1'
        else
            type = 'SEG2'
        endif
!
        if (nm .ne. 0) then
            nbmail = -nm
            call wkvect('&&ACEVDI.MAILLE', 'V V K8', nbmail, jmail)
            call getvtx(mcf, 'MAILLE', iocc=ioc, nbval=nbmail, vect=zk8(jmail), nbret=n1)
            do ima = 1, nbmail
                nomail = zk8(jmail+ima-1)
                call verima(noma, nomail, 1, 'MAILLE')
                call jenonu(jexnom(mailma, nomail), numa)
                nutyma = typmail(numa)
                call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), typel)
                if (typel(1:4) .ne. type) then
                    valk(1) = nomail
                    valk(2) = type
                    valk(3) = typel
                    valk(4) = cara
                    call utmess('F', 'MODELISA_56', nk=4, valk=valk)
                endif
            enddo
            call jedetr('&&ACEVDI.MAILLE')
        endif
!
        if (ng .ne. 0) then
            nbgrm = -ng
            AS_ALLOCATE(vk24=group_ma, size=nbgrm)
            call getvtx(mcf, 'GROUP_MA', iocc=ioc, nbval=nbgrm, vect=group_ma, nbret=n1)
            do ig = 1, nbgrm
                nogrm = group_ma(ig)
                call verima(noma, nogrm, 1, 'GROUP_MA')
                call jelira(jexnom(grmama, nogrm), 'LONUTI', nbmail)
                call jeveuo(jexnom(grmama, nogrm), 'L', jmail)
                do ima = 1, nbmail
                    numa = zi(jmail+ima-1)
                    nutyma = typmail(numa)
                    call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), typel)
                    if (typel(1:4) .ne. type) then
                        call jenuno(jexnum(mailma, numa), nomail)
                        valk(1) = nomail
                        valk(2) = type
                        valk(3) = typel
                        valk(4) = cara
                        call utmess('F', 'MODELISA_56', nk=4, valk=valk)
                    endif
                enddo
            enddo
            AS_DEALLOCATE(vk24=group_ma)
        endif
!
    enddo
!
    lmax2 = max(1,nlm,nlg,nln,nlj)
    call acevd2(noma, nomo, mcf, lmax2, nbocc)
end subroutine
