subroutine op5903(nbocci, compor)
    implicit none
#include "jeveux.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterc/lctest.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nmdoki.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nbocci
    character(len=8) :: compor
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
! person_in_charge: jean-luc.flejou at edf.fr
! ======================================================================
!
!     COMMANDE:  DEFI_COMPOR MOT-CLE MULTIFIBRE
!
!
    integer :: idbor, imi, imk, iocc, irett
    integer :: ibid, nbg, nbgmax, img, ig, ig1, jnfg, iaff
    integer :: nbvf, nbv, icp, nbkit, nbnvi(2), ncomel, numlc
    character(len=8) :: materi, sdgf, mator
    character(len=16) :: nomrel, algo1d, nomkit(2), lcomel(5), comcod
    character(len=16) :: comco2, texte(2), moclef
    character(len=24) :: vnbfig, rnomgf, kgroup
!
    call jemarq()
!
!     ON RECUPERE LES RENSEIGNEMENTS DANS LA SD_GROUP_FIBRE :
!        NOMS DE TOUS LES GROUPES
!        NB MAXI DE GROUPES
!        NB DE FIBRES PAR GROUPE
    nbvf=0
    moclef='MULTIFIBRE'
!
    call getvid(' ', 'GEOM_FIBRE', scal=sdgf, nbret=ibid)
    vnbfig = sdgf//'.NB_FIBRE_GROUPE'
    rnomgf = sdgf//'.NOMS_GROUPES'
    call jeveuo(vnbfig, 'L', jnfg)
    call jelira(vnbfig, 'LONMAX', nbgmax)
    call wkvect(compor//'.CPRK', 'G V K24', 6*nbgmax+1, imk)
    call wkvect('&&OP0059.NOMS_GROUPES', 'V V K24', nbgmax, img)
    call wkvect('&&OP0059.VERIF_AFFECT', 'V V I', nbgmax, iaff)
    do 50 ig = 1, nbgmax
        zi(iaff-1+ig) = 0
50  end do
    idbor = 0
!
    do 25 iocc = 1, nbocci
        call getvtx(moclef, 'GROUP_FIBRE', iocc=iocc, nbval=0, nbret=nbg)
        nbg=-nbg
        call getvtx(moclef, 'GROUP_FIBRE', iocc=iocc, nbval=nbg, vect=zk24(img),&
                    nbret=ibid)
        call getvid(moclef, 'MATER', iocc=iocc, scal=materi, nbret=ibid)
        call getvtx(moclef, 'RELATION', iocc=iocc, scal=nomrel, nbret=ibid)
        ncomel = 1
        lcomel(ncomel) = nomrel
!        AFFECTATION ALGO_1D ANALYTIQUE OU DEBORST
        algo1d = 'ANALYTIQUE'
        call lccree(ncomel, lcomel, comco2)
        call lctest(comco2, 'MODELISATION', '1D', irett)
        if (irett .eq. 0) then
            texte(1) = '1D'
            texte(2) = nomrel
            call utmess('I', 'COMPOR1_48', nk=2, valk=texte)
            algo1d='DEBORST'
            idbor = idbor+1
        endif
!
!        POUR COMPORTEMENTS KIT_DDI A COMPLETER
        call nmdoki(moclef, ' ', nomrel, iocc, 2,&
                    nbkit, nomkit, nbnvi, ncomel, lcomel,&
                    numlc, nbv)
!        APPEL A LCINFO POUR RECUPERER LE NOMBRE DE VARIABLES INTERNES
        call lccree(ncomel, lcomel, comcod)
        call lcinfo(comcod, numlc, nbv)
!
        do 27 ig = 1, nbg
!           NUMERO CORRESPONDANT AU NOM
            call jenonu(jexnom(rnomgf, zk24(img+ig-1)), ig1)
            if (ig1 .eq. 0) then
                call utmess('F', 'MODELISA8_8', sk=zk24(img+ig-1))
            endif
            icp=imk-1+(ig1-1)*6
            zk24(icp+1) = zk24(img+ig-1)
            zk24(icp+2) = materi
            zk24(icp+3) = nomrel
            zk24(icp+4) = algo1d
            zk24(icp+5) = 'VIDE'
            write(zk24(icp+6),'(I24)') zi(jnfg-1+ig1)
            zi(iaff-1+ig1) = 1
27      continue
!        ON MET À JOUR LE NOMBRE DE VARIABLES INTERNES MAXI
        nbvf=max(nbvf,nbv)
25  end do
!
!     VERIFICATION DE L'UTILISATION DE COMP_1D
    if (nbocci .gt. 1) then
        if (idbor .ge. 1) then
            call utmess('F', 'COMPOR1_15')
        endif
    endif
!     VERIF TOUT AFFECTE AU MOINS UNE FOIS
!     ON MARQUE PAR VIDE LES GROUPES NON AFFECTES
    do 51 ig = 1, nbgmax
        if (zi(iaff-1+ig) .eq. 0) then
            call jenuno(jexnum(rnomgf, ig), kgroup)
            icp=imk-1+(ig-1)*6
            zk24(icp+1) = kgroup
            zk24(icp+2) = 'VIDE'
        endif
51  end do
!
!     ON RECUPERE LE NOM DU MATERIAU POUR LA TORSION, MIS A LA FIN
    call getvid(' ', 'MATER_SECT', scal=mator, nbret=ibid)
    zk24(imk-1+nbgmax*6+1)=mator
    call wkvect(compor//'.CPRI', 'G V I', 3, imi)
!     TYPE 3 = MULTIFIBRE
    zi(imi) = 3
    zi(imi+1) = nbvf
    zi(imi+2) = nbgmax
!
    call jedema()
end subroutine
