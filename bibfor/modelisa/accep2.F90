subroutine accep2(modmec, nbm, pgout, phiout, sphout)
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
!
    implicit none
!-----------------------------------------------------------------------
!     OPERATEUR PROJ_SPEC_BASE
!     CREATION DE LA MATRICE DES MODES PROPRES DEFINIS SUR LES POINTS DE
!     GAUSS ET DE LA LISTE DES POINTS DE GAUSS ASSOCIES AVEC LEURS
!     COORDONNEES
!-----------------------------------------------------------------------
! IN  : MODMEC : BASE DE MODES A EXPTRAPOLER
! IN  : NBM    : NOMBRE DE MODES PROPRES
! OUT : PGOUT  : CHAM_ELEM_S CONTENANT LES COORDONNEES DES POINTS DE
!                GAUSS ET LEURS POIDS RESPECTIFS
! OUT : PHIOUT : VECTEUR CONTENANT LES NOMS DES MODES PROPRES DEFINIS
!                AUX POINTS DE GAUSS (CHAM_ELEM_S)
! OUT : SPHOUT: VECTEUR CONTENANT LES NOMS DES CHAM_ELEM_S INITIALISES
!                A 0 COMPLEXES
!-----------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsces.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlima.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/manopg.h"
#include "asterfort/megeom.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: iret, idm, ibid, nbm, inocha, isncha, nbchin, nbchou
    character(len=6) :: chaine
    character(len=8) :: moint, modmec
    parameter    (nbchin=1,nbchou=1)
    character(len=8) :: lpain(nbchin), lpaou(nbchou)
    character(len=19) :: nochno, nochns, nches1, nches2, nchel1, mnoga
    character(len=19) :: lchin(nbchin), lchou(nbchou), pgout, phiout
    character(len=19) :: nchelc, nchesc, sphout
    character(len=24) :: ligrel, chgeom
    character(len=8) :: param
    character(len=16) :: option
!
!
!-----------------------------------------------------------------------
    call jemarq()
!
! RECUPERE LE MODELE ASSOCIE A LA BASE
    call getvid(' ', 'MODELE_INTERFACE', scal=moint, nbret=ibid)
    if (ibid .eq. 0) then
        call utmess('F', 'MODELISA10_14')
    endif
!
! NOMS DE CHAMPS PROVISOIRES
    nochns='&&ACCEP2.CHNOS'
    ligrel='&&ACCEP2.LIGREL'
    nchel1='&&ACCEP2.CHELEL'
    nchelc='&&ACCEP2.CHELELC'
    nches1='&&ACCEP2.CHELES1'
    mnoga='&&ACCEP.MNOGA'
! LIGREL ASSOCIE AU MODELE ET A UN GROUPE DE MAILLE
    call exlima(' ', 0, 'V', moint, ligrel)
!
! CREATION DES CHAMPS DES MODES PROPRES INTERPOLES SUR LEURS
! POINTS DE GAUSS - LES '&&SFIFJ.0000XX'
!
! VECTEUR DE TRAVAIL CONTENANT LES NOMS '&&SFIFJ.0000XX'
    call wkvect('&&SFIFJ.PHI', 'V V K24', nbm, inocha)
    call wkvect('&&SFIFJ.SPHI', 'V V K24', nbm, isncha)
!
! BOUCLE SUR LES NUMEROS D'ORDRE
    do 10 idm = 1, nbm
        call codent(idm, 'D', chaine)
! NCHESC : CHAM_ELEM_S COMPLEXE. UNIQUEMENT POUR INITIALISATION
        nchesc='&&SFIFJ.SPHI.'//chaine
! NCHES2 : CHAM_ELEM_S CONTENANT LES MODES INTERPOLES AUX PDG
        nches2='&&SFIFJ.PHI.'//chaine
        zk24(inocha-1+idm)=nches2
        zk24(isncha-1+idm)=nchesc
! RECUPERATION DU CHAMP CORRESPONDANT AU NUM ORDRE
        call rsexch('F', modmec, 'DEPL', idm, nochno,&
                    iret)
        call cnocns(nochno, 'V', nochns)
! FABRICATION D'UN CHAM_ELEM VIERGE (UN REEL) ET UN COMPLEXE)
        call alchml(ligrel, 'TOU_INI_ELGA', 'PDEPL_R', 'V', nchel1,&
                    iret, ' ')
        call alchml(ligrel, 'TOU_INI_ELGA', 'PDEPL_C', 'V', nchelc,&
                    iret, ' ')
        call celces(nchel1, 'V', nches1)
        call celces(nchelc, 'V', nchesc)
        call dismoi('F', 'NOM_OPTION', nchel1, 'CHAM_ELEM', ibid,&
                    option, ibid)
        call dismoi('F', 'NOM_PARAM', nchel1, 'CHAM_ELEM', ibid,&
                    param, ibid)
        call manopg(ligrel, option, param, mnoga)
! INTERPOLER LE CHAM NO SIMPLE SUR LES PDG
        call cnsces(nochns, 'ELGA', nches1, mnoga, 'V',&
                    nches2)
! DESTRUCTION DES CHAMPS TEMPORAIRES
        call detrsd('CHAM_NO_S', nochns)
        call detrsd('CHAM_ELEM_S', nches1)
        call detrsd('CHAM_ELEM', nchelc)
        call detrsd('CHAM_ELEM', nchel1)
10  end do
!
    phiout='&&SFIFJ.PHI'
    sphout='&&SFIFJ.SPHI'
!
! 2 - CREATION D'UN CHAM_ELEM_S CONTENANT LES COORDONNEES
!     DES POINTS DE GAUSS ET LEUR POIDS
    call megeom(moint, chgeom)
    lchin(1)=chgeom(1:19)
    lpain(1)='PGEOMER'
    lchou(1)='&&ACCEP2.PGCOOR'
    lpaou(1)='PCOORPG'
    call calcul('C', 'COOR_ELGA', ligrel, nbchin, lchin,&
                lpain, nbchou, lchou, lpaou, 'V',&
                'OUI')
    pgout='&&SFIFJ.PGCOOR'
    call celces(lchou(1), 'V', pgout)
!
    call jedema()
end subroutine
